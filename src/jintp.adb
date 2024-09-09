with Ada.Containers.Vectors;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Directories;
with Ada.Calendar;
with Ada.Unchecked_Deallocation;
with Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;

with Jintp.Input;
with Jintp.Scanner;

package body Jintp is

   use Ada.Streams;
   use Ada.Streams.Stream_IO;
   use Ada.Calendar;
   use Ada.Containers;
   use Ada.Strings;
   use Template_Maps;

   type Value_Kind is (String_Expression_Value, Boolean_Expression_Value,
                       Integer_Expression_Value, Float_Expression_Value,
                       Dictionary_Expression_Value, List_Expression_Value);

   type Expression_Value (Kind : Value_Kind := String_Expression_Value)
   is record
      case Kind is
         when String_Expression_Value =>
            S : Unbounded_String;
         when Integer_Expression_Value =>
            I : Integer;
         when Boolean_Expression_Value =>
            B : Boolean;
         when Float_Expression_Value =>
            F : Long_Float;
         when Dictionary_Expression_Value =>
            Dictionary_Value : Dictionary;
         when List_Expression_Value =>
            List_Value : List;
      end case;
   end record;

   package Expression_Value_Vectors is new
     Ada.Containers.Vectors (Index_Type => Natural,
                             Element_Type => Expression_Value);

   use Expression_Value_Vectors;

   type List_Elements is record
      Ref_Count : Natural;
      Values : Expression_Value_Vectors.Vector;
   end record;

   function Hash (Value : Expression_Value) return Hash_Type is
   begin
      case Value.Kind is
         when String_Expression_Value =>
            return Ada.Strings.Unbounded.Hash (Value.S);
         when Integer_Expression_Value =>
            return Hash_Type (Value.I);
         when others =>
            raise Template_Error
              with "only string and integer values can be keys";
      end case;
   end Hash;

   package Association_Maps is new
     Ada.Containers.Hashed_Maps (Key_Type => Expression_Value,
                                 Element_Type => Expression_Value,
                                 Hash => Hash,
                                 Equivalent_Keys => "=");

   use Association_Maps;

   type Dictionary_Assocs is record
      Ref_Count : Natural;
      Value_Assocs : Association_Maps.Map;
   end record;

   type Expression;

   type Expression_Access is access Expression;

   type Statement_Kind is (If_Statement, Elif_Statement, Else_Statement,
                           Endif_Statement, For_Statement, Endfor_Statement,
                           Include_Statement, Macro_Statement,
                           Endmacro_Statement);

   type Parameter (Has_Default_Value : Boolean := False) is record
      Name : Unbounded_String;
      case Has_Default_Value is
         when True =>
            Default_Value : Expression_Value;
         when False =>
            null;
      end case;
   end record;

   type Parameters is array (Positive range <>) of Parameter;

   package Parameter_Vectors is new
     Ada.Containers.Vectors (Index_Type => Positive,
                             Element_Type => Parameter);

   type Statement (Kind : Statement_Kind := If_Statement) is record
      case Kind is
         when If_Statement | Elif_Statement =>
            If_Condition : Expression_Access;
         when For_Statement =>
            For_Variable_1_Name : Unbounded_String;
            For_Variable_2_Name : Unbounded_String;
            For_Expression : Expression_Access;
         when Include_Statement =>
            Filename : Unbounded_String;
         when Macro_Statement =>
            Macro_Name : Unbounded_String;
            Macro_Parameters : Parameter_Vectors.Vector;
         when others =>
            null;
      end case;
   end record;

   type Tag_Kind is (Expression_Element, Statement_Element, Comment_Element);

   subtype Element_Kind is Tag_Kind
     range Expression_Element .. Statement_Element;

   type Template_Element (Kind : Element_Kind := Expression_Element) is record
      Line : Positive;
      case Kind is
         when Expression_Element =>
            Expr : Expression_Access;
         when Statement_Element =>
            Stmt : Statement;
      end case;
   end record;

   package Template_Element_Vectors is new
     Ada.Containers.Vectors (Index_Type => Positive,
                             Element_Type => Template_Element);

   use Template_Element_Vectors;

   type Macro is record
      Parameters : Parameter_Vectors.Vector;
      Elements : Template_Element_Vectors.Vector;
   end record;

   type Macro_Access is access Macro;

   procedure Free_Macro is new Ada.Unchecked_Deallocation
     (Macro, Macro_Access);

   package Macro_Maps is new
     Ada.Containers.Hashed_Maps (Key_Type => Unbounded_String,
                                 Element_Type => Macro_Access,
                                 Hash => Hash,
                                 Equivalent_Keys => "=");

   type Template is new Ada.Finalization.Limited_Controlled with record
      Timestamp : Time;
      Filename : Unbounded_String;
      Elements : Template_Element_Vectors.Vector;
      Macros : Macro_Maps.Map;
   end record;

   overriding procedure Finalize (Self : in out Template);

   type Named_Argument is record
      Name : Unbounded_String;
      Argument : Expression_Access;
   end record;

   package Named_Argument_Vectors is new
     Ada.Containers.Vectors (Index_Type => Positive,
                             Element_Type => Named_Argument);

   use Named_Argument_Vectors;

   type Expression_Kind is (Literal, Operator, Variable, Filter, Test);

   type Expression_Access_Array is array (Positive range <>)
     of Expression_Access;

   Argument_Capacity : constant Positive := 6;

   type Expression (Kind : Expression_Kind) is record
      case Kind is
         when Literal =>
            Value : Expression_Value;
         when Variable =>
            Variable_Name : Unbounded_String;
         when Filter | Test =>
            Name : Unbounded_String;
            Arguments : Expression_Access_Array (1 .. Argument_Capacity);
         when Operator =>
            Operator_Name : Unbounded_String;
            Named_Arguments : Named_Argument_Vectors.Vector;
      end case;
   end record;

   procedure Free_Expression is new Ada.Unchecked_Deallocation
     (Expression, Expression_Access);

   procedure Delete_Expression (Expr : in out Expression_Access) is
   begin
      if Expr = null then
         return;
      end if;
      case Expr.Kind is
         when Filter | Test =>
            for I in Expr.Arguments'First ..  Expr.Arguments'Last
            loop
               Delete_Expression (Expr.Arguments (I));
            end loop;
         when Operator =>
            for E of Expr.Named_Arguments loop
               Delete_Expression (E.Argument);
            end loop;
         when others =>
            null;
      end case;
      Free_Expression (Expr);
   end Delete_Expression;

   procedure Init (Container : in out List) is
   begin
      if Container.Elements = null then
         Container.Elements := new List_Elements;
         Container.Elements.Ref_Count := 1;
      end if;
   end Init;

   procedure Init (Container : in out Dictionary) is
   begin
      if Container.Assocs = null then
         Container.Assocs := new Dictionary_Assocs;
         Container.Assocs.Ref_Count := 1;
      end if;
   end Init;

   function Element (Source : Dictionary;
                     Key : Unbounded_String)
                     return Expression_Value is
   begin
      return Source.Assocs.Value_Assocs
        ((Kind => String_Expression_Value,
          S => Key));
   end Element;

   function Find_Named_Argument (Source : Named_Argument_Vectors.Vector;
                                 Name : Unbounded_String)
                                 return Named_Argument_Vectors.Cursor
   is
      Position : Named_Argument_Vectors.Cursor := Source.First;
   begin
      while Position /= Named_Argument_Vectors.No_Element loop
         if Named_Argument_Vectors.Element (Position).Name = Name then
            return Position;
         end if;
         Position := Named_Argument_Vectors.Next (Position);
      end loop;
      return Named_Argument_Vectors.No_Element;
   end Find_Named_Argument;

   package Expression_Parser is

      function Parse
        (Scanner : in out Jintp.Scanner.Scanner_State;
         Input : in out Jintp.Input.Character_Iterator'Class;
         Settings : Environment)
         return Jintp.Expression_Access;

      function Parse_With_End
        (Input : in out Jintp.Input.Character_Iterator'Class;
         Settings : Environment)
         return Jintp.Expression_Access;

   end Expression_Parser;

   package body Expression_Parser is separate;

   package Resolvers is

      type Variable_Resolver is abstract tagged limited record
         Settings : access constant Environment;
      end record;

      function Resolve (Resolver : Variable_Resolver;
                        Name : Unbounded_String)
                        return Expression_Value is abstract;

      function Get_Macro (Resolver : Variable_Resolver;
                          Name : Unbounded_String)
                          return Macro_Access is abstract;

   end Resolvers;

   type Dictionary_Resolver is new Resolvers.Variable_Resolver with record
      Values : Dictionary;
      Template_Ref : access constant Template;
   end record;

   overriding function Resolve (Resolver : Dictionary_Resolver;
                                Name : Unbounded_String)
                                return Expression_Value;

   overriding function Get_Macro (Resolver : Dictionary_Resolver;
                       Name : Unbounded_String)
                       return Macro_Access;

   overriding function Resolve (Resolver : Dictionary_Resolver;
                                Name : Unbounded_String)
                                return Expression_Value is
   begin
      return Element (Resolver.Values, Name);
   exception
      when Constraint_Error =>
         raise Template_Error with "'" & To_String (Name) & "' is undefined";
   end Resolve;

   use Macro_Maps;

   overriding function Get_Macro (Resolver : Dictionary_Resolver;
                       Name : Unbounded_String)
                       return Macro_Access is
      Position : Macro_Maps.Cursor;
   begin
      if Resolver.Template_Ref = null then
         return null;
      end if;
      Position := Macro_Maps.Find (Resolver.Template_Ref.Macros, Name);
      if Position = Macro_Maps.No_Element then
         return null;
      end if;
      return Element (Position);
   end Get_Macro;

   function Evaluate (Source : Expression;
                      Resolver : Resolvers.Variable_Resolver'class)
                      return Expression_Value;

   package Statement_Parser is

      use Jintp.Input;

      procedure Parse (Input : in out Character_Iterator'Class;
                       Settings : Environment;
                       Result : out Statement;
                       End_Modifier : out Character);

   end Statement_Parser;

   package body Statement_Parser is separate;

   procedure Free_Template is new Ada.Unchecked_Deallocation (Template,
                                                              Template_Access);

   protected body Template_Cache is
      function Get (Path : String) return Template_Access is
         C : constant Template_Maps.Cursor
           := Templates_Map.Find (To_Unbounded_String (Path));
      begin
         if C = Template_Maps.No_Element then
            return null;
         end if;
         return Templates_Map (C);
      end Get;

      procedure Put (Path : String;
                     Template : Template_Access;
                     Max_Size : Natural;
                     Inserted : out Boolean) is
         Position : Template_Maps.Cursor;
         Insert_Succeeded : Boolean;
      begin
         if Natural (Templates_Map.Length) >= Max_Size then
            Inserted := False;
            return;
         end if;
         Templates_Map.Insert (To_Unbounded_String (Path), Template,
                               Position, Insert_Succeeded);
         if not Insert_Succeeded then
            Templates_Map.Replace_Element (Position, Template);
         end if;
         Inserted := True;
      end Put;

      function Size return Natural is
      begin
         return Natural (Templates_Map.Length);
      end Size;

      procedure Cleanup is
      begin
         for C in Templates_Map.Iterate loop
            Free_Template (Templates_Map (C));
         end loop;
      end Cleanup;

   end Template_Cache;

   Default_Environment : Environment;

   overriding procedure Finalize (Self : in out Environment) is
   begin
      Self.Cached_Templates.Cleanup;
   end Finalize;

   procedure Cleanup (Element : in out Template_Element) is
   begin
      case Element.Kind is
         when Expression_Element =>
            Delete_Expression (Element.Expr);
         when Statement_Element =>
            case Element.Stmt.Kind is
               when If_Statement | Elif_Statement =>
                  Delete_Expression (Element.Stmt.If_Condition);
               when For_Statement =>
                  Delete_Expression (Element.Stmt.For_Expression);
               when others =>
                  null;
            end case;
      end case;
   end Cleanup;

   overriding procedure Finalize (Self : in out Template) is
      Macro : Macro_Access;
   begin
      for E of Self.Elements loop
         Cleanup (E);
      end loop;

      for C in Self.Macros.Iterate loop
         Macro := Element (C);
         for E of Macro.Elements loop
            Cleanup (E);
         end loop;
         Free_Macro (Macro);
      end loop;
   end Finalize;

   type Stream_Element_Array_Access is access Stream_Element_Array;

   type File_Parser_Input is new Jintp.Input.Character_Iterator with record
      Buffer : Stream_Element_Array_Access;
      Pos : Stream_Element_Offset;
   end record;

   procedure Free_Stream_Element_Array is new Ada.Unchecked_Deallocation
     (Stream_Element_Array, Stream_Element_Array_Access);

   overriding function Next (Source : in out File_Parser_Input)
                             return Character;

   overriding procedure Back (Source : in out File_Parser_Input);

   overriding procedure Match (Source : in out File_Parser_Input;
                    Pattern : String;
                    Matches : out Boolean);

   overriding function Next (Source : in out File_Parser_Input)
                             return Character is
   begin
      Source.Pos := Source.Pos + 1;
      return Character'Val (Source.Buffer (Source.Pos - 1));
   end Next;

   overriding procedure Back (Source : in out File_Parser_Input) is
   begin
      Source.Pos := Source.Pos - 1;
   end Back;

   overriding procedure Match (Source : in out File_Parser_Input;
                               Pattern : String;
                               Matches : out Boolean) is
   begin
      if Natural (Source.Buffer'Last - Source.Pos) + 1 < Pattern'Length then
         Matches := False;
         return;
      end if;
      for I in Pattern'First .. Pattern'Last loop
         if Character'Val (Source.Buffer (Source.Pos
                           + Stream_Element_Offset (I - Pattern'First)))
           /= Pattern (I)
         then
            Matches := False;
            return;
         end if;
      end loop;
      Matches := True;
      Source.Pos := Source.Pos + Stream_Element_Offset (Pattern'Length);
   end Match;

   function End_String_Length (Kind : Tag_Kind;
                               Settings : Environment)
                               return Stream_Element_Offset is
   begin
      case Kind is
         when Expression_Element =>
            return Stream_Element_Offset (Length (Settings.Expression_End));
         when Statement_Element =>
            return Stream_Element_Offset (Length (Settings.Statement_End));
         when Comment_Element =>
            return Stream_Element_Offset (Length (Settings.Comment_End));
      end case;
   end End_String_Length;

   procedure Get_Template (Filename : String;
                           Target : out Template;
                           Settings : Environment := Default_Environment) is
      Input : File_Parser_Input;

      function Buffer_Matches (Str : Unbounded_String;
                               Index : Stream_Element_Offset)
                               return Boolean is
      begin
         if Natural (Index) + Length (Str) - 1
           > Natural (Input.Buffer'Last)
         then
            return False;
         end if;
         for I in 1 .. Length (Str) loop
            if Character'Val (Input.Buffer (Index + Stream_Element_Offset (I)
                              - 1))
              /= Element (Str, I)
            then
               return False;
            end if;
         end loop;
         return True;
      end Buffer_Matches;

      procedure Find_Start (Result : out Tag_Kind;
                            Found_Pos : out Stream_Element_Offset;
                            Modifier : out Character) is
         Modifier_Candidate : Character;
      begin
         Modifier := ' ';
         for I in Input.Pos .. Input.Buffer'Last loop
            if Buffer_Matches (Settings.Expression_Start, I) then
               Found_Pos := I;
               Result := Expression_Element;
               return;
            end if;
            if Buffer_Matches (Settings.Statement_Start, I) then
               if Natural (I) + Length (Settings.Statement_Start) + 1
                 <= Natural (Input.Buffer'Last)
               then
                  Modifier_Candidate := Character'Val
                    (Input.Buffer (I + Stream_Element_Offset (
                     Length (Settings.Statement_Start))));
                  if Modifier_Candidate = '+'
                    or else Modifier_Candidate = '-'
                  then
                     Modifier := Modifier_Candidate;
                  end if;
               end if;
               Found_Pos := I;
               Result := Statement_Element;
               return;
            end if;
            if Buffer_Matches (Settings.Comment_Start, I) then
               Found_Pos := I;
               Result := Comment_Element;
               return;
            end if;
         end loop;
         Found_Pos := Input.Buffer'Last + 1;
      end Find_Start;

      procedure Find_Comment_End (Found_Pos : out Stream_Element_Offset) is
      begin
         for I in Input.Pos .. Input.Buffer'Last loop
            if Buffer_Matches (Settings.Comment_End, I) then
               Found_Pos := I;
               return;
            end if;
         end loop;
         Found_Pos := Input.Buffer'Last + 1;
      end Find_Comment_End;

      function Buffer_Slice (Low : Stream_Element_Offset;
                             High : Stream_Element_Offset)
                             return Unbounded_String is
         Slice : Unbounded_String;
      begin
         for I in Low .. High loop
            Append (Slice, Character'Val (Input.Buffer (I)));
         end loop;
         return Slice;
      end Buffer_Slice;

      procedure SkipLinebreak is
      begin
         if Input.Pos <= Input.Buffer'Last
           and then Character'Val (Input.Buffer (Input.Pos)) = ASCII.LF
         then
            Input.Pos := Input.Pos + 1;
         elsif Input.Pos + 1 <= Input.Buffer'Last
           and then Character'Val (Input.Buffer (Input.Pos)) = ASCII.CR
           and then Character'Val (Input.Buffer (Input.Pos + 1)) = ASCII.LF
         then
            Input.Pos := Input.Pos + 2;
         end if;
      end SkipLinebreak;

      procedure Skip_Comment is
         New_Pos : Stream_Element_Offset;
      begin
         Find_Comment_End (New_Pos);
         if New_Pos <= Input.Buffer'Last then
            Input.Pos := New_Pos
              + Stream_Element_Offset (Length (Settings.Comment_End));
            if Settings.Trim_Blocks then
               SkipLinebreak;
            end if;
         end if;
      end Skip_Comment;

      function Line_Count (From : Stream_Element_Offset := 1;
                           To : Stream_Element_Offset := Input.Pos)
                           return Natural is
         Result : Natural := 0;
         Last : Stream_Element_Offset := To;
      begin
         if Character'Val (Input.Buffer (Last)) = ASCII.LF then
            Last := Last - 1;
         end if;
         for I in From .. Last loop
            if Character'Val (Input.Buffer (I)) = ASCII.LF then
               Result := Result + 1;
            end if;
         end loop;
         return Result;
      end Line_Count;

      File : File_Type;
      Kind : Tag_Kind;
      New_Expression : Expression_Access;
      New_Statement : Statement;
      New_Pos : Stream_Element_Offset;
      Input_Size : Ada.Directories.File_Size
        := Ada.Directories.Size (Filename);
      Current_Line : Positive := 1;
      Last_Pos : Stream_Element_Offset := 1;
      Modifier : Character;
      Macro_Name : Unbounded_String;
   begin
      Input.Pos := 1;
      Input.Buffer := new Stream_Element_Array
        (1 .. Stream_Element_Offset (Input_Size));
      Open (File, In_File, Filename);
      Read (File, Input.Buffer.all,
            Stream_Element_Offset (Input_Size));
      Close (File);
      Target.Filename := To_Unbounded_String (Filename);
      loop
         Find_Start (Kind, New_Pos, Modifier);
         if New_Pos <= Input.Buffer'Last then
            if New_Pos > Input.Pos then
               declare
                  Last_Char_Pos : Stream_Element_Offset := New_Pos - 1;
               begin
                  if (Settings.Lstrip_Blocks and then Modifier /= '+') and then
                    (Kind = Statement_Element or else Kind = Comment_Element)
                  then
                     while Last_Char_Pos > 0
                       and then (Character'Val (Input.Buffer (Last_Char_Pos))
                                 = ' '
                                 or else Character'Val (Input.Buffer (Last_Char_Pos))
                                 = ASCII.VT) loop
                        Last_Char_Pos := Last_Char_Pos - 1;
                     end loop;
                  end if;
                  if Modifier = '-' then
                     while Last_Char_Pos > 0
                       and then Jintp.Scanner.Is_Whitespace
                         (Character'Val (Input.Buffer (Last_Char_Pos))) loop
                        Last_Char_Pos := Last_Char_Pos - 1;
                     end loop;
                  end if;
                  New_Expression := new Expression'
                    (Kind => Literal,
                     Value => (Kind => String_Expression_Value,
                               S => Buffer_Slice (Input.Pos, Last_Char_Pos)));
               end;
               if Macro_Name = Null_Unbounded_String then
                  Target.Elements.Append ((Line => Current_Line,
                                           Kind => Expression_Element,
                                           Expr => New_Expression));
               else
                  Target.Macros.Element (Macro_Name)
                    .Elements.Append ((Line => Current_Line,
                                       Kind => Expression_Element,
                                       Expr => New_Expression));
               end if;
            end if;
            Input.Pos := New_Pos + End_String_Length (Kind, Settings);
            if Modifier = '+' or else Modifier = '-' then
               Input.Pos := Input.Pos + 1;
            end if;
            begin
               case Kind is
               when Expression_Element =>
                  New_Expression := Jintp.Expression_Parser.
                    Parse_With_End (Input, Settings);
                  Current_Line := Current_Line + Line_Count (Last_Pos,
                                                             Input.Pos - 1);
                  Last_Pos := Input.Pos;
                  if Macro_Name = Null_Unbounded_String then
                     Target.Elements.Append ((Line => Current_Line,
                                              Kind => Expression_Element,
                                              Expr => New_Expression));
                  else
                     Target.Macros.Element (Macro_Name)
                          .Elements.Append ((Line => Current_Line,
                                             Kind => Expression_Element,
                                             Expr => New_Expression));
                  end if;
               when Statement_Element =>
                  Jintp.Statement_Parser.Parse
                    (Input, Settings, New_Statement, Modifier);
                  Current_Line := Current_Line + Line_Count (Last_Pos,
                                                             Input.Pos - 1);
                  if Settings.Trim_Blocks and then Modifier /= '+' then
                     SkipLinebreak;
                  end if;
                  Last_Pos := Input.Pos;
                  if New_Statement.Kind = Macro_Statement then
                     Macro_Name := New_Statement.Macro_Name;
                     Target.Macros.Insert (
                        New_Statement.Macro_Name,
                        new Macro'
                          (Parameters => New_Statement.Macro_Parameters,
                           Elements => Template_Element_Vectors.Empty_Vector));
                  elsif New_Statement.Kind = Endmacro_Statement then
                     Macro_Name := Null_Unbounded_String;
                  else
                     if Macro_Name = Null_Unbounded_String then
                        Target.Elements.Append ((Line => Current_Line,
                                                 Kind => Statement_Element,
                                                 Stmt => New_Statement));
                     else
                        Target.Macros.Element (Macro_Name)
                          .Elements.Append ((Line => Current_Line,
                                             Kind => Statement_Element,
                                             Stmt => New_Statement));
                     end if;
                  end if;
                  if Modifier = '-' then
                     while Input.Pos <= Input.Buffer'Last and then
                       Jintp.Scanner.Is_Whitespace
                         (Character'Val (Input.Buffer (Input.Pos)))
                     loop
                        Input.Pos := Input.Pos + 1;
                     end loop;
                  end if;
               when Comment_Element =>
                  Skip_Comment;
               end case;
            exception
               when E : Template_Error =>
                  raise Template_Error with "File """ & Filename & """, line"
                    & Natural'Image (Line_Count + 1) & ": "
                    & Ada.Exceptions.Exception_Message (E);
            end;
         else
            New_Expression := new Expression'
              (Kind => Literal,
               Value => (Kind => String_Expression_Value,
                         S => Buffer_Slice (
                         Input.Pos,
                         Input.Buffer'Last)
                        )
              );
            if Macro_Name = Null_Unbounded_String then
               Target.Elements.Append ((Line => Current_Line,
                                        Kind => Expression_Element,
                                        Expr => New_Expression));
            else
               Target.Macros.Element (Macro_Name)
                 .Elements.Append ((Line => Current_Line,
                                    Kind => Expression_Element,
                                    Expr => New_Expression));
            end if;
            exit;
         end if;
      end loop;
      Free_Stream_Element_Array (Input.Buffer);
   exception
      when others =>
         Free_Stream_Element_Array (Input.Buffer);
         if Is_Open (File) then
            Close (File);
         end if;
         raise;
   end Get_Template;

   function Remove_Trailing_Zeroes (Source : Unbounded_String)
                                    return String
   is
      Last : Positive := Length (Source);
   begin
      while Last > 2
        and then Element (Source, Last) = '0'
        and then Element (Source, Last - 1) /= '.' loop
         Last := Last - 1;
      end loop;
      return Slice (Source, 1, Last);
   end Remove_Trailing_Zeroes;

   package Long_Float_IO is new
     Ada.Text_IO.Float_IO (Num => Long_Float);

   function To_Unbounded_String (Value : Expression_Value)
                                 return Unbounded_String is
      Buffer : Unbounded_String;
      First_Element : Boolean := True;
      Float_Buffer : String (1 .. 32);
      Dec_Exponent : Integer;
   begin
      case Value.Kind is
         when String_Expression_Value =>
            return Value.S;
         when Boolean_Expression_Value =>
            return To_Unbounded_String (Value.B'Image);
         when Integer_Expression_Value =>
            return Trim (To_Unbounded_String (Value.I'Image), Both);
         when Float_Expression_Value =>
            Dec_Exponent := Long_Float'Exponent (Value.F) * 3 / 10;
            if Dec_Exponent
              <= Float_Buffer'Last - Long_Float_IO.Default_Aft - 3
              and then Dec_Exponent >= -3
            then
               Long_Float_IO.Put (Float_Buffer, Value.F,
                                  Long_Float_IO.Default_Aft, 0);
               return To_Unbounded_String
                 (Remove_Trailing_Zeroes (Trim (
                    To_Unbounded_String (Float_Buffer),
                  Both)));
            end if;
            Long_Float_IO.Put (Float_Buffer, Value.F);
            return Trim (To_Unbounded_String (Float_Buffer),
                         Both);
         when List_Expression_Value =>
            Append (Buffer, '[');
            for V of Value.List_Value.Elements.Values loop
               if not First_Element then
                  Append (Buffer, ", ");
               end if;
               First_Element := False;
               Append (Buffer, "'");
               Append (Buffer, To_Unbounded_String (V));
               Append (Buffer, "'");
            end loop;
            Append (Buffer, ']');
            return Buffer;
         when Dictionary_Expression_Value =>
            Append (Buffer, '{');
            for C in Value.Dictionary_Value.Assocs.Value_Assocs.Iterate loop
               if not First_Element then
                  Append (Buffer, ", ");
               end if;
               First_Element := False;
               Append (Buffer, "'");
               Append (Buffer, To_Unbounded_String (Key (C)));
               Append (Buffer, "'");
               Append (Buffer, ": ");
               Append (Buffer, "'");
               Append (Buffer, To_Unbounded_String (Element (C)));
               Append (Buffer, "'");
            end loop;
            Append (Buffer, '}');
            return Buffer;
      end case;
   end To_Unbounded_String;

   function "<" (Left, Right : Expression_Value) return Boolean is
   begin
      if Left.Kind /= Right.Kind then
         raise Template_Error
           with "comparison not supported for values of different type";
      end if;
      case Left.Kind is
         when String_Expression_Value =>
            return Left.S < Right.S;
         when Boolean_Expression_Value =>
            return Left.B < Right.B;
         when Integer_Expression_Value =>
            return Left.I < Right.I;
         when Float_Expression_Value =>
            return Left.F < Right.F;
         when others =>
            raise Template_Error
              with "comparison not supported for this type";
      end case;
   end "<";

   function Evaluate (Source : Expression;
                      Resolver : Resolvers.Variable_Resolver'Class)
                      return Unbounded_String;
   package Filters is

      function Evaluate_Filter
        (Source : Expression;
         Resolver : Resolvers.Variable_Resolver'Class)
      return Jintp.Expression_Value;

   end Filters;

   package body Filters is separate;

   procedure Put (File : Ada.Text_IO.File_Type;
                  Item : Expression_Value) is
   begin
      Ada.Text_IO.Put (File, Item.Kind'Image);
      Ada.Text_IO.Put (File, ": ");
      Ada.Text_IO.Put (File, To_String (To_Unbounded_String (Item)));
   end Put;

   procedure Put (File : Ada.Text_IO.File_Type;
                  Item : Expression) is
   begin
      Ada.Text_IO.Put (File, Item.Kind'Image);
      Ada.Text_IO.Put (File, ':');
      case Item.Kind is
         when Literal =>
            Put (File, Item.Value);
         when Variable =>
            Ada.Text_IO.Put (File, To_String (Item.Variable_Name));
         when Operator =>
            Ada.Text_IO.Put (File, To_String (Item.Name));
            Ada.Text_IO.Put (File, '(');
            for I in Item.Arguments'First .. Item.Arguments'Last loop
               if Item.Arguments (I) = null then
                  exit;
               end if;
               Put (File, Item.Arguments (I).all);
               Ada.Text_IO.Put (File, ',');
            end loop;
         when Filter =>
            Put (File, Item.Arguments (1).all);
            Ada.Text_IO.Put ('|');
            Ada.Text_IO.Put (To_String (Item.Name));
         when Test =>
            Put (File, Item.Arguments (1).all);
            Ada.Text_IO.Put (File, " is ");
            Ada.Text_IO.Put (To_String (Item.Name));
      end case;
   end Put;

   procedure Put (File : Ada.Text_IO.File_Type;
                  Item : Statement) is
   begin
      Ada.Text_IO.Put (File, Item.Kind'Image);
      Ada.Text_IO.Put (File, ':');
      case Item.Kind is
         when If_Statement =>
            Put (File, Item.If_Condition.all);
         when others =>
            null;
      end case;
   end Put;
   pragma Unreferenced (Put);

   function Is_Numeric (Kind : Value_Kind) return Boolean is
   begin
      return Kind = Integer_Expression_Value
        or else Kind = Float_Expression_Value;
   end Is_Numeric;

   function To_Float (V : Expression_Value) return Long_Float is
   begin
      case V.Kind is
         when Integer_Expression_Value => return Long_Float (V.I);
         when Float_Expression_Value => return V.F;
         when others => raise Template_Error  with "numeric value expected";
      end case;
   end To_Float;

   function Evaluate_Add (Source : Expression;
                          Resolver : Resolvers.Variable_Resolver'Class)
                          return Expression_Value
   is
      Left_Arg : constant Expression_Value
        := Evaluate (Source.Named_Arguments (1).Argument.all, Resolver);
      Right_Arg : Expression_Value;
   begin
      if Length (Source.Named_Arguments) = 1 then
         return Left_Arg;
      end if;
      Right_Arg := Evaluate (Source.Named_Arguments (2).Argument.all, Resolver);
      if Left_Arg.Kind = String_Expression_Value
        and then Right_Arg.Kind = String_Expression_Value
      then
         return (Kind => String_Expression_Value,
                 S => Left_Arg.S & Right_Arg.S);
      end if;
      if not Is_Numeric (Left_Arg.Kind)
        or else not Is_Numeric (Right_Arg.Kind)
      then
         raise Template_Error with
           "arguments of + operator must both be either string or numeric";
      end if;
      if Left_Arg.Kind = Integer_Expression_Value
        and then Right_Arg.Kind = Integer_Expression_Value
      then
         return (Kind => Integer_Expression_Value,
                 I => Left_Arg.I + Right_Arg.I);
      end if;
      return (Kind => Float_Expression_Value,
              F => To_Float (Left_Arg) + To_Float (Right_Arg));
   end Evaluate_Add;

   function Evaluate_Subtract (Source : Expression;
                               Resolver : Resolvers.Variable_Resolver'class)
                               return Expression_Value
   is
      Left_Arg : constant Expression_Value := Evaluate
        (Source.Named_Arguments (1).Argument.all, Resolver);
      Right_Arg : Expression_Value;

   begin
      if not Is_Numeric (Left_Arg.Kind) then
         raise Template_Error with
           "arguments of - operator must be numeric";
      end if;
      if Length (Source.Named_Arguments) = 1 then
         if Left_Arg.Kind = Integer_Expression_Value
         then
            return (Kind => Integer_Expression_Value,
                    I => -Left_Arg.I);
         end if;
         return (Kind => Float_Expression_Value,
                 F => -Left_Arg.F);
      end if;
      Right_Arg := Evaluate (Source.Named_Arguments (2).Argument.all,
                                Resolver);
      if not Is_Numeric (Right_Arg.Kind) then
         raise Template_Error with
           "arguments of - operator must be numeric";
      end if;
      if Left_Arg.Kind = Integer_Expression_Value
        and then Right_Arg.Kind = Integer_Expression_Value
      then
         return (Kind => Integer_Expression_Value,
                 I => Left_Arg.I - Right_Arg.I);
      end if;
      return (Kind => Float_Expression_Value,
              F => To_Float (Left_Arg) - To_Float (Right_Arg));
   end Evaluate_Subtract;

   function Evaluate_Mul (Source : Expression;
                          Resolver : Resolvers.Variable_Resolver'Class)
                          return Expression_Value
   is
      Left_Arg : constant Expression_Value
        := Evaluate (Source.Named_Arguments (1).Argument.all, Resolver);
      Right_Arg : constant Expression_Value
        := Evaluate (Source.Named_Arguments (2).Argument.all, Resolver);
   begin
      if not Is_Numeric (Left_Arg.Kind) or else not Is_Numeric (Right_Arg.Kind)
      then
         raise Template_Error with
           "arguments of * operator must both be numeric";
      end if;
      if Left_Arg.Kind = Integer_Expression_Value
        and then Right_Arg.Kind = Integer_Expression_Value
      then
         return (Kind => Integer_Expression_Value,
                 I => Left_Arg.I * Right_Arg.I);
      end if;
      return (Kind => Float_Expression_Value,
              F => To_Float (Left_Arg) * To_Float (Right_Arg));
   end Evaluate_Mul;

   function Evaluate_Div (Source : Expression;
                          Resolver : Resolvers.Variable_Resolver'Class)
                          return Expression_Value
   is
      Left_Arg : constant Expression_Value
        := Evaluate (Source.Named_Arguments (1).Argument.all, Resolver);
      Right_Arg : constant Expression_Value
        := Evaluate (Source.Named_Arguments (2).Argument.all, Resolver);
   begin
      if not Is_Numeric (Left_Arg.Kind)
        or else not Is_Numeric (Right_Arg.Kind)
      then
         raise Template_Error with
           "arguments of / operator must both be numeric";
      end if;
      return (Kind => Float_Expression_Value,
              F => To_Float (Left_Arg) / To_Float (Right_Arg));
   end Evaluate_Div;

   function Evaluate_Integer_Div (Source : Expression;
                                  Resolver : Resolvers.Variable_Resolver'Class)
                                  return Expression_Value
   is
      Left_Arg : constant Expression_Value
        := Evaluate (Source.Named_Arguments (1).Argument.all, Resolver);
      Right_Arg : constant Expression_Value
        := Evaluate (Source.Named_Arguments (2).Argument.all, Resolver);
   begin
      if Left_Arg.Kind /= Integer_Expression_Value
        or else Right_Arg.Kind /= Integer_Expression_Value
      then
         raise Template_Error with
           "arguments of // operator must both be integer";
      end if;
      return (Kind => Integer_Expression_Value,
              I => Left_Arg.I / Right_Arg.I);
   end Evaluate_Integer_Div;

   package Long_Float_Elementary_Functions is new
     Ada.Numerics.Generic_Elementary_Functions (Float_Type => Long_Float);

   use Long_Float_Elementary_Functions;

   function Evaluate_Power (Source : Expression;
                            Resolver : Resolvers.Variable_Resolver'Class)
                            return Expression_Value
   is
      Left_Arg : constant Expression_Value
        := Evaluate (Source.Named_Arguments (1).Argument.all, Resolver);
      Right_Arg : constant Expression_Value
        := Evaluate (Source.Named_Arguments (2).Argument.all, Resolver);
   begin
      if not Is_Numeric (Left_Arg.Kind) or else not Is_Numeric (Right_Arg.Kind)
      then
         raise Template_Error with
           "arguments of * operator must both be numeric";
      end if;
      return (Kind => Float_Expression_Value,
              F => To_Float (Left_Arg) ** To_Float (Right_Arg));
   end Evaluate_Power;

   procedure Execute_Statement
     (Stmt : Statement;
      Current : in out Template_Element_Vectors.Cursor;
      Out_Buffer : in out Unbounded_String;
      Resolver : Resolvers.Variable_Resolver'class);

   function Render (Source : Template_Element_Vectors.Vector;
                    Filename : String;
                    Resolver : Resolvers.Variable_Resolver'Class)
                    return Unbounded_String is
      Out_Buffer : Unbounded_String;
      Current : Template_Element_Vectors.Cursor := First (Source);
      Element : Template_Element;
   begin
      while Current /= Template_Element_Vectors.No_Element loop
         Element := Template_Element_Vectors.Element (Current);
         begin
            case Element.Kind is
            when Expression_Element =>
               Append (Out_Buffer,
                       Evaluate (Element.Expr.all, Resolver));
            when Statement_Element =>
               Execute_Statement (Element.Stmt, Current, Out_Buffer, Resolver);
            end case;
         exception
            when E : Template_Error =>
               raise Template_Error with "File """ & Filename
                 & """, line" & Element.Line'Image & ": "
                 & Ada.Exceptions.Exception_Message (E);
         end;
         Next (Current);
      end loop;
      return Out_Buffer;
   end Render;

   function Evaluate_Operator
     (Source : Expression;
      Resolver : Resolvers.Variable_Resolver'class)
      return Expression_Value
   is
      Left_Arg : Expression_Value;
      Right_Arg : Expression_Value;
      Name : constant String := To_String (Source.Operator_Name);
      Macro : Macro_Access;
   begin
      if Name = "NOT" then
         Left_Arg := Evaluate (Source.Named_Arguments (1).Argument.all,
                               Resolver);
         if Left_Arg.Kind /= Boolean_Expression_Value then
            raise Template_Error with "boolean expression expected";
         end if;
         return (Kind => Boolean_Expression_Value,
                 B => not Left_Arg.B);
      end if;
      if Name = "OR" then
         Left_Arg := Evaluate (Source.Named_Arguments (1).Argument.all,
                               Resolver);
         if Left_Arg.Kind /= Boolean_Expression_Value then
            raise Template_Error with "boolean expression expected";
         end if;
         if Left_Arg.B then
            return (Kind => Boolean_Expression_Value,
                    B => True);
         end if;
         Right_Arg := Evaluate (Source.Named_Arguments (2).Argument.all,
                                Resolver);
         if Right_Arg.Kind /= Boolean_Expression_Value then
            raise Template_Error with "boolean expression expected";
         end if;
         return (Kind => Boolean_Expression_Value,
                 B => Right_Arg.B);
      end if;
      if Name = "AND" then
         Left_Arg := Evaluate (Source.Named_Arguments (1).Argument.all,
                               Resolver);
         if Left_Arg.Kind /= Boolean_Expression_Value then
            raise Template_Error with "boolean expression expected";
         end if;
         if not Left_Arg.B then
            return (Kind => Boolean_Expression_Value,
                    B => False);
         end if;
         Right_Arg := Evaluate (Source.Named_Arguments (2).Argument.all,
                                Resolver);
         if Right_Arg.Kind /= Boolean_Expression_Value then
            raise Template_Error with "boolean expression expected";
         end if;
         return (Kind => Boolean_Expression_Value,
                 B => Right_Arg.B);
      end if;
      if Name = "==" then
         Left_Arg := Evaluate (Source.Named_Arguments (1).Argument.all,
                               Resolver);
         Right_Arg := Evaluate (Source.Named_Arguments (2).Argument.all,
                                Resolver);
         return (Kind => Boolean_Expression_Value,
                 B => Left_Arg = Right_Arg);
      end if;
      if Name = "<" then
         Left_Arg := Evaluate (Source.Named_Arguments (1).Argument.all,
                               Resolver);
         Right_Arg := Evaluate (Source.Named_Arguments (2).Argument.all,
                                Resolver);
         return (Kind => Boolean_Expression_Value,
                 B => Left_Arg < Right_Arg);
      end if;
      if Name = "<=" then
         Left_Arg := Evaluate (Source.Named_Arguments (1).Argument.all,
                               Resolver);
         Right_Arg := Evaluate (Source.Named_Arguments (2).Argument.all,
                                Resolver);
         return (Kind => Boolean_Expression_Value,
                 B => not (Right_Arg < Left_Arg));
      end if;
      if Name = ">" then
         Left_Arg := Evaluate (Source.Named_Arguments (1).Argument.all,
                               Resolver);
         Right_Arg := Evaluate (Source.Named_Arguments (2).Argument.all,
                                Resolver);
         return (Kind => Boolean_Expression_Value,
                 B => Right_Arg < Left_Arg);
      end if;
      if Name = ">=" then
         Left_Arg := Evaluate (Source.Named_Arguments (1).Argument.all,
                               Resolver);
         Right_Arg := Evaluate (Source.Named_Arguments (2).Argument.all,
                                Resolver);
         return (Kind => Boolean_Expression_Value,
                 B => not (Left_Arg < Right_Arg));
      end if;
      if Name = "~" then
         declare
            Left_String : constant Unbounded_String
              := Evaluate (Source.Named_Arguments (1).Argument.all,
                           Resolver);
            Right_String : constant Unbounded_String
              := Evaluate (Source.Named_Arguments (2).Argument.all,
                           Resolver);
         begin
            return (Kind => String_Expression_Value,
                    S => Left_String & Right_String);
         end;
      end if;
      if Name = "+" then
         return Evaluate_Add (Source, Resolver);
      end if;
      if Name = "-" then
         return Evaluate_Subtract (Source, Resolver);
      end if;
      if Name = "*" then
         return Evaluate_Mul (Source, Resolver);
      end if;
      if Name = "/" then
         return Evaluate_Div (Source, Resolver);
      end if;
      if Name = "//" then
         return Evaluate_Integer_Div (Source, Resolver);
      end if;
      if Name = "**" then
         return Evaluate_Power (Source, Resolver);
      end if;
      if Name = "." then
         if Source.Named_Arguments (1).Argument.Kind = Variable
           and then Source.Named_Arguments (1).Argument.Variable_Name = "loop"
           and then Source.Named_Arguments (1).Argument.Kind = Variable
         then
            return Resolver.Resolve
              ("loop." & Source.Named_Arguments (2).Argument.Variable_Name);
         end if;
         Left_Arg := Evaluate (Source.Named_Arguments (1).Argument.all,
                               Resolver);
         if Left_Arg.Kind /= Dictionary_Expression_Value then
            raise Template_Error with "dictionary expected before '.'";
         end if;
         if Source.Named_Arguments (2).Argument.Kind /= Variable then
            raise Template_Error with "identifier expected after '.'";
         end if;
         begin
            return Element (Left_Arg.Dictionary_Value,
                            Source.Named_Arguments (2).Argument.Variable_Name);
         exception
            when Constraint_Error =>
               raise Template_Error with "dictionary has no attribute '"
                 & To_String (Source.Named_Arguments (2).Argument.Variable_Name)
                 & ''';
         end;
      end if;
      if Name = "[]" then
         Left_Arg := Evaluate (Source.Named_Arguments (1).Argument.all,
                               Resolver);
         Right_Arg := Evaluate (Source.Named_Arguments (2).Argument.all,
                                Resolver);
         if Left_Arg.Kind = Dictionary_Expression_Value then
            if Right_Arg.Kind /= String_Expression_Value then
               raise Template_Error with "dictionary index must be a string";
            end if;
            begin
               return Left_Arg.Dictionary_Value.Assocs.Value_Assocs
                 (Right_Arg);
            exception
            when Constraint_Error =>
               raise Template_Error with "dictionary has no attribute '"
                 & To_String (Right_Arg.S) & ''';
            end;
         elsif Left_Arg.Kind = List_Expression_Value then
            if Right_Arg.Kind /= Integer_Expression_Value then
               raise Template_Error with "list index must be integer";
            end if;
            begin
               return Left_Arg.List_Value.Elements.Values (Right_Arg.I);
            exception
               when Constraint_Error =>
                  raise Template_Error with "list has no element "
                    & Right_Arg.I'Image;
            end;
         else
            raise Template_Error with "dictionary or list expected before '['";
         end if;
      end if;
      if Name = "items" then
         raise Template_Error with "unsupported usage of 'items'";
      end if;

      Macro := Resolver.Get_Macro (Source.Operator_Name);
      if Macro /= null then
         declare
            Macro_Resolver : Dictionary_Resolver;
            Position : Named_Argument_Vectors.Cursor;
         begin
            for I in 1 .. Macro.Parameters.Length loop
               if I <= Source.Named_Arguments.Length
                 and then Source.Named_Arguments (Positive (I)).Name
                   = Null_Unbounded_String
               then
                  --  Positional parameter
                  Macro_Resolver.Values.Insert
                    (Macro.Parameters (Positive (I)).Name,
                     Evaluate
                       (Source.Named_Arguments (Positive (I)).Argument.all,
                        Resolver));
               else
                  Position := Find_Named_Argument
                    (Source.Named_Arguments,
                     Macro.Parameters (Positive (I)).Name);
                  if Position /= Named_Argument_Vectors.No_Element then
                     --  Named parameter
                     Macro_Resolver.Values.Insert
                       (Macro.Parameters (Positive (I)).Name,
                        Evaluate
                          (Named_Argument_Vectors.Element (Position).Argument.all,
                           Resolver));
                  elsif Macro.Parameters (Positive (I)).Has_Default_Value then
                     --  Default parameter value
                     Init (Macro_Resolver.Values);
                     Include (Macro_Resolver.Values.Assocs.Value_Assocs,
                              (Kind => String_Expression_Value,
                               S => Macro.Parameters (Positive (I)).Name),
                              Macro.Parameters (Positive (I)).Default_Value);
                  end if;
               end if;
            end loop;
            return (Kind => String_Expression_Value,
                   S => Render (Macro.Elements, "", Macro_Resolver));
         end;
      end if;

      raise Template_Error with "unknown operator " & To_String (Source.Operator_Name);
   end Evaluate_Operator;

   function Evaluate_Test
     (Source : Expression;
      Resolver : Resolvers.Variable_Resolver'class)
      return Expression_Value
   is
      Source_Value : Expression_Value;
   begin
      if Source.Name = "defined" then
         if Source.Arguments (1).Kind /= Variable then
            raise Template_Error with "variable expected";
         end if;
         begin
            Source_Value := Resolvers.Resolve (Resolver,
                                               Source.Arguments (1)
                                                 .Variable_Name);
            return (Kind => Boolean_Expression_Value,
                    B => True
                   );
         exception
            when Template_Error =>
               return (Kind => Boolean_Expression_Value,
                       B => False
                      );
         end;
      end if;
      if Source.Name = "undefined" then
         if Source.Arguments (1).Kind /= Variable then
            raise Template_Error with "variable expected";
         end if;
         begin
            Source_Value := Resolvers.Resolve (Resolver,
                                               Source.Arguments (1)
                                               .Variable_Name);
            return (Kind => Boolean_Expression_Value,
                    B => False
                   );
         exception
            when Template_Error =>
               return (Kind => Boolean_Expression_Value,
                       B => True
                      );
         end;
      end if;
      Source_Value := Evaluate (Source.Arguments (1).all,
                                Resolver);
      if Source.Name = "boolean" then
         return (Kind => Boolean_Expression_Value,
                 B => Source_Value.Kind = Boolean_Expression_Value);
      end if;
      if Source.Name = "false" then
         return (Kind => Boolean_Expression_Value,
                 B => Source_Value.Kind = Boolean_Expression_Value
                      and then not Source_Value.B);
      end if;
      if Source.Name = "true" then
         return (Kind => Boolean_Expression_Value,
                 B => Source_Value.Kind = Boolean_Expression_Value
                      and then Source_Value.B);
      end if;
      if Source.Name = "integer" then
         return (Kind => Boolean_Expression_Value,
                 B => Source_Value.Kind = Integer_Expression_Value);
      end if;
      if Source.Name = "even" then
         return (Kind => Boolean_Expression_Value,
                 B => Source_Value.I mod 2 = 0);
      end if;
      if Source.Name = "odd" then
         return (Kind => Boolean_Expression_Value,
                 B => Source_Value.I mod 2 = 1);
      end if;
      if Source.Name = "float" then
         return (Kind => Boolean_Expression_Value,
                 B => Source_Value.Kind = Float_Expression_Value);
      end if;
      if Source.Name = "string" then
         return (Kind => Boolean_Expression_Value,
                 B => Source_Value.Kind = String_Expression_Value);
      end if;
      if Source.Name = "sequence" then
         return (Kind => Boolean_Expression_Value,
                 B => Source_Value.Kind = List_Expression_Value
                      or else Source_Value.Kind = Dictionary_Expression_Value
                );
      end if;
      if Source.Name = "divisibleby" then
         if Source.Arguments (1) = null
           or else Source.Arguments (2) = null
         then
            raise Template_Error
              with "invalid number of arguments to 'divisibleby'";
         end if;
         declare
            Source_Value_2 : constant Expression_Value
              := Evaluate (Source.Arguments (2).all,
                           Resolver);
         begin
            if Source_Value.Kind /= Integer_Expression_Value
              or else Source_Value_2.Kind /= Integer_Expression_Value
            then
               raise Template_Error with "integer arguments expected";
            end if;
            return (Kind => Boolean_Expression_Value,
                    B => Source_Value.I mod Source_Value_2.I = 0);
         end;
      end if;
      raise Template_Error with "no test named '"
        & To_String (Source.Name) & "'";
   end Evaluate_Test;

   function Evaluate (Source : Expression;
                      Resolver : Resolvers.Variable_Resolver'class)
                      return Expression_Value is
   begin
      case Source.Kind is
         when Literal =>
            return Source.Value;
         when Variable =>
            return Resolvers.Resolve (Resolver, Source.Variable_Name);
         when Operator =>
            return Evaluate_Operator (Source, Resolver);
         when Filter =>
            return Filters.Evaluate_Filter (Source, Resolver);
         when Test =>
            return Evaluate_Test (Source, Resolver);
      end case;
   end Evaluate;

   function Evaluate (Source : Expression;
                      Resolver : Resolvers.Variable_Resolver'class)
                      return Unbounded_String is
   begin
      return To_Unbounded_String (Evaluate (Source, Resolver));
   exception
      when Template_Error =>
         case Source.Kind is
            when Variable =>
               return Null_Unbounded_String;
            when Operator =>
               if To_String (Source.Operator_Name) = "[]"
                 or else To_String (Source.Operator_Name) = "."
               then
                  return Null_Unbounded_String;
               end if;
            when others =>
               null;
         end case;
         raise;
   end Evaluate;

   procedure Process_Block_Elements
     (Current : in out Template_Element_Vectors.Cursor;
      Out_Buffer : in out Unbounded_String;
      Resolver : Resolvers.Variable_Resolver'class)
   is
      Current_Element : Template_Element;
   begin
      while Current /= Template_Element_Vectors.No_Element loop
         Current_Element := Template_Element_Vectors.Element (Current);
         case Current_Element.Kind is
            when Expression_Element =>
               Append (Out_Buffer,
                       Evaluate (Current_Element.Expr.all,
                                 Resolver));
            when Statement_Element =>
               case Current_Element.Stmt.Kind is
                  when Endif_Statement | Elif_Statement | Else_Statement
                       | Endfor_Statement =>
                     exit;
                  when others =>
                     null;
               end case;
               Execute_Statement (Current_Element.Stmt,
                                  Current,
                                  Out_Buffer,
                                  Resolver);
         end case;
         Next (Current);
      end loop;
   end Process_Block_Elements;

   procedure Skip_Block_Elements
     (Current : in out Template_Element_Vectors.Cursor;
      Resolver : Resolvers.Variable_Resolver'class)
   is
      pragma Unreferenced (Resolver);
      Current_Element : Template_Element;
      Level : Natural := 0;
   begin
      Next (Current);
      while Current /= Template_Element_Vectors.No_Element loop
         Current_Element := Template_Element_Vectors.Element (Current);
         case Current_Element.Kind is
            when Statement_Element =>
               case Current_Element.Stmt.Kind is
                  when Endif_Statement | Elif_Statement | Else_Statement
                       | Endfor_Statement =>
                     if Level = 0 then
                        exit;
                     end if;
                     Level := Level - 1;
                  when If_Statement | For_Statement =>
                     Level := Level + 1;
                  when others =>
                     null;
               end case;
            when others =>
               null;
         end case;
         Next (Current);
      end loop;
   end Skip_Block_Elements;

   procedure Execute_If
     (Condition : Expression;
      Current : in out Template_Element_Vectors.Cursor;
      Out_Buffer : in out Unbounded_String;
      Resolver : Resolvers.Variable_Resolver'class)
   is
      Condition_Value : constant Expression_Value := Evaluate
        (Condition, Resolver);
      Element : Template_Element;
   begin
      if Condition_Value.Kind /= Boolean_Expression_Value then
         raise Template_Error with "boolean expression expected";
      end if;
      if Condition_Value.B then
         Next (Current);
         Process_Block_Elements (Current, Out_Buffer, Resolver);
         Element := Template_Element_Vectors.Element (Current);
         if Element.Kind = Statement_Element
           and then Element.Stmt.Kind = Else_Statement
         then
            Skip_Block_Elements (Current, Resolver);
         end if;
      else
         Skip_Block_Elements (Current, Resolver);
         Element := Template_Element_Vectors.Element (Current);
         if Element.Kind = Statement_Element
           and then Element.Stmt.Kind = Else_Statement
         then
            Next (Current);
            Process_Block_Elements (Current, Out_Buffer, Resolver);
         end if;
      end if;
   end Execute_If;

   type Chained_Resolver is new Resolvers.Variable_Resolver with record
      Parent_Resolver : access constant Resolvers.Variable_Resolver'Class;
      Variable_Name : Unbounded_String;
      Variable_Value : Expression_Value;
      Index : Natural;
      Length : Ada.Containers.Count_Type;
   end record;

   overriding function Get_Macro (Resolver : Chained_Resolver;
                                  Name : Unbounded_String)
                                  return Macro_Access;

   overriding function Resolve (Resolver : Chained_Resolver;
                     Name : Unbounded_String)
                     return Expression_Value is
   begin
      if Name = "loop.index" then
         return (Kind => Integer_Expression_Value,
                 I => Resolver.Index + 1);
      end if;
      if Name = "loop.index0" then
         return (Kind => Integer_Expression_Value,
                 I => Resolver.Index);
      end if;
      if Name = "loop.length" then
         return (Kind => Integer_Expression_Value,
                 I => Natural (Resolver.Length));
      end if;
      if Name = "loop.revindex" then
         return (Kind => Integer_Expression_Value,
                 I => Natural (Resolver.Length) - Resolver.Index);
      end if;
      if Name = "loop.revindex0" then
         return (Kind => Integer_Expression_Value,
                 I => Natural (Resolver.Length) - Resolver.Index - 1);
      end if;
      if Name = "loop.first" then
         return (Kind => Boolean_Expression_Value,
                 B => Resolver.Index = 0);
      end if;
      if Name = "loop.last" then
         return (Kind => Boolean_Expression_Value,
                 B => Resolver.Index = Natural (Resolver.Length) - 1);
      end if;
      if Name = Resolver.Variable_Name then
         return Resolver.Variable_Value;
      end if;
      return Resolvers.Resolve (Resolver.Parent_Resolver.all, Name);
   end Resolve;

   overriding function Get_Macro (Resolver : Chained_Resolver;
                                  Name : Unbounded_String)
                                  return Macro_Access is
   begin
      return Resolvers.Get_Macro (Resolver.Parent_Resolver.all, Name);
   end Get_Macro;

   package Value_Sorting is new
     Expression_Value_Vectors.Generic_Sorting ("<" => "<");

   function Less_Case_Insensitive (Left, Right : Expression_Value)
     return Boolean is
   begin
      if Left.Kind /= Right.Kind then
         raise Template_Error
           with "comparison not supported for values of different type";
      end if;
      if Left.Kind = String_Expression_Value then
         return Ada.Characters.Handling.To_Upper (To_String (Left.S))
           < Ada.Characters.Handling.To_Upper (To_String (Right.S));
      end if;
      return Left < Right;
   end Less_Case_Insensitive;

   package Value_Sorting_Case_Insensitive is new
     Expression_Value_Vectors.Generic_Sorting
       ("<" => Less_Case_Insensitive);

   type Key_And_Value is record
      Key : Expression_Value;
      Value : Expression_Value;
   end record;

   function Value_Less (Left, Right : Key_And_Value) return Boolean is
   begin
      return Left.Value < Right.Value;
   end Value_Less;

   package Key_And_Value_Vectors is new
     Ada.Containers.Vectors (Index_Type => Natural,
                             Element_Type => Key_And_Value);

   package Key_And_Value_Sorting_By_Value is new
     Key_And_Value_Vectors.Generic_Sorting ("<" => Value_Less);

   function Value_Less_Case_Insensitive (Left, Right : Key_And_Value)
                                         return Boolean is
   begin
      if Left.Value.Kind /= Right.Value.Kind then
         raise Template_Error
           with "comparison not supported for values of different type";
      end if;
      if Left.Value.Kind = String_Expression_Value then
         return Ada.Characters.Handling.To_Upper (To_String (Left.Value.S))
           < Ada.Characters.Handling.To_Upper (To_String (Right.Value.S));
      end if;
      return Left.Value < Right.Value;
   end Value_Less_Case_Insensitive;

   package Key_And_Value_Sorting_Case_Insensitive_By_Value is new
     Key_And_Value_Vectors.Generic_Sorting ("<" => Value_Less_Case_Insensitive);

   function Evaluate_Boolean (Source : Expression;
                              Resolver : Resolvers.Variable_Resolver'Class)
                              return Boolean is
      Value : constant Expression_Value := Evaluate (Source, Resolver);
   begin
      if Value.Kind /= Boolean_Expression_Value then
         raise Template_Error
           with "argument 'case_sensitive' must be boolean";
      end if;
      return Value.B;
   end Evaluate_Boolean;

   procedure Execute_For
     (Collection : Expression;
      Variable_1_Name : Unbounded_String;
      Variable_2_Name : Unbounded_String;
      Current : in out Template_Element_Vectors.Cursor;
      Out_Buffer : in out Unbounded_String;
      Resolver : Resolvers.Variable_Resolver'Class)
   is
      Start_Cursor : constant Template_Element_Vectors.Cursor := Current;
      Loop_Resolver : aliased Chained_Resolver;
      Empty_Loop : Boolean := True;
      Value_Resolver : Chained_Resolver;
      I : Natural;

      procedure Execute_For_Items is
         Collection_Value : constant Expression_Value
           := Evaluate (Collection.Named_Arguments (1).Argument.all,
                        Resolver);
      begin
         if Collection_Value.Kind /= Dictionary_Expression_Value then
            raise Template_Error with "dictionary expected";
         end if;
         Value_Resolver.Length := Collection_Value.Dictionary_Value.Assocs
           .Value_Assocs.Length;
         I := 0;
         for C in Collection_Value.Dictionary_Value.Assocs.Value_Assocs.Iterate
         loop
            Value_Resolver.Settings := Resolver.Settings;
            Loop_Resolver.Variable_Name := Variable_1_Name;
            Loop_Resolver.Variable_Value := Key (C);
            Value_Resolver.Variable_Name := Variable_2_Name;
            Value_Resolver.Variable_Value :=
              Collection_Value.Dictionary_Value.Assocs.Value_Assocs (C);
            Value_Resolver.Parent_Resolver := Loop_Resolver'Unchecked_Access;
            Value_Resolver.Index := I;
            Current := Start_Cursor;
            Next (Current);
            Process_Block_Elements (Current, Out_Buffer, Value_Resolver);
            I := I + 1;
            Empty_Loop := False;
         end loop;
      end Execute_For_Items;

      procedure Process_Block_Elements
        (Resolver : Resolvers.Variable_Resolver'class)
      is
      begin
         Current := Start_Cursor;
         Next (Current);
         Process_Block_Elements (Current, Out_Buffer, Resolver);
         Empty_Loop := False;
      end Process_Block_Elements;

      use Key_And_Value_Vectors;

      procedure Sort_By_Key (Value_Assocs : Association_Maps.Map;
                             Case_Sensitive : Boolean;
                             Reverse_Sort : Boolean) is
         Keys : Expression_Value_Vectors.Vector;
         Value_Resolver : Chained_Resolver;
      begin
         for C in Value_Assocs.Iterate loop
            Append (Keys, Key (C));
         end loop;
         if Case_Sensitive then
            Value_Sorting.Sort (Keys);
         else
            Value_Sorting_Case_Insensitive.Sort (Keys);
         end if;
         Value_Resolver.Length := Length (Keys);
         if Reverse_Sort then
            for I in reverse 0 .. Natural (Value_Resolver.Length - 1) loop
               Loop_Resolver.Variable_Name := Variable_1_Name;
               Loop_Resolver.Variable_Value := Keys (I);
               Value_Resolver.Variable_Name := Variable_2_Name;
               Value_Resolver.Variable_Value := Value_Assocs (Keys (I));
               Value_Resolver.Parent_Resolver := Loop_Resolver'Unchecked_Access;
               Value_Resolver.Settings := Resolver.Settings;
               Value_Resolver.Index := Natural (Value_Resolver.Length) - 1 - I;
               Process_Block_Elements (Value_Resolver);
            end loop;
         else
            for I in 0 .. Natural (Value_Resolver.Length - 1) loop
               Loop_Resolver.Variable_Name := Variable_1_Name;
               Loop_Resolver.Variable_Value := Keys (I);
               Value_Resolver.Variable_Name := Variable_2_Name;
               Value_Resolver.Variable_Value := Value_Assocs (Keys (I));
               Value_Resolver.Parent_Resolver := Loop_Resolver'Unchecked_Access;
               Value_Resolver.Settings := Resolver.Settings;
               Value_Resolver.Index := I;
               Process_Block_Elements (Value_Resolver);
            end loop;
         end if;
      end Sort_By_Key;

      procedure Sort_By_Value (Value_Assocs : Association_Maps.Map;
                               Case_Sensitive : Boolean;
                               Reverse_Sort : Boolean) is
         Items : Key_And_Value_Vectors.Vector;
         Value_Resolver : Chained_Resolver;
      begin
         for C in Value_Assocs.Iterate loop
            Append (Items,
                    (Key (C), Element (C))
                   );
         end loop;
         if Case_Sensitive then
            Key_And_Value_Sorting_By_Value.Sort (Items);
         else
            Key_And_Value_Sorting_Case_Insensitive_By_Value.Sort (Items);
         end if;
         Value_Resolver.Length := Length (Items);
         if Reverse_Sort then
            for I in reverse 0 .. Natural (Value_Resolver.Length - 1) loop
               Loop_Resolver.Variable_Name := Variable_1_Name;
               Loop_Resolver.Variable_Value := Items (I).Key;
               Value_Resolver.Variable_Name := Variable_2_Name;
               Value_Resolver.Variable_Value := Items (I).Value;
               Value_Resolver.Parent_Resolver := Loop_Resolver'Unchecked_Access;
               Value_Resolver.Settings := Resolver.Settings;
               Value_Resolver.Index := Natural (Value_Resolver.Length) - 1 - I;
               Process_Block_Elements (Value_Resolver);
            end loop;
         else
            for I in 0 .. Natural (Value_Resolver.Length - 1) loop
               Loop_Resolver.Variable_Name := Variable_1_Name;
               Loop_Resolver.Variable_Value := Items (I).Key;
               Value_Resolver.Variable_Name := Variable_2_Name;
               Value_Resolver.Variable_Value := Items (I).Value;
               Value_Resolver.Parent_Resolver := Loop_Resolver'Unchecked_Access;
               Value_Resolver.Settings := Resolver.Settings;
               Value_Resolver.Index := I;
               Process_Block_Elements (Value_Resolver);
            end loop;
         end if;
      end Sort_By_Value;

      procedure Execute_For_Dictsort is
         Collection_Value : constant Expression_Value
           := Evaluate (Collection.Arguments (1).all,
                        Resolver);
         Case_Sensitive_Argument : constant Expression_Access
           := Collection.Arguments (2);
         Case_Sensitive : Boolean := False;
         By_Argument : constant Expression_Access
           := Collection.Arguments (3);
         By_Value : Expression_Value;
         By_Key : Boolean := True;
         Reverse_Argument : constant Expression_Access
           := Collection.Arguments (4);
         Reverse_Sort : Boolean := False;
      begin
         if Collection_Value.Kind /= Dictionary_Expression_Value then
            raise Template_Error with "dictionary expected";
         end if;
         if Case_Sensitive_Argument /= null then
            Case_Sensitive := Evaluate_Boolean (Case_Sensitive_Argument.all,
                                                Resolver);
         end if;
         if By_Argument /= null then
            By_Value := Evaluate (By_Argument.all, Resolver);
            if By_Value.Kind /= String_Expression_Value then
               raise Template_Error
                 with "you can sort either by 'key' or by 'value'";
            end if;
            if By_Value.S = "key" then
               By_Key := True;
            elsif By_Value.S = "value" then
               By_Key := False;
            else
               raise Template_Error
                 with "you can sort either by 'key' or by 'value'";
            end if;
         end if;
         if Reverse_Argument /= null then
            Reverse_Sort := Evaluate_Boolean (Reverse_Argument.all,
                                              Resolver);
         end if;
         if By_Key then
            Sort_By_Key (Collection_Value.Dictionary_Value.Assocs.Value_Assocs,
                         Case_Sensitive,
                         Reverse_Sort);
         else
            Sort_By_Value
              (Collection_Value.Dictionary_Value.Assocs.Value_Assocs,
               Case_Sensitive,
               Reverse_Sort);
         end if;
      end Execute_For_Dictsort;

      procedure Execute_For_Default is
         Collection_Value : Expression_Value := Evaluate (Collection, Resolver);
      begin
         case Collection_Value.Kind is
            when List_Expression_Value =>
               Init (Collection_Value.List_Value);
               Loop_Resolver.Length := Length
                 (Collection_Value.List_Value.Elements.Values);
               for I in 0 .. Natural (Loop_Resolver.Length - 1) loop
                  Loop_Resolver.Variable_Name := Variable_1_Name;
                  Loop_Resolver.Variable_Value :=
                    Collection_Value.List_Value.Elements.Values (I);
                  Loop_Resolver.Index := I;
                  Current := Start_Cursor;
                  Next (Current);
                  Process_Block_Elements (Current, Out_Buffer, Loop_Resolver);
                  Empty_Loop := False;
               end loop;
            when Dictionary_Expression_Value =>
               if Variable_2_Name /= Null_Unbounded_String then
                  raise Template_Error
                    with "too many values to unpack (expected 2)";
               end if;
               declare
                  I : Natural := 0;
               begin
                  Loop_Resolver.Length := Length
                    (Collection_Value.Dictionary_Value.Assocs.Value_Assocs);
                  for C in Collection_Value.Dictionary_Value.Assocs.Value_Assocs
                    .Iterate loop
                     Loop_Resolver.Variable_Name := Variable_1_Name;
                     Loop_Resolver.Variable_Value := Key (C);
                     Loop_Resolver.Index := I;
                     Current := Start_Cursor;
                     Next (Current);
                     Process_Block_Elements (Current,
                                             Out_Buffer,
                                             Loop_Resolver);
                     Empty_Loop := False;
                     I := I + 1;
                  end loop;
               end;
            when others =>
               raise Template_Error with "list or dictionary expected";
         end case;
      end Execute_For_Default;

   begin
      Loop_Resolver.Parent_Resolver := Resolver'Unchecked_Access;
      Loop_Resolver.Settings := Resolver.Settings;
      if Collection.Kind = Operator
        and then Collection.Operator_Name = "items"
      then
         Execute_For_Items;
      elsif Collection.Kind = Filter
        and then Collection.Name = "dictsort"
      then
         Execute_For_Dictsort;
      else
         Execute_For_Default;
      end if;
      declare
         Element : constant Template_Element
           := Template_Element_Vectors.Element (Current);
      begin
         if Element.Kind = Statement_Element
           and then Element.Stmt.Kind = Else_Statement
         then
            if Empty_Loop then
               Next (Current);
               Process_Block_Elements (Current, Out_Buffer, Resolver);
            else
               Skip_Block_Elements (Current, Resolver);
            end if;
         end if;
      end;
   end Execute_For;

   procedure Execute_Include (Filename : String;
                              Out_Buffer : in out Unbounded_String;
                              Resolver : Resolvers.Variable_Resolver'Class) is
      Included_Template : Template;
      Current : Template_Element_Vectors.Cursor;
   begin
      Get_Template (Filename, Included_Template, Resolver.Settings.all);
      Current := First (Included_Template.Elements);
      Process_Block_Elements (Current, Out_Buffer, Resolver);
   end Execute_Include;

   procedure Execute_Statement
     (Stmt : Statement;
      Current : in out Template_Element_Vectors.Cursor;
      Out_Buffer : in out Unbounded_String;
      Resolver : Resolvers.Variable_Resolver'Class) is
   begin
      case Stmt.Kind is
         when If_Statement =>
            Execute_If (Stmt.If_Condition.all,
                        Current,
                        Out_Buffer,
                        Resolver);
         when For_Statement =>
            Execute_For (Stmt.For_Expression.all,
                         Stmt.For_Variable_1_Name,
                         Stmt.For_Variable_2_Name,
                         Current,
                         Out_Buffer,
                         Resolver);
         when Include_Statement =>
            Execute_Include (To_String (Stmt.Filename),
                             Out_Buffer,
                             Resolver);
         when others =>
            null;
      end case;
   end Execute_Statement;

   procedure Configure (Settings : in out Environment;
                        Expression_Start : String := Default_Expression_Start;
                        Expression_End : String := Default_Expression_End;
                        Statement_Start : String := Default_Statement_Start;
                        Statement_End : String := Default_Statement_End;
                        Comment_Start : String := Default_Comment_Start;
                        Comment_End : String := Default_Comment_End;
                        Max_Cache_Size : Natural := 200;
                        Trim_Blocks : Boolean := False;
                        Lstrip_Blocks : Boolean := False)
   is
   begin
      Settings.Expression_Start := To_Unbounded_String (Expression_Start);
      Settings.Expression_End := To_Unbounded_String (Expression_End);
      Settings.Statement_Start := To_Unbounded_String (Statement_Start);
      Settings.Statement_End := To_Unbounded_String (Statement_End);
      Settings.Comment_Start := To_Unbounded_String (Comment_Start);
      Settings.Comment_End := To_Unbounded_String (Comment_End);
      Settings.Max_Cache_Size := Max_Cache_Size;
      Settings.Trim_Blocks := Trim_Blocks;
      Settings.Lstrip_Blocks := Lstrip_Blocks;
   end Configure;

   overriding function "=" (Left, Right : Dictionary) return Boolean is
   begin
      if Left.Assocs = null or else Right.Assocs = null then
         return Left.Assocs = Right.Assocs;
      end if;
      return Left.Assocs.Value_Assocs = Right.Assocs.Value_Assocs;
   end "=";

   overriding function "=" (Left, Right : List) return Boolean is
   begin
      if Left.Elements = null or else Right.Elements = null then
         return Left.Elements = Right.Elements;
      end if;
      return Left.Elements.Values = Right.Elements.Values;
   end "=";

   function Render (Source : Template'Class;
                    Values : Dictionary;
                    Settings : Environment'Class)
                    return Unbounded_String is
      Resolver : constant Dictionary_Resolver :=
        (Settings => Settings'Unchecked_Access,
         Values => Values,
         Template_Ref => Source'Unchecked_Access);
   begin
      return Render (Source.Elements, To_String (Source.Filename),
                     Resolver);
   end Render;

   function Render (Filename : String;
                    Values : Dictionary;
                    Settings : in out Environment'Class)
                    return Unbounded_String is
      New_Template : Template_Access :=
        Settings.Cached_Templates.Get (Filename);
      File_Time : Time;
      Must_Free : Boolean := False;
      Inserted : Boolean;
   begin
      File_Time := Ada.Directories.Modification_Time (Filename);
      if New_Template = null or else File_Time > New_Template.Timestamp then
         begin
            New_Template := new Template;
            New_Template.Timestamp := File_Time;
            Get_Template (Filename, New_Template.all, Environment (Settings));
            Settings.Cached_Templates.Put (Filename,
                                           New_Template,
                                           Settings.Max_Cache_Size,
                                           Inserted);
         exception
            when others =>
               Free_Template (New_Template);
               raise;
         end;
         Must_Free := not Inserted;
      end if;
      if Must_Free then
         declare
            Result : constant Unbounded_String
              := Render (New_Template.all, Values, Settings);
         begin
            Free_Template (New_Template);
            return Result;
         end;
      end if;
      return Render (New_Template.all, Values, Settings);
   exception
      when Name_Error =>
         raise Template_Error with "template not found: " & Filename;
   end Render;

   function Render (Filename : String;
                    Values : Dictionary;
                    Settings : in out Environment'Class)
                    return String is
   begin
      return To_String (Render (Filename, Values, Settings));
   end Render;

   function Render (Filename : String;
                    Values : Dictionary)
                    return String is
   begin
      return Render (Filename, Values, Default_Environment);
   end Render;

   function Render (Filename : String;
                    Values : Dictionary)
                    return Unbounded_String is
   begin
      return Render (Filename, Values, Default_Environment);
   end Render;

   procedure Insert (Container : in out Dictionary;
                     Key : Unbounded_String;
                     New_Item : Unbounded_String) is
   begin
      Init (Container);
      Include (Container.Assocs.Value_Assocs,
              (Kind => String_Expression_Value,
               S => Key),
              (Kind => String_Expression_Value,
               S => New_Item));
   end Insert;

   procedure Insert (Container : in out Dictionary;
                     Key : String;
                     New_Item : Unbounded_String) is
   begin
      Insert (Container, To_Unbounded_String (Key), New_Item);
   end Insert;

   procedure Insert (Container : in out Dictionary;
                     Key : String;
                     New_Item : String) is
   begin
      Insert (Container, Key, To_Unbounded_String (New_Item));
   end Insert;

   procedure Insert (Container : in out Dictionary;
                     Key : Unbounded_String;
                     New_Item : String) is
   begin
      Insert (Container, Key, To_Unbounded_String (New_Item));
   end Insert;

   procedure Insert (Container : in out Dictionary;
                     Key : String;
                     New_Item : Integer) is
   begin
      Init (Container);
      Include (Container.Assocs.Value_Assocs,
              (Kind => String_Expression_Value,
               S => To_Unbounded_String (Key)),
              (Kind => Integer_Expression_Value,
               I => New_Item));
   end Insert;

   procedure Insert (Container : in out Dictionary;
                     Key : Unbounded_String;
                     New_Item : Boolean) is
   begin
      Include (Container.Assocs.Value_Assocs,
              (Kind => String_Expression_Value,
               S => Key),
              (Kind => Boolean_Expression_Value,
               B => New_Item));
   end Insert;

   function Refers (Source : Dictionary'Class;
                    Target : Dictionary)
                    return Boolean is
   begin
      if Source.Assocs = null then
         return False;
      end if;
      if Source.Assocs = Target.Assocs then
         return True;
      end if;
      for C in Source.Assocs.Value_Assocs.Iterate loop
         case Element (C).Kind is
            when Dictionary_Expression_Value =>
               if Refers (Element (C).Dictionary_Value, Target) then
                  return True;
               end if;
            when List_Expression_Value =>
               if Refers (Element (C).List_Value, Target) then
                  return True;
               end if;
            when others =>
               null;
         end case;
      end loop;
      return False;
   end Refers;

   function Refers (Source : Dictionary'Class;
                    Target : List)
                    return Boolean is
   begin
      if Source.Assocs = null then
         return False;
      end if;
      for C in Source.Assocs.Value_Assocs.Iterate loop
         case Element (C).Kind is
            when Dictionary_Expression_Value =>
               if Refers (Element (C).Dictionary_Value, Target) then
                  return True;
               end if;
            when List_Expression_Value =>
               if Refers (Element (C).List_Value, Target) then
                  return True;
               end if;
            when others =>
               null;
         end case;
      end loop;
      return False;
   end Refers;

   function Refers (Source : List'Class;
                    Target : Dictionary)
                    return Boolean is
   begin
      if Source.Elements = null then
         return False;
      end if;
      for E of Source.Elements.Values loop
         case E.Kind is
            when Dictionary_Expression_Value =>
               if Refers (E.Dictionary_Value, Target) then
                  return True;
               end if;
            when List_Expression_Value =>
               if Refers (E.List_Value, Target) then
                  return True;
               end if;
            when others =>
               null;
         end case;
      end loop;
      return False;
   end Refers;

   function Refers (Source : List'Class;
                   Target : List)
                   return Boolean is
   begin
      if Source.Elements = null then
         return False;
      end if;
      if Source.Elements = Target.Elements then
         return True;
      end if;
      for E of Source.Elements.Values loop
         case E.Kind is
            when Dictionary_Expression_Value =>
               if Refers (E.Dictionary_Value, Target) then
                  return True;
               end if;
            when List_Expression_Value =>
               if Refers (E.List_Value, Target) then
                  return True;
               end if;
            when others =>
               null;
         end case;
      end loop;
      return False;
   end Refers;

   procedure Insert (Container : in out Dictionary;
                     Key : Unbounded_String;
                     New_Item : List'Class) is
   begin
      Init (Container);
      Include (Container.Assocs.Value_Assocs,
              (Kind => String_Expression_Value,
               S => Key),
              (Kind => List_Expression_Value,
               List_Value => List (New_Item)));
   end Insert;

   procedure Insert (Container : in out Dictionary;
                     Key : Unbounded_String;
                     New_Item : Dictionary) is
   begin
      Init (Container);
      Include (Container.Assocs.Value_Assocs,
              (Kind => String_Expression_Value,
               S => Key),
              (Kind => Dictionary_Expression_Value,
               Dictionary_Value => New_Item));
   end Insert;

   procedure Insert (Container : in out Dictionary;
                     Key : String;
                     New_Item : Dictionary) is
   begin
      Insert (Container, To_Unbounded_String (Key), New_Item);
   end Insert;

   procedure Insert (Container : in out Dictionary;
                     Key : String;
                     New_Item : Boolean) is
   begin
      Insert (Container, To_Unbounded_String (Key), New_Item);
   end Insert;

   procedure Insert (Container : in out Dictionary;
                     Key : String;
                     New_Item : List'Class) is
   begin
      Insert (Container, To_Unbounded_String (Key), New_Item);
   end Insert;

   procedure Insert (Container : in out Dictionary;
                     Key : Integer;
                     New_Item : String) is
   begin
      Init (Container);
      Include (Container.Assocs.Value_Assocs,
              (Kind => Integer_Expression_Value,
               I => Key),
              (Kind => String_Expression_Value,
               S => To_Unbounded_String (New_Item)));
   end Insert;

   procedure Insert (Container : in out Dictionary;
                     Key : Integer;
                     New_Item : Unbounded_String) is
   begin
      Init (Container);
      Include (Container.Assocs.Value_Assocs,
              (Kind => Integer_Expression_Value,
               I => Key),
              (Kind => String_Expression_Value,
               S => New_Item));
   end Insert;

   procedure Insert (Container : in out Dictionary;
                     Key : Integer;
                     New_Item : Integer) is
   begin
      Init (Container);
      Include (Container.Assocs.Value_Assocs,
              (Kind => Integer_Expression_Value,
               I => Key),
              (Kind => Integer_Expression_Value,
               I => New_Item));
   end Insert;

   procedure Insert (Container : in out Dictionary;
                     Key : Integer;
                     New_Item : Boolean) is
   begin
      Init (Container);
      Include (Container.Assocs.Value_Assocs,
              (Kind => Integer_Expression_Value,
               I => Key),
              (Kind => Boolean_Expression_Value,
               B => New_Item));
   end Insert;

   procedure Insert (Container : in out Dictionary;
                     Key : Integer;
                     New_Item : List'Class) is
   begin
      Init (Container);
      Include (Container.Assocs.Value_Assocs,
              (Kind => Integer_Expression_Value,
               I => Key),
              (Kind => List_Expression_Value,
               List_Value => List (New_Item)));
   end Insert;

   procedure Insert (Container : in out Dictionary;
                     Key : Integer;
                     New_Item : Dictionary'Class) is
   begin
      Init (Container);
      Include (Container.Assocs.Value_Assocs,
              (Kind => Integer_Expression_Value,
               I => Key),
              (Kind => Dictionary_Expression_Value,
               Dictionary_Value => Dictionary (New_Item)));
   end Insert;

   procedure Clear (Container : in out Dictionary) is
   begin
      if Container.Assocs /= null then
         Clear (Container.Assocs.Value_Assocs);
      end if;
   end Clear;

   procedure Free_Assocs is new Ada.Unchecked_Deallocation (Dictionary_Assocs,
                                                            Assocs_Access);

   procedure Free_Elements is new Ada.Unchecked_Deallocation
     (List_Elements,
      List_Elements_Access);

   procedure Finalize (Value : in out Expression_Value) is
   begin
      case Value.Kind is
         when Dictionary_Expression_Value => Finalize (Value.Dictionary_Value);
         when List_Expression_Value => Finalize (Value.List_Value);
      when others =>
         null;
      end case;
   end Finalize;

   procedure Delete_Assocs (Assocs : in out Assocs_Access) is
   begin
      for E in Assocs.Value_Assocs.Iterate loop
         Finalize (Assocs.Value_Assocs (Key (E)));
      end loop;
      Free_Assocs (Assocs);
   end Delete_Assocs;

   overriding procedure Adjust (D : in out Dictionary) is
   begin
      if D.Assocs /= null then
         D.Assocs.Ref_Count := D.Assocs.Ref_Count + 1;
      end if;
   end Adjust;

   overriding procedure Finalize (D : in out Dictionary) is
      Assocs : Assocs_Access := D.Assocs;
   begin
      D.Assocs := null;

      if Assocs /= null then
         Assocs.Ref_Count := Assocs.Ref_Count - 1;
         if Assocs.Ref_Count = 0 then
            Delete_Assocs (Assocs);
         end if;
      end if;
   end Finalize;

   procedure Delete_Elements (Elements : in out List_Elements_Access) is
   begin
      for E of Elements.Values loop
         Finalize (E);
      end loop;
      Free_Elements (Elements);
   end Delete_Elements;

   overriding procedure Adjust (L : in out List) is
   begin
      if L.Elements /= null then
         L.Elements.Ref_Count := L.Elements.Ref_Count + 1;
      end if;
   end Adjust;

   overriding procedure Finalize (L : in out List) is
      Elements : List_Elements_Access := L.Elements;
   begin
      L.Elements := null;

      if Elements /= null then
         Elements.Ref_Count := Elements.Ref_Count - 1;
         if Elements.Ref_Count = 0 then
            Delete_Elements (Elements);
         end if;
      end if;
   end Finalize;

   procedure Append (Container : in out List;
                     New_Item : Unbounded_String) is
   begin
      Init (Container);
      Container.Elements.Values.Append ((Kind => String_Expression_Value,
                                         S => New_Item));
   end Append;

   procedure Append (Container : in out List;
                     New_Item : String) is
   begin
      Append (Container, To_Unbounded_String (New_Item));
   end Append;

   procedure Append (Container : in out List;
                     New_Item : Dictionary'Class) is
   begin
      Init (Container);
      Container.Elements.Values.Append
        ((Kind => Dictionary_Expression_Value,
          Dictionary_Value => Dictionary (New_Item)));
   end Append;

   procedure Append (Container : in out List;
                     New_Item : List) is
   begin
      Init (Container);
      Container.Elements.Values.Append ((Kind => List_Expression_Value,
                                         List_Value => New_Item));
   end Append;

   procedure Clear (Container : in out List) is
   begin
      if Container.Elements /= null then
         Clear (Container.Elements.Values);
      end if;
   end Clear;

   procedure Register_Filter (Settings : in out Environment;
                              Filter : Filter_Function;
                              Name : String) is
      Position  : Filter_Maps.Cursor;
      Inserted  : Boolean;
   begin
      Settings.Filters.Insert (To_Unbounded_String (Name),
                               Filter,
                               Position,
                               Inserted
                              );
      if not Inserted then
         Settings.Filters.Replace_Element (Position, Filter);
      end if;
   end Register_Filter;

end Jintp;
