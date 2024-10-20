with Ada.Containers.Vectors;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Directories;
with Ada.Calendar;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Fixed;
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

   Empty_String_Value : constant Expression_Value
     := (Kind => String_Expression_Value,
         S => Null_Unbounded_String);

   type Expression;

   type Expression_Access is access Expression;

   type Statement_Kind is (If_Statement, Elif_Statement, Else_Statement,
                           Endif_Statement, For_Statement, Endfor_Statement,
                           Include_Statement, Macro_Statement,
                           Endmacro_Statement, Raw_Statement, Endraw_Statement,
                           Extends_Statement, Block_Statement,
                           Endblock_Statement);

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
            For_Condition : Expression_Access;
         when Include_Statement =>
            Filename : Unbounded_String;
         when Macro_Statement =>
            Macro_Name : Unbounded_String;
            Macro_Parameters : Parameter_Vectors.Vector;
         when Extends_Statement =>
            Parent_Name : Unbounded_String;
         when Block_Statement =>
            Block_Name : Unbounded_String;
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

   package Block_Maps is new
     Ada.Containers.Hashed_Maps (Key_Type => Unbounded_String,
                                 Element_Type => Positive,
                                 Hash => Hash,
                                 Equivalent_Keys => "=");

   use Block_Maps;

   type Template is new Ada.Finalization.Limited_Controlled with record
      Timestamp : Time;
      Filename : Unbounded_String;
      Elements : Template_Element_Vectors.Vector;
      Macros : Macro_Maps.Map;
      Block_Map : Block_Maps.Map;
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

   type Expression_Kind is (Literal, Operator_Super, Operator_Not, Operator_Or,
                            Operator_And, Operator_Eq, Operator_Neq,
                            Operator_Lt, Operator_Le, Operator_Gt, Operator_Ge,
                            Operator_Tilde, Operator_Plus, Operator_Minus,
                            Operator_Mul, Operator_Div, Operator_Integer_Div,
                            Operator_Power, Operator_Dot, Operator_Brackets,
                            Operator_Items, Operator_Macro,
                            Variable, Filter, Test);

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
         when Operator_Super .. Operator_Items =>
            Named_Arguments : Named_Argument_Vectors.Vector;
         when Operator_Macro =>
            Macro_Name : Unbounded_String;
            Macro_Arguments : Named_Argument_Vectors.Vector;
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
         when Operator_Super .. Operator_Items =>
            for E of Expr.Named_Arguments loop
               Delete_Expression (E.Argument);
            end loop;
         when Operator_Macro =>
            for E of Expr.Macro_Arguments loop
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
         Settings : Environment'Class)
         return Jintp.Expression_Access;

      function Parse_With_End
        (Input : in out Jintp.Input.Character_Iterator'Class;
         Settings : Environment'Class)
         return Jintp.Expression_Access;

   end Expression_Parser;

   package body Expression_Parser is separate;

   package Template_Access_Vectors is new
     Ada.Containers.Vectors (Index_Type => Positive,
                             Element_Type => Template_Access);

   type Environment_Access is access all Environment'Class;

   function To_String (Value : Expression_Value)
                       return String;

   package Contexts is

      type Context is abstract tagged limited record
         null;
      end record;

      function Resolve (Resolver : Context;
                        Name : Unbounded_String)
                        return Expression_Value is abstract;

      function Get_Macro (Resolver : Context;
                          Name : Unbounded_String)
                          return Macro_Access is abstract;

      procedure Append (Resolver : Context;
                        Target : in out Unbounded_String;
                        Name : Unbounded_String);

      function Get_Environment (Resolver : Context)
                                return Environment_Access is abstract;

      function Current_Template (Resolver : Context)
                                 return Template_Access is abstract;

      function Current_Template_Index (Resolver : Context)
                                 return Natural is abstract;

      procedure Set_Current_Template_Index (Resolver : in out Context;
                                            Index : Positive)
      is abstract;

      function Template_Count (Resolver : Context)
                               return Natural
                               is abstract;

      function Get_Template (Resolver : Context;
                             Index : Positive)
                             return Template_Access
                             is abstract;

      procedure Add_Parent_Template (Resolver : in out Context;
                                    Item : Template_Access)
                                    is abstract;

      function Current_Block_Name (Resolver : Context)
                                   return Unbounded_String is abstract;

      procedure Set_Current_Block_Name (Resolver : in out Context;
                                        Block_Name : Unbounded_String)
      is abstract;

   end Contexts;

   package body Contexts is

      procedure Append (Resolver : Context;
                        Target : in out Unbounded_String;
                        Name : Unbounded_String) is
      begin
         Append (Target, To_String (Resolve (Context'Class (Resolver), Name)));
      end Append;

   end Contexts;

   type Root_Context is new Contexts.Context with record
      Settings : Environment_Access;
      Template_Refs : Template_Access_Vectors.Vector;
      Template_Index : Positive;
      Block_Name : Unbounded_String;
      Values : Dictionary;
   end record;

   overriding function Resolve (Resolver : Root_Context;
                                Name : Unbounded_String)
                                return Expression_Value;

   overriding function Get_Macro (Resolver : Root_Context;
                                  Name : Unbounded_String)
                                  return Macro_Access;

   overriding procedure Append (Resolver : Root_Context;
                                Target : in out Unbounded_String;
                                Name : Unbounded_String);

   overriding function Get_Environment (Resolver : Root_Context)
                                        return Environment_Access;

   overriding function Current_Template (Resolver : Root_Context)
                                         return Template_Access;

   overriding function Current_Template_Index (Resolver : Root_Context)
                                         return Natural;

   overriding procedure Set_Current_Template_Index
     (Resolver : in out Root_Context;
      Index : Positive);

   overriding function Template_Count (Resolver : Root_Context)
                                       return Natural;

   overriding function Get_Template (Resolver : Root_Context;
                                     Index : Positive)
                                     return Template_Access;

   overriding procedure Add_Parent_Template (Resolver : in out Root_Context;
                                             Item : Template_Access);

   overriding function Current_Block_Name (Resolver : Root_Context)
                                           return Unbounded_String;

   overriding procedure Set_Current_Block_Name
     (Resolver : in out Root_Context;
      Block_Name : Unbounded_String);

   overriding function Resolve (Resolver : Root_Context;
                                Name : Unbounded_String)
                                return Expression_Value is
   begin
      return Element (Resolver.Values, Name);
   exception
      when Constraint_Error =>
         raise Template_Error with "'" & To_String (Name) & "' is undefined";
   end Resolve;

   use Macro_Maps;

   overriding procedure Append (Resolver : Root_Context;
                                Target : in out Unbounded_String;
                                Name : Unbounded_String) is
   begin
      Append (Target,
              To_String (Association_Maps.Constant_Reference
                  (Container => Resolver.Values.Assocs.Value_Assocs,
                   Key  => (Kind => String_Expression_Value,
                            S => Name))));
   exception
      when Constraint_Error =>
         null;
   end Append;

   overriding function Get_Macro (Resolver : Root_Context;
                       Name : Unbounded_String)
                       return Macro_Access is
      Position : Macro_Maps.Cursor;
   begin
      if Resolver.Template_Refs.Is_Empty then
         return null;
      end if;
      Position := Macro_Maps.Find
        (Resolver.Template_Refs.Element (Resolver.Current_Template_Index).Macros, Name);
      if Position = Macro_Maps.No_Element then
         return null;
      end if;
      return Element (Position);
   end Get_Macro;

   overriding function Get_Environment (Resolver : Root_Context)
                                        return Environment_Access is
   begin
      return Resolver.Settings;
   end Get_Environment;

   overriding function Current_Template (Resolver : Root_Context)
                                         return Template_Access is
   begin
      return Resolver.Template_Refs (Resolver.Current_Template_Index);
   end Current_Template;

   overriding function Current_Template_Index (Resolver : Root_Context)
                                         return Natural is
   begin
      if Resolver.Template_Refs.Is_Empty then
         return 0;
      end if;
      return Resolver.Template_Index;
   end Current_Template_Index;

   overriding procedure Set_Current_Template_Index (Resolver : in out Root_Context;
                                                    Index : Positive)
   is
   begin
      Resolver.Template_Index := Index;
   end Set_Current_Template_Index;

   overriding function Template_Count (Resolver : Root_Context)
                                       return Natural is
   begin
      return Natural (Resolver.Template_Refs.Length);
   end Template_Count;

   overriding function Get_Template (Resolver : Root_Context;
                                     Index : Positive)
                                     return Template_Access is
   begin
      return Resolver.Template_Refs (Index);
   end Get_Template;

   overriding procedure Add_Parent_Template (Resolver : in out Root_Context;
                                             Item : Template_Access) is
   begin
      Resolver.Template_Refs.Prepend (Item);
   end Add_Parent_Template;

   overriding function Current_Block_Name (Resolver : Root_Context)
                                                return Unbounded_String is
   begin
      return Resolver.Block_Name;
   end Current_Block_Name;

   overriding procedure Set_Current_Block_Name
     (Resolver : in out Root_Context;
      Block_Name : Unbounded_String) is
   begin
      Resolver.Block_Name := Block_Name;
   end Set_Current_Block_Name;

   function Evaluate (Source : Expression;
                      Resolver : in out Contexts.Context'class)
                      return Expression_Value;

   package Statement_Parser is

      use Jintp.Input;

      procedure Parse (Input : in out Character_Iterator'Class;
                       Settings : Environment'Class;
                       Result : out Statement;
                       End_Modifier : out Character);

      function Parse_Endraw  (Input : in out Character_Iterator'Class;
                              Settings : Environment'Class)
                              return Boolean;

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

   procedure Cleanup (Target : in out Statement) is
   begin
      case Target.Kind is
         when If_Statement | Elif_Statement =>
            Delete_Expression (Target.If_Condition);
         when For_Statement =>
            Delete_Expression (Target.For_Expression);
            Delete_Expression (Target.For_Condition);
         when others =>
            null;
      end case;
   end Cleanup;

   procedure Cleanup (Element : in out Template_Element) is
   begin
      case Element.Kind is
         when Expression_Element =>
            Delete_Expression (Element.Expr);
         when Statement_Element =>
            Cleanup (Element.Stmt);
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

   function Start_String_Length (Kind : Tag_Kind;
                                 Settings : Environment'Class)
                                 return Stream_Element_Offset is
   begin
      case Kind is
         when Expression_Element =>
            return Stream_Element_Offset (Length (Settings.Expression_Start));
         when Statement_Element =>
            return Stream_Element_Offset (Length (Settings.Statement_Start));
         when Comment_Element =>
            return Stream_Element_Offset (Length (Settings.Comment_Start));
      end case;
   end Start_String_Length;

   procedure Raise_With_Location (Message : String;
                                  Filename : String;
                                  Line : Positive);

   pragma No_Return (Raise_With_Location);

   procedure Raise_With_Location (Message : String;
                                  Filename : String;
                                  Line : Positive) is
   begin
      if Ada.Strings.Fixed.Index (Message, "File ") = 0 then
         raise Template_Error with "File """ & Filename & """, line"
           & Line'Image & ": " & Message;
      end if;
      raise Template_Error with Message;
   end Raise_With_Location;

   procedure Get_Template (Filename : String;
                           Target : out Template;
                           Settings : Environment'Class := Default_Environment) is
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
         begin
            if Character'Val (Input.Buffer (Last)) = ASCII.LF then
               Last := Last - 1;
            end if;
         exception
            when Constraint_Error =>
               Last := Input.Buffer'Last;
         end;
         for I in From .. Last loop
            if Character'Val (Input.Buffer (I)) = ASCII.LF then
               Result := Result + 1;
            end if;
         end loop;
         return Result;
      end Line_Count;

      function Get_Raw (Modifier : Character) return Expression_Access is
         Kind : Tag_Kind;
         Initial_Pos : Stream_Element_Offset := Input.Pos;
         Text_End_Pos : Stream_Element_Offset;
         New_Pos : Stream_Element_Offset;
         Closing_Modifier : Character;
      begin
         loop
            Find_Start (Kind, New_Pos, Closing_Modifier);
            if New_Pos > Input.Buffer'Last then
               raise Template_Error with "missing end of raw statement";
            end if;
            Text_End_Pos := New_Pos - 1;
            Input.Pos := New_Pos + Start_String_Length (Kind, Settings);
            if Modifier = '-' then
               while Jintp.Scanner.Is_Whitespace
                 (Character'Val (Input.Buffer (Initial_Pos))) loop
                  Initial_Pos := Initial_Pos + 1;
               end loop;
            end if;
            if Jintp.Statement_Parser.Parse_Endraw (Input, Settings) then
               return new Expression'
                 (Kind => Literal,
                  Value => (Kind => String_Expression_Value,
                            S => Buffer_Slice (Initial_Pos, Text_End_Pos)));
            end if;
         end loop;
      end Get_Raw;

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
            Input.Pos := New_Pos + Start_String_Length (Kind, Settings);
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
                  Jintp.Statement_Parser.Parse (Input,
                                                Settings,
                                                New_Statement,
                                                Modifier);
                  Current_Line := Current_Line + Line_Count (Last_Pos,
                                                             Input.Pos - 1);
                  if Settings.Trim_Blocks and then Modifier /= '+'
                  then
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
                  elsif New_Statement.Kind = Raw_Statement then
                     if Macro_Name = Null_Unbounded_String then
                        Target.Elements.Append ((Line => Current_Line,
                                                 Kind => Expression_Element,
                                                 Expr => Get_Raw (Modifier)));
                     else
                        Target.Macros.Element (Macro_Name)
                          .Elements.Append ((Line => Current_Line,
                                             Kind => Expression_Element,
                                             Expr => Get_Raw (Modifier)));
                     end if;
                  else
                     if Macro_Name = Null_Unbounded_String then
                        Target.Elements.Append ((Line => Current_Line,
                                                 Kind => Statement_Element,
                                                 Stmt => New_Statement));
                        if New_Statement.Kind = Block_Statement then
                           Target.Block_Map.Include
                             (New_Statement.Block_Name,
                              Positive (Target.Elements.Length));
                        end if;
                     else
                        Target.Macros.Element (Macro_Name)
                          .Elements.Append ((Line => Current_Line,
                                             Kind => Statement_Element,
                                             Stmt => New_Statement));
                     end if;
                  end if;
                  if Modifier = '-'
                  then
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
                  Raise_With_Location (Message =>
                                          Ada.Exceptions.Exception_Message (E),
                                       Filename => Filename,
                                       Line => Line_Count + 1);
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

   function To_String (Value : Expression_Value)
                       return String is
      Buffer : Unbounded_String;
      First_Element : Boolean := True;
      Float_Buffer : String (1 .. 32);
      Dec_Exponent : Integer;
   begin
      case Value.Kind is
         when String_Expression_Value =>
            return To_String (Value.S);
         when Boolean_Expression_Value =>
            return Value.B'Image;
         when Integer_Expression_Value =>
            return Ada.Strings.Fixed.Trim (Value.I'Image, Both);
         when Float_Expression_Value =>
            Dec_Exponent := Long_Float'Exponent (Value.F) * 3 / 10;
            if Dec_Exponent
              <= Float_Buffer'Last - Long_Float_IO.Default_Aft - 3
              and then Dec_Exponent >= -3
            then
               Long_Float_IO.Put (Float_Buffer, Value.F,
                                  Long_Float_IO.Default_Aft, 0);
               return Remove_Trailing_Zeroes (Trim (
                    To_Unbounded_String (Float_Buffer),
                  Both));
            end if;
            Long_Float_IO.Put (Float_Buffer, Value.F);
            return Ada.Strings.Fixed.Trim (Float_Buffer, Both);
         when List_Expression_Value =>
            Append (Buffer, '[');
            for V of Value.List_Value.Elements.Values loop
               if not First_Element then
                  Append (Buffer, ", ");
               end if;
               First_Element := False;
               Append (Buffer, "'");
               Append (Buffer, To_String (V));
               Append (Buffer, "'");
            end loop;
            Append (Buffer, ']');
            return To_String (Buffer);
         when Dictionary_Expression_Value =>
            Append (Buffer, '{');
            for C in Value.Dictionary_Value.Assocs.Value_Assocs.Iterate loop
               if not First_Element then
                  Append (Buffer, ", ");
               end if;
               First_Element := False;
               Append (Buffer, "'");
               Append (Buffer, To_String (Key (C)));
               Append (Buffer, "'");
               Append (Buffer, ": ");
               Append (Buffer, "'");
               Append (Buffer, To_String (Element (C)));
               Append (Buffer, "'");
            end loop;
            Append (Buffer, '}');
            return To_String (Buffer);
      end case;
   end To_String;

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
                      Resolver : in out Contexts.Context'Class)
                      return String;

   function To_Float (V : Expression_Value) return Long_Float is
   begin
      case V.Kind is
         when Integer_Expression_Value => return Long_Float (V.I);
         when Float_Expression_Value => return V.F;
         when others => raise Template_Error  with "numeric value expected";
      end case;
   end To_Float;

   package Filters is

      function Evaluate_Filter
        (Source : Expression;
         Resolver : in out Contexts.Context'Class)
      return Jintp.Expression_Value;

   end Filters;

   package body Filters is separate;

   procedure Put (File : Ada.Text_IO.File_Type;
                  Item : Expression_Value) is
   begin
      Ada.Text_IO.Put (File, Item.Kind'Image);
      Ada.Text_IO.Put (File, ": ");
      Ada.Text_IO.Put (File, To_String (Item));
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
         when Operator_Super .. Operator_Macro =>
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

   function Evaluate_Add (Source : Expression;
                          Resolver : in out Contexts.Context'Class)
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
                               Resolver : in out Contexts.Context'class)
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
                          Resolver : in out Contexts.Context'Class)
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
                          Resolver : in out Contexts.Context'Class)
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
                                  Resolver : in out Contexts.Context'Class)
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
                            Resolver : in out Contexts.Context'Class)
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
      Resolver : in out Contexts.Context'Class);

   procedure Append_Value (Target : in out Unbounded_String;
                           Source : Expression;
                           Resolver : in out Contexts.Context'class) is
   begin
      case Source.Kind is
         when Literal =>
            Append (Target, To_String (Source.Value));
         when Variable =>
            Contexts.Append (Resolver, Target, Source.Variable_Name);
         when others =>
            Append (Target, Evaluate (Source, Resolver));
      end case;
   end Append_Value;

   function Render (Filename : String;
                    Resolver : in out Contexts.Context'Class)
                    return Unbounded_String;

   function Render (Source : Template_Element_Vectors.Vector;
                    Filename : String;
                    Resolver : in out Contexts.Context'Class)
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
               Append_Value (Out_Buffer, Element.Expr.all, Resolver);
            when Statement_Element =>
               case Element.Stmt.Kind is
               when Extends_Statement =>
                  Append (Out_Buffer,
                          Render (To_String (Element.Stmt.Parent_Name),
                            Resolver));
                  return Out_Buffer;
               when others =>
                  Execute_Statement (Element.Stmt,
                                     Current,
                                     Out_Buffer,
                                     Resolver);
               end case;
            end case;
         exception
            when E : Template_Error =>
                  Raise_With_Location (Message =>
                                          Ada.Exceptions.Exception_Message (E),
                                       Filename => Filename,
                                       Line => Element.Line);
         end;
         Next (Current);
      end loop;
      return Out_Buffer;
   end Render;

   procedure Execute_Block (Start_Index : Positive;
                            T : Template;
                            Out_Buffer : in out Unbounded_String;
                            Resolver : in out Contexts.Context'Class) is
      Position : Template_Element_Vectors.Cursor
        := T.Elements.To_Cursor (Start_Index + 1);
      Current_Element : Template_Element;
   begin
      while Position /= Template_Element_Vectors.No_Element loop
         Current_Element := Template_Element_Vectors.Element (Position);
         case Current_Element.Kind is
            when Expression_Element =>
               Append (Out_Buffer,
                       Evaluate (Current_Element.Expr.all,
                         Resolver));
            when Statement_Element =>
               if Current_Element.Stmt.Kind = Endblock_Statement then
                  exit;
               end if;
               Execute_Statement (Current_Element.Stmt,
                                  Position,
                                  Out_Buffer,
                                  Resolver);
         end case;
         Position := Template_Element_Vectors.Next (Position);
      end loop;
   exception
      when E : Template_Error =>
         Raise_With_Location (Message =>
                                 Ada.Exceptions.Exception_Message (E),
                              Filename => To_String (T.Filename),
                              Line => Current_Element.Line);
   end Execute_Block;

   function Evaluate_Super (Resolver : in out Contexts.Context'Class;
                            Level : Positive)
                            return Unbounded_String
   is
      Old_Template_Index : constant Positive := Resolver.Current_Template_Index;
      Out_Buffer : Unbounded_String;
   begin
      if Resolver.Current_Template_Index < Level + 1 then
         raise Template_Error with "parent block not found";
      end if;
      Resolver.Set_Current_Template_Index (Resolver.Current_Template_Index - Level);
      declare
         Element_Index : constant Positive := Resolver.Current_Template
           .Block_Map (Resolver.Current_Block_Name);
      begin
         Execute_Block (Element_Index,
                        Resolver.Current_Template.all,
                        Out_Buffer,
                        Resolver);
      exception
         when Constraint_Error =>
            raise Template_Error with "parent block not found";
      end;
      Resolver.Set_Current_Template_Index (Old_Template_Index);
      return Out_Buffer;
   end Evaluate_Super;

   function Evaluate_Operator
     (Source : Expression;
      Resolver : in out Contexts.Context'Class)
      return Expression_Value
     with Pre => Source.Kind in Operator_Super .. Operator_Macro
   is
      Left_Arg : Expression_Value;
      Right_Arg : Expression_Value;
      Macro : Macro_Access;
      Nested_Expression : Expression_Access;
      Level : Positive;
   begin
      case Source.Kind is
         when Operator_Super =>
            if Source.Named_Arguments.Length <= 1 then
               Level := 1;
               if not Source.Named_Arguments.Is_Empty then
                  Level := 2;
                  Nested_Expression := Source.Named_Arguments.Element (1).Argument;
                  while Nested_Expression.Kind = Operator_Dot
                    and then Nested_Expression.Named_Arguments.Length = 2
                    and then Nested_Expression.Named_Arguments.Element (2).Argument.Kind = Variable
                    and then Nested_Expression.Named_Arguments.Element (2).Argument.Variable_Name = "super"
                  loop
                     Level := Level + 1;
                     Nested_Expression := Nested_Expression.Named_Arguments.Element (1).Argument;
                  end loop;
                  if Nested_Expression.Kind /= Variable then
                     raise Template_Error with "invalid use of 'super'";
                  end if;
                  if Nested_Expression.Variable_Name /= "super" then
                     raise Template_Error with To_String (Nested_Expression.Variable_Name)
                       & " is undefined";
                  end if;
               end if;
               return (Kind => String_Expression_Value,
                       S => Evaluate_Super (Resolver, Level));
            end if;
         when Operator_Not =>
            Left_Arg := Evaluate (Source.Named_Arguments (1).Argument.all,
                                  Resolver);
            if Left_Arg.Kind /= Boolean_Expression_Value then
               raise Template_Error with "boolean expression expected";
            end if;
            return (Kind => Boolean_Expression_Value,
                    B => not Left_Arg.B);
         when Operator_Or =>
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
         when Operator_And =>
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
         when Operator_Eq =>
            Left_Arg := Evaluate (Source.Named_Arguments (1).Argument.all,
                                  Resolver);
            Right_Arg := Evaluate (Source.Named_Arguments (2).Argument.all,
                                   Resolver);
            return (Kind => Boolean_Expression_Value,
                    B => Left_Arg = Right_Arg);
         when Operator_Neq =>
            Left_Arg := Evaluate (Source.Named_Arguments (1).Argument.all,
                                  Resolver);
            Right_Arg := Evaluate (Source.Named_Arguments (2).Argument.all,
                                   Resolver);
            return (Kind => Boolean_Expression_Value,
                    B => Left_Arg /= Right_Arg);
         when Operator_Lt =>
            Left_Arg := Evaluate (Source.Named_Arguments (1).Argument.all,
                                  Resolver);
            Right_Arg := Evaluate (Source.Named_Arguments (2).Argument.all,
                                   Resolver);
            return (Kind => Boolean_Expression_Value,
                    B => Left_Arg < Right_Arg);
         when Operator_Ge =>
            Left_Arg := Evaluate (Source.Named_Arguments (1).Argument.all,
                                  Resolver);
            Right_Arg := Evaluate (Source.Named_Arguments (2).Argument.all,
                                   Resolver);
            return (Kind => Boolean_Expression_Value,
                    B => not (Right_Arg < Left_Arg));
         when Operator_Gt =>
            Left_Arg := Evaluate (Source.Named_Arguments (1).Argument.all,
                                  Resolver);
            Right_Arg := Evaluate (Source.Named_Arguments (2).Argument.all,
                                   Resolver);
            return (Kind => Boolean_Expression_Value,
                    B => Right_Arg < Left_Arg);
         when Operator_Le =>
            Left_Arg := Evaluate (Source.Named_Arguments (1).Argument.all,
                                  Resolver);
            Right_Arg := Evaluate (Source.Named_Arguments (2).Argument.all,
                                   Resolver);
            return (Kind => Boolean_Expression_Value,
                    B => not (Left_Arg < Right_Arg));
         when Operator_Tilde =>
            declare
               Left_String : constant String
                 := Evaluate (Source.Named_Arguments (1).Argument.all,
                              Resolver);
               Right_String : constant String
                 := Evaluate (Source.Named_Arguments (2).Argument.all,
                              Resolver);
            begin
               return (Kind => String_Expression_Value,
                       S => To_Unbounded_String (Left_String & Right_String));
            end;
         when Operator_Plus =>
            return Evaluate_Add (Source, Resolver);
         when Operator_Minus =>
            return Evaluate_Subtract (Source, Resolver);
         when Operator_Mul =>
            return Evaluate_Mul (Source, Resolver);
         when Operator_Div =>
            return Evaluate_Div (Source, Resolver);
         when Operator_Integer_Div =>
            return Evaluate_Integer_Div (Source, Resolver);
         when Operator_Power =>
            return Evaluate_Power (Source, Resolver);
         when Operator_Dot =>
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
         when Operator_Brackets =>
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
         when Operator_Macro =>
            Macro := Resolver.Get_Macro (Source.Macro_Name);
            if Macro /= null then
               declare
                  Macro_Resolver : Root_Context;
                  Position : Named_Argument_Vectors.Cursor;
               begin
                  for I in 1 .. Macro.Parameters.Length loop
                     if I <= Source.Macro_Arguments.Length
                       and then Source.Macro_Arguments (Positive (I)).Name
                         = Null_Unbounded_String
                     then
                        --  Positional parameter
                        Macro_Resolver.Values.Insert
                          (Macro.Parameters (Positive (I)).Name,
                           Evaluate
                             (Source.Macro_Arguments (Positive (I)).Argument.all,
                              Resolver));
                     else
                        Position := Find_Named_Argument
                          (Source.Macro_Arguments,
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
         when others =>
            null;
      end case;
      raise Template_Error with "invalid usage of " & Source.Kind'Image;
   end Evaluate_Operator;

   function Evaluate_Test
     (Source : Expression;
      Resolver : in out Contexts.Context'class)
      return Expression_Value
   is
      Source_Value : Expression_Value;
   begin
      if Source.Name = "defined" then
         if Source.Arguments (1).Kind /= Variable then
            raise Template_Error with "variable expected";
         end if;
         begin
            Source_Value := Contexts.Resolve (Resolver,
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
            Source_Value := Contexts.Resolve (Resolver,
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
         if Source.Arguments (2) = null then
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
      if Source.Name = "in" then
         if Source.Arguments (2) = null then
            raise Template_Error
              with "invalid number of arguments to 'in'";
         end if;
         declare
            Source_Value_2 : constant Expression_Value
              := Evaluate (Source.Arguments (2).all,
                           Resolver);
         begin
            if Source_Value_2.Kind /= List_Expression_Value
            then
               raise Template_Error with "'in' is only supported for lists";
            end if;
            return (Kind => Boolean_Expression_Value,
                    B => Contains (Source_Value_2.List_Value.Elements.Values,
                     Source_Value));
         end;
      end if;
      raise Template_Error with "no test named '"
        & To_String (Source.Name) & "'";
   end Evaluate_Test;

   function Evaluate (Source : Expression;
                      Resolver : in out Contexts.Context'class)
                      return Expression_Value is
   begin
      case Source.Kind is
         when Literal =>
            return Source.Value;
         when Variable =>
            return Contexts.Resolve (Resolver, Source.Variable_Name);
         when Operator_Super .. Operator_Macro =>
            return Evaluate_Operator (Source, Resolver);
         when Filter =>
            return Filters.Evaluate_Filter (Source, Resolver);
         when Test =>
            return Evaluate_Test (Source, Resolver);
      end case;
   end Evaluate;

   function Evaluate (Source : Expression;
                      Resolver : in out Contexts.Context'class)
                      return String
   is
   begin
      return To_String (Evaluate (Source, Resolver));
   exception
      when Template_Error =>
         case Source.Kind is
            when Variable =>
               return "";
            when Operator_Brackets | Operator_Dot =>
               return "";
            when others =>
               null;
         end case;
         raise;
   end Evaluate;

   procedure Process_Control_Block_Elements
     (Current : in out Template_Element_Vectors.Cursor;
      Out_Buffer : in out Unbounded_String;
      Resolver : in out Contexts.Context'class)
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
   end Process_Control_Block_Elements;

   procedure Skip_Control_Block_Elements
     (Current : in out Template_Element_Vectors.Cursor)
   is
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
   end Skip_Control_Block_Elements;

   function Evaluate_Boolean (Source : Expression;
                              Resolver : in out Contexts.Context'class)
                              return Boolean is
      Result_Value : constant Expression_Value := Evaluate (Source, Resolver);
   begin
      if Result_Value.Kind /= Boolean_Expression_Value then
         raise Template_Error with "boolean expression expected";
      end if;
      return Result_Value.B;
   end Evaluate_Boolean;

   procedure Execute_If
     (Condition : Expression;
      Current : in out Template_Element_Vectors.Cursor;
      Out_Buffer : in out Unbounded_String;
      Resolver : in out Contexts.Context'class)
   is
      Condition_Value : constant Boolean := Evaluate_Boolean
        (Condition, Resolver);
      Element : Template_Element;
   begin
      if Condition_Value then
         Next (Current);
         Process_Control_Block_Elements (Current, Out_Buffer, Resolver);
         Element := Template_Element_Vectors.Element (Current);
         if Element.Kind = Statement_Element
           and then Element.Stmt.Kind = Else_Statement
         then
            Skip_Control_Block_Elements (Current);
         end if;
      else
         Skip_Control_Block_Elements (Current);
         Element := Template_Element_Vectors.Element (Current);
         if Element.Kind = Statement_Element
           and then Element.Stmt.Kind = Else_Statement
         then
            Next (Current);
            Process_Control_Block_Elements (Current, Out_Buffer, Resolver);
         end if;
      end if;
   exception
      when Constraint_Error =>
         raise Template_Error with "unbalanced 'if'";
   end Execute_If;

   type Chained_Context is new Contexts.Context with record
      Parent_Resolver : access Contexts.Context'Class;
      Variable_Name : Unbounded_String;
      Variable_Value : Expression_Value;
      Index : Natural;
      Length : Ada.Containers.Count_Type;
   end record;

   overriding function Get_Macro (Resolver : Chained_Context;
                                  Name : Unbounded_String)
                                  return Macro_Access;

   overriding function Get_Environment (Resolver : Chained_Context)
                                        return Environment_Access;

   overriding function Current_Template (Resolver : Chained_Context)
                                         return Template_Access;

   overriding function Current_Template_Index (Resolver : Chained_Context)
                                               return Natural;

   overriding procedure Set_Current_Template_Index
     (Resolver : in out Chained_Context;
      Index : Positive);

   overriding function Template_Count (Resolver : Chained_Context)
                                       return Natural;

   overriding function Get_Template (Resolver : Chained_Context;
                                     Index : Positive)
                                     return Template_Access;

   overriding procedure Add_Parent_Template (Resolver : in out Chained_Context;
                                             Item : Template_Access);

   overriding function Current_Block_Name (Resolver : Chained_Context)
                                           return Unbounded_String;

   overriding procedure Set_Current_Block_Name
     (Resolver : in out Chained_Context;
      Block_Name : Unbounded_String);

   overriding function Resolve (Resolver : Chained_Context;
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
      return Contexts.Resolve (Resolver.Parent_Resolver.all, Name);
   end Resolve;

   overriding function Get_Macro (Resolver : Chained_Context;
                                  Name : Unbounded_String)
                                  return Macro_Access is
   begin
      return Contexts.Get_Macro (Resolver.Parent_Resolver.all, Name);
   end Get_Macro;

   overriding function Get_Environment (Resolver : Chained_Context)
                                        return Environment_Access is
   begin
      return Resolver.Parent_Resolver.Get_Environment;
   end Get_Environment;

   overriding function Current_Template (Resolver : Chained_Context)
                                         return Template_Access is
   begin
      return Resolver.Parent_Resolver.Current_Template;
   end Current_Template;

   overriding function Current_Template_Index (Resolver : Chained_Context)
                                               return Natural is
   begin
      return Resolver.Parent_Resolver.Current_Template_Index;
   end Current_Template_Index;

   overriding procedure Set_Current_Template_Index
     (Resolver : in out Chained_Context;
      Index : Positive) is
   begin
      Resolver.Parent_Resolver.Set_Current_Template_Index (Index);
   end Set_Current_Template_Index;

   overriding function Template_Count (Resolver : Chained_Context)
                                       return Natural is
   begin
      return Resolver.Parent_Resolver.Template_Count;
   end Template_Count;

   overriding function Get_Template (Resolver : Chained_Context;
                                     Index : Positive)
                                     return Template_Access is
   begin
      return Resolver.Parent_Resolver.Get_Template (Index);
   end Get_Template;

   overriding procedure Add_Parent_Template (Resolver : in out Chained_Context;
                                             Item : Template_Access) is
   begin
      Resolver.Parent_Resolver.Add_Parent_Template (Item);
   end Add_Parent_Template;

   overriding function Current_Block_Name (Resolver : Chained_Context)
                                           return Unbounded_String is
   begin
      return Resolver.Parent_Resolver.Current_Block_Name;
   end Current_Block_Name;

   overriding procedure Set_Current_Block_Name
     (Resolver : in out Chained_Context;
      Block_Name : Unbounded_String) is
   begin
      Resolver.Parent_Resolver.Set_Current_Block_Name (Block_Name);
   end Set_Current_Block_Name;

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

   procedure Execute_For
     (Collection : Expression;
      Variable_1_Name : Unbounded_String;
      Variable_2_Name : Unbounded_String;
      Condition : Expression_Access;
      Current : in out Template_Element_Vectors.Cursor;
      Out_Buffer : in out Unbounded_String;
      Resolver : in out Contexts.Context'Class)
   is
      Start_Cursor : constant Template_Element_Vectors.Cursor := Current;
      Loop_Resolver : aliased Chained_Context;
      Empty_Loop : Boolean := True;
      Value_Resolver : Chained_Context;

      procedure Execute_For_Items is
         Collection_Value : constant Expression_Value
           := Evaluate (Collection.Named_Arguments (1).Argument.all,
                        Resolver);
      begin
         Loop_Resolver.Variable_Name := Variable_1_Name;
         Value_Resolver.Variable_Name := Variable_2_Name;
         if Collection_Value.Kind /= Dictionary_Expression_Value then
            raise Template_Error with "dictionary expected";
         end if;
         if Condition = null then
            Value_Resolver.Length := Collection_Value.Dictionary_Value.Assocs
              .Value_Assocs.Length;
         else
            Value_Resolver.Length := 0;
            for C in Collection_Value.Dictionary_Value.Assocs.Value_Assocs.Iterate
            loop
               Loop_Resolver.Variable_Value := Key (C);
               Value_Resolver.Variable_Value :=
                 Collection_Value.Dictionary_Value.Assocs.Value_Assocs (C);
               Value_Resolver.Parent_Resolver := Loop_Resolver'Unchecked_Access;
               if Evaluate_Boolean (Condition.all,
                                    Value_Resolver)
               then
                  Value_Resolver.Length := Value_Resolver.Length + 1;
               end if;
            end loop;
         end if;
         Value_Resolver.Index := 0;
         for C in Collection_Value.Dictionary_Value.Assocs.Value_Assocs.Iterate
         loop
            Loop_Resolver.Variable_Value := Key (C);
            Value_Resolver.Variable_Value :=
              Collection_Value.Dictionary_Value.Assocs.Value_Assocs (C);
            Value_Resolver.Parent_Resolver := Loop_Resolver'Unchecked_Access;
            if Condition = null or else Evaluate_Boolean (Condition.all,
                                                          Value_Resolver)
            then
               Current := Start_Cursor;
               Next (Current);
               Process_Control_Block_Elements (Current, Out_Buffer, Value_Resolver);
               Value_Resolver.Index := Value_Resolver.Index + 1;
               Empty_Loop := False;
            end if;
         end loop;
         if Empty_Loop then
            Skip_Control_Block_Elements (Current);
         end if;
      end Execute_For_Items;

      procedure Process_Control_Block_Elements
        (Resolver : in out Contexts.Context'class)
      is
      begin
         Current := Start_Cursor;
         Next (Current);
         Process_Control_Block_Elements (Current, Out_Buffer, Resolver);
         Empty_Loop := False;
      end Process_Control_Block_Elements;

      use Key_And_Value_Vectors;

      procedure Execute_Sorted_By_Key
        (Value_Assocs : Association_Maps.Map;
         Case_Sensitive : Boolean;
         Reverse_Sort : Boolean)
      is
         Keys : Expression_Value_Vectors.Vector;
         Value_Resolver : Chained_Context;
      begin
         for C in Value_Assocs.Iterate loop
            Append (Keys, Key (C));
         end loop;
         if Case_Sensitive then
            Value_Sorting.Sort (Keys);
         else
            Value_Sorting_Case_Insensitive.Sort (Keys);
         end if;
         Loop_Resolver.Variable_Name := Variable_1_Name;
         Value_Resolver.Variable_Name := Variable_2_Name;
         if Condition = null then
            Value_Resolver.Length := Length (Keys);
         else
            Value_Resolver.Length := 0;
            for C in Value_Assocs.Iterate loop
               Loop_Resolver.Variable_Value := Key (C);
               Value_Resolver.Variable_Value := Value_Assocs (Key (C));
               Value_Resolver.Parent_Resolver := Loop_Resolver'Unchecked_Access;
               if Evaluate_Boolean (Condition.all, Value_Resolver) then
                  Value_Resolver.Length := Value_Resolver.Length + 1;
               end if;
            end loop;
         end if;
         if Reverse_Sort then
            for I in reverse 0 .. Natural (Length (Keys) - 1) loop
               Loop_Resolver.Variable_Value := Keys (I);
               Value_Resolver.Variable_Value := Value_Assocs (Keys (I));
               Value_Resolver.Parent_Resolver := Loop_Resolver'Unchecked_Access;
               Value_Resolver.Index := Natural (Value_Resolver.Length) - 1 - I;
               if Condition = null
                 or else Evaluate_Boolean (Condition.all, Value_Resolver)
               then
                  Process_Control_Block_Elements (Value_Resolver);
                  Empty_Loop := False;
               end if;
            end loop;
         else
            Value_Resolver.Index := 0;
            for C in Value_Assocs.Iterate loop
               Loop_Resolver.Variable_Value := Key (C);
               Value_Resolver.Variable_Value := Value_Assocs (Key (C));
               Value_Resolver.Parent_Resolver := Loop_Resolver'Unchecked_Access;
               if Condition = null
                 or else Evaluate_Boolean (Condition.all, Value_Resolver)
               then
                  Process_Control_Block_Elements (Value_Resolver);
                  Empty_Loop := False;
                  Value_Resolver.Index := Value_Resolver.Index + 1;
               end if;
            end loop;
         end if;
         if Empty_Loop then
            Skip_Control_Block_Elements (Current);
         end if;
      end Execute_Sorted_By_Key;

      procedure Execute_Sorted_By_Value (Value_Assocs : Association_Maps.Map;
                               Case_Sensitive : Boolean;
                               Reverse_Sort : Boolean) is
         Items : Key_And_Value_Vectors.Vector;
         Value_Resolver : Chained_Context;
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
         Loop_Resolver.Variable_Name := Variable_1_Name;
         Value_Resolver.Variable_Name := Variable_2_Name;
         if Condition = null then
            Value_Resolver.Length := Length (Items);
         else
            Value_Resolver.Length := 0;
            for I in 0 .. Natural (Length (Items)) - 1 loop
               Loop_Resolver.Variable_Value := Items (I).Key;
               Value_Resolver.Variable_Value := Items (I).Value;
               Value_Resolver.Parent_Resolver := Loop_Resolver'Unchecked_Access;
               if Evaluate_Boolean (Condition.all, Value_Resolver) then
                  Value_Resolver.Length := Value_Resolver.Length + 1;
               end if;
            end loop;
         end if;
         if Reverse_Sort then
            for I in reverse 0 .. Natural (Length (Items)) - 1 loop
               Loop_Resolver.Variable_Value := Items (I).Key;
               Value_Resolver.Variable_Value := Items (I).Value;
               Value_Resolver.Parent_Resolver := Loop_Resolver'Unchecked_Access;
               Value_Resolver.Index := Natural (Value_Resolver.Length) - 1 - I;
               if Condition = null
                 or else Evaluate_Boolean (Condition.all, Value_Resolver)
               then
                  Process_Control_Block_Elements (Value_Resolver);
               end if;
            end loop;
         else
            for I in 0 .. Natural (Length (Items)) - 1 loop
               Loop_Resolver.Variable_Value := Items (I).Key;
               Value_Resolver.Variable_Value := Items (I).Value;
               Value_Resolver.Parent_Resolver := Loop_Resolver'Unchecked_Access;
               Value_Resolver.Index := I;
               Process_Control_Block_Elements (Value_Resolver);
            end loop;
         end if;
      end Execute_Sorted_By_Value;

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
            Execute_Sorted_By_Key
              (Collection_Value.Dictionary_Value.Assocs.Value_Assocs,
               Case_Sensitive,
               Reverse_Sort);
         else
            Execute_Sorted_By_Value
              (Collection_Value.Dictionary_Value.Assocs.Value_Assocs,
               Case_Sensitive,
               Reverse_Sort);
         end if;
      end Execute_For_Dictsort;

      procedure Execute_For_Default is
         Collection_Value : Expression_Value := Evaluate (Collection, Resolver);
      begin
         Loop_Resolver.Variable_Name := Variable_1_Name;
         Loop_Resolver.Index := 0;
         case Collection_Value.Kind is
            when List_Expression_Value =>
               Init (Collection_Value.List_Value);
               if Condition = null then
                  Loop_Resolver.Length := Length
                    (Collection_Value.List_Value.Elements.Values);
               else
                  Loop_Resolver.Length := 0;
                  for E of Collection_Value.List_Value.Elements.Values loop
                     Loop_Resolver.Variable_Value := E;
                     if Evaluate_Boolean (Condition.all,
                                          Loop_Resolver)
                     then
                        Loop_Resolver.Length := Loop_Resolver.Length + 1;
                     end if;
                  end loop;
               end if;
               for E of Collection_Value.List_Value.Elements.Values loop
                  Loop_Resolver.Variable_Value := E;
                  if Condition = null or else Evaluate_Boolean (Condition.all,
                                                                Loop_Resolver)
                  then
                     Current := Start_Cursor;
                     Next (Current);
                     Process_Control_Block_Elements (Current, Out_Buffer, Loop_Resolver);
                     Empty_Loop := False;
                     Loop_Resolver.Index := Loop_Resolver.Index + 1;
                  end if;
               end loop;
               if Empty_Loop then
                  Skip_Control_Block_Elements (Current);
               end if;
            when Dictionary_Expression_Value =>
               if Variable_2_Name /= Null_Unbounded_String then
                  raise Template_Error
                    with "too many values to unpack (expected 2)";
               end if;
               if Condition = null then
                  Loop_Resolver.Length := Length
                    (Collection_Value.Dictionary_Value.Assocs.Value_Assocs);
               else
                  Loop_Resolver.Length := 0;
                  for C in Collection_Value.Dictionary_Value.Assocs.Value_Assocs
                    .Iterate loop
                     Loop_Resolver.Variable_Value := Key (C);
                     if Evaluate_Boolean (Condition.all,
                                          Loop_Resolver)
                     then
                        Loop_Resolver.Length := Loop_Resolver.Length + 1;
                     end if;
                  end loop;
               end if;
               for C in Collection_Value.Dictionary_Value.Assocs.Value_Assocs
                 .Iterate loop
                  Loop_Resolver.Variable_Value := Key (C);
                  if Condition = null or else Evaluate_Boolean (Condition.all,
                                                                Loop_Resolver)
                  then
                     Current := Start_Cursor;
                     Next (Current);
                     Process_Control_Block_Elements (Current,
                                                     Out_Buffer,
                                                     Loop_Resolver);
                     Empty_Loop := False;
                     Loop_Resolver.Index := Loop_Resolver.Index + 1;
                  end if;
               end loop;
               if Empty_Loop then
                  Skip_Control_Block_Elements (Current);
               end if;
            when others =>
               raise Template_Error with "list or dictionary expected";
         end case;
      end Execute_For_Default;

   begin
      Loop_Resolver.Parent_Resolver := Resolver'Unchecked_Access;
      if Collection.Kind = Operator_Items
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
               Process_Control_Block_Elements (Current, Out_Buffer, Resolver);
            else
               Skip_Control_Block_Elements (Current);
            end if;
         end if;
      end;
   exception
      when Constraint_Error =>
         raise Template_Error with "unbalanced 'for'";
   end Execute_For;

   procedure Execute_Include (Filename : String;
                              Out_Buffer : in out Unbounded_String;
                              Resolver : in out Contexts.Context'Class)
   is
      Included_Template : Template;
      Current : Template_Element_Vectors.Cursor;
   begin
      Get_Template (Filename, Included_Template, Resolver.Get_Environment.all);
      Current := First (Included_Template.Elements);
      Process_Control_Block_Elements (Current, Out_Buffer, Resolver);
   end Execute_Include;

   procedure Find_Child_Block
     (Resolver : Contexts.Context'Class;
      Name : Unbounded_String;
      Template_Index : out Natural;
      Element_Index : out Positive)
   is
      Map_Position : Block_Maps.Cursor;
   begin
      if Resolver.Current_Template_Index + 1 > Resolver.Template_Count
      then
         Template_Index := 0;
         Element_Index := 1; -- not a valid index, only to initialize the field
         return;
      end if;
      for I in reverse Resolver.Current_Template_Index + 1 .. Resolver.Template_Count
      loop
         Map_Position := Resolver.Get_Template (I).Block_Map.Find (Name);
         if Map_Position /= Block_Maps.No_Element then
            Template_Index := I;
            Element_Index := Block_Maps.Element (Map_Position);
            return;
         end if;
      end loop;
      Template_Index := 0;
      Element_Index := 1;
   end Find_Child_Block;

   procedure Execute_Statement
     (Stmt : Statement;
      Current : in out Template_Element_Vectors.Cursor;
      Out_Buffer : in out Unbounded_String;
      Resolver : in out Contexts.Context'Class) is

      procedure Replace_Block is
         Current_Element : Template_Element;
         Level : Natural;
         Template_Index : Natural;
         Element_Index : Positive;
         Old_Template_Index : constant Positive := Resolver.Current_Template_Index;
      begin
         Find_Child_Block (Resolver, Stmt.Block_Name,
                           Template_Index, Element_Index);
         if Template_Index = 0 then
            return;
         end if;
         Resolver.Set_Current_Template_Index (Template_Index);
         Resolver.Set_Current_Block_Name (Stmt.Block_Name);
         Execute_Block (Element_Index,
                        Resolver.Get_Template (Template_Index).all,
                        Out_Buffer,
                        Resolver);
         Resolver.Set_Current_Template_Index (Old_Template_Index);

         --  Skip replaced block
         Current := Template_Element_Vectors.Next (Current);
         Level := 0;
         while Current /= Template_Element_Vectors.No_Element loop
            Current_Element := Template_Element_Vectors.Element (Current);
            if Current_Element.Kind = Statement_Element then
               case Current_Element.Stmt.Kind is
               when Block_Statement =>
                  Level := Level + 1;
               when Endblock_Statement =>
                  if Level = 0 then
                     exit;
                  end if;
                  Level := Level - 1;
               when others =>
                  null;
               end case;
            end if;
            Current := Template_Element_Vectors.Next (Current);
         end loop;
      end Replace_Block;

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
                         Stmt.For_Condition,
                         Current,
                         Out_Buffer,
                         Resolver);
         when Include_Statement =>
            Execute_Include (To_String (Stmt.Filename),
                             Out_Buffer,
                             Resolver);
         when Block_Statement =>
            if Resolver.Template_Count > 0 then
               Replace_Block;
            end if;
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

   function Render (Filename : String;
                    Resolver : in out Contexts.Context'Class)
                    return Unbounded_String is
      New_Template : Template_Access :=
        Resolver.Get_Environment.Cached_Templates.Get (Filename);
      File_Time : Time;
      Must_Free : Boolean := False;
      Inserted : Boolean;
   begin
      File_Time := Ada.Directories.Modification_Time (Filename);
      if New_Template = null or else File_Time > New_Template.Timestamp then
         begin
            New_Template := new Template;
            New_Template.Timestamp := File_Time;
            Get_Template (Filename, New_Template.all, Resolver.Get_Environment.all);
            Resolver.Get_Environment.Cached_Templates.Put (Filename,
                                           New_Template,
                                           Resolver.Get_Environment.Max_Cache_Size,
                                           Inserted);
         exception
            when others =>
               Free_Template (New_Template);
               raise;
         end;
         Must_Free := not Inserted;
      end if;
      Resolver.Add_Parent_Template (New_Template);
      if Must_Free then
         declare
            Result : constant Unbounded_String
              := Render (New_Template.Elements, Filename, Resolver);
         begin
            Free_Template (New_Template);
            return Result;
         end;
      end if;
      return Render (New_Template.Elements, Filename, Resolver);
   exception
      when Name_Error =>
         raise Template_Error with "template not found: " & Filename;
   end Render;

   function Render (Filename : String;
                    Values : Dictionary;
                    Settings : in out Environment'Class)
                    return Unbounded_String is
      Resolver : Root_Context :=
        (Settings => Settings'Unchecked_Access,
         Template_Refs => Template_Access_Vectors.Empty_Vector,
         Template_Index => 1,
         Block_Name => Null_Unbounded_String,
         Values => Values);
   begin
      return Render (Filename, Resolver);
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
