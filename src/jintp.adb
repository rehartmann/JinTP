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
                           Endblock_Statement, Import_Statement,
                           From_Import_Statement);

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

   type Name_Mapping is record
      Source : Unbounded_String;
      Target : Unbounded_String;
   end record;

   package String_Mapping_Vectors is new
     Ada.Containers.Vectors (Index_Type => Positive,
                             Element_Type => Name_Mapping);

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
            File_Name : Unbounded_String;
         when Macro_Statement =>
            Macro_Name : Unbounded_String;
            Macro_Parameters : Parameter_Vectors.Vector;
         when Extends_Statement =>
            Parent_Name : Unbounded_String;
         when Block_Statement =>
            Block_Name : Unbounded_String;
         when Import_Statement =>
            Import_File_Name : Unbounded_String;
            Import_Variable_Name : Unbounded_String;
         when From_Import_Statement =>
            From_File_Name : Unbounded_String;
            Import_Variable_Names : String_Mapping_Vectors.Vector;
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
      File_Name : Unbounded_String;
      Elements : Template_Element_Vectors.Vector;
      Block_Map : Block_Maps.Map;
      Cached : Boolean;
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
            Macro_Variable_Name : Unbounded_String;
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

   function Element (Source : Dictionary;
                     Key : Unbounded_String;
                     Value : out Expression_Value)
                     return Boolean is
      Position : constant Association_Maps.Cursor := Find
        (Source.Assocs.Value_Assocs,
         (Kind => String_Expression_Value,
          S => Key));
   begin
      if Position = Association_Maps.No_Element then
         return False;
      end if;
      Value := Association_Maps.Element (Position);
      return True;
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

   type Context;

   type Context_Access is access all Context;

   type Context is new Ada.Finalization.Controlled with record
      Settings : Environment_Access;
      Parent_Resolver : Context_Access;
      Values : Dictionary;
      Template_Refs : Template_Access_Vectors.Vector; -- For template inheritance
      Template_Index : Positive;
      Block_Name : Unbounded_String;
      Included_Templates : Template_Access_Vectors.Vector;
      Macros : Macro_Maps.Map;
      Imported_Templates : Template_Maps.Map;
   end record;

   overriding procedure Finalize (Self : in out Context);

   Loop_Index0_Name : constant Unbounded_String
     := To_Unbounded_String ("loop.index0");
   Loop_Length_Name : constant Unbounded_String
     := To_Unbounded_String ("loop.length");

   function Loop_Index (Resolver : Context)
                        return Natural is
      Index0_Value : Expression_Value;
      Found : constant Boolean := Element (Resolver.Values,
                                  Loop_Index0_Name,
                                  Index0_Value);
   begin
      if Found and then Index0_Value.Kind = Integer_Expression_Value then
         return Index0_Value.I + 1;
      end if;
      raise Template_Error with "'loop.index' is undefined";
   end Loop_Index;

   function Loop_Revindex (Resolver : Context)
                        return Natural is
      Index0_Value, Length_Value : Expression_Value;
      Found : Boolean := Element (Resolver.Values,
                                  Loop_Index0_Name,
                                  Index0_Value);
   begin
      if Found and then Index0_Value.Kind = Integer_Expression_Value then
         Found := Element (Resolver.Values, Loop_Length_Name, Length_Value);
         if Found
           and then Length_Value.Kind = Integer_Expression_Value
         then
            return Length_Value.I - Index0_Value.I;
         end if;
      end if;
      raise Template_Error with "'loop.revindex' is undefined";
   end Loop_Revindex;

   function Loop_First (Resolver : Context)
                        return Boolean is
      Index0_Value : Expression_Value;
      Found : constant Boolean := Element (Resolver.Values,
                                  Loop_Index0_Name,
                                  Index0_Value);
   begin
      if Found and then Index0_Value.Kind = Integer_Expression_Value then
         return Index0_Value.I = 0;
      end if;
      raise Template_Error with "'loop.first' is undefined";
   end Loop_First;

   function Loop_Last (Resolver : Context)
                       return Boolean is
      Index0_Value, Length_Value : Expression_Value;
      Found : Boolean := Element (Resolver.Values,
                                  Loop_Index0_Name,
                                  Index0_Value);
   begin
      if Found and then Index0_Value.Kind = Integer_Expression_Value then
         Found := Element (Resolver.Values, Loop_Length_Name, Length_Value);
         if Found
           and then Length_Value.Kind = Integer_Expression_Value
         then
            return Index0_Value.I = Length_Value.I - 1;
         end if;
      end if;
      raise Template_Error with "'loop.last' is undefined";
   end Loop_Last;

   function Resolve (Resolver : Context;
                     Name : Unbounded_String)
                     return Expression_Value is
      Value : Expression_Value;
      Found : constant Boolean := Element (Resolver.Values, Name, Value);
   begin
      if Found then
         return Value;
      end if;
      if Name = "loop.index" then
         return (Kind => Integer_Expression_Value,
                 I => Loop_Index (Resolver));
      end if;
      if Name = "loop.revindex" then
         return (Kind => Integer_Expression_Value,
                 I => Loop_Revindex (Resolver));
      end if;
      if Name = "loop.revindex0" then
         return (Kind => Integer_Expression_Value,
                 I => Loop_Revindex (Resolver) - 1);
      end if;
      if Name = "loop.first" then
         return (Kind => Boolean_Expression_Value,
                 B => Loop_First (Resolver));
      end if;
      if Name = "loop.last" then
         return (Kind => Boolean_Expression_Value,
                 B => Loop_Last (Resolver));
      end if;
      if Resolver.Parent_Resolver /= null then
         return Resolve (Resolver.Parent_Resolver.all, Name);
      end if;
      raise Template_Error with "'" & To_String (Name) & "' is undefined";
   end Resolve;

   use Macro_Maps;

   function To_String (N : Integer)
                       return String is
   begin
      return Ada.Strings.Fixed.Trim (Integer'Image (N), Both);
   end To_String;

   procedure Append (Resolver : Context;
                     Target : in out Unbounded_String;
                     Name : Unbounded_String) is
      Key : constant Expression_Value := (Kind => String_Expression_Value,
                                 S => Name);
   begin
      if Contains (Resolver.Values.Assocs.Value_Assocs, Key) then
         Append (Target,
                 To_String (Association_Maps.Constant_Reference
                   (Resolver.Values.Assocs.Value_Assocs, Key)));
      elsif Name = "loop.index" then
         Append (Target, To_String (Loop_Index (Resolver)));
      elsif Name = "loop.revindex" then
         Append (Target, To_String (Loop_Revindex (Resolver)));
      elsif Name = "loop.revindex0" then
         Append (Target, To_String (Loop_Revindex (Resolver) - 1));
      elsif Name = "loop.first" then
         Append (Target, Boolean'Image (Loop_First (Resolver)));
      elsif Name = "loop.last" then
         Append (Target, Boolean'Image (Loop_Last (Resolver)));
      elsif Resolver.Parent_Resolver /= null then
         Append (Resolver.Parent_Resolver.all, Target, Name);
      end if;
   exception
      when Template_Error =>
         null; -- ignore
   end Append;

   function Current_Template_Index (Resolver : Context)
                                    return Natural is
   begin
      if not Resolver.Template_Refs.Is_Empty then
         return Resolver.Template_Index;
      end if;
      if Resolver.Parent_Resolver /= null then
         return Current_Template_Index (Resolver.Parent_Resolver.all);
      end if;
      return 0;
   end Current_Template_Index;

   function Get_Macro (Resolver : Context;
                       Name : Unbounded_String)
                       return Macro_Access is
      Position : Macro_Maps.Cursor;
   begin
      if Resolver.Parent_Resolver /= null then
         return Get_Macro (Resolver.Parent_Resolver.all,
                           Name);
      end if;
      Position := Macro_Maps.Find (Resolver.Macros, Name);
      if Position = Macro_Maps.No_Element then
         return null;
      end if;
      return Element (Position);
   end Get_Macro;

   function Get_Environment (Resolver : Context)
                             return Environment_Access is
   begin
      if Resolver.Settings /= null then
         return Resolver.Settings;
      end if;
      if Resolver.Parent_Resolver /= null then
         return Get_Environment (Resolver.Parent_Resolver.all);
      end if;
      return null;
   end Get_Environment;

   function Current_Template (Resolver : Context)
                              return Template_Access is
   begin
      if not Resolver.Template_Refs.Is_Empty then
         return Resolver.Template_Refs (Current_Template_Index (Resolver));
      end if;
      if Resolver.Parent_Resolver /= null then
         return Current_Template (Resolver.Parent_Resolver.all);
      end if;
      raise Template_Error with "internal error: no current template found";
   end Current_Template;

   procedure Set_Current_Template_Index (Resolver : in out Context;
                                                    Index : Positive)
   is
   begin
      if Resolver.Template_Refs.Is_Empty
        and then Resolver.Parent_Resolver /= null
      then
         Set_Current_Template_Index (Resolver.Parent_Resolver.all, Index);
      else
         Resolver.Template_Index := Index;
      end if;
   end Set_Current_Template_Index;

   function Template_Count (Resolver : Context)
                                       return Natural is
   begin
      if Resolver.Template_Refs.Is_Empty
        and then Resolver.Parent_Resolver /= null
      then
         return Template_Count (Resolver.Parent_Resolver.all);
      end if;
      return Natural (Resolver.Template_Refs.Length);
   end Template_Count;

   function Get_Template (Resolver : Context;
                          Index : Positive)
                          return Template_Access is
   begin
      if Resolver.Template_Refs.Is_Empty
        and then Resolver.Parent_Resolver /= null
      then
         return Get_Template (Resolver.Parent_Resolver.all, Index);
      end if;
      return Resolver.Template_Refs (Index);
   end Get_Template;

   procedure Add_Parent_Template (Resolver : in out Context;
                                  Item : Template_Access) is
   begin
      if Resolver.Template_Refs.Is_Empty
        and then Resolver.Parent_Resolver /= null
      then
         Add_Parent_Template (Resolver.Parent_Resolver.all, Item);
      else
         Resolver.Template_Refs.Prepend (Item);
      end if;
   end Add_Parent_Template;

   function Current_Block_Name (Resolver : Context)
                                return Unbounded_String is
   begin
      return Resolver.Block_Name;
   end Current_Block_Name;

   procedure Set_Current_Block_Name
     (Resolver : in out Context;
      Block_Name : Unbounded_String) is
   begin
      Resolver.Block_Name := Block_Name;
   end Set_Current_Block_Name;

   function Evaluate (Source : Expression;
                      Resolver : aliased in out Context)
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

   overriding procedure Finalize (Self : in out Context) is
      Macro : Macro_Access;
   begin
      for C in Self.Macros.Iterate loop
         Macro := Element (C);
         Free_Macro (Macro);
      end loop;
      Self.Macros.Clear;

      for C in Self.Included_Templates.Iterate loop
         Free_Template (Self.Included_Templates (C));
      end loop;
      Self.Included_Templates.Clear;

      for C in Self.Imported_Templates.Iterate loop
         if not Self.Imported_Templates (C).Cached then
            Free_Template (Self.Imported_Templates (C));
         end if;
      end loop;
      Self.Imported_Templates.Clear;
   end Finalize;

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
   begin
      for E of Self.Elements loop
         Cleanup (E);
      end loop;
      Self.Elements.Clear;
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
                                  File_Name : String;
                                  Line : Positive);

   pragma No_Return (Raise_With_Location);

   procedure Raise_With_Location (Message : String;
                                  File_Name : String;
                                  Line : Positive) is
   begin
      if Ada.Strings.Fixed.Index (Message, "File ") = 0 then
         raise Template_Error with "File """ & File_Name & """, line"
           & Line'Image & ": " & Message;
      end if;
      raise Template_Error with Message;
   end Raise_With_Location;

   procedure Get_Template (File_Name : String;
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
        := Ada.Directories.Size (File_Name);
      Current_Line : Positive := 1;
      Last_Pos : Stream_Element_Offset := 1;
      Modifier : Character;
   begin
      Input.Pos := 1;
      Input.Buffer := new Stream_Element_Array
        (1 .. Stream_Element_Offset (Input_Size));
      Open (File, In_File, File_Name);
      Read (File, Input.Buffer.all,
            Stream_Element_Offset (Input_Size));
      Close (File);
      Target.File_Name := To_Unbounded_String (File_Name);
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
               Target.Elements.Append ((Line => Current_Line,
                                        Kind => Expression_Element,
                                        Expr => New_Expression));
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
                  Target.Elements.Append ((Line => Current_Line,
                                           Kind => Expression_Element,
                                           Expr => New_Expression));
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
                  if New_Statement.Kind = Raw_Statement then
                     Target.Elements.Append ((Line => Current_Line,
                                              Kind => Expression_Element,
                                              Expr => Get_Raw (Modifier)));
                  else
                     Target.Elements.Append ((Line => Current_Line,
                                              Kind => Statement_Element,
                                              Stmt => New_Statement));
                     if New_Statement.Kind = Block_Statement then
                        Target.Block_Map.Include
                          (New_Statement.Block_Name,
                           Positive (Target.Elements.Length));
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
                                       File_Name => File_Name,
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
            Target.Elements.Append ((Line => Current_Line,
                                     Kind => Expression_Element,
                                     Expr => New_Expression));
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
            return To_String (Value.I);
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
                      Resolver : aliased in out Context)
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
         Resolver : aliased in out Context)
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
                          Resolver : aliased in out Context)
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
                               Resolver : aliased in out Context)
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
                          Resolver : aliased in out Context)
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
                          Resolver : aliased in out Context)
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
                                  Resolver : aliased in out Context)
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
                            Resolver : aliased in out Context)
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
      Resolver : aliased in out Context);

   procedure Append_Value (Target : in out Unbounded_String;
                           Source : Expression;
                           Resolver : aliased in out Context) is
   begin
      case Source.Kind is
         when Literal =>
            Append (Target, To_String (Source.Value));
         when Variable =>
            Append (Resolver, Target, Source.Variable_Name);
         when Operator_Dot =>
            if Length (Source.Named_Arguments) = 2
              and then Source.Named_Arguments (1).Argument.Kind = Variable
              and then Source.Named_Arguments (1).Argument.Variable_Name = "loop"
              and then Source.Named_Arguments (2).Argument.Kind = Variable
            then
               Append
                 (Resolver,
                  Target,
                  To_Unbounded_String ("loop.")
                  & Source.Named_Arguments (2).Argument.Variable_Name);
            else
               Append (Target, Evaluate (Source, Resolver));
            end if;
         when others =>
            Append (Target, Evaluate (Source, Resolver));
      end case;
   end Append_Value;

   function Render (File_Name : String;
                    Resolver : aliased in out Context)
                    return Unbounded_String;

   function Render (Source : Template_Element_Vectors.Vector;
                    File_Name : String;
                    Resolver : aliased in out Context)
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
                                       File_Name => File_Name,
                                       Line => Element.Line);
         end;
         Next (Current);
      end loop;
      return Out_Buffer;
   end Render;

   procedure Execute_Block (Start_Index : Positive;
                            T : Template;
                            Out_Buffer : in out Unbounded_String;
                            Resolver : aliased in out Context) is
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
                              File_Name => To_String (T.File_Name),
                              Line => Current_Element.Line);
   end Execute_Block;

   function Evaluate_Super (Resolver : aliased in out Context;
                            Level : Positive)
                            return Unbounded_String
   is
      Old_Template_Index : constant Positive := Current_Template_Index (Resolver);
      Out_Buffer : Unbounded_String;
   begin
      if Current_Template_Index (Resolver) < Level + 1 then
         raise Template_Error with "parent block not found";
      end if;
      Set_Current_Template_Index (Resolver, Current_Template_Index (Resolver) - Level);
      declare
         Element_Index : constant Positive := Current_Template (Resolver)
           .Block_Map (Current_Block_Name (Resolver));
      begin
         Execute_Block (Element_Index,
                        Current_Template (Resolver).all,
                        Out_Buffer,
                        Resolver);
      exception
         when Constraint_Error =>
            raise Template_Error with "parent block not found";
      end;
      Set_Current_Template_Index (Resolver, Old_Template_Index);
      return Out_Buffer;
   end Evaluate_Super;

   function Evaluate_Operator
     (Source : Expression;
      Resolver : aliased in out Context)
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
               return Resolve
                 (Resolver,
                  "loop." & Source.Named_Arguments (2).Argument.Variable_Name);
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
            Macro := (if Source.Macro_Variable_Name = Null_Unbounded_String
                      then Get_Macro (Resolver, Source.Macro_Name)
                      else Get_Macro (Resolver, Source.Macro_Variable_Name
                        & "." & Source.Macro_Name));
            if Macro = null then
               raise Template_Error with "macro '"
                 & To_String (Source.Macro_Name) & "' not found";
            end if;
            declare
               Macro_Resolver : aliased Context;
               Position : Named_Argument_Vectors.Cursor;
            begin
               Macro_Resolver.Parent_Resolver := Resolver'Unchecked_Access;
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
         when others =>
            null;
      end case;
      raise Template_Error with "invalid usage of " & Source.Kind'Image;
   end Evaluate_Operator;

   function Evaluate_Test
     (Source : Expression;
      Resolver : aliased in out Context)
      return Expression_Value
   is
      Source_Value : Expression_Value;
   begin
      if Source.Name = "defined" then
         if Source.Arguments (1).Kind /= Variable then
            raise Template_Error with "variable expected";
         end if;
         begin
            Source_Value := Resolve (Resolver,
                                     Source.Arguments (1).Variable_Name);
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
            Source_Value := Resolve (Resolver,
                                     Source.Arguments (1).Variable_Name);
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
                      Resolver : aliased in out Context)
                      return Expression_Value is
   begin
      case Source.Kind is
         when Literal =>
            return Source.Value;
         when Variable =>
            return Resolve (Resolver, Source.Variable_Name);
         when Operator_Super .. Operator_Macro =>
            return Evaluate_Operator (Source, Resolver);
         when Filter =>
            return Filters.Evaluate_Filter (Source, Resolver);
         when Test =>
            return Evaluate_Test (Source, Resolver);
      end case;
   end Evaluate;

   function Evaluate (Source : Expression;
                      Resolver : aliased in out Context)
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

   procedure Insert_Value (Container : in out Dictionary;
                     Key : Unbounded_String;
                     New_Item : Expression_Value) is
   begin
      Init (Container);
      Include (Container.Assocs.Value_Assocs,
              (Kind => String_Expression_Value,
               S => Key),
              New_Item);
   end Insert_Value;

   procedure Process_Control_Block_Elements
     (Current : in out Template_Element_Vectors.Cursor;
      Out_Buffer : in out Unbounded_String;
      Resolver : aliased in out Context)
   is
      Current_Element : Template_Element;
   begin
      while Current /= Template_Element_Vectors.No_Element loop
         Current_Element := Template_Element_Vectors.Element (Current);
         case Current_Element.Kind is
            when Expression_Element =>
               Append_Value (Out_Buffer,
                             Current_Element.Expr.all,
                             Resolver);
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
                              Resolver : aliased in out Context)
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
      Resolver : aliased in out Context)
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

   procedure Set_Loop_Index0 (Resolver : in out Context;
                              Index0 : Natural) is
   begin
      Insert (Resolver.Values, Loop_Index0_Name, Index0);
   end Set_Loop_Index0;

   procedure Set_Loop_Length (Resolver : in out Context;
                              Length : Natural) is
   begin
      Insert (Resolver.Values, Loop_Length_Name, Length);
   end Set_Loop_Length;

   procedure Execute_For
     (Collection : Expression;
      Variable_1_Name : Unbounded_String;
      Variable_2_Name : Unbounded_String;
      Condition : Expression_Access;
      Current : in out Template_Element_Vectors.Cursor;
      Out_Buffer : in out Unbounded_String;
      Resolver : aliased in out Context)
   is
      Start_Cursor : constant Template_Element_Vectors.Cursor := Current;
      Loop_Resolver : aliased Context;
      Empty_Loop : Boolean := True;
      Loop_Length : Natural := 0;
      Loop_Index0 : Natural := 0;

      procedure Execute_For_Items is
         Collection_Value : constant Expression_Value
           := Evaluate (Collection.Named_Arguments (1).Argument.all,
                        Resolver);
      begin
         if Collection_Value.Kind /= Dictionary_Expression_Value then
            raise Template_Error with "dictionary expected";
         end if;
         if Condition = null then
            Set_Loop_Length (Loop_Resolver,
                             Natural (Collection_Value.Dictionary_Value.Assocs
                             .Value_Assocs.Length));
         else
            Loop_Length := 0;
            for C in Collection_Value.Dictionary_Value.Assocs.Value_Assocs.Iterate
            loop
               Insert_Value (Loop_Resolver.Values, Variable_1_Name, Key (C));
               Insert_Value
                 (Loop_Resolver.Values,
                  Variable_2_Name,
                  Collection_Value.Dictionary_Value.Assocs.Value_Assocs (C));
               if Evaluate_Boolean (Condition.all,
                                    Loop_Resolver)
               then
                  Loop_Length := Loop_Length + 1;
               end if;
            end loop;
            Set_Loop_Length (Loop_Resolver,
                             Loop_Length);
         end if;
         for C in Collection_Value.Dictionary_Value.Assocs.Value_Assocs.Iterate
         loop
            Insert_Value (Loop_Resolver.Values, Variable_1_Name, Key (C));
               Insert_Value
                 (Loop_Resolver.Values,
                  Variable_2_Name,
                  Collection_Value.Dictionary_Value.Assocs.Value_Assocs (C));
            if Condition = null or else Evaluate_Boolean (Condition.all,
                                                          Loop_Resolver)
            then
               Current := Start_Cursor;
               Next (Current);
               Set_Loop_Index0 (Loop_Resolver, Loop_Index0);
               Process_Control_Block_Elements (Current, Out_Buffer, Loop_Resolver);
               Loop_Index0 := Loop_Index0 + 1;
               Empty_Loop := False;
            end if;
         end loop;
         if Empty_Loop then
            Skip_Control_Block_Elements (Current);
         end if;
      end Execute_For_Items;

      procedure Process_Control_Block_Elements
        (Resolver : aliased in out Context)
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
         Loop_Length : Natural := 0;
         Loop_Index0 : Natural := 0;
      begin
         for C in Value_Assocs.Iterate loop
            Append (Keys, Key (C));
         end loop;
         if Case_Sensitive then
            Value_Sorting.Sort (Keys);
         else
            Value_Sorting_Case_Insensitive.Sort (Keys);
         end if;
         if Condition = null then
            Loop_Length := Natural (Length (Keys));
         else
            for C in Value_Assocs.Iterate loop
               Insert_Value (Loop_Resolver.Values, Variable_1_Name, Key (C));
               Insert_Value (Loop_Resolver.Values,
                             Variable_2_Name,
                             Value_Assocs (Key (C)));
               if Evaluate_Boolean (Condition.all, Loop_Resolver) then
                  Loop_Length := Loop_Length + 1;
               end if;
            end loop;
         end if;
         Set_Loop_Length (Loop_Resolver, Loop_Length);
         if Reverse_Sort then
            for I in reverse 0 .. Natural (Length (Keys) - 1) loop
               Insert_Value (Loop_Resolver.Values, Variable_1_Name, Keys (I));
               Insert_Value (Loop_Resolver.Values,
                             Variable_2_Name,
                             Value_Assocs (Keys (I)));
               Set_Loop_Index0 (Loop_Resolver,
                                Loop_Length - 1 - I);
               if Condition = null
                 or else Evaluate_Boolean (Condition.all, Loop_Resolver)
               then
                  Process_Control_Block_Elements (Loop_Resolver);
                  Empty_Loop := False;
               end if;
            end loop;
         else
            Loop_Index0 := 0;
            for C in Value_Assocs.Iterate loop
               Insert_Value (Loop_Resolver.Values, Variable_1_Name, Key (C));
               Insert_Value (Loop_Resolver.Values,
                             Variable_2_Name,
                             Value_Assocs (Key (C)));
               if Condition = null
                 or else Evaluate_Boolean (Condition.all, Loop_Resolver)
               then
                  Set_Loop_Index0 (Loop_Resolver, Loop_Index0);
                  Process_Control_Block_Elements (Loop_Resolver);
                  Empty_Loop := False;
                  Loop_Index0 := Loop_Index0 + 1;
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
         Loop_Length : Natural := 0;
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
         if Condition = null then
            Loop_Length := Natural (Length (Items));
         else
            for I in 0 .. Natural (Length (Items)) - 1 loop
               Insert_Value (Loop_Resolver.Values, Variable_1_Name, Items (I).Key);
               Insert_Value (Loop_Resolver.Values, Variable_2_Name, Items (I).Value);
               if Evaluate_Boolean (Condition.all, Loop_Resolver) then
                  Loop_Length := Loop_Length + 1;
               end if;
            end loop;
         end if;
         Set_Loop_Length (Loop_Resolver, Loop_Length);
         if Reverse_Sort then
            for I in reverse 0 .. Natural (Length (Items)) - 1 loop
               Insert_Value (Loop_Resolver.Values, Variable_1_Name, Items (I).Key);
               Insert_Value (Loop_Resolver.Values, Variable_2_Name, Items (I).Value);
               Set_Loop_Index0 (Loop_Resolver,
                                Loop_Length - 1 - I);
               if Condition = null
                 or else Evaluate_Boolean (Condition.all, Loop_Resolver)
               then
                  Process_Control_Block_Elements (Loop_Resolver);
               end if;
            end loop;
         else
            for I in 0 .. Natural (Length (Items)) - 1 loop
               Insert_Value (Loop_Resolver.Values, Variable_1_Name, Items (I).Key);
               Insert_Value (Loop_Resolver.Values, Variable_2_Name, Items (I).Value);
               Set_Loop_Index0 (Loop_Resolver, I);
               Process_Control_Block_Elements (Loop_Resolver);
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
         Loop_Length : Natural := 0;
         Loop_Index0 : Natural := 0;
      begin
         case Collection_Value.Kind is
            when List_Expression_Value =>
               Init (Collection_Value.List_Value);
               if Condition = null then
                  Loop_Length := Natural
                    (Length (Collection_Value.List_Value.Elements.Values));
               else
                  for E of Collection_Value.List_Value.Elements.Values loop
                     Insert_Value (Loop_Resolver.Values, Variable_1_Name, E);
                     if Evaluate_Boolean (Condition.all,
                                          Loop_Resolver)
                     then
                        Loop_Length := Loop_Length + 1;
                     end if;
                  end loop;
               end if;
               Set_Loop_Length (Loop_Resolver, Loop_Length);
               for E of Collection_Value.List_Value.Elements.Values loop
                  Insert_Value (Loop_Resolver.Values, Variable_1_Name, E);
                  if Condition = null or else Evaluate_Boolean (Condition.all,
                                                                Loop_Resolver)
                  then
                     Current := Start_Cursor;
                     Next (Current);
                     Set_Loop_Index0 (Loop_Resolver, Loop_Index0);
                     Process_Control_Block_Elements (Current, Out_Buffer, Loop_Resolver);
                     Empty_Loop := False;
                     Loop_Index0 := Loop_Index0 + 1;
                  end if;
               end loop;
               if Empty_Loop then
                  Skip_Control_Block_Elements (Current);
               end if;
            when Dictionary_Expression_Value =>
               if Variable_2_Name /= Null_Unbounded_String then
                  raise Template_Error
                    with "too many loop variables";
               end if;
               if Condition = null then
                  Loop_Length := Natural
                    (Length (Collection_Value.Dictionary_Value.Assocs.Value_Assocs));
               else
                  Loop_Length := 0;
                  for C in Collection_Value.Dictionary_Value.Assocs.Value_Assocs
                    .Iterate loop
                     Insert_Value (Loop_Resolver.Values, Variable_1_Name, Key (C));
                     if Evaluate_Boolean (Condition.all,
                                          Loop_Resolver)
                     then
                        Loop_Length := Loop_Length + 1;
                     end if;
                  end loop;
               end if;
               Set_Loop_Length (Loop_Resolver, Loop_Length);
               for C in Collection_Value.Dictionary_Value.Assocs.Value_Assocs
                 .Iterate loop
                     Insert_Value (Loop_Resolver.Values, Variable_1_Name, Key (C));
                  if Condition = null or else Evaluate_Boolean (Condition.all,
                                                                Loop_Resolver)
                  then
                     Current := Start_Cursor;
                     Next (Current);
                     Set_Loop_Index0 (Loop_Resolver, Loop_Index0);
                     Process_Control_Block_Elements (Current,
                                                     Out_Buffer,
                                                     Loop_Resolver);
                     Empty_Loop := False;
                     Loop_Index0 := Loop_Index0 + 1;
                  end if;
               end loop;
               if Empty_Loop then
                  Skip_Control_Block_Elements (Current);
               end if;
            when others =>
               raise Template_Error with "list or dictionary expected, got "
                 & Collection_Value.Kind'Image;
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

   procedure Execute_Include (File_Name : String;
                              Out_Buffer : in out Unbounded_String;
                              Resolver : aliased in out Context)
   is
      Included_Template : Template_Access;
      Current : Template_Element_Vectors.Cursor;
   begin
      if Resolver.Parent_Resolver /= null then
         raise Template_Error with "including templates is only permitted at top level";
      end if;
      begin
         Included_Template := new Template;
         Get_Template (File_Name, Included_Template.all, Get_Environment (Resolver).all);
      exception
         when Name_Error =>
            Free_Template (Included_Template);
            raise Template_Error with "template not found: " & File_Name;
         when Constraint_Error =>
            Free_Template (Included_Template);
            raise Template_Error with "including a template twice is not supported";
         when others =>
            Free_Template (Included_Template);
            raise;
      end;
      Resolver.Included_Templates.Append (Included_Template);
      Current := First (Included_Template.Elements);
      Process_Control_Block_Elements (Current, Out_Buffer, Resolver);
   end Execute_Include;

   procedure Execute_Macro (Name : Unbounded_String;
                            Parameters : Parameter_Vectors.Vector;
                            Current : in out Template_Element_Vectors.Cursor;
                            Resolver : aliased in out Context)
   is
      Position : Macro_Maps.Cursor;
      M : Macro_Access;
      E : Template_Element;
      Elements : Template_Element_Vectors.Vector;
      Root_Context : Context_Access := Resolver'Unchecked_Access;
   begin
      while Root_Context.Parent_Resolver /= null loop
         Root_Context := Root_Context.Parent_Resolver;
      end loop;
      Position := Macro_Maps.Find (Root_Context.Macros, Name);
      if Position /= Macro_Maps.No_Element then
         --  Delete old macro
         M := Element (Position);
         Free_Macro (M);
         Root_Context.Macros.Delete (Position);
      end if;
      Current := Template_Element_Vectors.Next (Current);
      while Current /= Template_Element_Vectors.No_Element loop
         E := Element (Current);
         if E.Kind = Statement_Element
           and then E.Stmt.Kind = Endmacro_Statement
         then
            exit;
         end if;
         Elements.Append (E);
         Current := Template_Element_Vectors.Next (Current);
      end loop;
      M := new Macro'
           (Parameters => Parameters,
            Elements => Elements);
      Root_Context.Macros.Insert (Name, M);
   end Execute_Macro;

   function Get_Template (File_Name : String;
                          Resolver : aliased in out Context)
                          return Template_Access is
      New_Template : Template_Access :=
        Get_Environment (Resolver).Cached_Templates.Get (File_Name);
      File_Time : Time;
   begin
      File_Time := Ada.Directories.Modification_Time (File_Name);
      if New_Template = null or else File_Time > New_Template.Timestamp then
         begin
            New_Template := new Template;
            New_Template.Timestamp := File_Time;
            Get_Template (File_Name, New_Template.all, Get_Environment (Resolver).all);
            Get_Environment (Resolver).Cached_Templates.Put
              (Path     => File_Name,
               Template => New_Template,
               Max_Size => Get_Environment (Resolver).Max_Cache_Size,
               Inserted => New_Template.Cached);
         exception
            when others =>
               Free_Template (New_Template);
               raise;
         end;
      end if;
      return New_Template;
   end Get_Template;

   function Find (Source : String_Mapping_Vectors.Vector;
                  Name : Unbounded_String)
                  return Natural is
   begin
      for I in 1 .. String_Mapping_Vectors.Length (Source) loop
         if String_Mapping_Vectors.Element (Source, Positive (I)).Source = Name then
            return Natural (I);
         end if;
      end loop;
      return 0;
   end Find;

   procedure Execute_Import (File_Name : String;
                             Variable_Name : Unbounded_String;
                             Resolver : aliased in out Context) is
      New_Template : Template_Access;
      Current : Template_Element_Vectors.Cursor;
      E : Template_Element;
   begin
      New_Template := Get_Template (File_Name, Resolver);
      Resolver.Imported_Templates.Insert (Variable_Name, New_Template);
      Current := First (New_Template.Elements);
      while Current /= Template_Element_Vectors.No_Element loop
         E := Template_Element_Vectors.Element (Current);
         if E.Kind = Statement_Element
           and then E.Stmt.Kind = Macro_Statement
         then
            Execute_Macro (Variable_Name & "." & To_String (E.Stmt.Macro_Name),
                        E.Stmt.Macro_Parameters,
                        Current,
                        Resolver);
         end if;
         if Current = Template_Element_Vectors.No_Element then
            exit;
         end if;
         Next (Current);
      end loop;
   exception
      when Constraint_Error =>
         if not New_Template.Cached then
            Free_Template (New_Template);
         end if;
         raise Template_Error with "importing a template twice is not supported";
      when Name_Error =>
         raise Template_Error with "template not found: " & File_Name;
   end Execute_Import;

   procedure Execute_Import (File_Name : String;
                             Variable_Names : String_Mapping_Vectors.Vector;
                             Resolver : aliased in out Context) is
      New_Template : Template_Access;
      Current : Template_Element_Vectors.Cursor;
      E : Template_Element;
      Name_Index : Natural;
      Mapping : Name_Mapping;
   begin
      New_Template := Get_Template (File_Name, Resolver);
      Resolver.Imported_Templates.Insert
        (To_Unbounded_String ("$") & File_Name, New_Template);
      Current := First (New_Template.Elements);
      while Current /= Template_Element_Vectors.No_Element loop
         E := Template_Element_Vectors.Element (Current);
         if E.Kind = Statement_Element
           and then E.Stmt.Kind = Macro_Statement
         then
            Name_Index := Find (Variable_Names, E.Stmt.Macro_Name);
            if Name_Index /= 0 then
               Mapping := String_Mapping_Vectors.Element (Variable_Names,
                                                          Name_Index);
               Execute_Macro
                 ((if Mapping.Target = Null_Unbounded_String
                   then E.Stmt.Macro_Name else Mapping.Target),
                  E.Stmt.Macro_Parameters,
                  Current,
                  Resolver);
            end if;
         end if;
         if Current = Template_Element_Vectors.No_Element then
            exit;
         end if;
         Next (Current);
      end loop;
   exception
      when Constraint_Error =>
         if not New_Template.Cached then
            Free_Template (New_Template);
         end if;
         raise Template_Error with "importing a template twice is not supported";
      when Name_Error =>
         raise Template_Error with "template not found: " & File_Name;
   end Execute_Import;

   procedure Find_Child_Block
     (Resolver : Context;
      Name : Unbounded_String;
      Template_Index : out Natural;
      Element_Index : out Positive)
   is
      Map_Position : Block_Maps.Cursor;
   begin
      if Current_Template_Index (Resolver) + 1 > Template_Count (Resolver)
      then
         Template_Index := 0;
         Element_Index := 1; -- not a valid index, only to initialize the field
         return;
      end if;
      for I in reverse Current_Template_Index (Resolver) + 1 .. Template_Count (Resolver)
      loop
         Map_Position := Get_Template (Resolver, I).Block_Map.Find (Name);
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
      Resolver : aliased in out Context) is

      procedure Replace_Block is
         Current_Element : Template_Element;
         Level : Natural;
         Template_Index : Natural;
         Element_Index : Positive;
         Old_Template_Index : constant Positive := Current_Template_Index (Resolver);
      begin
         Find_Child_Block (Resolver, Stmt.Block_Name,
                           Template_Index, Element_Index);
         if Template_Index = 0 then
            return;
         end if;
         Set_Current_Template_Index (Resolver, Template_Index);
         Set_Current_Block_Name (Resolver, Stmt.Block_Name);
         Execute_Block (Element_Index,
                        Get_Template (Resolver, Template_Index).all,
                        Out_Buffer,
                        Resolver);
         Set_Current_Template_Index (Resolver, Old_Template_Index);

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
            Execute_Include (To_String (Stmt.File_Name),
                             Out_Buffer,
                             Resolver);
         when Block_Statement =>
            if Template_Count (Resolver) > 0 then
               Replace_Block;
            end if;
         when Macro_Statement =>
            Execute_Macro (Stmt.Macro_Name,
                           Stmt.Macro_Parameters,
                           Current,
                           Resolver);
         when Import_Statement =>
            Execute_Import (To_String (Stmt.Import_File_Name),
                            Stmt.Import_Variable_Name,
                            Resolver);
         when From_Import_Statement =>
            Execute_Import (To_String (Stmt.From_File_Name),
                            Stmt.Import_Variable_Names,
                            Resolver);
         when others =>
            raise Template_Error with "internal error: invalid statement";
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

   function Render (File_Name : String;
                    Resolver : aliased in out Context)
                    return Unbounded_String is
      New_Template : Template_Access := Get_Template (File_Name, Resolver);
   begin
      Add_Parent_Template (Resolver, New_Template);
      if not New_Template.Cached then
         declare
            Result : Unbounded_String;
         begin
            Result := Render (New_Template.Elements, File_Name, Resolver);
            Resolver.Template_Refs.Clear;
            Free_Template (New_Template);
            return Result;
         exception
            when others =>
               Free_Template (New_Template);
               Resolver.Template_Refs.Clear;
               raise;
         end;
      end if;
      return Render (New_Template.Elements, File_Name, Resolver);
   exception
      when Name_Error =>
         raise Template_Error with "template not found: " & File_Name;
   end Render;

   function Render (File_Name : String;
                    Values : Dictionary;
                    Settings : in out Environment'Class)
                    return Unbounded_String is
      Resolver : aliased Context :=
        (Ada.Finalization.Controlled with
         Settings => Settings'Unchecked_Access,
         Template_Refs => Template_Access_Vectors.Empty_Vector,
         Template_Index => 1,
         Block_Name => Null_Unbounded_String,
         Values => Values,
         Parent_Resolver => null,
         Included_Templates => Template_Access_Vectors.Empty_Vector,
         Macros => Macro_Maps.Empty_Map,
         Imported_Templates => Template_Maps.Empty_Map);
   begin
      return Render (File_Name, Resolver);
   exception
      when others =>
         for E of Resolver.Included_Templates loop
            Free_Template (E);
         end loop;
         raise;
   end Render;

   function Render (File_Name : String;
                    Values : Dictionary;
                    Settings : in out Environment'Class)
                    return String is
   begin
      return To_String (Render (File_Name, Values, Settings));
   end Render;

   function Render (File_Name : String;
                    Values : Dictionary)
                    return String is
   begin
      return Render (File_Name, Values, Default_Environment);
   end Render;

   function Render (File_Name : String;
                    Values : Dictionary)
                    return Unbounded_String is
   begin
      return Render (File_Name, Values, Default_Environment);
   end Render;

   procedure Insert (Container : in out Dictionary;
                     Key : String;
                     New_Item : Unbounded_String) is
   begin
      Insert (Container, To_Unbounded_String (Key), New_Item);
   end Insert;

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
                     Key : Unbounded_String;
                     New_Item : Integer) is
   begin
      Init (Container);
      Include (Container.Assocs.Value_Assocs,
              (Kind => String_Expression_Value,
               S => Key),
              (Kind => Integer_Expression_Value,
               I => New_Item));
   end Insert;

   procedure Insert (Container : in out Dictionary;
                     Key : String;
                     New_Item : Integer) is
   begin
      Insert (Container, To_Unbounded_String (Key), New_Item);
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
