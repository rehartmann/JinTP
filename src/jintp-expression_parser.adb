with Jintp.Input;
with Jintp.Scanner;
with Ada.Characters.Handling;
with Ada.Text_IO; use Ada.Text_IO;

separate (Jintp)
package body Expression_Parser is

   use Jintp.Scanner;

   function Parse_Primitive
     (Scanner : in out Scanner_State;
      Input : in out Jintp.Input.Character_Iterator'Class;
      Settings : Environment)
      return Jintp.Expression_Access;

   function Parse_List
     (Scanner : in out Scanner_State;
      Input : in out Jintp.Input.Character_Iterator'Class;
      Settings : Environment)
      return Jintp.Expression_Access
   is
      Current_Token : Token := Jintp.Scanner.Current_Token (Scanner);
      Result : Jintp.Expression_Access;
   begin
      Result := new Expression'
        (Kind => Literal,
         Value => (Kind => List_Expression_Value,
                   List_Value => (Ada.Finalization.Controlled
                                  with Elements => null))
        );
      Init (Result.Value.List_Value);
      Next_Token (Scanner, Input, Current_Token, Settings);
      if Current_Token.Kind = Right_Bracket_Token then
         Next_Token (Scanner, Input, Current_Token, Settings);
         return Result;
      end if;
      loop
         declare
            New_Element : Expression_Access := Parse_Primitive (Scanner,
                                                                Input,
                                                                Settings);
         begin
            if New_Element.Kind /= Literal then
               Delete_Expression (New_Element);
               raise Template_Error with "literal expected";
            end if;
            Append (Result.Value.List_Value.Elements.Values,
                    New_Element.Value);
            Delete_Expression (New_Element);
         end;
         Current_Token := Jintp.Scanner.Current_Token (Scanner);
         if Current_Token.Kind = Right_Bracket_Token then
            exit;
         end if;
         if Current_Token.Kind /= Comma_Token then
            raise Template_Error with "']' or ',' expected, read "
              & Current_Token.Kind'Image;
         end if;
         Next_Token (Scanner, Input, Current_Token, Settings);
      end loop;
      Next_Token (Scanner, Input, Current_Token, Settings);
      return Result;
   exception
      when others =>
         Delete_Expression (Result);
         raise;
   end Parse_List;

   function Parse_Dictionary
     (Scanner : in out Scanner_State;
      Input : in out Jintp.Input.Character_Iterator'Class;
      Settings : Environment)
      return Jintp.Expression_Access
   is
      Current_Token : Token := Jintp.Scanner.Current_Token (Scanner);
      Result : Jintp.Expression_Access;
   begin
      Result := new Expression'
        (Kind => Literal,
         Value => (Kind => Dictionary_Expression_Value,
                   Dictionary_Value => (Ada.Finalization.Controlled
                                        with Assocs => null))
        );
      Init (Result.Value.Dictionary_Value);
      Next_Token (Scanner, Input, Current_Token, Settings);
      if Current_Token.Kind = Right_Brace_Token then
         Next_Token (Scanner, Input, Current_Token, Settings);
         return Result;
      end if;
      loop
         declare
            New_Key : Expression_Access := null;
            New_Element : Expression_Access := null;
         begin
            New_Key := Parse_Primitive (Scanner, Input, Settings);
            if New_Key.Kind /= Literal then
               raise Template_Error with "literal expected";
            end if;
            Current_Token := Jintp.Scanner.Current_Token (Scanner);
            if Current_Token.Kind /= Colon_Token then
               raise Template_Error with "':' expected";
            end if;
            Next_Token (Scanner, Input, Current_Token, Settings);
            New_Element := Parse_Primitive (Scanner, Input, Settings);
            Result.Value.Dictionary_Value.Assocs.Value_Assocs.Insert
              (New_Key.Value, New_Element.Value);
            Delete_Expression (New_Key);
            Delete_Expression (New_Element);
         exception
            when others =>
               Delete_Expression (New_Key);
               Delete_Expression (New_Element);
               raise;
         end;
         Current_Token := Jintp.Scanner.Current_Token (Scanner);
         if Current_Token.Kind = Right_Brace_Token then
            exit;
         end if;
         if Current_Token.Kind /= Comma_Token then
            raise Template_Error with "'}' or ',' expected, encountered "
              & Current_Token.Kind'Image;
         end if;
         Next_Token (Scanner, Input, Current_Token, Settings);
      end loop;
      Next_Token (Scanner, Input, Current_Token, Settings);
      return Result;
   exception
      when others =>
         Delete_Expression (Result);
         raise;
   end Parse_Dictionary;

   function To_Array (Arguments : Named_Argument_Vectors.Vector)
                      return Expression_Access_Array
   is
      Result : Expression_Access_Array (1 .. Argument_Capacity);
   begin
      for I in 1 .. Natural (Length (Arguments)) loop
         Result (I) := Arguments (I).Argument;
      end loop;
      return Result;
   exception
      when Constraint_Error =>
         raise Template_Error with "too many arguments";
   end To_Array;

   function To_Vector (Left : Expression_Access;
                      Right : Expression_Access := null)
                      return Named_Argument_Vectors.Vector
   is
      Result : Named_Argument_Vectors.Vector;
   begin
      Result.Append ((Name => Null_Unbounded_String,
                      Argument => Left));
      if Right /= null then
         Result.Append ((Name => Null_Unbounded_String,
                         Argument => Right));
      end if;
      return Result;
   end To_Vector;

   procedure Parse_Named_Arguments
     (Scanner : in out Scanner_State;
      Input : in out Jintp.Input.Character_Iterator'Class;
      Arguments : out Named_Argument_Vectors.Vector;
      Settings : Environment;
      Named_Arguments_All_Or_None : Boolean := True);

   function Parse_Primitive
     (Scanner : in out Scanner_State;
      Input : in out Jintp.Input.Character_Iterator'Class;
      Settings : Environment)
      return Jintp.Expression_Access
   is
      Current_Token : Token := Jintp.Scanner.Current_Token (Scanner);
      Result : Jintp.Expression_Access;
      Name : Unbounded_String;
      Arguments : Named_Argument_Vectors.Vector;
   begin
      case Current_Token.Kind is
         when Identifier_Token =>
            Name := Current_Token.Identifier;
            Next_Token (Scanner, Input, Current_Token, Settings);
            if Current_Token.Kind = Left_Paren_Token then
               Parse_Named_Arguments (Scanner, Input, Arguments, Settings,
                                      False);
               Result := new Expression'(Kind => Operator,
                                         Operator_Name => Name,
                                         Named_Arguments => Arguments);
            else
               Result := new Expression'(Kind => Variable,
                                         Variable_Name => Name);
            end if;
         when String_Literal_Token =>
            Result := new Expression'
              (Kind => Literal,
               Value => (Kind => String_Expression_Value,
                         S => Current_Token.String_Value
                        )
              );
            Next_Token (Scanner, Input, Current_Token, Settings);
         when Boolean_Literal_Token =>
            Result := new Expression'
              (Kind => Literal,
               Value => (Kind => Boolean_Expression_Value,
                         B => Current_Token.Boolean_Value
                        )
              );
            Next_Token (Scanner, Input, Current_Token, Settings);
         when Integer_Literal_Token =>
            Result := new Expression'
              (Kind => Literal,
               Value => (Kind => Integer_Expression_Value,
                         I => Current_Token.Integer_Value
                        )
              );
            Next_Token (Scanner, Input, Current_Token, Settings);
         when Float_Literal_Token =>
            Result := new Expression'
              (Kind => Literal,
               Value => (Kind => Float_Expression_Value,
                         F => Current_Token.Float_Value
                        )
              );
            Next_Token (Scanner, Input, Current_Token, Settings);
         when Left_Paren_Token =>
            Next_Token (Scanner, Input, Current_Token, Settings);
            Result := Parse (Scanner, Input, Settings);
            if Jintp.Scanner.Current_Token (Scanner).Kind /= Right_Paren_Token
            then
               raise Template_Error with "')' expected, found: "
                 & Current_Token.Kind'Image;
            end if;
            Next_Token (Scanner, Input, Current_Token, Settings);
            return Result;
         when Left_Bracket_Token =>
            return Parse_List (Scanner, Input, Settings);
         when Left_Brace_Token =>
            return Parse_Dictionary (Scanner, Input, Settings);
         when others =>
            raise Template_Error with
              "identifier, literal, '(', '[', or '{' expected, read "
              & Current_Token.Kind'Image;
      end case;
      while Current_Token.Kind = Period_Token
        or else Current_Token.Kind = Left_Bracket_Token loop
         if Current_Token.Kind = Period_Token then
            Next_Token (Scanner, Input, Current_Token, Settings);
            if Current_Token.Kind /= Identifier_Token then
               raise Template_Error with "identifier expected after "".""";
            end if;
            declare
               Id : constant Unbounded_String := Current_Token.Identifier;
            begin
               Next_Token (Scanner, Input, Current_Token, Settings);
               if Current_Token.Kind = Left_Paren_Token then
                  Next_Token (Scanner, Input, Current_Token, Settings);
                  if Current_Token.Kind /= Right_Paren_Token then
                     raise Template_Error with "')' expected";
                  end if;
                  Result := new Expression'
                    (Kind => Operator,
                     Operator_Name => Id,
                     Named_Arguments => To_Vector (Result)
                    );
                  Next_Token (Scanner, Input, Current_Token, Settings);
               else
                  Result := new Expression'
                    (Kind => Operator,
                     Operator_Name => To_Unbounded_String ("."),
                     Named_Arguments => To_Vector (Result,
                                   new Expression'
                                     (Kind => Variable,
                                      Variable_Name => Id
                                     )
                                   )
                    );
               end if;
            end;
         else
            Next_Token (Scanner, Input, Current_Token, Settings);
            declare
               New_Expression : constant Jintp.Expression_Access :=
                 Parse (Scanner, Input, Settings);
            begin
               Result := new Expression'
                  (Kind => Operator,
                   Operator_Name => To_Unbounded_String ("[]"),
                   Named_Arguments => To_Vector (Result, New_Expression)
                  );
            end;
            if Jintp.Scanner.Current_Token (Scanner).Kind
              /= Right_Bracket_Token
            then
               raise Template_Error with "']' expected";
            end if;
            Next_Token (Scanner, Input, Current_Token, Settings);
         end if;
      end loop;
      return Result;
   exception
      when others =>
         Delete_Expression (Result);
         raise;
   end Parse_Primitive;

   function Parse_Power (Scanner : in out Scanner_State;
                   Input : in out Jintp.Input.Character_Iterator'Class;
                   Settings : Environment)
                   return Jintp.Expression_Access is
      Right_Expression : Expression_Access;
      Result : Expression_Access := Parse_Primitive (Scanner, Input, Settings);
      Current_Token : Token := Jintp.Scanner.Current_Token (Scanner);
   begin
      while Current_Token.Kind = Power_Token loop
         Next_Token (Scanner, Input, Current_Token, Settings);
         Right_Expression := Parse_Primitive (Scanner, Input, Settings);
         Result := new Expression'
           (Kind => Operator,
            Operator_Name => To_Unbounded_String ("**"),
            Named_Arguments => To_Vector (Result, Right_Expression)
           );
         Current_Token := Jintp.Scanner.Current_Token (Scanner);
      end loop;
      return Result;
   exception
      when others =>
         Delete_Expression (Result);
         raise;
   end Parse_Power;

   function Parse_Unary (Scanner : in out Scanner_State;
                   Input : in out Jintp.Input.Character_Iterator'Class;
                   Settings : Environment)
                   return Jintp.Expression_Access is
      Current_Token : Token := Jintp.Scanner.Current_Token (Scanner);
      Operator_Name : Unbounded_String;
   begin
      case Current_Token.Kind is
         when Plus_Token =>
            Operator_Name := To_Unbounded_String ("+");
         when Minus_Token =>
            Operator_Name := To_Unbounded_String ("-");
         when others =>
            return Parse_Power (Scanner, Input, Settings);
      end case;
      Next_Token (Scanner, Input, Current_Token, Settings);
      return new Expression'
        (Kind => Operator,
         Operator_Name => Operator_Name,
         Named_Arguments => To_Vector
           (Parse_Power (Scanner, Input, Settings))
        );
   end Parse_Unary;

   function Parse_Mul_Div (Scanner : in out Scanner_State;
                   Input : in out Jintp.Input.Character_Iterator'Class;
                   Settings : Environment)
                   return Jintp.Expression_Access is
      Right_Expression : Expression_Access;
      Result : Expression_Access := Parse_Unary (Scanner, Input, Settings);
      Current_Token : Token := Jintp.Scanner.Current_Token (Scanner);
      Operator_Name : Unbounded_String;
   begin
      while Current_Token.Kind in Mul_Token .. Div_Token loop
         case Current_Token.Kind is
            when Mul_Token => Operator_Name := To_Unbounded_String ("*");
            when Div_Token => Operator_Name := To_Unbounded_String ("/");
            when Integer_Div_Token => Operator_Name
                 := To_Unbounded_String ("//");
            when others => null;
         end case;
         Next_Token (Scanner, Input, Current_Token, Settings);
         Right_Expression := Parse_Unary (Scanner, Input, Settings);
         Result := new Expression'
           (Kind => Operator,
            Operator_Name => Operator_Name,
            Named_Arguments => To_Vector (Result, Right_Expression)
           );
         Current_Token := Jintp.Scanner.Current_Token (Scanner);
      end loop;
      return Result;
   exception
      when others =>
         Delete_Expression (Result);
         raise;
   end Parse_Mul_Div;

   function Parse_Add_Sub (Scanner : in out Scanner_State;
                   Input : in out Jintp.Input.Character_Iterator'Class;
                   Settings : Environment)
                   return Jintp.Expression_Access is
      Right_Expression : Expression_Access;
      Result : Expression_Access := Parse_Mul_Div (Scanner, Input, Settings);
      Current_Token : Token := Jintp.Scanner.Current_Token (Scanner);
      Operator_Name : String (1 .. 1);
   begin
      while Current_Token.Kind in Plus_Token .. Tilde_Token loop
         case Current_Token.Kind is
            when Plus_Token => Operator_Name := "+";
            when Minus_Token => Operator_Name := "-";
            when Tilde_Token => Operator_Name := "~";
            when others => null;
         end case;
         Next_Token (Scanner, Input, Current_Token, Settings);
         Right_Expression := Parse_Mul_Div (Scanner, Input, Settings);
         Result := new Expression'
           (Kind => Operator,
            Operator_Name => To_Unbounded_String (Operator_Name),
            Named_Arguments => To_Vector (Result, Right_Expression)
           );
         Current_Token := Jintp.Scanner.Current_Token (Scanner);
      end loop;
      return Result;
   exception
      when others =>
         Delete_Expression (Result);
         raise;
   end Parse_Add_Sub;

   function To_Binop_Name (Kind : Token_Kind) return String is
   begin
      case Kind is
         when Eq_Token => return "==";
         when Ineq_Token => return "!=";
         when Le_Token => return "<=";
         when Lt_Token => return "<";
         when Ge_Token => return ">=";
         when Gt_Token => return ">";
         when others => raise Template_Error
              with "Internal error: invalid operator: " & Kind'Image;
      end case;
   end To_Binop_Name;

   function To_Binop_Name (Kind : Token_Kind) return Unbounded_String is
   begin
      return To_Unbounded_String (To_Binop_Name (Kind));
   end To_Binop_Name;

   function Is_Comparison (Kind : Token_Kind) return Boolean is
   begin
      case Kind is
         when Eq_Token | Ineq_Token | Le_Token | Lt_Token | Ge_Token
           | Gt_Token => return True;
         when others => return False;
      end case;
   end Is_Comparison;

   function Parse_Comparison
     (Scanner : in out Scanner_State;
      Input : in out Jintp.Input.Character_Iterator'Class;
      Settings : Environment)
      return Jintp.Expression_Access
   is
      Current_Token : Token;
      Left_Expression : Jintp.Expression_Access
        := Parse_Add_Sub (Scanner, Input, Settings);
      Right_Expression : Jintp.Expression_Access;
      Kind : constant Token_Kind := Jintp.Scanner.Current_Token (Scanner).Kind;
   begin
      if Kind = Is_Token then
         declare
            Name : Unbounded_String;
            Arguments : Named_Argument_Vectors.Vector;
         begin
            Next_Token (Scanner, Input, Current_Token, Settings);
            if Current_Token.Kind = Identifier_Token then
               Name := Current_Token.Identifier;
            elsif Current_Token.Kind = In_Token then
               Name := To_Unbounded_String ("in");
            else
               raise Template_Error with "expected test name, got "
                 & Current_Token.Kind'Image;
            end if;
            Next_Token (Scanner, Input, Current_Token, Settings);
            case Current_Token.Kind is
               when Left_Paren_Token =>
                  Parse_Named_Arguments (Scanner, Input, Arguments, Settings);
               when Identifier_Token .. Float_Literal_Token | Left_Bracket_Token =>
                  Arguments.Append ((Name => Null_Unbounded_String,
                                     Argument => Parse (Scanner,
                                       Input,
                                       Settings)
                                    ));
               when others =>
                  null;
            end case;
            return new Expression'
              (Kind => Test,
               Name => Name,
               Arguments => To_Array ((Null_Unbounded_String, Left_Expression)
                 & Arguments)
              );
         end;
      end if;
      if not Is_Comparison (Kind) then
         return Left_Expression;
      end if;
      Next_Token (Scanner, Input, Current_Token, Settings);
      Right_Expression := Parse_Add_Sub (Scanner, Input, Settings);
      return new Expression'
        (Kind => Operator,
         Operator_Name => To_Binop_Name (Kind),
         Named_Arguments => To_Vector (Left_Expression, Right_Expression)
        );
   exception
      when others =>
         Delete_Expression (Left_Expression);
         raise;
   end Parse_Comparison;

   function Parse_Not (Scanner : in out Scanner_State;
                   Input : in out Jintp.Input.Character_Iterator'Class;
                   Settings : Environment)
                   return Jintp.Expression_Access is
      Result : Expression_Access;
      Current_Token : Token := Jintp.Scanner.Current_Token (Scanner);
   begin
      case Current_Token.Kind is
         when Not_Token =>
            Next_Token (Scanner, Input, Current_Token, Settings);
            declare Subexpression : constant Expression_Access :=
              Parse_Comparison (Scanner, Input, Settings);
            begin
               Result := new Expression'
                 (Kind => Operator,
                  Operator_Name => To_Unbounded_String ("NOT"),
                  Named_Arguments => To_Vector (Subexpression)
                 );
            end;
         when others =>
            return Parse_Comparison (Scanner, Input, Settings);
      end case;
      return Result;
   end Parse_Not;

   function Parse_And (Scanner : in out Scanner_State;
                   Input : in out Jintp.Input.Character_Iterator'Class;
                   Settings : Environment)
                   return Jintp.Expression_Access is
      Right_Expression : Expression_Access;
      Result : Expression_Access := Parse_Not (Scanner, Input, Settings);
      Current_Token : Token := Jintp.Scanner.Current_Token (Scanner);
   begin
      while Current_Token.Kind = And_Token loop
         Next_Token (Scanner, Input, Current_Token, Settings);
         Right_Expression := Parse_Not (Scanner, Input, Settings);
         Result := new Expression'
           (Kind => Operator,
            Operator_Name => To_Unbounded_String ("AND"),
            Named_Arguments => To_Vector (Result, Right_Expression)
           );
         Current_Token := Jintp.Scanner.Current_Token (Scanner);
      end loop;
      return Result;
   exception
      when others =>
         Delete_Expression (Right_Expression);
         raise;
   end Parse_And;

   function Parse_Or (Scanner : in out Scanner_State;
                   Input : in out Jintp.Input.Character_Iterator'Class;
                   Settings : Environment)
                       return Jintp.Expression_Access is
      Right_Expression : Expression_Access;
      Result : Expression_Access := Parse_And (Scanner, Input, Settings);
      Current_Token : Token := Jintp.Scanner.Current_Token (Scanner);
   begin
      while Current_Token.Kind = Or_Token loop
         Next_Token (Scanner, Input, Current_Token, Settings);
         Right_Expression := Parse_And (Scanner, Input, Settings);
         Result := new Expression'(Kind => Operator,
                                   Operator_Name => To_Unbounded_String ("OR"),
                                   Named_Arguments => To_Vector (Result,
                                     Right_Expression)
                                   );
         Current_Token := Jintp.Scanner.Current_Token (Scanner);
      end loop;
      return Result;
   exception
      when others =>
         Delete_Expression (Result);
         raise;
   end Parse_Or;

   procedure Check_Naming_Consistency
     (Arguments : Named_Argument_Vectors.Vector;
      All_Or_None : Boolean)
   is
      Arg_Count : constant Natural := Natural (Length (Arguments));
   begin
      if Arg_Count < 2 then
         return;
      end if;

      --  Check if a name appears twice
      for I in 1 .. Arg_Count - 1 loop
         if Arguments (I).Name /= Null_Unbounded_String then
            for J in I + 1 .. Arg_Count loop
               if Arguments (J).Name = Arguments (I).Name then
                  raise Template_Error with "argument name repeated";
               end if;
            end loop;
         end if;
      end loop;

      if not All_Or_None then
         return;
      end if;
   end Check_Naming_Consistency;

   procedure Parse_Named_Arguments
     (Scanner : in out Scanner_State;
      Input : in out Jintp.Input.Character_Iterator'Class;
      Arguments : out Named_Argument_Vectors.Vector;
      Settings : Environment;
      Named_Arguments_All_Or_None : Boolean := True)
   is
      Argument : Named_Argument;
      Current_Token : Token := Jintp.Scanner.Current_Token (Scanner);
   begin
      Arguments := Named_Argument_Vectors.Empty_Vector;
      if Current_Token.Kind /= Left_Paren_Token then
         return;
      end if;
      Next_Token (Scanner, Input, Current_Token, Settings);
      if Current_Token.Kind = Right_Paren_Token then
         Next_Token (Scanner, Input, Current_Token, Settings);
         return;
      end if;
      loop
         if Current_Token.Kind = Identifier_Token then
            Argument.Name := Current_Token.Identifier;
            Next_Token (Scanner, Input, Current_Token, Settings);
            if Current_Token.Kind = Assign_Token then
               Next_Token (Scanner, Input, Current_Token, Settings);
               Argument.Argument := Parse_Or (Scanner, Input, Settings);
               Current_Token := Jintp.Scanner.Current_Token (Scanner);
            else
               Argument.Argument := new Expression'
                 (Kind => Variable,
                  Variable_Name => Argument.Name
                 );
               Argument.Name := Null_Unbounded_String;
            end if;
         else
            Argument.Argument := Parse_Or (Scanner, Input, Settings);
            Argument.Name := Null_Unbounded_String;
            Current_Token := Jintp.Scanner.Current_Token (Scanner);
         end if;
         Append (Arguments, Argument);
         if Current_Token.Kind = Right_Paren_Token then
            exit;
         end if;
         if Current_Token.Kind /= Comma_Token then
            raise Template_Error with "',' or ')' expected";
         end if;
         Next_Token (Scanner, Input, Current_Token, Settings);
      end loop;
      Check_Naming_Consistency (Arguments, Named_Arguments_All_Or_None);
      Next_Token (Scanner, Input, Current_Token, Settings);
   exception
      when others =>
         for A of Arguments loop
            Delete_Expression (A.Argument);
         end loop;
         raise;
   end Parse_Named_Arguments;

   procedure Extract_Arguments
     (Source : in out Named_Argument_Vectors.Vector;
      Target : in out Expression_Access_Array;
      Signature : Parameters)
   is
      Position : Named_Argument_Vectors.Cursor;
   begin
      for I in 1 .. Signature'Last - Signature'First + 1 loop
         if I <= Natural (Source.Length)
            and then Source (I).Name = Null_Unbounded_String
         then
            --  Positional parameter
            Target (Target'First + I - 1) := Source (I).Argument;
            Source (I).Argument := null;
         else
            Position := Find_Named_Argument
              (Source, Signature (Signature'First + I - 1).Name);
            if Position /= Named_Argument_Vectors.No_Element then
               --  Named parameter
               Target (Target'First + I - 1) := Source (Position).Argument;
               Source (Position).Argument := null;
            elsif Signature (Signature'First + I - 1).Has_Default_Value then
               --  Default parameter value
               Target (Target'First + I - 1) := new Expression'
                 (Kind => Literal,
                  Value => Signature (Signature'First + I - 1).Default_Value);
            end if;
         end if;
      end loop;
   end Extract_Arguments;

   function To_Array
     (Filter_Name : String;
      First_Argument : Expression_Access;
      Remaining_Arguments : in out Named_Argument_Vectors.Vector)
      return Expression_Access_Array
   is
      Result : Expression_Access_Array (1 .. Argument_Capacity);
   begin
      Result (1) := First_Argument;
      if Filter_Name = "dictsort" then
         Extract_Arguments
           (Remaining_Arguments,
            Result (2 .. Result'Last),
            ((Name => To_Unbounded_String ("case_sensitive"),
              Has_Default_Value => True,
              Default_Value => (Kind => Boolean_Expression_Value,
                                B => False)),
             (Name => To_Unbounded_String ("by"),
              Has_Default_Value => True,
              Default_Value => (Kind => String_Expression_Value,
                                S => To_Unbounded_String ("key"))),
             (Name => To_Unbounded_String ("reverse"),
              Has_Default_Value => True,
              Default_Value => (Kind => Boolean_Expression_Value,
                                B => False))));
      elsif Filter_Name = "batch" then
         Extract_Arguments
           (Remaining_Arguments,
            Result (2 .. Result'Last),
            ((Name => To_Unbounded_String ("linecount"),
              Has_Default_Value => False),
             (Name => To_Unbounded_String ("fill_with"),
              Has_Default_Value => False)));
      elsif Filter_Name = "slice" then
         Extract_Arguments
           (Remaining_Arguments,
            Result (2 .. Result'Last),
            ((Name => To_Unbounded_String ("slices"),
              Has_Default_Value => False),
             (Name => To_Unbounded_String ("fill_with"),
              Has_Default_Value => False)));
      elsif Filter_Name = "center" then
         Extract_Arguments
           (Remaining_Arguments,
            Result (2 .. Result'Last),
            (1 => (Name => To_Unbounded_String ("width"),
                   Has_Default_Value => True,
                   Default_Value => (Kind => Integer_Expression_Value,
                                     I => 80))));
      elsif Filter_Name = "join" then
         Extract_Arguments
           (Remaining_Arguments,
            Result (2 .. Result'Last),
            (1 => (Name => To_Unbounded_String ("d"),
                   Has_Default_Value => True,
                   Default_Value => (Kind => String_Expression_Value,
                                     S => Null_Unbounded_String))));
      elsif Filter_Name = "trim" then
         Extract_Arguments
           (Remaining_Arguments,
            Result (2 .. Result'Last),
            (1 => (Name => To_Unbounded_String ("chars"),
                   Has_Default_Value => True,
                   Default_Value => (Kind => String_Expression_Value,
                                     S => Null_Unbounded_String))));
      elsif Filter_Name = "max" or else Filter_Name = "min" then
         Extract_Arguments
           (Remaining_Arguments,
            Result (2 .. Result'Last),
            (1 => (Name => To_Unbounded_String ("case_sensitive"),
                   Has_Default_Value => True,
                   Default_Value => (Kind => Boolean_Expression_Value,
                                     B => False))));
      elsif Filter_Name = "round" then
         Extract_Arguments
           (Remaining_Arguments,
            Result (2 .. Result'Last),
            ((Name => To_Unbounded_String ("precision"),
              Has_Default_Value => True,
              Default_Value => (Kind => Integer_Expression_Value,
                                I => 0)),
             (Name => To_Unbounded_String ("method"),
              Has_Default_Value => True,
              Default_Value => (Kind => String_Expression_Value,
                                S => To_Unbounded_String ("common")))));
      elsif Filter_Name = "int" then
         Extract_Arguments
           (Remaining_Arguments,
            Result (2 .. Result'Last),
            ((Name => To_Unbounded_String ("default"),
              Has_Default_Value => True,
              Default_Value => (Kind => Integer_Expression_Value,
                                I => 0)),
             (Name => To_Unbounded_String ("base"),
              Has_Default_Value => True,
              Default_Value => (Kind => Integer_Expression_Value,
                                I => 10))));
      elsif Filter_Name = "float" then
         Extract_Arguments
           (Remaining_Arguments,
            Result (2 .. Result'Last),
            (1 => (Name => To_Unbounded_String ("default"),
                   Has_Default_Value => True,
                   Default_Value => (Kind => Float_Expression_Value,
                                     F => 0.0))));
      elsif Filter_Name = "indent" then
         Extract_Arguments
           (Remaining_Arguments,
            Result (2 .. Result'Last),
            ((Name => To_Unbounded_String ("width"),
              Has_Default_Value => True,
              Default_Value => (Kind => Integer_Expression_Value,
                                I => 4)),
             (Name => To_Unbounded_String ("first"),
              Has_Default_Value => True,
              Default_Value => (Kind => Boolean_Expression_Value,
                                B => False)),
             (Name => To_Unbounded_String ("blank"),
              Has_Default_Value => True,
              Default_Value => (Kind => Boolean_Expression_Value,
                                B => False))));
      else
         if Natural (Remaining_Arguments.Length) > Argument_Capacity + 1 then
            raise Template_Error with "too many arguments to " & Filter_Name;
         end if;
         for I in 1 .. Natural (Remaining_Arguments.Length) loop
            Result (1 + I) := Remaining_Arguments (I).Argument;
            Remaining_Arguments (I).Argument := null;
         end loop;
      end if;
      return Result;
   end To_Array;

   function Parse (Scanner : in out Scanner_State;
                   Input : in out Jintp.Input.Character_Iterator'Class;
                   Settings : Environment)
                   return Jintp.Expression_Access is
      Result : Expression_Access := Parse_Or (Scanner, Input, Settings);
      Current_Token : Token := Jintp.Scanner.Current_Token (Scanner);
      Named_Arguments : Named_Argument_Vectors.Vector;
      Filter_Name : Unbounded_String;
   begin
      while Current_Token.Kind = Pipe_Token loop
         Next_Token (Scanner, Input, Current_Token, Settings);
         if Current_Token.Kind /= Identifier_Token then
            raise Template_Error with "filter name expected after '|'";
         end if;
         Filter_Name := Current_Token.Identifier;
         Next_Token (Scanner, Input, Current_Token, Settings);
         Parse_Named_Arguments (Scanner, Input, Named_Arguments, Settings);
         Current_Token := Jintp.Scanner.Current_Token (Scanner);
         Result := new Expression'(Kind => Filter,
                                   Name => Filter_Name,
                                   Arguments =>
                                     To_Array (To_String (Filter_Name),
                                       Result,
                                       Named_Arguments));
      end loop;
      return Result;
   exception
      when others =>
         Delete_Expression (Result);
         for A of Named_Arguments loop
            Delete_Expression (A.Argument);
         end loop;
         raise;
   end Parse;

   function Parse_With_End (
     Input : in out Jintp.Input.Character_Iterator'Class;
     Settings : Environment)
     return Jintp.Expression_Access is
      Scanner : Scanner_State;
      Result : Expression_Access;
      Current_Token : Token;
   begin
      Next_Token (Scanner, Input, Current_Token, Settings);
      Result := Parse (Scanner, Input, Settings);
      Current_Token := Jintp.Scanner.Current_Token (Scanner);
      if Current_Token.Kind /= Expression_End_Token then
         raise Template_Error with "end of expression expected, got "
           & Current_Token.Kind'Image;
      end if;
      return Result;
   exception
      when others =>
         Delete_Expression (Result);
         raise;
   end Parse_With_End;

end Expression_Parser;
