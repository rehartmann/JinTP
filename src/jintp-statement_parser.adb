with Jintp.Input;
with Jintp.Scanner;
with Ada.Characters.Handling;
with Ada.Text_IO; use Ada.Text_IO;

separate (Jintp)
package body Statement_Parser is

   use Jintp.Scanner;

   procedure Parse_Value
     (Scanner : in out Scanner_State;
      Input : in out Jintp.Input.Character_Iterator'Class;
      Value : out Expression_Value;
      Settings : Environment'Class) is
      Expression : Expression_Access := Jintp.Expression_Parser.Parse
        (Scanner, Input, Settings);
      Resolver : Jintp.Root_Context;
   begin
      Value := Evaluate (Expression.all, Resolver);
      Delete_Expression (Expression);
   exception
      when others =>
         Delete_Expression (Expression);
   end Parse_Value;

   procedure Parse_Parameters
     (Scanner : in out Scanner_State;
      Input : in out Jintp.Input.Character_Iterator'Class;
      Parameters : out Parameter_Vectors.Vector;
      Settings : Environment'Class) is
      Name : Unbounded_String;
      Default_Value : Expression_Value;
      Current_Token : Token := Jintp.Scanner.Current_Token (Scanner);
   begin
      if Current_Token.Kind /= Left_Paren_Token then
         raise Template_Error with "'(' expected, found "
           & Current_Token.Kind'Image;
      end if;
      Next_Token (Scanner, Input, Current_Token, Settings);
      if Current_Token.Kind = Right_Paren_Token then
         return;
      end if;

      loop
         if Current_Token.Kind /= Identifier_Token then
            raise Template_Error with "identifier expected, found "
              & Current_Token.Kind'Image;
         end if;
         Name := Current_Token.Identifier;
         Next_Token (Scanner, Input, Current_Token, Settings);
         if Current_Token.Kind = Assign_Token then
            Next_Token (Scanner, Input, Current_Token, Settings);
            Parse_Value (Scanner, Input, Default_Value, Settings);
            Current_Token := Jintp.Scanner.Current_Token (Scanner);
            Parameters.Append (Parameter'
                      (Has_Default_Value => True,
                       Name => Name,
                       Default_Value => Default_Value));
         else
            Parameters.Append (Parameter'
                      (Has_Default_Value => False,
                       Name => Name));
         end if;
         if Current_Token.Kind = Right_Paren_Token then
            return;
         end if;
         if Current_Token.Kind /= Comma_Token then
            raise Template_Error with "',' expected, found "
                 & Current_Token.Kind'Image;
         end if;
         Next_Token (Scanner, Input, Current_Token, Settings);
      end loop;
   end Parse_Parameters;

   procedure Parse (Input : in out Character_Iterator'Class;
                    Settings : Environment'Class;
                    Result : out Statement;
                    End_Modifier : out Character) is
      Control_Expression : Expression_Access;
      Variable_Name : Unbounded_String;
      Variable_2_Name : Unbounded_String;
      Condition : Expression_Access := null;
      Scanner : Scanner_State;
      Current_Token : Token;
      Kind : Token_Kind;
      Parameters : Parameter_Vectors.Vector;
      Macro_Name : Unbounded_String;
   begin
      Next_Token (Scanner, Input, Current_Token, Settings);
      case Current_Token.Kind is
         when If_Token | Elif_Token =>
            Kind := Current_Token.Kind;
            Next_Token (Scanner, Input, Current_Token, Settings);
            Control_Expression := Jintp.Expression_Parser
              .Parse (Scanner, Input, Settings);
            if Kind = If_Token then
               Result := (Kind => If_Statement,
                          If_Condition => Control_Expression
                         );
            else
               Result := (Kind => Elif_Statement,
                          If_Condition => Control_Expression
                         );
            end if;
         when Else_Token =>
            Result := (Kind => Else_Statement);
            Next_Token (Scanner, Input, Current_Token, Settings);
         when Endif_Token =>
            Result := (Kind => Endif_Statement);
            Next_Token (Scanner, Input, Current_Token, Settings);
         when For_Token =>
            Next_Token (Scanner, Input, Current_Token, Settings);
            if Current_Token.Kind /= Identifier_Token then
               raise Template_Error with "loop variable expected, found"
                 & Current_Token.Kind'Image;
            end if;
            Variable_Name := Current_Token.Identifier;
            Next_Token (Scanner, Input, Current_Token, Settings);
            if Current_Token.Kind = Comma_Token then
               Next_Token (Scanner, Input, Current_Token, Settings);
               if Current_Token.Kind /= Identifier_Token then
                  raise Template_Error with "loop variable expected";
               end if;
               Variable_2_Name := Current_Token.Identifier;
               Next_Token (Scanner, Input, Current_Token, Settings);
            end if;
            if Current_Token.Kind /= In_Token then
               raise Template_Error with "'in' expected";
            end if;
            Next_Token (Scanner, Input, Current_Token, Settings);
            Control_Expression := Jintp.Expression_Parser
              .Parse (Scanner, Input, Settings);
            Current_Token := Jintp.Scanner.Current_Token (Scanner);
            if Current_Token.Kind = If_Token then
               Next_Token (Scanner, Input, Current_Token, Settings);
               Condition := Jintp.Expression_Parser
                 .Parse (Scanner, Input, Settings);
            end if;
            Result := (Kind => For_Statement,
                       For_Variable_1_Name => Variable_Name,
                       For_Variable_2_Name => Variable_2_Name,
                       For_Expression => Control_Expression,
                       For_Condition => Condition);
         when Endfor_Token =>
            Result := (Kind => Endfor_Statement);
            Next_Token (Scanner, Input, Current_Token, Settings);
         when Include_Token =>
            Next_Token (Scanner, Input, Current_Token, Settings);
            if Current_Token.Kind /= String_Literal_Token then
               raise Template_Error with "filename expected";
            end if;
            Result := (Kind => Include_Statement,
                       Filename => Current_Token.String_Value);
            Next_Token (Scanner, Input, Current_Token, Settings);
         when Macro_Token =>
            Next_Token (Scanner, Input, Current_Token, Settings);
            if Current_Token.Kind /= Identifier_Token then
               raise Template_Error with "macro name expected, got "
                 & Current_Token.Kind'Image;
            end if;
            Macro_Name := Current_Token.Identifier;
            Next_Token (Scanner, Input, Current_Token, Settings);
            Parse_Parameters (Scanner, Input, Parameters, Settings);
            Result := (Kind => Macro_Statement,
                       Macro_Name => Macro_Name,
                       Macro_Parameters => Parameters);
            Next_Token (Scanner, Input, Current_Token, Settings);
         when Endmacro_Token =>
            Result := (Kind => Endmacro_Statement);
            Next_Token (Scanner, Input, Current_Token, Settings);
         when Raw_Token =>
            Result := (Kind => Raw_Statement);
            Next_Token (Scanner, Input, Current_Token, Settings);
         when Endraw_Token =>
            Result := (Kind => Endraw_Statement);
            Next_Token (Scanner, Input, Current_Token, Settings);
         when Extends_Token =>
            Next_Token (Scanner, Input, Current_Token, Settings);
            if Current_Token.Kind /= String_Literal_Token then
               raise Template_Error with "template name expected, got "
                 & Current_Token.Kind'Image;
            end if;
            Result := (Kind => Extends_Statement,
                       Parent_Name => Current_Token.String_Value);
            Next_Token (Scanner, Input, Current_Token, Settings);
         when Block_Token =>
            Next_Token (Scanner, Input, Current_Token, Settings);
            if Current_Token.Kind /= Identifier_Token then
               raise Template_Error with "block name expected, got "
                 & Current_Token.Kind'Image;
            end if;
            Result := (Kind => Block_Statement,
                       Block_Name => Current_Token.Identifier);
            Next_Token (Scanner, Input, Current_Token, Settings);
         when Endblock_Token =>
            Result := (Kind => Endblock_Statement);
            Next_Token (Scanner, Input, Current_Token, Settings);
         when others =>
            raise Template_Error with "unexpected token "
              & Current_Token.Kind'Image;
      end case;
      Current_Token := Jintp.Scanner.Current_Token (Scanner);
      if Current_Token.Kind /= Statement_End_Token then
         raise Template_Error with "end of statement expected, got "
           & Current_Token.Kind'Image;
      end if;
      End_Modifier := Current_Token.Modifier;
   end Parse;

   function Parse_Endraw  (Input : in out Character_Iterator'Class;
                           Settings : Environment'Class)
                           return Boolean
   is
      Scanner : Scanner_State;
      Current_Token : Token;
   begin
      Next_Token (Scanner, Input, Current_Token, Settings);
      if Current_Token.Kind /= Endraw_Token then
         return False;
      end if;
      Next_Token (Scanner, Input, Current_Token, Settings);
      if Current_Token.Kind /= Statement_End_Token then
         return False;
      end if;
      return True;
   end Parse_Endraw;

end Statement_Parser;
