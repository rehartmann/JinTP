with Jintp.Input;
with Jintp.Scanner;
with Ada.Characters.Handling;
with Ada.Text_IO; use Ada.Text_IO;

separate (Jintp)
package body Statement_Parser is

   use Jintp.Scanner;

   procedure Parse (Input : in out Character_Iterator'Class;
                    Settings : Environment;
                    Result : out Statement;
                    End_Modifier : out Character) is
      Control_Expression : Expression_Access;
      Variable_Name : Unbounded_String;
      Variable_2_Name : Unbounded_String;
      Scanner : Scanner_State;
      Current_Token : Token;
      Kind : Token_Kind;
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
            Result := (Kind => For_Statement,
                       For_Variable_1_Name => Variable_Name,
                       For_Variable_2_Name => Variable_2_Name,
                       For_Expression => Control_Expression);
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
         when others =>
            raise Template_Error with "unexpected token "
              & Current_Token.Kind'Image;
      end case;
      Current_Token := Jintp.Scanner.Current_Token (Scanner);
      if Current_Token.Kind /= Statement_End_Token then
         raise Template_Error with "end of statement expected, found "
           & Current_Token.Kind'Image;
      end if;
      End_Modifier := Current_Token.Modifier;
   end Parse;

end Statement_Parser;
