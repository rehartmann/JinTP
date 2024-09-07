with Jintp.Input;

package Jintp.Scanner is

   type Scanner_State is private;

   type Token_Kind is (Identifier_Token, String_Literal_Token,
                       Boolean_Literal_Token, Integer_Literal_Token,
                       Float_Literal_Token,
                       Expression_End_Token, Statement_End_Token, In_Token,
                       Is_Token, If_Token, Else_Token, Elif_Token, Endif_Token,
                       For_Token, Endfor_Token, Include_Token, Macro_Token,
                       Endmacro_Token,
                       Or_Token, And_Token, Not_Token,
                       Assign_Token, Eq_Token, Ineq_Token,
                       Le_Token, Lt_Token, Ge_Token, Gt_Token, Comma_Token,
                       Period_Token, Colon_Token,
                       Left_Bracket_Token, Right_Bracket_Token,
                       Left_Paren_Token, Right_Paren_Token,
                       Left_Brace_Token, Right_Brace_Token,
                       Plus_Token, Minus_Token, Tilde_Token,
                       Mul_Token, Div_Token, Integer_Div_Token, Power_Token,
                       Remainder_Token, Pipe_Token
                      );

   subtype Symbolic_Token_Kind is Token_Kind range In_Token .. Not_Token;

   type Token (Kind : Token_Kind := If_Token) is record
      case Kind is
         when String_Literal_Token =>
            String_Value : Unbounded_String;
         when Boolean_Literal_Token =>
            Boolean_Value : Boolean;
         when Integer_Literal_Token =>
            Integer_Value : Integer;
         when Float_Literal_Token =>
            Float_Value : Long_Float;
         when Identifier_Token =>
            Identifier : Unbounded_String;
         when Statement_End_Token =>
            Modifier : Character;
         when others =>
            null;
      end case;
   end record;

   procedure Next_Token
     (State : in out Scanner_State;
      Input : in out Jintp.Input.Character_Iterator'Class;
      Result : out Token;
      Settings : Environment);

   function Current_Token (State : Scanner_State) return Token;

   function Is_Whitespace (C : Character) return Boolean;

private

   type Scanner_State is record
      Current_Token : Jintp.Scanner.Token;
   end record;

end Jintp.Scanner;
