with Ada.Characters.Handling;

package body Jintp.Scanner is

   use Jintp.Input;
   use Ada.Characters.Handling;

   procedure To_Token (Source : Unbounded_String; Result : out Token) is

      subtype Symbolic_Token_Kind is Token_Kind range In_Token .. Not_Token;

      False_Unbounded_String : constant Unbounded_String :=
        To_Unbounded_String ("false");
      True_Unbounded_String : constant Unbounded_String :=
        To_Unbounded_String ("true");

   begin
      if Source = False_Unbounded_String then
         Result := (Kind => Boolean_Literal_Token,
                    Boolean_Value => False);
         return;
      elsif Source = True_Unbounded_String then
         Result := (Kind => Boolean_Literal_Token,
                    Boolean_Value => True);
         return;
      end if;
      declare
         Tok_Kind : constant Symbolic_Token_Kind := Symbolic_Token_Kind'Value
           (To_Upper (To_String (Source)) & "_TOKEN");
         New_Token : Token (Kind => Tok_Kind);
      begin
         Result := New_Token;
      end;
   exception
      when Constraint_Error =>
         Result := (Kind => Identifier_Token,
                    Identifier => Source
                   );
   end To_Token;

   function Is_Whitespace (C : Character) return Boolean is
   begin
      case C is
         when ' ' | ASCII.LF | ASCII.HT | ASCII.VT | ASCII.FF | ASCII.CR =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Whitespace;

   procedure Next_Token (
                        State : in out Scanner_State;
                        Input : in out Character_Iterator'Class;
                        Result : out Token;
                        Settings : Environment) is
      C : Character := Next (Input);
      Buffer : Unbounded_String;
      Matches : Boolean;
      E_Found : Boolean := False;
   begin
      while Is_Whitespace (C) loop
         C := Next (Input);
      end loop;
      if C = Element (Settings.Expression_End, 1) then
         Match (Input, Slice (Settings.Expression_End,
                2, Length (Settings.Expression_End)),
                Matches);
         if Matches then
            Result := (Kind => Expression_End_Token);
            State.Current_Token := Result;
            return;
         end if;
      end if;
      if C = Element (Settings.Statement_End, 1) then
         Match (Input, Slice (Settings.Statement_End,
                2, Length (Settings.Statement_End)),
                Matches);
         if Matches then
            Result := (Kind => Statement_End_Token,
                       Modifier => ' '
                      );
            State.Current_Token := Result;
            return;
         end if;
      end if;
      if Is_Letter (C) or else C = '_' then
         Buffer := To_Unbounded_String ((1 => C));
         C := Next (Input);
         while Is_Alphanumeric (C) or else C = '_' loop
            Append (Buffer, C);
            C := Next (Input);
         end loop;
         Back (Input);
         To_Token (Buffer, Result);
         State.Current_Token := Result;
         return;
      end if;
      if Is_Digit (C) then
         loop
            Append (Buffer, C);
            C := Next (Input);
            if C = '.' then
               loop
                  Append (Buffer, C);
                  C := Next (Input);
                  if not Is_Digit (C) then
                     if C = 'e' or else C = 'E' then
                        if E_Found then
                           exit;
                        end if;
                        Append (Buffer, C);
                        C := Next (Input);
                        if not Is_Digit (C) and then C /= '+' and then C /= '-'
                        then
                           exit;
                        end if;
                        E_Found := True;
                     else
                        exit;
                     end if;
                  end if;
               end loop;
               Back (Input);
               Result := (Kind => Float_Literal_Token,
                          Float_Value => Long_Float'Value (To_String (Buffer)));
               State.Current_Token := Result;
               return;
            end if;
            if not Is_Digit (C) then
               exit;
            end if;
         end loop;
         Back (Input);
         Result := (Kind => Integer_Literal_Token,
                    Integer_Value => Integer'Value (To_String (Buffer)));
         State.Current_Token := Result;
         return;
      end if;
      case C is
         when '=' =>
            C := Next (Input);
            if C = '=' then
               Result := (Kind => Eq_Token);
               State.Current_Token := Result;
               return;
            else
               Back (Input);
               Result := (Kind => Assign_Token);
               State.Current_Token := Result;
               return;
            end if;
         when '!' =>
            C := Next (Input);
            if C = '=' then
               Result := (Kind => Ineq_Token);
               State.Current_Token := Result;
               return;
            else
               raise Template_Error with "'=' expected after '!'";
            end if;
         when '<' =>
            C := Next (Input);
            if C = '=' then
               Result := (Kind => Le_Token);
               State.Current_Token := Result;
               return;
            end if;
            Back (Input);
            Result := (Kind => Lt_Token);
            State.Current_Token := Result;
            return;
         when '>' =>
            C := Next (Input);
            if C = '=' then
               Result := (Kind => Ge_Token);
               State.Current_Token := Result;
               return;
            end if;
            Back (Input);
            Result := (Kind => Gt_Token);
            State.Current_Token := Result;
            return;
         when ',' =>
            Result := (Kind => Comma_Token);
            State.Current_Token := Result;
            return;
         when '.' =>
            Result := (Kind => Period_Token);
            State.Current_Token := Result;
            return;
         when ':' =>
            Result := (Kind => Colon_Token);
            State.Current_Token := Result;
            return;
         when '(' =>
            Result := (Kind => Left_Paren_Token);
            State.Current_Token := Result;
            return;
         when ')' =>
            Result := (Kind => Right_Paren_Token);
            State.Current_Token := Result;
            return;
         when '[' =>
            Result := (Kind => Left_Bracket_Token);
            State.Current_Token := Result;
            return;
         when ']' =>
            Result := (Kind => Right_Bracket_Token);
            State.Current_Token := Result;
            return;
         when '{' =>
            Result := (Kind => Left_Brace_Token);
            State.Current_Token := Result;
            return;
         when '}' =>
            Result := (Kind => Right_Brace_Token);
            State.Current_Token := Result;
            return;
         when '+' =>
            Match (Input, To_String (Settings.Statement_End), Matches);
            if Matches then
               Result := (Kind => Statement_End_Token,
                          Modifier => '+'
                         );
               State.Current_Token := Result;
               return;
            end if;
            Result := (Kind => Plus_Token);
            State.Current_Token := Result;
            return;
         when '-' =>
            Match (Input, To_String (Settings.Statement_End), Matches);
            if Matches then
               Result := (Kind => Statement_End_Token,
                          Modifier => '-'
                         );
               State.Current_Token := Result;
               return;
            end if;
            Result := (Kind => Minus_Token);
            State.Current_Token := Result;
            return;
         when '*' =>
            C := Next (Input);
            if C = '*' then
               Result := (Kind => Power_Token);
               State.Current_Token := Result;
               return;
            end if;
            Back (Input);
            Result := (Kind => Mul_Token);
            State.Current_Token := Result;
            return;
         when '/' =>
            C := Next (Input);
            if C = '/' then
               Result := (Kind => Integer_Div_Token);
               State.Current_Token := Result;
               return;
            end if;
            Back (Input);
            Result := (Kind => Div_Token);
            State.Current_Token := Result;
            return;
         when '%' =>
            Result := (Kind => Remainder_Token);
            State.Current_Token := Result;
            return;
         when '~' =>
            Result := (Kind => Tilde_Token);
            State.Current_Token := Result;
            return;
         when '|' =>
            Result := (Kind => Pipe_Token);
            State.Current_Token := Result;
            return;
         when ''' | '"' =>
            declare
               Str_Buf : Unbounded_String;
               Delimiter : constant Character := C;
            begin
               C := Next (Input);
               while C /= Delimiter loop
                  if C = '\' then
                     C := Next (Input);
                     case C is
                        when ASCII.CR =>
                           C := Next (Input);
                           if C = ASCII.LF then
                              C := Next (Input);
                           end if;
                        when ASCII.LF =>
                           C := Next (Input);
                        when '\' =>
                           C := '\';
                        when ''' =>
                           C := ''';
                        when '"' =>
                           C := '\';
                        when 'a' =>
                           C := ASCII.BEL;
                        when 'b' =>
                           C := ASCII.BS;
                        when 'f' =>
                           C := ASCII.FF;
                        when 'n' =>
                           C := ASCII.LF;
                        when 'r' =>
                           C := ASCII.CR;
                        when 't' =>
                           C := ASCII.HT;
                        when 'v' =>
                           C := ASCII.VT;
                        when others =>
                           Append (Str_Buf, '\');
                     end case;
                  end if;
                  Append (Str_Buf, C);
                  C := Next (Input);
               end loop;
               Result := (Kind => String_Literal_Token,
                          String_Value => Str_Buf);
               State.Current_Token := Result;
               return;
            end;
         when others =>
            null;
      end case;
      raise Template_Error with "unexpected char '" & C & ''';
   exception
      when Constraint_Error =>
         raise Template_Error with "unexpected end of input";
   end Next_Token;

   function Current_Token (State : Scanner_State) return Token is
   begin
      return State.Current_Token;
   end Current_Token;

end Jintp.Scanner;
