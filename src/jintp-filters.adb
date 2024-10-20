with Ada.Characters.Handling;
with Ada.Wide_Wide_Characters.Handling;
with Ada.Strings.UTF_Encoding;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Strings.Unbounded.Less_Case_Insensitive;
with Ada.Strings.Maps;

separate (Jintp)
package body Filters is

   use Ada.Strings.UTF_Encoding;

   Default_Trim_Characters : constant Ada.Strings.Maps.Character_Set
     := Ada.Strings.Maps.To_Set
       (Ada.Strings.Maps.Character_Sequence'
          (' ', ASCII.LF, ASCII.HT, ASCII.VT, ASCII.FF, ASCII.CR));

   function Html_Escape (Source : String) return Unbounded_String
     with Post =>
       Index (Html_Escape'Result, Ada.Strings.Maps.To_Set ("<>'""")) = 0
   is
      Result : Unbounded_String := Null_Unbounded_String;
   begin
      for I in Source'Range loop
         case Source (I) is
            when '&' => Append (Result, "&amp;");
            when '<' => Append (Result, "&lt;");
            when '>' => Append (Result, "&gt;");
            when '"' => Append (Result, "&#34;");
            when ''' => Append (Result, "&#39;");
            when others => Append (Result, Source (I));
         end case;
      end loop;
      return Result;
   end Html_Escape;

   function UTF_8_Length (Source : Unbounded_String) return Natural is
      Result : Natural := 0;
      C : Character;
   begin
      for I in 1 .. Length (Source) loop
         C := Element (Source, I);
         if Ada.Characters.Handling.Is_ISO_646 (C)
           or else Character'Pos (C) >= 192
         then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end UTF_8_Length;

   function Count (Source_Value : Expression_Value)
                   return Expression_Value is
      Result : Integer;
   begin
      case Source_Value.Kind is
         when String_Expression_Value =>
            Result := UTF_8_Length (Source_Value.S);
         when Dictionary_Expression_Value =>
            Result := Integer (Source_Value.Dictionary_Value.Assocs
                               .Value_Assocs.Length);
         when List_Expression_Value =>
            Result := Integer (Source_Value.List_Value.Elements.Values.Length);
         when others =>
            raise Template_Error with "invalid argument to 'count'";
      end case;
      return (Kind => Integer_Expression_Value,
              I => Result);
   end Count;

   function Slice (Source : Expression_Value_Vectors.Vector;
                   Slice_Length : Positive;
                   Spread : Boolean;
                   Fill : Boolean;
                   Fill_With : Expression_Value :=
                     (Kind => String_Expression_Value,
                      S => Null_Unbounded_String))
                   return Expression_Value
   is
      Result : List;
      Sublist : List;
      Row_Count : Positive;
      Row : Positive := 1;
   begin
      Row_Count := Natural (Length (Source)) / Slice_Length;
      if Natural (Length (Source)) mod Slice_Length > 0 then
         Row_Count := Row_Count + 1;
      end if;

      Init (Result);
      for V of Source loop
         Init (Sublist);
         Append (Sublist.Elements.Values, V);
         if Natural (Length (Sublist.Elements.Values)) = Slice_Length
           or else (Spread
                    and then Row > Row_Count + Natural (Length (Source))
                        - Slice_Length * Row_Count
                    and then Natural (Length (Sublist.Elements.Values))
                             = Slice_Length - 1)
         then
            if Fill and then Spread
              and then Natural (Length (Sublist.Elements.Values))
                = Slice_Length - 1
            then
               Append (Sublist.Elements.Values, Fill_With);
            end if;
            Append (Result.Elements.Values,
                    (Kind => List_Expression_Value,
                     List_Value => Sublist));
            Sublist := List'(Ada.Finalization.Controlled with Elements => null);
            Row := Row + 1;
         end if;
      end loop;
      if Fill and not Spread then
         for I in 1 .. Slice_Length * Row_Count - Natural (Length (Source))
         loop
            Init (Sublist);
            Append (Sublist.Elements.Values, Fill_With);
         end loop;
      end if;
      if Sublist.Elements /= null then
         Append (Result.Elements.Values,
                 (Kind => List_Expression_Value,
                  List_Value => Sublist));
      end if;
      return (Kind => List_Expression_Value,
              List_Value => Result);
   end Slice;

   function Is_Empty_Line (S : String) return Boolean is
   begin
      return (S'Length = 2 and then S (S'First) = ASCII.CR
              and then S (S'First + 1) = ASCII.LF)
        or else (S'Length = 1 and then S (S'First) = ASCII.LF);
   end Is_Empty_Line;

   function Evaluate_Filter (Source : Expression;
                             Resolver : in out Contexts.Context'class)
                             return Expression_Value is

      function Evaluate_Batch return Expression_Value is
         Value_Argument : constant Expression_Access := Source.Arguments (1);
         Count_Argument : constant Expression_Access := Source.Arguments (2);
         Fill_With_Argument : constant Expression_Access
           := Source.Arguments (3);
         Source_Value, Count_Value : Expression_Value;
      begin
         if Value_Argument = null or else Count_Argument = null then
            raise Template_Error with "invalid number of arguments to 'batch'";
         end if;
         Source_Value := Evaluate (Value_Argument.all, Resolver);
         if Source_Value.Kind /= List_Expression_Value then
            raise Template_Error
              with "'value' argument to 'batch' must be a list";
         end if;
         Count_Value := Evaluate (Count_Argument.all, Resolver);
         if Count_Value.Kind /= Integer_Expression_Value then
            raise Template_Error
              with "'linecount' argument to 'batch' must be integer";
         end if;
         if Count_Value.I < 0 then
            raise Template_Error
              with "negative value not allowed";
         end if;
         if Fill_With_Argument = null then
            return Slice (Source_Value.List_Value.Elements.Values,
                                 Count_Value.I,
                                 False,
                                 False);
         end if;
         return Slice (Source_Value.List_Value.Elements.Values,
                              Count_Value.I,
                              False,
                              True,
                              Evaluate (Fill_With_Argument.all, Resolver));
      end Evaluate_Batch;

      function Evaluate_Slice return Expression_Value is
         Value_Argument : constant Expression_Access := Source.Arguments (1);
         Slices_Argument : constant Expression_Access := Source.Arguments (2);
         Fill_With_Argument : constant Expression_Access
           := Source.Arguments (3);
         Source_Value, Slices_Value : Expression_Value;
         Slice_Length : Natural;
      begin
         if Value_Argument = null or else Slices_Argument = null then
            raise Template_Error with "invalid number of arguments to 'slice'";
         end if;
         Source_Value := Evaluate (Value_Argument.all, Resolver);
         if Source_Value.Kind /= List_Expression_Value then
            raise Template_Error
              with "'value' argument to 'slice' must be a list";
         end if;
         Slices_Value := Evaluate (Slices_Argument.all,
                                   Resolver);
         if Slices_Value.Kind /= Integer_Expression_Value then
            raise Template_Error
              with "'slices' argument to 'slice' must be integer";
         end if;
         if Slices_Value.I < 1 then
            raise Template_Error
              with "invalid value of 'slices' argument";
         end if;
         Slice_Length := Integer (Length
                                  (Source_Value.List_Value.Elements.Values))
           / Slices_Value.I;
         if Integer (Length (Source_Value.List_Value.Elements.Values))
           mod Slices_Value.I > 0
         then
            Slice_Length := Slice_Length + 1;
         end if;
         if Fill_With_Argument = null then
            return Slice (Source_Value.List_Value.Elements.Values,
                                 Slice_Length,
                                 True,
                                 False);
         end if;
         return Slice (Source_Value.List_Value.Elements.Values,
                              Slice_Length,
                              True,
                              True,
                              Evaluate (Fill_With_Argument.all, Resolver));
      end Evaluate_Slice;

      function Evaluate_Center return Expression_Value is
         Width_Value : Expression_Value;
         Left_Padding, Right_Padding : Natural;
         Value : constant String := Evaluate (Source.Arguments (1).all,
                                              Resolver);
      begin
         Width_Value := Evaluate (Source.Arguments (2).all,
                                  Resolver);
         if Width_Value.Kind /= Integer_Expression_Value then
            raise Template_Error with "argument must be integer";
         end if;
         if Value'Length >= Width_Value.I then
            return (Kind => String_Expression_Value,
                    S => To_Unbounded_String (Value));
         end if;
         Left_Padding := (Width_Value.I - Value'Length) / 2;
         Right_Padding := Left_Padding
           + (if 2 * Left_Padding + Value'Length < Width_Value.I
              then 1 else 0);
         return  (Kind => String_Expression_Value,
                  S => (Left_Padding * ' ') & Value
                       & (Right_Padding * ' '));
      end Evaluate_Center;

      function Evaluate_Max (Source_List : List)
                             return Expression_Value is
         Len : constant Ada.Containers.Count_Type
           := Length (Source_List.Elements.Values);
         Case_Sensitive : Expression_Value;
         Result : Expression_Value;
      begin
         if Len = 0 then
            return Result;
         end if;
         Case_Sensitive := Evaluate (Source.Arguments (2).all, Resolver);
         Result := Source_List.Elements.Values.First_Element;
         if Result.Kind = String_Expression_Value
           and then not Case_Sensitive.B
         then
            for I in 1 .. Natural (Len) - 1 loop
               if Ada.Strings.Unbounded.Less_Case_Insensitive
                 (Result.S, Source_List.Elements.Values.Element (I).S)
               then
                  Result := Source_List.Elements.Values (I);
               end if;
            end loop;
         else
            for I in 1 .. Natural (Len) - 1 loop
               if Result < Source_List.Elements.Values.Element (I) then
                  Result := Source_List.Elements.Values (I);
               end if;
            end loop;
         end if;
         return Result;
      end Evaluate_Max;

      function Evaluate_Min (Source_List : List)
                          return Expression_Value is
         Len : constant Ada.Containers.Count_Type
           := Length (Source_List.Elements.Values);
         Case_Sensitive : Expression_Value;
         Result : Expression_Value;
      begin
         if Len = 0 then
            return Result;
         end if;
         Case_Sensitive := Evaluate (Source.Arguments (2).all, Resolver);
         Result := Source_List.Elements.Values.First_Element;
         if Result.Kind = String_Expression_Value
           and then not Case_Sensitive.B
         then
            for I in 1 .. Natural (Len) - 1 loop
               if Ada.Strings.Unbounded.Less_Case_Insensitive
                 (Source_List.Elements.Values.Element (I).S, Result.S)
               then
                  Result := Source_List.Elements.Values (I);
               end if;
            end loop;
         else
            for I in 1 .. Natural (Len) - 1 loop
               if Source_List.Elements.Values.Element (I) < Result then
                  Result := Source_List.Elements.Values (I);
               end if;
            end loop;
         end if;
         return Result;
      end Evaluate_Min;

      Source_Value : Expression_Value;

      function Evaluate_Join return Expression_Value
      is
         Buffer : Unbounded_String := Null_Unbounded_String;
         First_Element : Boolean := True;
         Separator : constant String
           := Evaluate (Source.Arguments (2).all, Resolver);
      begin
         case Source_Value.Kind is
            when List_Expression_Value =>
               for V of Source_Value.List_Value.Elements.Values loop
                  if not First_Element then
                     Append (Buffer, Separator);
                  end if;
                  First_Element := False;
                  Append (Buffer, To_String (V));
               end loop;
               return (Kind => String_Expression_Value,
                       S => Buffer);
            when others =>
               raise Template_Error
                 with "argument type not supported by 'join'";
         end case;
      end Evaluate_Join;

      procedure Raise_Invalid_Method;
      pragma No_Return (Raise_Invalid_Method);

      procedure Raise_Invalid_Method is
      begin
         raise Template_Error with "method must be common, ceil or floor";
      end Raise_Invalid_Method;

      function Evaluate_Round return Expression_Value
      is
         Precision_Value : constant Expression_Value
           := Evaluate (Source.Arguments (2).all, Resolver);
         Method_Value : constant Expression_Value
           := Evaluate (Source.Arguments (3).all, Resolver);
         V : Long_Float := To_Float (Source_Value);
         Scale_Factor : Long_Float;
      begin
         if Precision_Value.Kind /= Integer_Expression_Value then
            raise Template_Error with "precision must be integer";
         end if;
         if Method_Value.Kind /= String_Expression_Value then
            Raise_Invalid_Method;
         end if;
         if Precision_Value.I > 0 then
            Scale_Factor := 10.0**Precision_Value.I;
         end if;
         if Method_Value.S = "floor" then
            if Precision_Value.I = 0 then
               return (Kind => Float_Expression_Value,
                       F => Long_Float'Floor (V));
            else
               return (Kind => Float_Expression_Value,
                       F => Long_Float'Floor (V * Scale_Factor) / Scale_Factor);
            end if;
         elsif Method_Value.S = "ceil" then
            if Precision_Value.I = 0 then
               return (Kind => Float_Expression_Value,
                       F => Long_Float'Ceiling (V));
            else
               return (Kind => Float_Expression_Value,
                       F => Long_Float'Ceiling (V * Scale_Factor) / Scale_Factor);
            end if;
         elsif Method_Value.S = "common" then
            if Precision_Value.I = 0 then
               if V >= 0.0 then
                  if Long_Float'Remainder (Long_Float'Floor (V), 2.0) = 0.0 then
                     --  Round down if first eliminated digit is 5
                     return (Kind => Float_Expression_Value,
                             F => Long_Float'Ceiling (V - 0.5));
                  else
                     --  Round up if first eliminated digit is 5
                     return (Kind => Float_Expression_Value,
                             F => Long_Float'Floor (V + 0.5));
                  end if;
               else
                  if Long_Float'Remainder (Long_Float'Floor (-V), 2.0) = 0.0 then
                     --  Round up if first eliminated digit is 5
                     return (Kind => Float_Expression_Value,
                             F => Long_Float'Floor (V + 0.5));
                  else
                     --  Round down if first eliminated digit is 5
                     return (Kind => Float_Expression_Value,
                             F => Long_Float'Ceiling (V - 0.5));
                  end if;
               end if;
            else
               V := V * Scale_Factor;
               if V >= 0.0 then
                  if Long_Float'Remainder (Long_Float'Floor (V), 2.0) = 0.0 then
                     return (Kind => Float_Expression_Value,
                             F => Long_Float'Ceiling (V - 0.5) / Scale_Factor);
                  else
                     return (Kind => Float_Expression_Value,
                             F => Long_Float'Floor (V + 0.5) / Scale_Factor);
                  end if;
               else
                  if Long_Float'Remainder (Long_Float'Floor (-V), 2.0) = 0.0 then
                     return (Kind => Float_Expression_Value,
                             F => Long_Float'Floor (V + 0.5) / Scale_Factor);
                  else
                     return (Kind => Float_Expression_Value,
                             F => Long_Float'Ceiling (V - 0.5) / Scale_Factor);
                  end if;
               end if;
            end if;
         else
            Raise_Invalid_Method;
         end if;
      end Evaluate_Round;

      function Evaluate_Int return Expression_Value
      is
         Default_Value : constant Expression_Value
           := Evaluate (Source.Arguments (2).all, Resolver);
         Base_Value : constant Expression_Value
           := Evaluate (Source.Arguments (3).all, Resolver);
      begin
         case Source_Value.Kind is
            when Integer_Expression_Value =>
               return Source_Value;
            when Float_Expression_Value =>
               return (Kind => Integer_Expression_Value,
                       I => Integer (Long_Float'Truncation (Source_Value.F)));
            when String_Expression_Value =>
               if Base_Value.I = 10 then
                  return (Kind => Integer_Expression_Value,
                          I => Integer'Value (To_String (Source_Value.S)));
               else
                  if Length (Source_Value.S) > 2
                    and then
                      ((Base_Value.I = 16
                        and then
                          (Slice (Source_Value.S, 1, 2) = "0x"
                           or else Slice (Source_Value.S, 1, 2) = "0X"))
                       or else
                         (Base_Value.I = 8
                          and then (Slice (Source_Value.S, 1, 2) = "0o"
                                    or else Slice (Source_Value.S, 1, 2) = "0O"))
                       or else
                         (Base_Value.I = 2
                          and then (Slice (Source_Value.S, 1, 2) = "0b"
                                    or else Slice (Source_Value.S, 1, 2) = "0B")))
                  then
                     Source_Value.S := To_Unbounded_String
                       (Slice (Source_Value.S, 3, Length (Source_Value.S)));
                  end if;
                  return (Kind => Integer_Expression_Value,
                          I => Integer'Value (Base_Value.I'Image
                            & '#' & To_String (Source_Value.S) & '#'));
               end if;
            when others =>
               return Default_Value;
         end case;
      exception
         when Constraint_Error =>
            return Default_Value;
      end Evaluate_Int;

      function Evaluate_Float return Expression_Value
      is
         Default_Value : constant Expression_Value
           := Evaluate (Source.Arguments (2).all, Resolver);
      begin
         case Source_Value.Kind is
            when Integer_Expression_Value =>
               return (Kind => Float_Expression_Value,
                       F => Long_Float (Source_Value.I));
            when Float_Expression_Value =>
               return Source_Value;
            when String_Expression_Value =>
               return (Kind => Float_Expression_Value,
                       F => Long_Float'Value
                         (To_String (Source_Value.S)));
            when others =>
               return Default_Value;
         end case;
      exception
         when Constraint_Error =>
            return Default_Value;
      end Evaluate_Float;

      function Evaluate_Indent return Expression_Value
      is
         Width_Value : constant Expression_Value
           := Evaluate (Source.Arguments (2).all, Resolver);
         First_Value : constant Expression_Value
           := Evaluate (Source.Arguments (3).all, Resolver);
         Blank_Value : constant Expression_Value
           := Evaluate (Source.Arguments (4).all, Resolver);
         Result : Unbounded_String;
         Indentation : Unbounded_String;
         Pos : Positive;
         New_Pos : Natural;
      begin
         if Source_Value.Kind /= String_Expression_Value then
            raise Template_Error
              with "first argument to 'indent' must be a string";
         end if;
         if First_Value.Kind /= Boolean_Expression_Value
           or else Blank_Value.Kind /= Boolean_Expression_Value
         then
            raise Template_Error
              with "invalid argument to 'indent'";
         end if;
         case Width_Value.Kind is
            when String_Expression_Value =>
               Indentation := Width_Value.S;
            when Integer_Expression_Value =>
               Indentation := Width_Value.I * ' ';
            when others =>
               raise Template_Error
                 with "invalid width argument to 'indent'";
         end case;

         if First_Value.B then
            Result := Indentation;
         end if;
         Pos := 1;
         New_Pos := Index (Source_Value.S, (1 => ASCII.LF), Pos);
         if New_Pos = 0 then
            Append (Result, Slice (Source_Value.S,
                                   Pos,
                                   Length (Source_Value.S)));
            return (Kind => String_Expression_Value,
                    S => Result);
         else
            Append (Result, Slice (Source_Value.S,
                                   Pos,
                                   New_Pos));
         end if;
         Pos := New_Pos + 1;
         loop
            if Pos > Length (Source_Value.S) then
               exit;
            end if;
            New_Pos := Index (Source_Value.S, (1 => ASCII.LF), Pos);
            declare
               Line : constant String := Slice
                 (Source_Value.S,
                  Pos,
                  (if New_Pos = 0 then Length (Source_Value.S) else New_Pos));
            begin
               if Blank_Value.B or else not Is_Empty_Line (Line) then
                  Append (Result, Indentation);
               end if;
               Append (Result, Line);
               if New_Pos = 0 then
                  --  This was the last line, so we're done
                  exit;
               else
                  Pos := New_Pos + 1;
               end if;
            end;
         end loop;
         return (Kind => String_Expression_Value,
                 S => Result);
      end Evaluate_Indent;

      function Evaluate_Replace return Expression_Value
      is
         Old_Value : constant Expression_Value
           := Evaluate (Source.Arguments (2).all, Resolver);
         New_Value : constant Expression_Value
           := Evaluate (Source.Arguments (3).all, Resolver);
         Count_Value : constant Expression_Value
           := Evaluate (Source.Arguments (4).all, Resolver);
         Result : Unbounded_String;
         Replaced_Count : Natural := 0;
         Pos : Positive := 1;
         New_Pos : Natural;
      begin
         if Source_Value.Kind /= String_Expression_Value then
            raise Template_Error
              with "first argument to 'replace' must be a string";
         end if;
         if Old_Value.Kind /= String_Expression_Value
           or else New_Value.Kind  /= String_Expression_Value
           or else Count_Value.Kind /= Integer_Expression_Value
         then
            raise Template_Error
              with "invalid argument to 'replace'";
         end if;
         while Count_Value.I < 0 or else Replaced_Count < Count_Value.I loop
            Replaced_Count := Replaced_Count + 1;
            New_Pos := Index (Source_Value.S, To_String (Old_Value.S), Pos);
            if New_Pos = 0 then
               exit;
            end if;
            if New_Pos > Pos then
               Append (Result, Slice (Source_Value.S, Pos, New_Pos - 1));
            end if;
            Append (Result, New_Value.S);
            Pos := New_Pos + Length (Old_Value.S);
         end loop;
         Append (Result, Slice (Source_Value.S,
                                Pos,
                                Length (Source_Value.S)));
         return (Kind => String_Expression_Value,
                 S => Result);
      end Evaluate_Replace;

      function Evaluate_Truncate return Expression_Value
      is
         Length_Value : constant Expression_Value
           := Evaluate (Source.Arguments (2).all, Resolver);
         Killwords_Value : constant Expression_Value
           := Evaluate (Source.Arguments (3).all, Resolver);
         End_Value : constant Expression_Value
           := Evaluate (Source.Arguments (4).all, Resolver);
         Leeway_Value : constant Expression_Value
           := Evaluate (Source.Arguments (5).all, Resolver);
         Len : Natural;
      begin
         if Length_Value.Kind /= Integer_Expression_Value
           or else Killwords_Value.Kind  /= Boolean_Expression_Value
           or else End_Value.Kind /= String_Expression_Value
           or else Leeway_Value.Kind /= Integer_Expression_Value
         then
            raise Template_Error
              with "invalid argument to 'truncate'";
         end if;

         declare
            Text : constant Wide_Wide_String := Wide_Wide_Strings.Decode
              (UTF_8_String (To_String (Source_Value.S)));
         begin
            if Text'Length <= Length_Value.I + Leeway_Value.I then
               return Source_Value;
            end if;
            Len := Integer'Max (Length_Value.I - Length (End_Value.S), 0);
            if not Killwords_Value.B
              and then Len + 1 <= Text'Length
              and then Ada.Wide_Wide_Characters.Handling
                .Is_Alphanumeric (Text (Text'First + Len))
            then
               while Len > 0
                 and then Ada.Wide_Wide_Characters.Handling
                   .Is_Alphanumeric (Text (Text'First + Len - 1))
               loop
                  Len := Len - 1;
               end loop;
               while Len > 0
                 and then Ada.Wide_Wide_Characters.Handling
                   .Is_Space (Text (Text'First + Len - 1))
               loop
                  Len := Len - 1;
               end loop;
            end if;

            declare
               Truncated : constant UTF_8_String := Wide_Wide_Strings.Encode
                 (Text (Text'First .. Text'First + Len - 1));
            begin
               return (Kind => String_Expression_Value,
                       S => To_Unbounded_String (String (Truncated))
                       & End_Value.S);
            end;
         end;
      end Evaluate_Truncate;

   begin
      if Source.Name = "slice" then
         return Evaluate_Slice;
      end if;
      if Source.Name = "batch" then
         return Evaluate_Batch;
      end if;
      if Source.Name = "center" then
         return Evaluate_Center;
      end if;
      if Source.Name = "capitalize" then
         declare
            Source_String : constant String
              := Evaluate (Source.Arguments (1).all,
                           Resolver);
         begin
            if Source_String = Null_Unbounded_String then
               return (Kind => String_Expression_Value,
                       S => Null_Unbounded_String);
            end if;
            return (Kind => String_Expression_Value,
                    S => Ada.Characters.Handling.To_Upper
                      (Source_String
                         (Source_String'First .. Source_String'First))
                    & To_Unbounded_String (
                      Ada.Characters.Handling.To_Lower
                        (Source_String (Source_String'First + 1
                         .. Source_String'Last))));
         end;
      end if;
      if Source.Name = "upper" then
         declare
            Source_String : constant String
              := Evaluate (Source.Arguments (1).all,
                           Resolver);
         begin
            return (Kind => String_Expression_Value,
                    S => To_Unbounded_String (Ada.Characters.Handling.To_Upper
                      (Source_String)));
         end;
      end if;
      if Source.Name = "lower" then
         declare
            Source_String : constant String
              := Evaluate (Source.Arguments (1).all,
                           Resolver);
         begin
            return (Kind => String_Expression_Value,
                    S => To_Unbounded_String (Ada.Characters.Handling.To_Lower
                      (Source_String)));
         end;
      end if;
      Source_Value := Evaluate (Source.Arguments (1).all,
                                Resolver);
      if Source.Name = "e" or else Source.Name = "escape" then
         declare
            Source_String : constant String :=
              Evaluate (Source.Arguments (1).all,
                        Resolver);
         begin
            return (Kind => String_Expression_Value,
                    S => Html_Escape (Source_String));
         end;
      end if;
      if Source.Name = "first" then
         if Source_Value.List_Value.Elements = null
           or else Is_Empty (Source_Value.List_Value.Elements.Values)
         then
            return Empty_String_Value;
         end if;
         return Source_Value.List_Value.Elements.Values.First_Element;
      end if;
      if Source.Name = "last" then
         if Source_Value.List_Value.Elements = null
           or else Is_Empty (Source_Value.List_Value.Elements.Values)
         then
            return Empty_String_Value;
         end if;
         return Source_Value.List_Value.Elements.Values.Last_Element;
      end if;
      if Source.Name = "max" then
         return Evaluate_Max (Source_Value.List_Value);
      end if;
      if Source.Name = "min" then
         return Evaluate_Min (Source_Value.List_Value);
      end if;
      if Source.Name = "count" or else Source.Name = "length" then
         return Count (Source_Value);
      end if;
      if Source.Name = "trim" then
         if Source_Value.Kind /= String_Expression_Value then
            return Source_Value;
         end if;
         declare
            Trim_Characters : Ada.Strings.Maps.Character_Set;
            Source_Value_2 : constant Expression_Value
              := Evaluate (Source.Arguments (2).all,
                           Resolver);
         begin
            if Source_Value_2.Kind /= String_Expression_Value then
               raise Template_Error with "argument to 'trim' must be a string";
            end if;
            if Source_Value_2.S = Null_Unbounded_String then
               Trim_Characters := Default_Trim_Characters;
            else
               Trim_Characters := Ada.Strings.Maps.To_Set (To_String
                                                           (Source_Value_2.S));
            end if;
            return (Kind => String_Expression_Value,
                    S => Ada.Strings.Unbounded.Trim (Source_Value.S,
                      Trim_Characters, Trim_Characters));
         end;
      end if;
      if Source.Name = "join" then
         return Evaluate_Join;
      end if;
      if Source.Name = "round" then
         return Evaluate_Round;
      end if;
      if Source.Name = "int" then
         return Evaluate_Int;
      end if;
      if Source.Name = "float" then
         return Evaluate_Float;
      end if;
      if Source.Name = "indent" then
         return Evaluate_Indent;
      end if;
      if Source.Name = "replace" then
         return Evaluate_Replace;
      end if;
      if Source.Name = "truncate" then
         return Evaluate_Truncate;
      end if;
      if Source.Name = "dictsort" then
         raise Template_Error with "unsupported usage of 'dictsort'";
      end if;

      declare
         Custom_Filter : Filter_Function;
         Args : Unbounded_String_Array (1 .. Argument_Capacity);
         I : Natural;
      begin
         Custom_Filter := Resolver.Get_Environment.Filters (Source.Name);
         Args (1) := To_Unbounded_String (To_String (Source_Value));
         I := 2;
         while Source.Arguments (I) /= null loop
            Args (I) := To_Unbounded_String
              (Evaluate (Source.Arguments (I).all, Resolver));
            I := I + 1;
         end loop;
         return (Kind => String_Expression_Value,
                 S => Custom_Filter (Args (1 .. I))
                );
      exception
         when Constraint_Error =>
            raise Template_Error with "no filter named '"
              & To_String (Source.Name) & "'";
      end;
   end Evaluate_Filter;

end Filters;
