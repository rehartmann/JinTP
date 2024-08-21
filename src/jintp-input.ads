package Jintp.Input is

   type Character_Iterator is abstract tagged limited null record;

   function Next (Source : in out Character_Iterator)
                  return Character is abstract;

   procedure Back (Source : in out Character_Iterator) is abstract;

   procedure Match (Source : in out Character_Iterator;
                    Pattern : String;
                    Matches : out Boolean) is abstract;

end Jintp.Input;
