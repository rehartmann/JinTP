with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Finalization;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

package Jintp is

   use Ada.Strings.Unbounded;

   Default_Expression_Start : constant String := "{{";
   Default_Expression_End : constant String := "}}";
   Default_Statement_Start : constant String := "{%";
   Default_Statement_End : constant String := "%}";
   Default_Comment_Start : constant String := "{#";
   Default_Comment_End : constant String := "#}";

   type Environment is new Ada.Finalization.Limited_Controlled with private;

   type Dictionary is new Ada.Finalization.Controlled with private;

   type List is new Ada.Finalization.Controlled with private;

   Template_Error : exception;

   overriding function "=" (Left, Right : Dictionary) return Boolean;

   overriding function "=" (Left, Right : List) return Boolean;

   function Render (File_Name : String;
                    Values : Dictionary)
                    return String;

   function Render (File_Name : String;
                    Values : Dictionary)
                    return Unbounded_String;

   procedure Configure (Settings : in out Environment;
                        Expression_Start : String := Default_Expression_Start;
                        Expression_End : String := Default_Expression_End;
                        Statement_Start : String := Default_Statement_Start;
                        Statement_End : String := Default_Statement_End;
                        Comment_Start : String := Default_Comment_Start;
                        Comment_End : String := Default_Comment_End;
                        Max_Cache_Size : Natural := 200;
                        Trim_Blocks : Boolean := False;
                        Lstrip_Blocks : Boolean := False);

   function Render (File_Name : String;
                    Values : Dictionary;
                    Settings : in out Environment'Class)
                    return String;

   function Render (File_Name : String;
                    Values : Dictionary;
                    Settings : in out Environment'Class)
                    return Unbounded_String;

   function Refers (Source : List'Class;
                    Target : Dictionary)
                    return Boolean;

   function Refers (Source : Dictionary'Class;
                    Target : Dictionary)
                    return Boolean;

   function Refers (Source : Dictionary'Class;
                    Target : List)
                    return Boolean;

   function Refers (Source : List'Class;
                    Target : List)
                    return Boolean;

   procedure Insert (Container : in out Dictionary;
                     Key : String;
                     New_Item : String);

   procedure Insert (Container : in out Dictionary;
                     Key : Unbounded_String;
                     New_Item : String);

   procedure Insert (Container : in out Dictionary;
                     Key : String;
                     New_Item : Unbounded_String);

   procedure Insert (Container : in out Dictionary;
                     Key : String;
                     New_Item : Long_Float);

   procedure Insert (Container : in out Dictionary;
                     Key : Unbounded_String;
                     New_Item : Unbounded_String);

   procedure Insert (Container : in out Dictionary;
                     Key : Unbounded_String;
                     New_Item : Long_Float);

   procedure Insert (Container : in out Dictionary;
                     Key : String;
                     New_Item : Integer);

   procedure Insert (Container : in out Dictionary;
                     Key : Unbounded_String;
                     New_Item : Integer);

   procedure Insert (Container : in out Dictionary;
                     Key : String;
                     New_Item : Boolean);

   procedure Insert (Container : in out Dictionary;
                     Key : String;
                     New_Item : List'Class)
   with Pre => not Refers (New_Item, Container);

   procedure Insert (Container : in out Dictionary;
                     Key : Unbounded_String;
                     New_Item : Boolean);

   procedure Insert (Container : in out Dictionary;
                     Key : String;
                     New_Item : Dictionary)
   with Pre => not Refers (New_Item, Container);

   procedure Append (Container : in out List;
                     New_Item : List)
   with Pre => not Refers (New_Item, Container);

   --  Enable precodition check for the following procedures
   --  to prevent the creation of cycles in the tree
   pragma Assertion_Policy (Pre => Check);

   procedure Insert (Container : in out Dictionary;
                     Key : Unbounded_String;
                     New_Item : List'Class)
     with Pre => not Refers (New_Item, Container)
     or else raise Template_Error with "inserting list would create cycle";

   procedure Insert (Container : in out Dictionary;
                     Key : Unbounded_String;
                     New_Item : Dictionary)
     with Pre => not Refers (New_Item, Container)
     or else raise Template_Error with "inserting dictionary would create cycle";

   procedure Insert (Container : in out Dictionary;
                     Key : Integer;
                     New_Item : String);

   procedure Insert (Container : in out Dictionary;
                     Key : Integer;
                     New_Item : Unbounded_String);

   procedure Insert (Container : in out Dictionary;
                     Key : Integer;
                     New_Item : Long_Float);

   procedure Insert (Container : in out Dictionary;
                     Key : Integer;
                     New_Item : Integer);

   procedure Insert (Container : in out Dictionary;
                     Key : Integer;
                     New_Item : Boolean);

   procedure Insert (Container : in out Dictionary;
                     Key : Integer;
                     New_Item : List'Class)
     with Pre => not Refers (New_Item, Container)
     or else raise Template_Error with "inserting list would create cycle";

   procedure Insert (Container : in out Dictionary;
                     Key : Integer;
                     New_Item : Dictionary'Class)
     with Pre => not Refers (New_Item, Container)
     or else raise Template_Error with "inserting dictionary would create cycle";

   procedure Clear (Container : in out Dictionary);

   procedure Append (Container : in out List;
                     New_Item : String);

   procedure Append (Container : in out List;
                     New_Item : Unbounded_String);

   procedure Append (Container : in out List;
                     New_Item : Dictionary'Class)
     with Pre => not Refers (New_Item, Container)
     or else raise Template_Error with "inserting dictionary would create cycle";

   procedure Clear (Container : in out List);

   type Unbounded_String_Array is array (Positive range <>)
     of Unbounded_String;

   type Filter_Function is access function (Arguments : Unbounded_String_Array)
                                            return Unbounded_String;

   procedure Register_Filter (Settings : in out Environment;
                              Filter : Filter_Function;
                              Name : String);

private
   type Dictionary_Assocs;

   type Assocs_Access is access Dictionary_Assocs;

   type Dictionary is new Ada.Finalization.Controlled with record
      Assocs : Assocs_Access;
   end record;

   overriding procedure Adjust (D : in out Dictionary);

   overriding procedure Finalize (D : in out Dictionary);

   type List_Elements;

   type List_Elements_Access is access List_Elements;

   type List is new Ada.Finalization.Controlled with record
      Elements : List_Elements_Access;
   end record;

   overriding procedure Adjust (L : in out List);

   overriding procedure Finalize (L : in out List);

   type Template;

   type Template_Access is access Template;

   package Template_Access_Maps is new
     Ada.Containers.Hashed_Maps (Key_Type => Unbounded_String,
                                 Element_Type => Template_Access,
                                 Hash => Ada.Strings.Unbounded.Hash,
                                 Equivalent_Keys => "=");

   protected type Template_Cache is
      function Get (Path : String) return Template_Access;
      procedure Put (Path : String;
                     Template : Template_Access;
                     Max_Size : Natural;
                     Inserted : out Boolean);
      function Size return Natural;
      procedure Cleanup;
   private
      Templates_Map : Template_Access_Maps.Map
        := Template_Access_Maps.Empty_Map;
   end Template_Cache;

   package Filter_Maps is new
     Ada.Containers.Hashed_Maps (Key_Type => Unbounded_String,
                                 Element_Type => Filter_Function,
                                 Hash => Ada.Strings.Unbounded.Hash,
                                 Equivalent_Keys => "=");

   type Environment is new Ada.Finalization.Limited_Controlled with record
      Expression_Start : Unbounded_String
        := To_Unbounded_String (Default_Expression_Start);
      Expression_End : Unbounded_String
        := To_Unbounded_String (Default_Expression_End);
      Statement_Start : Unbounded_String
        := To_Unbounded_String (Default_Statement_Start);
      Statement_End : Unbounded_String
        := To_Unbounded_String (Default_Statement_End);
      Comment_Start : Unbounded_String
        := To_Unbounded_String (Default_Comment_Start);
      Comment_End : Unbounded_String
        := To_Unbounded_String (Default_Comment_End);
      Cached_Templates : Template_Cache;
      Max_Cache_Size : Natural := 200;
      Trim_Blocks : Boolean := False;
      Lstrip_Blocks : Boolean := False;
      Filters : Filter_Maps.Map := Filter_Maps.Empty_Map;
   end record;

   overriding procedure Finalize (Self : in out Environment);

end Jintp;
