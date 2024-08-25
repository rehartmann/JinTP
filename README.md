# JinTP

JinTP is a template engine for Ada which is based on the Jinja2 template
engine for Python.

## Example

    with Ada.Text_IO; use Ada.Text_IO;
    with Jintp; use Jintp;

    procedure Main is
       D : Dictionary;
    begin
      Insert (D, "name", "John");
      Put_Line (Render ("template", D));
    end Main;
