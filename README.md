# JinTP

JinTP is a template engine for Ada which is based on the Jinja2 template
engine for Python.

## Example

File `main.adb`:

    with Ada.Text_IO; use Ada.Text_IO;
    with Jintp; use Jintp;

    procedure Main is
       D : Dictionary;
       Colors : List;
    begin
       Insert (D, "name", "John");
       Append (Colors, "Green");
       Append (Colors, "Red");
       Append (Colors, "Blue");
       Insert (D, "colors", Colors);
       Put_Line (Render ("template", D));
    end Main;

File `template`:

    <!DOCTYPE html>
    <html>
      <head>
        <title>Hello {{ name|e }}</title>
      </head>
      <body>
        {% for color in colors %}
          <p>{{ color }}
        {% endfor %}
      </body>
    </html>

Output:

    <!DOCTYPE html>
    <html>
      <head>
        <title>Hello John</title>
      </head>
      <body>
        
          <p>Green
        
          <p>Red
        
          <p>Blue
        
      </body>
    </html>

