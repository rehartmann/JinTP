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
          <p>{{ color }}</p>
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
        
          <p>Green</p>
        
          <p>Red</p>
        
          <p>Blue</p>
        
      </body>
    </html>

## Statements

The following statements are supported: block, extends, for, if, include,
macro, import.

## Filters

The following built-in filters are supported: batch, capitalize, center, count,
dictsort, escape, first, float, indent, int, join, last, lower, max, min,
replace, round, slice, trim, truncate, upper.

Custom filters can be registered using `Register_Filter`.

## Tests

The following tests are supported: boolean, defined, divisibleby, even,
false, float, in, integer, odd, sequence, string, true, undefined.
