# Lofn --- Web framework and template engine

## About the template engine

The engine works by compiling a source file into a Lisp form an
compiling it. This is done using the function `PARSE-TEMPLATE`. This
function takes a template and returns a compiled function that can be
called to invoke the template.

The fact that the template generates directly compiled Lisp code makes
the template execution very fast since there is no parsing happening
at run time.

## How to call the template engine

Normally the `PARSE-TEMPLATE` function is not called directly.
Instead, the function `SHOW-TEMPLATE` is used. This function accepts a
template file and arguments. This function caches the compiled
templates and recompiles them if necessary if the source files have
changed.

Here is a simple example of a template invocation:

```
(with-output-to-string (stream)
  (show-template stream "main.tmpl" '((:name . "Foo")
                                      (:values . (((:val0 . "A")
                                                   (:val1 . "B"))
                                                  ((:val0 . "A0")
                                                   (:val1 . "B0")))))))
```

This will apply the argument list to the template `main.tmpl` and
return the output as a string.

## Template language

Any text that is not surrounded by the begin and end code markers are
copied as-is to the output stream. The start and end markers are `<%`
and `%>` respectively.

### Loop over a list of values

Here is a simple template that renders HTML that represents the
content in the example call above:

```
Name: <% #name %>

Values:<br>
<% for values %>
  Val0 = <% #val0 %>,
  Val1 = <% #val1 %><br>
<% end %>
```

The `for` keyword loops over a list and for each iteration sets the
current arguments to the content of that list. A bare symbol causes a
lookup of a keyword in the current arguments. Finally, `#` causes the
value to be sent to the output stream.

### String output

By default `#` will perform HTML escaping on the output. I.e. `#"&"`
will output `&amp;`. To output a string without escaping, use the `r#`
prefix. I.e. `r#some-raw-html`.

### Include files

The content of another template file can be included in the input
using the `include` keyword:

```
<% include "other_file.tmpl" %>
```

### Reusable templates

A reusable template is like a function that is defined once, and can
be called multiple times in a file:

```
<% template foo %>
This string will be repeated multiple times<br>
<% end %>

<% call foo %>
<% call foo %>
```

### Conditionals

A conditional is issued using the `if` keyword:

```
<% for values %>
  Value 0 is: <% #val0 %><br>
  <% if (val1 == "foo") %>
    Value 1 is foo
  <% end %>
<% end %>
```

### Variables

Variables are declared using the `var` keyword:

```
var foo = "value from string"
var bar = lookup-by-keyword
```

The variable can be referenced by prefixing it by a comma: `,foo`.
Thus, if you want to output the value of a variable, use the
following: `#,foo`.

Variables are useful when looping through data and you need to refer
to a value on a higher level. For example:

```
<% var title = title %>
<ul>
  <% for values %>
    <li><% #,title %>: <% #value %></li>
  <% end %>
</ul>
