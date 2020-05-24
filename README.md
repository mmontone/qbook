# The qbook lisp documentation system

qbook generates HTML (or LaTeX) formatted code listings of Common Lisp source files. Comments in the source code are rendered as HTML paragraphs, text is rendered in <pre> blocks. Headings are created by preceding the text of the comment with one or more #\* chars.

This is inspired by Luke Gorrie's pbook.el.

## How to use it

qbook can be started directly from ASDF.

Be sure to have loaded the qbook system:

    `(asdf:oos 'asdf:load-op 'qbook)`

Assuming that your ASDF package is called 'my-system', use the following
commands to create the documentation in

1) HTML format:

```lisp

(qbook:publish-system-qbook :my-system
                        'qbook:html-generator
                        :output-directory "/path/to/folder/"
                        :title "Documentation for my system")
```                             

2) LaTeX format:

```lisp

(qbook:publish-system-qbook :my-system
                             'qbook:latex-generator
                             :output-file "/path/to/file"
                             :title "Documentation for my system")
```
