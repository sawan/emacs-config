;;; macros+-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "macros+" "macros+.el" (22724 39443 0 0))
;;; Generated autoloads from macros+.el

(autoload 'name-last-kbd-macro "macros+" "\
Assign a name to the last keyboard macro defined.
Argument SYMBOL is the name to define.  SYMBOL's function definition
becomes the keyboard macro string.  Such a \"function\" cannot be
called from Lisp, but it is a valid editor command.

\(fn SYMBOL)" t nil)

(autoload 'insert-kbd-macro "macros+" "\
Insert in buffer the definition of kbd macro MACRONAME, as Lisp code.
Optional second arg KEYS means also record the keys it is on
\(this is the prefix argument, when called interactively).

This Lisp code will, when executed, define the keyboard macro with the
same definition it has now.  If you say to record the keys, the Lisp
code will also rebind those keys to the macro.  Only global key
bindings are recorded since executing this Lisp code always makes
global bindings.

To save a keyboard macro, visit a file of Lisp code such as your
`~/.emacs', use this command, and then save the file.

\(fn MACRONAME &optional KEYS)" t nil)

(autoload 'apply-macro-to-region-lines "macros+" "\
For each complete line between point and mark, move to the beginning
of the line, and run the last keyboard macro.

When called from lisp, this function takes two arguments TOP and
BOTTOM, describing the current region.  TOP must be before BOTTOM.
The optional third argument MACRO specifies a keyboard macro to
execute.

This is useful for quoting or unquoting included text, adding and
removing comments, or producing tables where the entries are regular.

For example, in Usenet articles, sections of text quoted from another
author are indented, or have each line start with `>'.  To quote a
section of text, define a keyboard macro which inserts `>', put point
and mark at opposite ends of the quoted section, and use
`\\[apply-macro-to-region-lines]' to mark the entire section.

Suppose you wanted to build a keyword table in C where each entry
looked like this:

    { \"foo\", foo_data, foo_function },
    { \"bar\", bar_data, bar_function },
    { \"baz\", baz_data, baz_function },

You could enter the names in this format:

    foo
    bar
    baz

and write a macro to massage a word into a table entry:

    \\C-x (
       \\M-d { \"\\C-y\", \\C-y_data, \\C-y_function },
    \\C-x )

and then select the region of un-tablified names and use
`\\[apply-macro-to-region-lines]' to build the table from the names.

\(fn TOP BOTTOM &optional MACRO)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; macros+-autoloads.el ends here
