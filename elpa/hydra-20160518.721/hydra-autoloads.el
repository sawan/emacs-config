;;; hydra-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

<<<<<<< HEAD:elpa/hydra-20160518.721/hydra-autoloads.el
;;;### (autoloads nil "hydra" "hydra.el" (22342 63051 0 0))
=======
;;;### (autoloads nil "hydra" "hydra.el" (22334 45425 0 0))
>>>>>>> d946b38dc8004fffa247ff073cbe98951b4bc76e:elpa/hydra-20160518.721/hydra-autoloads.el
;;; Generated autoloads from hydra.el

(autoload 'defhydra "hydra" "\
Create a Hydra - a family of functions with prefix NAME.

NAME should be a symbol, it will be the prefix of all functions
defined here.

BODY has the format:

    (BODY-MAP BODY-KEY &rest BODY-PLIST)

DOCSTRING will be displayed in the echo area to identify the
Hydra.  When DOCSTRING starts with a newline, special Ruby-style
substitution will be performed by `hydra--format'.

Functions are created on basis of HEADS, each of which has the
format:

    (KEY CMD &optional HINT &rest PLIST)

BODY-MAP is a keymap; `global-map' is used quite often.  Each
function generated from HEADS will be bound in BODY-MAP to
BODY-KEY + KEY (both are strings passed to `kbd'), and will set
the transient map so that all following heads can be called
though KEY only.  BODY-KEY can be an empty string.

CMD is a callable expression: either an interactive function
name, or an interactive lambda, or a single sexp (it will be
wrapped in an interactive lambda).

HINT is a short string that identifies its head.  It will be
printed beside KEY in the echo erea if `hydra-is-helpful' is not
nil.  If you don't even want the KEY to be printed, set HINT
explicitly to nil.

The heads inherit their PLIST from BODY-PLIST and are allowed to
override some keys.  The keys recognized are :exit and :bind.
:exit can be:

- nil (default): this head will continue the Hydra state.
- t: this head will stop the Hydra state.

:bind can be:
- nil: this head will not be bound in BODY-MAP.
- a lambda taking KEY and CMD used to bind a head.

It is possible to omit both BODY-MAP and BODY-KEY if you don't
want to bind anything.  In that case, typically you will bind the
generated NAME/body command.  This command is also the return
result of `defhydra'.

\(fn NAME BODY &optional DOCSTRING &rest HEADS)" nil t)

(put 'defhydra 'lisp-indent-function 'defun)

;;;***

;;;### (autoloads nil nil ("hydra-examples.el" "hydra-ox.el" "hydra-pkg.el"
<<<<<<< HEAD:elpa/hydra-20160518.721/hydra-autoloads.el
;;;;;;  "lv.el") (22342 63052 37712 0))
=======
;;;;;;  "lv.el") (22334 45425 352607 100000))
>>>>>>> d946b38dc8004fffa247ff073cbe98951b4bc76e:elpa/hydra-20160518.721/hydra-autoloads.el

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; hydra-autoloads.el ends here
