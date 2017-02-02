;;; iregister-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "iregister" "iregister.el" (22674 32857 0 0))
;;; Generated autoloads from iregister.el

(autoload 'iregister-point-or-text-to-register "iregister" "\
Store point or text to any empty register. If region is active
then store a text, otherwise a point. With a `C-u' prefix
argument, delete active region. With a `C-u C-u' prefix argument,
kill active region. With a `C-u C-u C-u' prefix argument,
`kill-ring-save' active region.

\(fn &optional DELETE-FLAG)" t nil)

(autoload 'iregister-point-or-text-to-register-kill-ring-save "iregister" "\
Store point or text to any empty register. If region is active
then store a text, otherwise a point. If region is active then
perform `kill-ring-save' on it.

\(fn)" t nil)

(autoload 'iregister-point-to-register "iregister" "\
Find empty register and store current point to it.  Works
exactly as `point-to-register' does work except it doesn't prompt
for register-name.

\(fn)" t nil)

(autoload 'iregister-jump-to-current-marker "iregister" "\
Jump to the current marker from the minibuffer.

\(fn)" t nil)

(autoload 'iregister-jump-to-next-marker "iregister" "\
If the minibuffer is the current buffer then jump to the next
marker.  Otherwise show the minibuffer with the text arround next
marker and allows to select interactively required
marker.  Markers retrieves from the registers.

\(fn)" t nil)

(autoload 'iregister-jump-to-previous-marker "iregister" "\
If the minibuffer is the current buffer then jump to the
previous marker.  Otherwise show the minibuffer with the text
arround previous marker and allows to select interactively
required marker.  Markers retrieves from the registers.

\(fn)" t nil)

(autoload 'iregister-copy-to-register "iregister" "\
Copy region into the any empty register. With a `C-u' prefix
argument delete selected text. With a `C-u C-u' prefix argument
kill selected text. With a `C-u C-u C-u' prefix argument
`kill-ring-save' selected text.

\(fn START END &optional DELETE-FLAG)" t nil)

(autoload 'iregister-copy-to-register-delete "iregister" "\
Copy region into the any empty register and delete the region.

\(fn START END)" t nil)

(autoload 'iregister-copy-to-register-kill "iregister" "\
Copy region into the any empty register and kill the region.

\(fn START END)" t nil)

(autoload 'iregister-copy-to-register-kill-ring-save "iregister" "\
Copy region into the any empty register and `kill-ring-save'
the region.

\(fn START END)" t nil)

(autoload 'iregister-append-to-latest-register "iregister" "\
Append selected text to the latest used register in the
`iregister-copy-to-register' function. With a `C-u' prefix
argument delete selected text. With a `C-u C-u' prefix argument
kill selected text.

\(fn START END &optional DELETE-FLAG)" t nil)

(autoload 'iregister-append-to-latest-register-delete "iregister" "\
Append selected text to the latest used register in the
`iregister-copy-to-register' function. Delete selected text.

\(fn START END)" t nil)

(autoload 'iregister-append-to-latest-register-kill "iregister" "\
Append selected text to the latest used register in the
`iregister-copy-to-register' function. Kill selected text.

\(fn START END)" t nil)

(autoload 'iregister-next-text "iregister" "\
If the minibuffer is the current buffer then jump to the next
text.  Otherwise show the minibuffer with the next text and allows
to select interactively required text.  Texts retrieves from the
registers.

\(fn)" t nil)

(autoload 'iregister-previous-text "iregister" "\
If the minibuffer is the current buffer then jump to the
previous text.  Otherwise show the minibuffer with the previous text
and allows to select interactively required text. Texts retrieves
from the registers.

\(fn)" t nil)

(autoload 'iregister-latest-text "iregister" "\
If the minibuffer is the current buffer then jump to the
latest text. Otherwise show the minibuffer with the latest text
and allows to select interactively required text. Texts retrieves
from the registers.

\(fn)" t nil)

(autoload 'iregister-text "iregister" "\
Show the minibuffer with the current text.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("iregister-pkg.el") (22674 32857 774128
;;;;;;  500000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; iregister-autoloads.el ends here
