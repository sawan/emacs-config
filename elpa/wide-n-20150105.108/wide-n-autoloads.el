;;; wide-n-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "wide-n" "wide-n.el" (21704 52318 0 0))
;;; Generated autoloads from wide-n.el

(autoload 'wide-n "wide-n" "\
Widen to a previous buffer restriction.
With no prefix arg, widen to the previous restriction.
With a plain prefix arg (`C-u'), widen completely.
With a zero  prefix arg (`C-0'), widen completely and reset (empty)
 the list of restrictions for this buffer.
With a numeric prefix arg N, widen abs(N) times (to the abs(N)th
 previous restriction).  Positive and negative args work the same,
 except that a negative arg also pops entries off the ring: it removes
 the ring entries from the most recent back through the (-)Nth one.
 (It never pops off the `all' pseudo-entry that represents complete
 widening, however.)

\(fn ARG &optional MSGP)" t nil)

(autoload 'wide-n-repeat "wide-n" "\
Cycle to the next buffer restriction.
This is a repeatable version of `wide-n'.

\(fn ARG)" t nil)

(autoload 'narrow-to-defun "wide-n" "\
Make text outside current defun invisible.
The defun visible is the one that contains point or follows point.
Optional ARG is ignored.

\(fn &optional ARG)" t nil)

(autoload 'narrow-to-page "wide-n" "\
Make text outside current page invisible.
A numeric arg specifies to move forward or backward by that many pages,
thus showing a page other than the one point was originally in.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; wide-n-autoloads.el ends here
