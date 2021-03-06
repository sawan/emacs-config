;;; wide-n-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "wide-n" "wide-n.el" (21970 41721 0 0))
;;; Generated autoloads from wide-n.el

(autoload 'wide-n-select-region "wide-n" "\
Select a region.
The restrictions are those in the current `wide-n-restrictions-var'.
With no prefix arg, select the previous recorded region.
With a numeric prefix arg N, select the Nth previous region.

Note that if the value of `wide-n-restrictions-var' is not
buffer-local then you can use this command to cycle among regions in
multiple buffers.

\(fn ARG &optional MSGP)" t nil)

(autoload 'wide-n "wide-n" "\
Widen to a previous buffer restriction (narrowing).
The restrictions are those in the current `wide-n-restrictions-var'.

With no prefix arg, widen to the previous restriction.
With a plain prefix arg (`C-u'), widen completely.
With a zero  prefix arg (`C-0'), widen completely and reset (empty)
 the list of restrictions for this buffer.
With a numeric prefix arg N, widen abs(N) times (to the abs(N)th
 previous restriction).  Positive and negative args work the same,
 except that a negative arg also pops entries off the ring: it removes
 the ring entries from the most recent back through the (-)Nth one.

\(fn ARG &optional MSGP)" t nil)

(autoload 'wide-n-push "wide-n" "\
Add a restriction from START to END to those of VARIABLE.
Return the new value of VARIABLE.

This is a destructive operation: The list structure of the variable
value can be modified.

VARIABLE defaults to the value of `wide-n-restrictions-var'.
START and END are as for `narrow-to-region'.

With a prefix arg you are prompted for a different variable to use, in
place of the current value of `wide-n-restrictions-var'.  The
particular prefix arg determines whether the variable, if unbound, is
made buffer-local, and whether `wide-n-restrictions-var' is set to the
variable symbol:

prefix arg          buffer-local   set `wide-n-restrictions-var'
----------          ------------   -----------------------------
 Plain `C-u'         yes            yes
 > 0 (e.g. `C-1')    yes            no
 = 0 (e.g. `C-0')    no             yes
 < 0 (e.g. `C--')    no             no

Non-interactively:
* VARIABLE is the optional restrictions variable to use.
* Non-nil NOT-BUF-LOCAL-P means do not make VARIABLE buffer-local.
* Non-nil SET-VAR-P means set `wide-n-restrictions-var' to VARIABLE.
* Non-nil MSGP means echo the region size.

\(fn START END &optional VARIABLE NOT-BUF-LOCAL-P SET-VAR-P MSGP)" t nil)

(autoload 'wide-n-delete "wide-n" "\
Delete the restriction(s) numbered N from VARIABLE.
This renumbers the remaining restrictions.
Return the new value of VARIABLE.

This is a destructive operation: The list structure of the variable
value can be modified.

You are prompted for the number N.
VARIABLE defaults to the value of `wide-n-restrictions-var'.

With a prefix arg you are prompted for a different variable to use, in
place of the current value of `wide-n-restrictions-var'.  The
particular prefix arg determines whether the variable, if unbound, is
made buffer-local, and whether `wide-n-restrictions-var' is set to the
variable symbol:

prefix arg          buffer-local   set `wide-n-restrictions-var'
----------          ------------   -----------------------------
 Plain `C-u'         yes            yes
 > 0 (e.g. `C-1')    yes            no
 = 0 (e.g. `C-0')    no             yes
 < 0 (e.g. `C--')    no             no

Non-nil optional arg NOMSG means do not display a status message.

\(fn N &optional VARIABLE NOT-BUF-LOCAL-P SET-VAR-P MSGP)" t nil)

(autoload 'wide-n-repeat "wide-n" "\
Cycle to the next buffer restriction (narrowing).
This is a repeatable version of `wide-n'.

Note that if the value of `wide-n-restrictions-var' is not
buffer-local then you can use this command to cycle among regions in
multiple buffers.

\(fn ARG)" t nil)

(autoload 'wide-n-select-region-repeat "wide-n" "\
Cycle to the next region.
This is a repeatable version of `wide-n-select-region'.

\(fn ARG)" t nil)

(autoload 'wide-n-unite "wide-n" "\
Merge the restrictions of VARIABLE to form their union.
A non-destructive operation: The new value of VARIABLE is a new list.
Return the new value of VARIABLE.

VARIABLE defaults to the value of `wide-n-restrictions-var'.
With a prefix arg you are prompted for a different variable to use, in
place of the current value of `wide-n-restrictions-var'.  If the
prefix arg is non-negative (>= 0) then make the variable buffer-local.
If the prefix arg is non-positive (<= 0) then set
`wide-n-restrictions-var' to that variable symbol.  (Zero: do both.)

Non-interactively:
* VARIABLE is the optional restrictions variable to use.
* Non-nil MSGP show status message.

You need library `zones.el' for this command.

\(fn &optional VARIABLE MSGP)" t nil)

(autoload 'wide-n-add-to-union "wide-n" "\
Add a restriction from START to END to those of VARIABLE, and unite.
Uses `wide-n-push' to add the region, then applies `wide-n-unite'.
Return the new value of VARIABLE.

This is a destructive operation: The list structure of the variable
value can be modified.

VARIABLE defaults to the value of `wide-n-restrictions-var'.
START and END are as for `narrow-to-region'.

With a prefix arg you are prompted for a different variable to use, in
place of the current value of `wide-n-restrictions-var'.  If the
prefix arg is non-negative (>= 0) then make the variable buffer-local.
If the prefix arg is non-positive (<= 0) then set
`wide-n-restrictions-var' to that variable symbol.  (Zero: do both.)

Non-interactively:
* VARIABLE is the optional restrictions variable to use.
* Non-nil MSGP means echo the region size.

\(fn START END &optional VARIABLE MSGP)" t nil)

(autoload 'narrow-to-defun "wide-n" "\
Make text outside current defun invisible.
The visible defun is the one that contains point or follows point.
Optional ARG is ignored.

This is a destructive operation. The list structure of the variable
that is the value of `wide-n-restrictions-var' can be modified.

\(fn &optional ARG)" t nil)

(autoload 'narrow-to-page "wide-n" "\
Make text outside current page invisible.
A numeric arg specifies to move forward or backward by that many pages,
thus showing a page other than the one point was originally in.

This is a destructive operation. The list structure of the variable
that is the value of `wide-n-restrictions-var' can be modified.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; wide-n-autoloads.el ends here
