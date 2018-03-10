;; http://milkbox.net/note/single-file-master-emacs-configuration/
;;;; package.el
(require 'package)

(setq package-user-dir "~/.emacs.d/elpa/")
(setq package-list-archives ())
(setq package-archives '(
	     ("gnu" . "https://elpa.gnu.org/packages/")
             ("melpa" . "http://melpa.milkbox.net/packages/")
	     ("marmalade" . "http://marmalade-repo.org/packages/")
	     ("elpy" . "http://jorgenschaefer.github.io/packages/")
))


(package-initialize)

(defun mp-install-rad-packages ()
  "Install only the sweetest of packages."
  (interactive)
  (package-refresh-contents)
  (mapc #'(lambda (package)
	    (unless (package-installed-p package)
	      (package-install package)))
        '(
	  browse-kill-ring
	  ido-ubiquitous
	  magit
	  paredit
	  smex
	  undo-tree
	  bm
	  pos-tip
	  ecb
	  yasnippet
	  rainbow-delimiters
	  rainbow-mode
	  fastnav
	  cedet
	  hungry-delete
	  full-ack
	  visual-regexp
	  visual-regexp-steroids
	  csv-mode
	  key-chord
	  yaml-mode
	  wide-n
	  color-moccur
	  table
	  easy-kill
	  iedit
	  multiple-cursors
	  macros+
	  macrostep
	  jedi
	  elpy
	  expand-region
	  hydra
	  smyx-theme
	  autopair
	  google-this
	  wrap-region
	  git-timemachine
	  ace-jump-mode
	  ace-jump-buffer
	  ace-jump-window
	  move-text
	  guide-keys
	  easy-kill
	  easy-kill-extras
	  back-button
	  visible-mark
	  markdown-mode
	  markdown-mode+
	  paradox
	  visual-regexp-steroids
	  aggressive-indent
	  beacon
	  react-snippets
	  jsx-mode
	  tj-mode
	  volatile-highlights
	  boxquote
	  ov
	  swoop
	  )))

(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))


;;;; init.el

(add-to-list 'load-path "~/.emacs.d/vendors/")
(add-to-list 'load-path "~/.emacs.d/vendors/DrewsLibraries/")
(add-to-list 'load-path "~/.emacs.d/vendors/exec-abbrev-cmd.el")
(add-to-list 'load-path "~/.emacs.d/vendors/revbufs.el")
(add-to-list 'load-path "~/.emacs.d/vendors/extraedit.el")
(add-to-list 'load-path "~/.emacs.d/vendors/breadcrumb.el")
(add-to-list 'load-path "~/.emacs.d/vendors/kill-lines.el")
(add-to-list 'load-path "~/.emacs.d/vendors/emacros.el")
(add-to-list 'load-path "~/.emacs.d/vendors/emacs-for-python-master/")
(add-to-list 'load-path "~/.emacs.d/vendors/no-easy-keys.el")
(add-to-list 'load-path "~/.emacs.d/vendors/thing-cmds.el")
(add-to-list 'load-path "~/.emacs.d/vendors/moccur-edit.el")
(add-to-list 'load-path "~/.emacs.d/vendors/electric-align.el")
(add-to-list 'load-path "~/.emacs.d/vendors/deep-blue-day-theme")
(add-to-list 'load-path "~/.emacs.d/vendors/jadedragon-theme.el")
(add-to-list 'load-path "~/.emacs.d/vendors/mechanical-turq-theme.el")
(add-to-list 'load-path "~/.emacs.d/vendors/soothe-theme.el")
(add-to-list 'load-path "~/.emacs.d/vendors/emacs-ixio.el")

(require 'pos-tip)
(require 'magit)
(require 'wide-n)
(require 'kill-lines)
(require 'multiple-cursors)
(require 'wide-n)
(require 'extraedit)
(require 'highlight-tail)
(require 'smyx-theme)
(require 'moccur-edit)
(require 'electric-align)
(require 'ixio)

(require 'back-button)
(back-button-mode 1)

(require 'no-easy-keys)
(no-easy-keys)

(require 'google-this)
(google-this-mode 1)

(require 'autopair)
(autopair-global-mode)

(require 'thing-cmds)

(idle-highlight-mode t)

(defun bigger-text ()
  (interactive)
  (text-scale-increase 2.5)
  )

(defun slightly-bigger-text ()
  (interactive)
  (text-scale-adjust 0)
  (text-scale-adjust 1.35)
  )

(defun smaller-text ()
  (interactive)
  (text-scale-decrease 2.5)
)

(defun minibuffer-text-size ()
  (setq-local  face-remapping-alist
	       '((default :height 1.5))))

(defun echo-area-text-size()
;; https://www.emacswiki.org/emacs/EchoArea
  ;; Most strange.....
  (with-current-buffer (get-buffer " *Echo Area 0*")
    (setq-local face-remapping-alist
		'((default (:height 1.5 variable-pitch)))))
)

(add-hook 'find-file-hook 'bigger-text)
(add-hook 'minibuffer-setup-hook 'minibuffer-text-size)
(echo-area-text-size)

(wrap-region-mode t)
(beacon-mode)

(smooth-scrolling-mode t)
(syntax-subword-mode t)

(hl-line-mode t)

(global-set-key [remap kill-ring-save] 'easy-kill)

(defun really-kill-emacs ()
  "Like `kill-emacs', but ignores `kill-emacs-hook'."
  (interactive)
  (let (kill-emacs-hook)
    (kill-emacs)))

;; start native Emacs server ready for client connections.
(setq server-socket-dir "~/.emacs.d/server/")
(add-hook 'after-init-hook 'server-start)

;; save history
(savehist-mode 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(setq savehist-file "~/.emacs.d/savehist")

;; use y/n for all yes-no answers
(defalias 'yes-or-no-p 'y-or-n-p)
;; less beeping
(setq-default visible-bell t)

;; control how Emacs backup files are handled
(setq
 backup-directory-alist '(("." . "~/.emacs.d/saves"))
 delete-old-versions t
 kept-new-versions 20
 kept-old-versions 10
 version-control t
 backup-by-copying t)

;; delete trailing whitespace before file is saved
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; columns
(column-number-mode 1)
(display-time)

;; use ibuffers for buffer listing
(defalias 'list-buffers 'ibuffer)
(setq ibuffer-default-sorting-mode 'major-mode)

;; Split windows horizontally by default
(setq split-width-threshold nil)

;; control how to move between windows
(windmove-default-keybindings 'meta)

(global-visual-line-mode t)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;; wrap lines at 80 columns
(setq-default fill-column 80)
(add-hook 'find-file-hook 'turn-on-auto-fill)


;; (with-current-buffer (get-buffer  " *Minibuf-1*")
;;   (setq-local  face-remapping-alist
;; 	       '((default :height 2.0))))


;; required on OS X -- pyflakes
(add-to-list 'exec-path "/opt/local/bin/")

;;;; desktop
;; Auto save desktop as well during buffer auto-save
(require 'desktop)
(setq desktop-path '("~/.emacs.d/"))
(setq desktop-dirname "~/.emacs.d/")
(setq desktop-base-file-name "emacs-desktop-24")
(desktop-save-mode 1)
(add-hook 'auto-save-hook (lambda () (desktop-save-in-desktop-dir)))

;; enable paren highliting for all files
(add-hook 'find-file-hook (
			    lambda()
				  (show-paren-mode t)
				  (volatile-highlights-mode t)
				  (electric-align-mode t)
				  ))

;; display path to file in frame title
;(setq-default mode-line-format
(setq-default frame-title-format
              (list '((buffer-file-name " %f"
		(dired-directory
		dired-directory
		(revert-buffer-function " %b"
					("%b - Dir:  " default-directory)))))))


(require 'swoop)
(setq swoop-window-split-current-window: t)
(setq swoop-window-split-direction: 'split-window-vertically)
(setq swoop-use-target-magnifier: t)
(setq swoop-use-target-magnifier-around: 10)
(setq swoop-use-target-magnifier-size: 1.2)

;; Company mode
(eval-after-load 'company
  '(progn
     (define-key company-active-map
       (kbd "TAB") 'company-complete-common-or-cycle)
     (define-key company-active-map (kbd "<tab>")
       'company-complete-common-or-cycle)))

(eval-after-load 'company
  '(progn
     (define-key company-active-map
       (kbd "S-TAB") 'company-select-previous)
     (define-key company-active-map
       (kbd "<backtab>") 'company-select-previous)))

(global-company-mode t)
(setq company-minimum-prefix-length 1)
(add-hook 'after-init-hook 'global-company-mode)

;;;; utility functions

;; http://emacs.stackexchange.com/a/13122
(require 'ov)
(defun highlight-duplicate-lines-in-region-or-buffer ()
  (interactive)
  (ov-clear)
  (let* (
	($beg (if mark-active (region-beginning) (point-min)))
	($end (if mark-active (region-end) (point-max)))
	($st (buffer-substring-no-properties $beg $end))
	($lines)
	($dup))
  (deactivate-mark t)
  (save-excursion
    (goto-char $beg)
    (while (< (point) $end)
      (let* (($b (point))
	     ($e (point-at-eol))
	     ($c (buffer-substring-no-properties $b $e))
	     ($a (assoc $c $lines)))
	(when (not (eq $b $e))
	  (if $a
	      (progn
		(setq $dup (cons $b $dup))
		(setq $dup (cons (cdr $a) $dup)))
	    (setq $lines
		  (cons (cons $c $b) $lines)))))
      (forward-line 1))
    (mapc (lambda ($p)
	    (ov-set (ov-line $p) 'face '(:foreground "red")))
	  (sort (delete-dups $dup) '<)))))

(defhydra hydra-dup-lines ()
  "Duplicate lines"
  ("h" highlight-duplicate-lines-in-region-or-buffer :color red)
  ("n" ov-goto-next :color red)
  ("p" ov-goto-previous :color red)
  ("c" ov-clear :color blue)
  ("q" nil :color blue)
  )

;; http://www.emacswiki.org/emacs-en/PosTip
(defun describe-function (function)
   "Display the full documentation of FUNCTION (a symbol) in tooltip."
   (interactive (list (function-called-at-point)))
   (if (null function)
       (pos-tip-show
        "** You didn't specify a function! **" '("red"))
     (pos-tip-show
      (with-temp-buffer
        (let ((standard-output (current-buffer)))
          (prin1 function)
          (princ " is ")
          (describe-function-1 function)
          (buffer-string)))
      nil nil nil 0)))


; http://blogs.fluidinfo.com/terry/2011/11/10/emacs-buffer-mode-histogram/
(defun buffer-mode-histogram ()
  "Display a histogram of emacs buffer modes."
  (interactive)
  (let* ((totals `())
         (buffers (buffer-list()))
         (total-buffers (length buffers))
         (ht (make-hash-table :test `equal)))
    (save-excursion
      (dolist (buffer buffers)
        (set-buffer buffer)
        (let
            ((mode-name (symbol-name major-mode)))
          (puthash mode-name (1+ (gethash mode-name ht 0)) ht))))
    (maphash (lambda (key value)
               (setq totals (cons (list key value) totals)))
             ht)
    (setq totals (sort totals (lambda (x y) (> (cadr x) (cadr y)))))
    (with-output-to-temp-buffer "Buffer mode histogram"
      (princ (format "%d buffers open, in %d distinct modes\n\n"
                      total-buffers (length totals)))
      (dolist (item totals)
        (let
            ((key (car item))
             (count (cadr item)))
          (if (equal (substring key -5) "-mode")
              (setq key (substring key 0 -5)))
          (princ (format "%2d %20s %s\n" count key
                         (make-string count ?+))))))))

;; word count function -- similar to wc on the command line
(defun wc (&optional start end)
   "Prints number of lines, words and characters in region or whole buffer."
   (interactive)
   (let ((n 0)
         (start (if mark-active (region-beginning) (point-min)))
         (end (if mark-active (region-end) (point-max))))
     (save-excursion
       (goto-char start)
       (while (< (point) end) (if (forward-word 1) (setq n (1+ n)))))
     (message "%4d lines %4d words %5d chars" (count-lines start end) n (- end start))))


(defun count-string-matches (strn)
  "Return number of matches STRING following the point.
Continues until end of buffer.  Also display the count as a message."
  (interactive (list (read-string "Enter string: ")))
  (save-excursion
    (let ((count -1))
      (while
          (progn
            (setq count (1+ count))
            (search-forward strn nil t)))
      (message "%d matches" count)
      count)))

;; http://www.emacswiki.org/emacs/BasicNarrowing
(defun replace-regexp-in-region (start end)
  (interactive "*r")
  (save-excursion
    (save-restriction
      (let ((regexp (read-string "Regexp: "))
	    (to-string (read-string "Replacement: ")))
	(narrow-to-region start end)
	(goto-char (point-min))
	(while (re-search-forward regexp nil t)
	  (replace-match to-string nil nil))))))

;; edit files as root
(defun sudo-find-file (file-name)
  (interactive "Find file (sudo): ")
  (find-file (concat "/sudo::" file-name)))

(defun delete-this-file ()
  (interactive)
  (or (buffer-file-name) (error "no file is currently being edited"))
  (when (yes-or-no-p "Really delete this file?")
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun indent-whole-buffer ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun djcb-duplicate-line (&optional commentfirst)
  "comment line at point; if COMMENTFIRST is non-nil, comment the original"
  (interactive)
  (beginning-of-line)
  (push-mark)
  (end-of-line)
  (let ((str (buffer-substring (region-beginning) (region-end))))
    (when commentfirst
      (comment-region (region-beginning) (region-end)))
    (insert-string
     (concat (if (= 0 (forward-line 1)) "" "\n") str "\n"))
    (forward-line -1)))

;; http://tsdh.wordpress.com/2007/06/22/zapping-to-strings-and-regexps/
(defun th-zap-to-string (arg str)
  "Same as `zap-to-char' except that it zaps to the given string
instead of a char."
  (interactive "p\nsZap to string: ")
  (kill-region (point) (progn
                         (search-forward str nil nil arg)
                         (point))))

(defun th-zap-to-regexp (arg regexp)
  "Same as `zap-to-char' except that it zaps to the given regexp
instead of a char."
  (interactive "p\nsZap to regexp: ")
  (kill-region (point) (progn
                         (re-search-forward regexp nil nil arg)
                         (point))))

;; I-search with initial contents -- current token at point
;; http://platypope.org/blog/2007/8/5/a-compendium-of-awesomeness
(defvar isearch-initial-string nil)

(defun isearch-set-initial-string ()
  (remove-hook 'isearch-mode-hook 'isearch-set-initial-string)
  (setq isearch-string isearch-initial-string)
  (isearch-search-and-update))

(defun isearch-forward-at-point (&optional regexp-p no-recursive-edit)
  "Interactive search forward for the symbol at point."
  (interactive "P\np")
  (if regexp-p (isearch-forward regexp-p no-recursive-edit)
    (let* ((end (progn (skip-syntax-forward "w_") (point)))
           (begin (progn (skip-syntax-backward "w_") (point))))
      (if (eq begin end)
          (isearch-forward regexp-p no-recursive-edit)
        (setq isearch-initial-string (buffer-substring begin end))
        (add-hook 'isearch-mode-hook 'isearch-set-initial-string)
        (isearch-forward regexp-p no-recursive-edit)))))

(global-set-key (kbd "M-s") 'isearch-forward-at-point)
(define-key isearch-mode-map (kbd "<return>") 'isearch-exit)

;; jump to matching parenthesis -- currently seems to support () and []
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis. Else go to the
   opening parenthesis one level up."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1))
        (t
         (backward-char 1)
         (cond ((looking-at "\\s\)")
                (forward-char 1) (backward-list 1))
               (t
                (while (not (looking-at "\\s("))
                  (backward-char 1)
                  (cond ((looking-at "\\s\)")
                         (message "->> )")
                         (forward-char 1)
                         (backward-list 1)
                         (backward-char 1)))
                  ))))))

(global-set-key (kbd "<C-f10>") 'goto-match-paren)

(require 'hungry-delete)
(global-hungry-delete-mode)

;; http://whattheemacsd.com/key-bindings.el-02.html
;; Move more quickly
(global-set-key (kbd "C-S-n")
                (lambda ()
                  (interactive)
                  (ignore-errors (next-line 10))))

(global-set-key (kbd "C-S-p")
                (lambda ()
                  (interactive)
                  (ignore-errors (previous-line 10))))

(global-set-key (kbd "C-S-f")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-char 10))))

(global-set-key (kbd "C-S-b")
                (lambda ()
                  (interactive)
                  (ignore-errors (backward-char 10))))


;; http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

;; http://oremacs.com/2014/12/23/upcase-word-you-silly/
(defadvice upcase-word (before upcase-word-advice activate)
  (unless (looking-back "\\b")
    (backward-word)))

(defadvice downcase-word (before downcase-word-advice activate)
  (unless (looking-back "\\b")
    (backward-word)))

(defadvice capitalize-word (before capitalize-word-advice activate)
  (unless (looking-back "\\b")
    (backward-word)))

;; http://oremacs.com/2014/12/25/ode-to-toggle/
(defun char-upcasep (letter)
  (eq letter (upcase letter)))

(defun upcase-word-toggle ()
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'symbol))
        beg end
        (regionp
         (if (eq this-command last-command)
             (get this-command 'regionp)
           (put this-command 'regionp nil))))
    (cond
      ((or (region-active-p) regionp)
       (setq beg (region-beginning)
             end (region-end))
       (put this-command 'regionp t))
      (bounds
       (setq beg (car bounds)
             end (cdr bounds)))
      (t
       (setq beg (point)
             end (1+ beg))))
    (save-excursion
      (goto-char (1- beg))
      (and (re-search-forward "[A-Za-z]" end t)
           (funcall (if (char-upcasep (char-before))
                        'downcase-region
                      'upcase-region)
                    beg end)))))

(global-set-key (kbd "C-z") 'upcase-word-toggle)

;;;; emacs lisp

;; https://www.masteringemacs.org/article/searching-buffers-occur-mode
(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
   (dolist (buf (buffer-list))
     (with-current-buffer buf
       (if (eq mode major-mode)
           (add-to-list 'buffer-mode-matches buf))))
   buffer-mode-matches))

(defun multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))

;; occur
;; http://oremacs.com/2015/01/26/occur-dwim/
(defun multi-occur-all-buffers (regexp &optional allbufs)
  "Show all lines matching REGEXP in all buffers."
  (interactive (occur-read-primary-args))
  (multi-occur-in-matching-buffers ".*" regexp))

(defun sane-occurs (occur-fn)
  "Call various `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history)
  (call-interactively occur-fn))

(defun occur-dwim ()
  (interactive)
  (sane-occurs 'occur))

(defun multi-occur-dwim ()
  (interactive)
  (sane-occurs 'multi-occur))

(defun multi-occur-in-mode-dwim ()
  (interactive)
  (sane-occurs 'multi-occur-in-this-mode))

(defun multi-occur-all-dwim ()
  (interactive)
  (sane-occurs 'multi-occur-all-buffers))

(add-hook 'occur-hook (lambda () (other-window 1)))

;; Keeps focus on *Occur* window, even when when target is visited via RETURN key.
;; See hydra-occur-dwim for more options.
(defadvice occur-mode-goto-occurrence (after occur-mode-goto-occurrence-advice activate)
  (other-window 1)
  (hydra-occur-dwim/body))

(defadvice occur-edit-mode-finish (after occur-cease-edit-advice activate)
  (save-some-buffers))

(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))

(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)

;; start native Emacs server ready for client connections                  .
(add-hook 'after-init-hook 'server-start)

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))


;; http://emacsredux.com/blog/2013/06/15/open-line-above/
(defun smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun kill-line-remove-blanks (&optional arg)
"Delete current line and remove blanks after it"
    (interactive "p")
    (kill-whole-line arg)
    (back-to-indentation))

(global-set-key [(control return)] 'smart-open-line)
(global-set-key [(control shift return)] 'smart-open-line-above)

(require 'fuzzy)
(turn-on-fuzzy-isearch)

(defun debugger-pp-frame ()
  (interactive)
  (let ((inhibit-read-only t)
        (frame (backtrace-frame (debugger-frame-number))))
    (set-buffer (pop-to-buffer "*BT: Frame*"))
    (cl-destructuring-bind (special fn &rest args) frame
      (erase-buffer)
      (progn
        (insert "(" (pp-to-string fn))
        (dolist (arg args)
          (insert "\n" (pp-to-string arg)))
        (insert ")"))
      (goto-char (point-min))
      (indent-pp-sexp))))

;;(define-key debugger-mode-map "r" 'debugger-pp-frame)


;;;; MISC libraries

(require 'highlight-indentation)

;; restore point at location upon file re-visit
(require 'saveplace)
(setq-default save-place t)

;; DrewsLibraries from EmacsWiki
; crosshairs
(require 'crosshairs)
(global-set-key (kbd "<M-f12>") 'flash-crosshairs)

(require 'exec-abbrev-cmd)
(exec-abbrev-cmd-mode 1)
(global-set-key (kbd "C-x x") 'exec-abbrev-cmd)

;; http://www.emacswiki.org/emacs/ThingEdit
; copy and paste various types of data
(require 'thing-edit)

;; revert all open buffers, useful when VC changes happen in the background
(require 'revbufs)


;; http://endlessparentheses.com/implementing-comment-line.html and
(defun endless/comment-line-or-region (n)
  "Comment or uncomment current line and leave point after it.
  With positive prefix, apply to N lines including current one.
  With negative prefix, apply to -N lines above.
  If region is active, apply to active region instead."
  (interactive "p")
  (if (use-region-p)
      (comment-or-uncomment-region
       (region-beginning) (region-end))
    (let ((range
           (list (line-beginning-position)
                 (goto-char (line-end-position n)))))
      (comment-or-uncomment-region
       (apply #'min range)
       (apply #'max range)))
    (forward-line 1)
    (back-to-indentation)))

(global-set-key (kbd "M-;") #'endless/comment-line-or-region)

(defun yank-n-times (n)
  "yank n number of times."
  (interactive "nPaste how many times? ")
  (setq last-kill (current-kill 0 t))
  (dotimes 'n (insert last-kill)))


;; http://emacsredux.com/blog/2013/05/30/joining-lines/
(defun join-region (beg end)
"Apply join-line over region."
(interactive "r")
(if mark-active
(let ((beg (region-beginning))
(end (copy-marker (region-end))))
(goto-char beg)
(while (< (point) end)
  (join-line 1)))))

(defun top-join-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))

(defun xah-shrink-whitespaces ()
  "Remove whitespaces around cursor to just one or none.
Remove whitespaces around cursor to just one space, or remove neighboring blank lines to just one or none.
URL `http://ergoemacs.org/emacs/emacs_shrink_whitespace.html'
Version 2015-03-03"
  (interactive)
  (let ((pos (point))
        line-has-char-p ; current line contains non-white space chars
        has-space-tab-neighbor-p
        whitespace-begin whitespace-end
        space-or-tab-begin space-or-tab-end
        )
    (save-excursion
      (setq has-space-tab-neighbor-p (if (or (looking-at " \\|\t") (looking-back " \\|\t")) t nil))
      (beginning-of-line)
      (setq line-has-char-p (search-forward-regexp "[[:graph:]]" (line-end-position) t))

      (goto-char pos)
      (skip-chars-backward "\t ")
      (setq space-or-tab-begin (point))

      (skip-chars-backward "\t \n")
      (setq whitespace-begin (point))

      (goto-char pos)
      (skip-chars-forward "\t ")
      (setq space-or-tab-end (point))
      (skip-chars-forward "\t \n")
      (setq whitespace-end (point)))

    (if  line-has-char-p
        (if has-space-tab-neighbor-p
            (let (deleted-text)
              ;; remove all whitespaces in the range
              (setq deleted-text
                    (delete-and-extract-region space-or-tab-begin space-or-tab-end))
              ;; insert a whitespace only if we have removed something different than a simple whitespace
              (when (not (string= deleted-text " "))
                (insert " ")))

          (progn
            (when (equal (char-before) 10) (delete-char -1))
            (when (equal (char-after) 10) (delete-char 1))))
      (progn (delete-blank-lines)))))

(defun xah-select-current-line ()
  "Select current line.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2015-02-07
"
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))

;;;; emacros
;; Emacros http://thbecker.net/free_software_utilities/emacs_lisp/emacros/emacros.html
(require 'emacros)
(setq emacros-global-dir "~/.emacs.d")
;; Load predefined macros
(add-hook 'after-init-hook 'emacros-load-macros)

(load-file "~/.emacs.d/vendors/term-fix.el")


;;;; Tramp
(require 'tramp)
(setq  tramp-completion-reread-directory-timeout 0)
(add-hook 'tramp-mode-hook
	  #'(setq ag-executable "~/bin/ag"))

;; (setq tramp-verbose 10)
;; (setq tramp-debug-buffer t)
;; (require 'trace)
;;      (dolist (elt (all-completions "tramp-" obarray 'functionp))
;;        (trace-function-background (intern elt)))
;;      (untrace-function 'tramp-read-passwd)
;;      (untrace-function 'tramp-gw-basic-authentication)

;; clean up after Tramp
(add-hook 'kill-emacs-hook '(lambda nil
                              (tramp-cleanup-all-connections)
                              (tramp-cleanup-all-buffers) ))

;;;; Setup some MS Windows specific stuff
(when (window-system) 'w32
      (setq tramp-default-method "plink")
      (add-to-list 'tramp-remote-path "~/bin")

      (setq w32-pass-lwindow-to-system nil)
      (setq w32-lwindow-modifier 'super) ; Left Windows key

      (setq w32-pass-rwindow-to-system nil)
      (setq w32-rwindow-modifier 'super) ; Right Windows key

      ;; ag.el
      (setq ag-executable "c:/msys64/mingw64/bin/ag.exe")

      (setq explicit-shell-file-name "c:/msys64/cygwin64/bin/bash.exe")
      (setq shell-file-name "bash.exe")
      (setq explicit-bash.exe-args '("--noediting" "--login" "-i"))
      (setenv "SHELL" shell-file-name)
      (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)

      ;; Make this behave the same was as on Mac OS X
      (global-set-key (kbd "s-s") 'save-buffer))


;;;; key-chord
(require 'key-chord)
(key-chord-mode 1)
(key-chord-define emacs-lisp-mode-map "eb" 'eval-buffer)
(key-chord-define emacs-lisp-mode-map "ed" 'eval-defun)
(key-chord-define emacs-lisp-mode-map "er" 'eval-region)

;;; guide-keys
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" ))
(setq guide-key/highlight-command-regexp '(
                         ("register" . font-lock-type-face) ))
(guide-key-mode 1)

;;;; broswe-kill-ring config
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
(global-set-key (kbd "C-c k") 'browse-kill-ring)

;; This only seems to work in GUIs
(when (display-graphic-p)
  ;;;; bm.el config
  (require 'bm)

  ;; reload bookmarks
  (setq bm-restore-repository-on-load t)

  ;; ask for annotation upon defining a bookmark
  (setq-default bm-annotate-on-create t)

  ;; bookmark indicator
  (setq bm-highlight-style 'bm-highlight-only-fringe)

  ;; make bookmarks persistent as default
  (setq-default bm-buffer-persistence t)

  ;; Loading the repository from file when on start up.
  (add-hook' after-init-hook 'bm-repository-load)

  ;; Restoring bookmarks when on file find.
  (add-hook 'find-file-hooks 'bm-buffer-restore)

  ;; Saving bookmark data on killing a buffer
  (add-hook 'kill-buffer-hook 'bm-buffer-save)

  ;; Restore on revert
  (add-hook 'after-revert-hook #'bm-buffer-restore)

  ;; Saving the repository to file when on exit.
  ;; kill-buffer-hook is not called when emacs is killed, so we
  ;; must save all bookmarks first.
  (add-hook 'kill-emacs-hook '(lambda nil
				(bm-buffer-save-all)
				(bm-repository-save)))

  (setq bm-marker 'bm-marker-right)

  (setq bm-highlight-style 'bm-highlight-line-and-fringe)

  (defhydra hydra-bookmarks ()
    "Bookmarks"
    ("t" bm-toggle "toggle" :color blue)
    ("n" bm-next   "next"   :color red)
    ("p" bm-previous "previous" :color red)
    ("s" bm-show "show" :color blue)
    ("S" bm-show-all "SHOW" :color blue)
    ("c" bm-remove-all-current-buffer "clear" :color blue)
    ("l" bm-bookmark-line "line" :color blue)
    ("r" bm-bookmark-regexp "regex" :color blue)
    ("w" bm-save "save" :color blue)
    ("q" nil :color red)
    )

  (defun bm()
    (interactive)
    (hydra-bookmarks/body))
)

;; http://acidwords.com/posts/2017-10-19-closing-all-parentheses-at-once.html
(defun close-all-parentheses ()
  (interactive "*")
  (let ((closing nil))
    (save-excursion
      (while (condition-case nil
         (progn
           (backward-up-list)
           (let ((syntax (syntax-after (point))))
             (case (car syntax)
               ((4) (setq closing (cons (cdr syntax) closing)))
               ((7 8) (setq closing (cons (char-after (point)) closing)))))
           t)
           ((scan-error) nil))))
    (apply #'insert (nreverse closing))))

;;;; ido mode
(require 'ido)
(ido-mode t)
(setq ido-everywhere t)

(require 'flx-ido)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; Set GC parameter as reccommended: https://github.com/lewang/flx
(setq gc-cons-threshold 20000000)

(add-hook 'ido-setup-hook
 (lambda ()
   ;; Go straight home
   (define-key ido-file-completion-map
     (kbd "~")
     (lambda ()
       (interactive)
       (if (looking-back "/")
           (insert "~/")
         (call-interactively 'self-insert-command))))))

;; (defun iswitchb-local-keys ()
;;   (mapc (lambda (K)
;;           (let* ((key (car K)) (fun (cdr K)))
;;             (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
;;         '(("<right>" . iswitchb-next-match)
;;           ("<left>"  . iswitchb-prev-match)
;;           ("<up>"    . ignore             )
;;           ("<down>"  . ignore             ))))
;; (add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)

;; http://whattheemacsd.com/setup-ido.el-01.html
;; Use ido everywhere
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; Fix ido-ubiquitous for newer packages
(defmacro ido-ubiquitous-use-new-completing-read (cmd package)
  `(eval-after-load ,package
     '(defadvice ,cmd (around ido-ubiquitous-new activate)
        (let ((ido-ubiquitous-enable-compatibility nil))
          ad-do-it))))

(ido-ubiquitous-use-new-completing-read webjump 'webjump)
(ido-ubiquitous-use-new-completing-read yas/expand 'yasnippet)
(ido-ubiquitous-use-new-completing-read yas/visit-snippet-file 'yasnippet)

;;; integrate ido with artist-mode
(defun artist-ido-select-operation (type)
  "Use ido to select a drawing operation in artist-mode"
  (interactive (list (ido-completing-read "Drawing operation: "
(list "Pen" "Pen Line" "line" "straight line" "rectangle"
 "square" "poly-line" "straight poly-line" "ellipse"
 "circle" "text see-thru" "text-overwrite" "spray-can"
 "erase char" "erase rectangle" "vaporize line" "vaporize lines"
 "cut rectangle" "cut square" "copy rectangle" "copy square"
 "paste" "flood-fill"))))
 (artist-select-operation type))

(defun artist-ido-select-settings (type)
  "Use ido to select a setting to change in artist-mode"
  (interactive (list (ido-completing-read "Setting: "
   (list "Set Fill" "Set Line" "Set Erase" "Spray-size" "Spray-chars"
         "Rubber-banding" "Trimming" "Borders"))))
  (if (equal type "Spray-size")
      (artist-select-operation "spray set size")
    (call-interactively (artist-fc-get-fn-from-symbol
                         (cdr (assoc type '(("Set Fill" . set-fill)
                                            ("Set Line" . set-line)
                                            ("Set Erase" . set-erase)
                                            ("Rubber-banding" . rubber-band)
                                            ("Trimming" . trimming)
                                            ("Borders" . borders)
                                            ("Spray-chars" . spray-chars))))))))
(add-hook 'artist-mode-init-hook
          (lambda ()
            (define-key artist-mode-map
	      (kbd "C-c C-a C-o") 'artist-ido-select-operation)
            (define-key artist-mode-map
	      (kbd "C-c C-a C-c") 'artist-ido-select-settings)))


;;;; smex
;; Http://github.com/nonsequitur/smex/
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(setq smex-save-file "~/.emacs.d/smex-items")
;; This is your old M-x.
;(global-set-key (kbd "M-x") 'execute-extended-command)


;;;; fastnav
(require 'fastnav)
(global-set-key "\M-r" 'replace-char-forward)
(global-set-key "\M-R" 'replace-char-backward)
(global-set-key "\M-i" 'insert-at-char-forward)
(global-set-key "\M-I" 'insert-at-char-backward)
(global-set-key "\M-j" 'execute-at-char-forward)
(global-set-key "\M-J" 'execute-at-char-backward)
(global-set-key "\M-k" 'delete-char-forward)
(global-set-key "\M-K" 'delete-char-backward)
(global-set-key "\M-p" 'sprint-forward)
(global-set-key "\M-P" 'sprint-backward)

;;;; undo-tree
;; http://www.dr-qubit.org/emacs.php#undo-tree
;; hot damn.....
(require 'undo-tree)
(global-undo-tree-mode t)
(setq undo-tree-visualizer-relative-timestamps t)
(setq undo-tree-visualizer-timestamps t)

;; http://whattheemacsd.com/my-misc.el-02.html
;; Keep region when undoing in region
(defadvice undo-tree-undo (around keep-region activate)
  (if (use-region-p)
      (let ((m (set-marker (make-marker) (mark)))
            (p (set-marker (make-marker) (point))))
        ad-do-it
        (goto-char p)
        (set-mark m)
        (set-marker p nil)
        (set-marker m nil))
    ad-do-it))


;; http://endlessparentheses.com/faster-pop-to-mark-command.html
;; When popping the mark, continue popping until the cursor
;; actually moves
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

(setq set-mark-command-repeat-pop t)

;;;; iedit
(require 'iedit)
;; http://www.masteringemacs.org/articles/2012/10/02/iedit-interactive-multi-occurrence-editing-in-your-buffer/
(defun iedit-defun (arg)
  "Starts iedit but uses \\[narrow-to-defun] to limit its scope."
  (interactive "P")
  (if arg
      (iedit-mode)
    (save-excursion
      (save-restriction
        (widen)
        ;; this function determines the scope of `iedit-start'.
        (narrow-to-defun)
        (if iedit-mode
            (iedit-done)
          ;; `current-word' can of course be replaced by other
          ;; functions.
          (iedit-start (current-word)))))))

(require 'csv-mode)
(autoload 'csv-mode "csv-mode"
   "Major mode for editing comma-separated value files." t)
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.sls$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.prod$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.model$" . yaml-mode))
(add-hook 'yaml-mode-hook '(lambda ()
                             (define-key yaml-mode-map
                               (kbd "RET") 'newline-and-indent)))

;;;; python mode
(require 'python)

(defun python-add-debug-highlight ()
  "Adds a highlighter for use by `python-pdb-breakpoint-string'"
  (highlight-lines-matching-regexp "## DEBUG ##\\s-*$" 'hi-red-b))

(add-hook 'python-mode-hook 'python-add-debug-highlight)

(defvar python-pdb-breakpoint-string
  ;;"from pudb import set_trace;set_trace() ## DEBUG ##"
  "import ipdb,pprint;\
pp=pprint.PrettyPrinter(width=2,indent=2).pprint;\
ipdb.set_trace(); ## DEBUG ##"
  "Python breakpoint string used by `python-insert-breakpoint'")

(defun python-insert-breakpoint ()
  "Inserts a python breakpoint using `ipdb'"
  (interactive)
  ;; this preserves the correct indentation in case the line above
  ;; point is a nested block
  (insert python-pdb-breakpoint-string)
  (back-to-indentation)
)

(defun python-remove-debug-breaks ()
   "Removes all debug breakpoints"
   (flush-lines "\#\# DEBUG \#\#")
   (flush-lines "import ipdb")
   (flush-lines "pp = pprint")
   (flush-lines "ipdb.set"))

(defun pdb ()
  (interactive)
  (save-excursion
  (python-insert-string python-pdb-breakpoint-string)))

(defun rpdb()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (python-remove-debug-breaks)))

(defun python-insert-string(in-string)
  "Inserts string"
  (interactive)
  (back-to-indentation)
  (split-line)
  (insert in-string)
  (python-indent-line)
  (backward-char 3))


(add-hook 'python-mode-hook 'python-remove-debug-breaks)

(defun lwarn()
  "Insert warning log entry"
  (interactive)
  (python-insert-string "log.warning(' %s' % () )"))

(defun lerror()
  "Insert error log entry"
  (interactive)
  (python-insert-string "log.error(' %s' % () )"))

(defun lexcept()
  "Insert exception log entry"
  (interactive)
  (python-insert-string "log.exception(' %s' % () )"))

(defun linfo()
  "Insert info log entry"
  (interactive)
  (python-insert-string "log.info(' %s' % () )"))

(defun ldebug()
  "Insert debug log entry"
  (interactive)
  (python-insert-string "log.debug(' %s' % () )"))

;;;elpy
(elpy-enable)
(setq elpy-rpc-backend "jedi")
(when (window-system) 'ns
      (pyvenv-activate "/Users/svithlani/src/sports-app/"))

(add-hook 'python-mode-hook 'which-function-mode)

;;;; ag.el
(require 'ag)
(when (window-system) 'ns
      (setq ag-executable "/opt/local/bin/ag")
)

;;;; Hydra configurations

(defhydra hydra-breadcrumb ()
  "Breadcrumb"
  ("s" bc-set :color blue)
  ("n" bc-next :color red)
  ("p" bc-previous :color red)
  ("c" bc-clear :color red)
  ("l" bc-list :color blue)
  ("q" nil :color red)
  )


(defhydra hydra-avy ()
  "Avy"
  ("l" avy-goto-line "line" :color blue)

  ("C" avy-goto-char   "char" :color blue)
  ("c" avy-goto-char-2 "char-2" :color blue)

  ("r" avy-copy-region "copy-region" :color blue)
  ("L" avy-copy-line "copy-line" :color blue)

  ("m" avy-move-line "move-line" :color blue)
  ("M" avy-move-region "move-region" :color blue)

  ("w" avy-goto-word-1 "word-1" :color blue)
  ("W" avy-goto-word-0 "word-0" :color blue)

  ("k" avy-kill-region "kill-region" :color blue)
  ("K" avy-kill-whole-line "kill-line" :color blue)

  ("q" nil "quit"))

(global-set-key (kbd "<f1>") 'hydra-avy/body)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(defhydra hydra-er()
  "Expand Region"
  ("w" er/mark-word "word" :color red)
  ("s" er/mark-symbol "symbol" :color red)
  ("m" er/mark-method-call "method-call" :color red)
  ("q" er/mark-inside-quotes "i-quotes" :color red)
  ("p" er/mark-inside-pairs "i-pairs" :color red)
  ("P" er/mark-outside-pairs "o-pairs" :color red)
  ("c" copy-region-as-kill "copy-region" :color blue)
  ("<return>" nil))

(defun er()
  (interactive)
  (hydra-er/body))


(defhydra hydra-text-commands ()
  "Text commands"
  ("c" copy-region-as-kill "copy-region" :color blue)
  ("w" thing-copy-word "copy-word" :color blue)
  ("l" thing-copy-line "copy-line"  :color blue)
  ("s" thing-copy-symbol "copy-symbol" :color blue)
  ("y" yank-n-times "multiple paste" :color blue )
  ("e" hydra-er/body "expand-region" :color blue)
  ("q" nil "quit"))

(global-set-key (kbd "<f2>") 'hydra-text-commands/body)


(defhydra hydra-lines (goto-map ""
                           :pre (linum-mode 1)
                           :post (linum-mode -1))
  "Lines"
  ("c" thing-copy-line "copy" :color blue)
  ("e" thing-copy-to-line-end "copy-end" :color blue)
  ("b" thing-copy-to-line-beginning "copy-begin" :color blue)
  ("D" djcb-duplicate-line "dup-line" :color red)
  ("g" goto-line "goto-line")
  ("m" set-mark-command "mark" :bind nil)
  ("s" xah-select-current-line "Select current" :color red)
  ("r" copy-region-as-kill "copy-region" :color blue)
  ("R" join-region "join-region" :color blue)
  ("n" forward-line "forward")
  ("p" previous-line "backwards")
  ("u" move-text-up "move-up" :color red)
  ("d" move-text-down "move-down" :color red)
  ("k" kill-lines "kill-lines" :color blue)
  ("l" linum-mode "linum" :color blue)
  ("x" kill-line-remove-blanks "kill-line-rb" :color blue)
  ("j" top-join-line "join-next-line" :color red)
  ("J" delete-indentation "join-prev-line" :color red)
  ("h" highlight-duplicate-lines-in-region-or-buffer "dup-line" :color red)
  ("o" ov-clear "ov-clear")
  ("q" nil "quit"))

(global-set-key (kbd "<f4>") 'hydra-lines/body)

(require 'highlight-symbol)
(defhydra hydra-highlight-symbol ()
  "Highlight symbol"
  ("h" highlight-symbol-at-point "highlight-toggle" :color red)
  ("n" highlight-symbol-next "next" :color red)
  ("p" highlight-symbol-prev "previous" :color red)
  ("r" highlight-symbol-remove-all "remove-all ":color blue)
  ("q" nil "quit"))

(global-set-key (kbd "<f3>") 'hydra-highlight-symbol/body)

(defun reattach-occur ()
  (if (get-buffer "*Occur*")
    (switch-to-buffer-other-window "*Occur*")
    (hydra-occur-dwim/body) ))

;; Used in conjunction with occur-mode-goto-occurrence-advice this helps keep
;; focus on the *Occur* window and hides upon request in case needed later.
(defhydra hydra-occur-dwim ()
  "Occur mode"
  ("o" occur-dwim "occur-dwim" :color red)
  ("m" multi-occur-dwim "multi-occur-dwim" :color red)
  ("a" multi-occur-all-dwim "multi-occur-all-dwin" :color red)
  ("M" multi-occur-in-mode-dwim "Mode multi-occur" :color red)
  ("n" occur-next "Next" :color red)
  ("p" occur-prev "Prev":color red)
  ("h" delete-window "Hide" :color blue)
  ("r" (reattach-occur) "Re-attach" :color red)
  ("q" delete-window "quit" :color blue))

(global-set-key (kbd "C-x o") 'hydra-occur-dwim/body)


;;;; fastnav
(require 'fastnav)
(defhydra hydra-fastnav ()
  "FastNav on chars"
  ("m" mark-to-char-forward "Mark forward" :color blue)
  ("M" mark-to-char-backward "Mark back" :color blue)
  ("r" replace-char-forward "Replace forward" :color blue)
  ("R" replace-char-backward "Replace back" :color blue)
  ("d" delete-char-forward "Delete forward" :color blue)
  ("D" delete-char-backward "Delete back" :color blue)
  ("i" insert-at-char-forward "Insert forward" :color blue)
  ("I" insert-at-char-backward "Insert back" :color blue)
  ("z" zap-up-to-char-forward "Zap up-to forward" :color blue)
  ("Z" zap-up-to-char-backward "Zap up-to backwards" :color blue)
  ("e" execute-at-char-forward "Execute forward" :color blue)
  ("E" execute-at-char-backward "Execute backwards" :color blue)
  ("s" th-zap-to-string "Zap to string" :color blue)
  ("p" th-zap-to-regexp "Zap to reg-exp" :color blue)
  ("q" nil "quit"))

(global-set-key (kbd "<f5>") 'hydra-fastnav/body)


(defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                           :color pink
                           :post (deactivate-mark))
"
  ^_p_^     _d_elete    _s_tring
_b_   _f_   _q_uit      _y_ank
  ^_n_^     _k_new-copy _r_eset
^^^^        _e_xchange  _u_ndo
^^^^        ^ ^         _p_aste
"
  ("f" forward-char nil)
  ("b" backward-char nil)
  ("p" previous-line nil)
  ("n" next-line nil)
  ("e" exchange-point-and-mark nil)
  ("k" copy-rectangle-as-kill nil)
  ("d" delete-rectangle nil)
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1))
   nil)
  ("y" yank-rectangle nil)
  ("u" undo nil)
  ("s" string-rectangle nil)
  ("p" kill-rectangle nil)
  ("q" nil nil))


;; https://github.com/abo-abo/hydra/wiki/Switch-to-buffer
(defun my/name-of-buffers (n)
  "Return the names of the first N buffers from `buffer-list'."
  (let ((bns
         (delq nil
               (mapcar
                (lambda (b)
                  (unless (string-match "^ " (setq b (buffer-name b)))
                    b))
                (buffer-list)))))
    (subseq bns 1 (min (1+ n) (length bns)))))

;; Given ("a", "b", "c"), return "1. a, 2. b, 3. c".
(defun my/number-names (list)
  "Enumerate and concatenate LIST."
  (let ((i 0))
    (mapconcat
     (lambda (x)
       (format "%d. %s" (cl-incf i) x))
     list ", ")))

(defvar my/last-buffers nil)

(defun my/switch-to-buffer (arg)
  (interactive "p")
  (switch-to-buffer
   (nth (1- arg) my/last-buffers)))

(defun my/switch-to-buffer-other-window (arg)
  (interactive "p")
  (switch-to-buffer-other-window
   (nth (1- arg) my/last-buffers)))


(defhydra my/switch-to-buffer (:exit t
			       :body-pre (setq my/last-buffers
		               (my/name-of-buffers 5)))
"
Other buffers: %s(my/number-names my/last-buffers) b: ibuffer q: quit w: other-window
"
   ("o" (my/switch-to-buffer 0))
   ("1" (my/switch-to-buffer 1))
   ("2" (my/switch-to-buffer 2))
   ("3" (my/switch-to-buffer 3))
   ("4" (my/switch-to-buffer 4))
   ("5" (my/switch-to-buffer 5))
   ("i" (ido-switch-buffer))
   ("f" (ido-find-file) "Find file")
   ("b" (ibuffer) "IBuffer")
   ("w" other-window "o-window")
   ("d" delete-other-windows "d-o-window")
   ("q" nil)
   )

(global-set-key (kbd "C-x b") 'my/switch-to-buffer/body)
(global-set-key (kbd "C-x C-b") 'my/switch-to-buffer/body)

(defhydra hydra-tags ()
  "Navigate matching tags"
  ("f" sgml-skip-tag-forward "Forward" :color red)
  ("b" sgml-skip-tag-backward "Backward" :color red)
  ("q" nil "quit"))

(global-set-key (kbd "M-y") #'helm-show-kill-ring)


(defun move-and-hydra(fn)
  (flash-line-highlight 0.5)
  (funcall fn 1)
  (hydra-move/body))

(defun hydra-move-post()
  (set-cursor-color "#ffffff"))

(defhydra hydra-move
  (:timeout 5
	    :body-pre (set-cursor-color "#5BFF33")
	    :post (hydra-move-post))
  "move"
  ("a" smarter-move-beginning-of-line)
  ("e" move-end-of-line)
  ("n" next-line)
  ("p" previous-line)
  ("f" forward-char)
  ("b" backward-char)
  ("w" forward-word)
  ("q" backward-word)
  ("d" scroll-up)
  ("u" scroll-down)
  ("t" beginning-of-buffer)
  ("T" end-of-buffer)
  ("g" avy-goto-line "goto-line")
  ("c" avy-goto-char-2 "goto-char-2")
  ("l" recenter-top-bottom "re-center")
  ("<return>" nil "quit" :color blue)
  ("<RETURN>" nil "quit" :color blue)
  ("<ESC>" nil "quit" :color blue)

)

(defun hydra-move-keys()
  (interactive)
  (global-set-key (kbd "C-n") (lambda() (interactive)
				(move-and-hydra #'next-line)))
  (global-set-key (kbd "C-p") (lambda() (interactive)
				(move-and-hydra #'previous-line)))
  (global-set-key (kbd "C-f") (lambda() (interactive)
				(move-and-hydra #'forward-char)))
  (global-set-key (kbd "C-b") (lambda() (interactive)
				(move-and-hydra #'backward-char)))
  (global-set-key (kbd "M-f") (lambda() (interactive)
				(move-and-hydra #'forward-word)))
  (global-set-key (kbd "M-b") (lambda() (interactive)
				(move-and-hydra #'backward-word))))

(defun hydra-move-no-keys()
  (interactive)
  (global-set-key (kbd "C-n") #'next-line)
  (global-set-key (kbd "C-p") #'previous-line)
  (global-set-key (kbd "C-f") #'forward-char)
  (global-set-key (kbd "C-b") #'backward-char)
  (global-set-key (kbd "M-f") #'forward-word)
  (global-set-key (kbd "M-b") #'backward-word))

(hydra-move-keys)

(global-origami-mode 1)

(defhydra hydra-origami()
  "Origami"
  ("o" origami-open-node)
  ("c" origami-close-node)
  ("t" origami-toggle-node)
  ("O" origami-open-all-nodes "Open")
  ("C" origami-close-all-nodes "Close")
  ("p" origami-previous-fold)
  ("n" origami-next-fold)
  ("u" origami-undo "undo")
  ("r" origami-redo "redo")
  ("R" origami-reset "Reset")
  ("q" nil :color blue)
  )


(defhydra hydra-macro (:hint nil :color pink
			     :pre
                             (when defining-kbd-macro
                                 (kmacro-end-macro 1)))
  "
  ^Create-Cycle^   ^Basic^           ^Insert^        ^Save^         ^Edit^
╭─────────────────────────────────────────────────────────────────────────╯
     ^_i_^           [_e_] execute    [_n_] insert    [_b_] name      [_'_] previous
     ^^↑^^           [_d_] delete     [_t_] set       [_K_] key       [_,_] last
 _j_ ←   → _l_       [_o_] edit       [_a_] add       [_x_] register
     ^^↓^^           [_r_] region     [_f_] format    [_B_] defun
     ^_k_^           [_m_] step
    ^^   ^^          [_s_] swap
"
  ("j" kmacro-start-macro :color blue)
  ("l" kmacro-end-or-call-macro-repeat)
  ("i" kmacro-cycle-ring-previous)
  ("k" kmacro-cycle-ring-next)
  ("r" apply-macro-to-region-lines)
  ("d" kmacro-delete-ring-head)
  ("e" kmacro-end-or-call-macro-repeat)
  ("o" kmacro-edit-macro-repeat)
  ("m" kmacro-step-edit-macro)
  ("s" kmacro-swap-ring)
  ("n" kmacro-insert-counter)
  ("t" kmacro-set-counter)
  ("a" kmacro-add-counter)
  ("f" kmacro-set-format)
  ("b" kmacro-name-last-macro)
  ("K" kmacro-bind-to-key)
  ("B" insert-kbd-macro)
  ("x" kmacro-to-register)
  ("'" kmacro-edit-macro)
  ("," edit-kbd-macro)
  ("q" nil :color blue))

(put 'downcase-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("e6370c4899d463555a6aecf2da2700e2e039f93273212ce1836e9f94ad4af278" "0ca5a450034c92069769e071e63a3d2b2346c304bf186245467f59d993f5b979" "0e6e456b15dbeb6e7bcad4131f029e027cceecc3cf1598fc49141343860bfce6" "7f6796a9b925f727bbe1781dc65f7f23c0aa4d4dc19613aa3cf96e41a96651e4" "5b388add509c423e4ac275668662486628690e7ffe0050998615fc4c3669c16c" "473c69b2e448e37861e2051f793a8981ac419cc06ac66b2be6c08fddcf898175" "55baf0e5235a0268ea0b9b32f7099eb5e85a8e347fa63d6e2c9d6046362e1efb" "f142c876b896c6ca19149cacd80ddd68a351f67f7fe3b786274ceee970276780" "e3f648bb477a2e2332124f5ca8bd070e8624f152be6b4478668a69e5de7510ff" "33119c11708b5e5fe0b97bcce0b0565e456cf9c1172f3c08fad9282330325667" "355e1c0eb7cd20794e983b4c6f5c0c978a85b159d6aadb2fae15faa25fb344e5" "c442464ca37a1cc2b6bc1c3b815d9944a7c66b608b7021308de1ebd9ae37aa75" "b7a112711a92e540425c5270f7b3e41f8e357911ef9cdefd970d9662fcf01e74" "c18112b0999ffea6f8d21d86ab76f43f28448ff1969947c8a9d168e674a0d01d" "80050f721c3abddb96e775a5dd2517dd8f93f71349e8f300f7240ab18827e616" default)))
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-yasnippet elpy-module-django elpy-module-sane-defaults)))
 '(package-selected-packages
   (quote
    (zerodark-theme kaolin-themes flatui-dark-theme flatui-theme hl-spotlight idle-highlight-mode 0xc monky color-theme-actress color-theme-approximate color-theme-cobalt color-theme-complexity color-theme-dg color-theme-dpaste color-theme-eclipse color-theme-emacs-revert-theme color-theme-github color-theme-gruber-darker color-theme-heroku color-theme-ir-black color-theme-library color-theme-modern color-theme-molokai color-theme-monokai color-theme-railscasts color-theme-sanityinc-solarized color-theme-sanityinc-tomorrow color-theme-tango color-theme-tangotango color-theme-twilight color-theme-vim-insert-mode color-theme-wombat color-theme-x color-theme-zenburn colour-region color-theme-buffer-local company-dict company-emoji company-shell zenburn-theme zen-and-art-theme yaml-mode wrap-region wide-n volatile-highlights visual-regexp-steroids visible-mark virtualenv undo-tree twilight-theme twilight-bright-theme twilight-anti-bright-theme tommyh-theme tj-mode tangotango-theme syntax-subword swoop swiper suscolors-theme soothe-theme solarized-theme soft-morning-theme smyx-theme smooth-scrolling smooth-scroll smex smart-mode-line-powerline-theme react-snippets rainbow-mode rainbow-delimiters pretty-mode pos-tip plur paredit paradox ov origami nose noctilux-theme nginx-mode names multiple-cursors move-text moe-theme markdown-mode+ magit-push-remote macrostep macros+ leuven-theme key-chord jsx-mode jedi jazz-theme itail iregister iedit idomenu ido-ubiquitous hungry-delete hemisu-theme hc-zenburn-theme guide-key gruber-darker-theme grandshell-theme google-this git-timemachine fuzzy full-ack firecode-theme firebelly-theme fastnav faff-theme expand-region espresso-theme emmet-mode elpy ecb easy-kill-extras doom-themes django-theme django-snippets django-mode django-manage distinguished-theme display-theme deft darkmine-theme darkburn-theme darkane-theme dark-mint-theme danneskjold-theme cyberpunk-theme csv-mode color-theme-solarized color-moccur cherry-blossom-theme bug-hunter bubbleberry-theme browse-kill-ring boxquote bm bliss-theme birds-of-paradise-plus-theme beacon basic-theme badger-theme back-button autumn-light-theme autopair aurora-theme atom-one-dark-theme atom-dark-theme angry-police-captain ample-zen-theme ample-theme ample-regexps ahungry-theme aggressive-indent ag ace-window ace-link ace-jump-zap ace-jump-buffer ace-isearch)))
 '(paradox-github-token t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(minibuffer-prompt ((default (:foreground "blue")) (nil (:background "grey")))))
