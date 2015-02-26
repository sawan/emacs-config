;; http://milkbox.net/note/single-file-master-emacs-configuration/

;;;; package.el

(require 'package)

(setq package-user-dir "~/.emacs.d/elpa/")
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/")
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(defun mp-install-rad-packages ()
  "Install only the sweetest of packages."
  (interactive)
  (package-refresh-contents)
  (mapc #'(lambda (package)
           (unless (package-installed-p package)
             (package-install package)))
        '(browse-kill-ring
          ido-ubiquitous
          magit
          paredit
          smex
          undo-tree
	  bm
	  pos-tip
	  auto-complete
	  ecb
	  yasnippet
	  rainbow-delimiters
	  rainbow-mode
	  fastnav
	  cedet
          hungry-delete
	  full-ack
	  undo-tree
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

(require 'magit)
(require 'wide-n)
(require 'kill-lines)
(require 'multiple-cursors)
(require 'wide-n)
(require 'extraedit)
(require 'highlight-tail)
(require 'no-easy-keys)
(require 'smyx-theme)

(require 'google-this)
(google-this-mode 1)


(require 'autopair)
(autopair-global-mode)


(defun eshell/force-close ()
    "Eshell alias to force close when it complains about read-only text"
    (interactive)
    (let ((inhibit-read-only t))
      (ignore-errors)
        (kill-buffer "*eshell*")))

(add-hook 'kill-emacs-hook '(lambda nil
                              (eshell/force-close)))

;; start native Emacs server ready for client connections                  .
(add-hook 'after-init-hook 'server-start)

;; save history
(savehist-mode 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(setq savehist-file "~/.emacs.d/savehist")

;; use y/n for all yes-no answers
(defalias 'yes-or-no-p 'y-or-n-p)

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

;; wrap lines at 80 columns
(setq-default fill-column 80)
(add-hook 'find-file-hook 'turn-on-auto-fill)

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
(add-hook 'find-file-hooks (lambda() (show-paren-mode t)))

(global-set-key (kbd "<C-f6>") 'linum-mode)

;; display path to file in frame title
;(setq-default mode-line-format
(setq-default frame-title-format
              (list '((buffer-file-name " %f"
                       (dired-directory
			dired-directory
			(revert-buffer-function " %b"
			      ("%b - Dir:  " default-directory)))))))


;;;; utility functions

;; http://www.emacswiki.org/emacs-en/PosTip
(defun describe-function (function)
   "Display the full documentation of FUNCTION (a symbol) in tooltip."
   (interactive (list (function-called-at-point)))
   (if (null function)
       (pos-tip-show
        "** You didn't specify a function! **" '("red"))
     (pos-tip-show
      (with-temp-buffer
        (let ((standard-output (current-buffer))
              (help-xref-following t))
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
  (interactive "*r")      (save-excursion
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

(key-chord-define-global "dl" 'djcb-duplicate-line)

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

(key-chord-define-global "zs" 'th-zap-to-string)
(key-chord-define-global "zr" 'th-zap-to-regexp)

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
                  (ignore-errors (next-line 5))))

(global-set-key (kbd "C-S-p")
                (lambda ()
                  (interactive)
                  (ignore-errors (previous-line 5))))

(global-set-key (kbd "C-S-f")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-char 5))))

(global-set-key (kbd "C-S-b")
                (lambda ()
                  (interactive)
                  (ignore-errors (backward-char 5))))


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


;;;; emacs lisp

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

;;;; MISC libraries

(require 'highlight-indentation)

;; restore point at location upon file re-visit
(require 'saveplace)
(setq-default save-place t)

;; DrewsLibraries from EmacsWiki
; crosshairs
(require 'crosshairs)
(global-set-key (kbd "<M-f6>") 'flash-crosshairs)

(require 'exec-abbrev-cmd)
(exec-abbrev-cmd-mode 1)
(global-set-key (kbd "C-x x") 'exec-abbrev-cmd)

;; http://www.emacswiki.org/emacs/ThingEdit
; copy and paste various types of data
(require 'thing-edit)
(key-chord-define-global "cw" 'thing-copy-word)
(key-chord-define-global "cl" 'thing-copy-line)
(key-chord-define-global "cs" 'thing-copy-symbol)
(key-chord-define-global "lb" 'thing-copy-to-line-beginning)
(key-chord-define-global "le" 'thing-copy-to-line-end)
(key-chord-define-global "cr" 'copy-region-as-kill)
(key-chord-define-global "rl" 'kill-line-remove-blanks)

(require 'highlight-symbol)
(global-set-key (kbd "<f9>")   'highlight-symbol-at-point)
(global-set-key (kbd "<C-f9>") 'highlight-symbol-next)
(global-set-key (kbd "<S-f9>") 'highlight-symbol-prev)
(global-set-key (kbd "<M-f9>") 'highlight-symbol-remove-all)

;; revert all open buffers, useful when VC changes happen in the background
(require 'revbufs)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Bastardised version from
;; http://endlessparentheses.com/implementing-comment-line.html and
;; https://github.com/kaushalmodi/.emacs.d/blob/13bc1313e786ce1f1ab41d5aaff3dc39dfc57852/setup-files/setup-editing.el#L110-117
(defun comment-dwim-lines-or-region (n)
  "Comment or uncomment current line or active region and leave point after it.
   With positive prefix, apply to N lines including current one.
   With negative prefix, apply to -N lines above."
  (interactive "p")
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
      (comment-or-uncomment-region (line-beginning-position) (goto-char (line-end-position n))))
  (forward-line 1)
  (back-to-indentation))

(global-set-key (kbd "M-;") #'comment-dwim-lines-or-region)

;;;; emacros
;; Emacros http://thbecker.net/free_software_utilities/emacs_lisp/emacros/emacros.html
(require 'emacros)
(setq emacros-global-dir "~/.emacs.d")
(global-set-key [f12] #'emacros-auto-execute-named-macro)
;; Load predefined macros
(add-hook 'after-init-hook 'emacros-load-macros)

;;;; Tramp
(require 'tramp)
(when (window-system) 'w32
      (setq tramp-default-method "plink"))
(setq  tramp-completion-reread-directory-timeout 0)

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
                              (tramp-cleanup-all-buffers)
                              ))

;;;; key-chord
(require 'key-chord)
(key-chord-mode 1)
(key-chord-define emacs-lisp-mode-map "eb" 'eval-buffer)
(key-chord-define emacs-lisp-mode-map "ed" 'eval-defun)
(key-chord-define emacs-lisp-mode-map "er" 'eval-region)
(key-chord-define emacs-lisp-mode-map "kl" 'kill-lines)

;;;; broswe-kill-ring config
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
(global-set-key (kbd "C-c k") 'browse-kill-ring)

;;;; bm.el config -- bookmarks
(setq bm-restore-repository-on-load t)
(require 'bm)
(global-set-key (kbd "<C-f1>") 'bm-toggle)
(global-set-key (kbd "<C-f2>") 'bm-next)
(global-set-key (kbd "<C-f3>") 'bm-show)
(global-set-key (kbd "<C-f4>") 'bm-show-all)

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

;; Saving the repository to file when on exit.
;; kill-buffer-hook is not called when emacs is killed, so we
;; must save all bookmarks first.
(add-hook 'kill-emacs-hook '(lambda nil
                              (bm-buffer-save-all)
                              (bm-repository-save)))

;;;; ido mode
(require 'ido)
(ido-mode t)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)

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
            (define-key artist-mode-map (kbd "C-c C-a C-o") 'artist-ido-select-operation)
            (define-key artist-mode-map (kbd "C-c C-a C-c") 'artist-ido-select-settings)))


;;;; smex
;; Http://github.com/nonsequitur/smex/
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(setq smex-save-file "~/.emacs.d/smex-items")
;; This is your old M-x.
;(global-set-key (kbd "M-x") 'execute-extended-command)


;; Set breadcrumbs in visited buffers for navigation
(require 'breadcrumb)
(key-chord-define-global "bs" 'bc-set)
(key-chord-define-global "bp" 'bc-previous)
(key-chord-define-global "bn" 'bc-next)
(key-chord-define-global "bl" 'bc-list)

;;;; fastnav
(require 'fastnav)
;; (global-set-key "\M-z" 'zap-up-to-char-forward)
;; (global-set-key "\M-Z" 'zap-up-to-char-backward)
;; (global-set-key "\M-s" 'jump-to-char-forward)
;; (global-set-key "\M-S" 'jump-to-char-backward)
(global-set-key "\M-r" 'replace-char-forward)
(global-set-key "\M-R" 'replace-char-backward)
(global-set-key "\M-i" 'insert-at-char-forward)
(global-set-key "\M-I" 'insert-at-char-backward)
(global-set-key "\M-j" 'execute-at-char-forward)
(global-set-key "\M-J" 'execute-at-char-backward)
(global-set-key "\M-k" 'delete-char-forward)
(global-set-key "\M-K" 'delete-char-backward)
(global-set-key "\M-m" 'mark-to-char-forward)
(global-set-key "\M-M" 'mark-to-char-backward)
(global-set-key "\M-p" 'sprint-forward)
(global-set-key "\M-P" 'sprint-backward)

(key-chord-define-global "zf" 'zap-up-to-char-forward)
(key-chord-define-global "zb" 'zap-up-to-char-backward)

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
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.prod$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.model$" . yaml-mode))
(add-hook 'yaml-mode-hook '(lambda ()
                             (define-key yaml-mode-map
                               (kbd "RET") 'newline-and-indent)))

;;(add-to-list 'ac-modes 'yaml-mode)

;;;; autocomplete
(require 'auto-complete-config)
(ac-config-default)
(auto-complete-mode 1)

;;;; python mode
(require 'python)
;re-bind RET to newline and indent, mode defines C-j for doing this
(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map (kbd "RET") 'newline-and-indent)))

;; Rebind RET
(add-hook 'python-mode-hook '(lambda ()
			       (define-key python-mode-map
				 (kbd "RET") 'newline-and-indent)))

(defun python-add-debug-highlight ()
  "Adds a highlighter for use by `python-pdb-breakpoint-string'"
  (highlight-lines-matching-regexp "## DEBUG ##\\s-*$" 'hi-red-b))

(add-hook 'python-mode-hook 'python-add-debug-highlight)

(defvar python-pdb-breakpoint-string
  ;;"from pudb import set_trace;set_trace() ## DEBUG ##"
  "import ipdb,pprint;pp=pprint.PrettyPrinter(width=2,indent=2).pprint;ipdb.set_trace() ## DEBUG ##"
  "Python breakpoint string used by `python-insert-breakpoint'")

(defun python-insert-breakpoint ()
  "Inserts a python breakpoint using `ipdb'"
  (interactive "p")
  (back-to-indentation)
  ;; this preserves the correct indentation in case the line above
  ;; point is a nested block
  (split-line)
  (insert python-pdb-breakpoint-string)
  (python-indent-line)
  (save-buffer) )

(defun python-insert-string(in-string)
  "Inserts string"
  (interactive)
  (back-to-indentation)
  (split-line)
  (insert in-string)
  (python-indent-line))

(defun linfo()
  "Insert info log entry"
  (interactive)
  (python-insert-string "log.info(' %s' % () )"))

(defun ldebug()
  "Insert debug log entry"
  (interactive)
  (python-insert-string "log.debug(' %s' % () )"))

(key-chord-define python-mode-map "dd" 'python-insert-breakpoint)

;;;elpy
;;(elpy-enable)
;;(setq elpy-rpc-backend "rope")

(key-chord-define python-mode-map "yi" 'yas-insert-snippet)

(add-hook 'python-mode-hook 'which-function-mode)

;;;; ack
;; http://nschum.de/src/emacs/full-ack/
(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)
;(setq ack-executable "~/../../bin/ack")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
