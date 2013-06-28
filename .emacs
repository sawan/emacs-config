(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/vendors")
(add-to-list 'load-path "~/.emacs.d/vendors/bm.el")
(add-to-list 'load-path "~/.emacs.d/vendors/pos-tip.el")
(add-to-list 'load-path "~/.emacs.d/vendors/auto-complete-1.2")
(add-to-list 'load-path "~/.emacs.d/vendors/ecb-2.40/")
(add-to-list 'load-path "~/.emacs.d/vendors/nonsequitur-smex-7d5d797/")
(add-to-list 'load-path "~/.emacs.d/vendors/yasnippet-0.6.1c/")
(add-to-list 'load-path "~/.emacs.d/vendors/Pymacs-0.24b/")
(add-to-list 'load-path "~/.emacs.d/vendors/DrewsLibraries/")
(add-to-list 'load-path "~/.emacs.d/vendors/exec-abbrev-cmd.el")
(add-to-list 'load-path "~/.emacs.d/vendors/revbufs.el")
(add-to-list 'load-path "~/.emacs.d/vendors/extraedit.el")
(add-to-list 'load-path "~/.emacs.d/vendors/breadcrumb.el")
(add-to-list 'load-path "~/.emacs.d/vendors/pycomplete.el")
(add-to-list 'load-path "~/.emacs.d/vendors/rainbow-delimiters.el")
(add-to-list 'load-path "~/.emacs.d/vendors/fastnav.el")
(add-to-list 'load-path "~/.emacs.d/vendors/jdee-2.4.0.1/")
(add-to-list 'load-path "~/.emacs.d/vendors/cedet-1.0/eieio/")
(add-to-list 'load-path "~/.emacs.d/vendors/cedet-1.0/common/")
(add-to-list 'load-path "~/.emacs.d/vendors/cedet-1.0/speedbar/")
(add-to-list 'load-path "~/.emacs.d/vendors/cedet-1.0/eieio/")
(add-to-list 'load-path "~/.emacs.d/vendors/cedet-1.0/semantic/")
(add-to-list 'load-path "~/.emacs.d/vendors/elib-1.0/")
(add-to-list 'load-path "~/.emacs.d/vendors/full-ack.el")
(add-to-list 'load-path "~/.emacs.d/vendors/undo-tree.el")
(add-to-list 'load-path "~/.emacs.d/vendors/csv-mode.el")
(add-to-list 'load-path "~/.emacs.d/vendors/key-chord.el")
(add-to-list 'load-path "~/.emacs.d/vendors/yaml-mode.el")
(add-to-list 'load-path "~/.emacs.d/vendors/highlight-tail.el")
(add-to-list 'load-path "~/.emacs.d/vendors/wide-n.el")
(add-to-list 'load-path "~/.emacs.d/vendors/color-moccur.el")
(add-to-list 'load-path "~/.emacs.d/vendors/table-1.5.54.el")
(add-to-list 'load-path "~/.emacs.d/vendors/kill-lines.el")
(add-to-list 'load-path "~/.emacs.d/vendors/iedit.el")
(add-to-list 'load-path "~/.emacs.d/vendors/multiple-cursors/")
(add-to-list 'load-path "~/.emacs.d/vendors/magit-1.2.0/")
(add-to-list 'load-path "~/.emacs.d/vendors/ido-ubiquitous.el")
(add-to-list 'load-path  "~/.emacs.d/vendors/emacros.el")

(require 'magit)
(require 'wide-n)
(require 'kill-lines)
(require 'multiple-cursors)
(require 'wide-n)
(require 'extraedit)
(require 'highlight-tail)

;; start native Emacs server ready for client connections
(add-hook 'after-init-hook 'server-start)

;; Load predefined macros
(add-hook 'after-init-hook 'emacros-load-macros)

(defface paren-face
   '((((class color) (background dark))
      (:foreground "grey20"))
     (((class color) (background light))
      (:foreground "grey90")))
   "Face used to dim parentheses.")

(add-hook 'lisp-mode-hook
 	  (lambda ()
 	    (font-lock-add-keywords nil
 				    '(("(\\|)" . 'paren-face)))))

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

(global-set-key [(control return)] 'smart-open-line)
(global-set-key [(control shift return)] 'smart-open-line-above)

;;Tramp
(require 'tramp)
(setq tramp-default-method "plink")
;; clean up after Tramp
(add-hook 'kill-emacs-hook '(lambda nil
                              (tramp-cleanup-all-connections)
                              (tramp-cleanup-all-buffers)
                              ))
(require 'key-chord)
(key-chord-mode 1)
(key-chord-define emacs-lisp-mode-map "eb" 'eval-buffer)
(key-chord-define emacs-lisp-mode-map "ed" 'eval-defun)
(key-chord-define emacs-lisp-mode-map "er" 'eval-region)
(key-chord-define emacs-lisp-mode-map "kl" 'kill-lines)

;; Emacros http://thbecker.net/free_software_utilities/emacs_lisp/emacros/emacros.html
(setq emacros-global-dir "~/.emacs.d")
(global-set-key [f12] 'emacros-auto-execute-named-macro)

;; Auto save desktop as well during buffer auto-save
(require 'desktop)
(setq desktop-path '("~/.emacs.d/"))
(setq desktop-dirname "~/.emacs.d/")
(setq desktop-base-file-name "emacs-desktop")
(desktop-save-mode 1)
(add-hook 'auto-save-hook (lambda () (desktop-save-in-desktop-dir)))

;; save history
(savehist-mode 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(setq savehist-file "~/.emacs.d/savehist")

;; revert buffer from file -- useful for when file has been changed by version control
(global-set-key (kbd "<f5>")   'revert-buffer)
(global-set-key (kbd "<C-f5>") 'vc-revert-buffer)

;; use y/n for all yes-no answers
(defalias 'yes-or-no-p 'y-or-n-p)

;; broswe-kill-ring config
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
(global-set-key (kbd "C-c k") 'browse-kill-ring)

;; bm.el config -- bookmarks
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

;; switching buffers
(iswitchb-mode 1)
;(setq iswitchb-buffer-ignore '("^ " "*Buffer"))

;; columns
(column-number-mode 1)
(display-time)

;; ido mode
(require 'ido)
(ido-mode t)
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


(defun iswitchb-local-keys ()
  (mapc (lambda (K)
          (let* ((key (car K)) (fun (cdr K)))
            (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
        '(("<right>" . iswitchb-next-match)
          ("<left>"  . iswitchb-prev-match)
          ("<up>"    . ignore             )
          ("<down>"  . ignore             ))))
(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)

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
  (interactive "^p")
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


;; use ibuffers for buffer listing
(defalias 'list-buffers 'ibuffer)
(setq ibuffer-default-sorting-mode 'major-mode)

;; Split windows horizontally by default
(setq split-width-threshold nil)

;; control how to move between windows
(windmove-default-keybindings 'meta)

;; code folding and hiding shortcuts ....
(global-set-key [f1] 'hs-hide-all)
(global-set-key [f2] 'hs-show-all)

(global-set-key [f3] 'hs-hide-block)
(global-set-key [f4] 'hs-show-block)

add-hook 'prog-mode-hook 'hs-minor-mode)

;; yasnippet
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/vendors/yasnippet-0.6.1c/snippets/")

;; wrap lines at 80 columns
(setq-default fill-column 80)
(add-hook 'find-file-hook 'turn-on-auto-fill)

;; pos-tip, used by autocomplete
(require 'pos-tip)

;; auto-complete config
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/vendors/auto-complete-1.2/dict")
(ac-config-default)

;; dotmode -- vi like command redo, bound to C-.
(require 'dot-mode)
(add-hook 'find-file-hooks 'dot-mode-on)

;; highlight symbols http://nschum.de/src/emacs/highlight-symbol/
(add-to-list 'load-path "/path/to/highlight-symbol")
(require 'highlight-symbol)

(global-set-key (kbd "<f9>")   'highlight-symbol-at-point)
(global-set-key (kbd "<C-f9>") 'highlight-symbol-next)
(global-set-key (kbd "<S-f9>") 'highlight-symbol-prev)
(global-set-key (kbd "<M-f9>") 'highlight-symbol-remove-all)

;; enable paren highliting for all files
(add-hook 'find-file-hooks (lambda() (show-paren-mode t)))

(require 'rainbow-delimiters)
(add-hook 'find-file-hook 'rainbow-delimiters-mode)

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

;; unique names for duplicate buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Http://github.com/nonsequitur/smex/
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(setq smex-save-file "~/.emacs.d/smex-items")
;; This is your old M-x.
;(global-set-key (kbd "M-x") 'execute-extended-command)


;;multi-term.el
(require 'multi-term)
(setq multi-term-program "/bin/bash")
(global-set-key (kbd "<f8>") 'multi-term)
(global-set-key (kbd "<C-f8>") 'multi-term-prev)

;; Other general shortcuts
(global-set-key (kbd "<C-f6>") 'linum-mode)

;; display path to file in frame title
;(setq-default mode-line-format
(setq-default frame-title-format
              (list '((buffer-file-name " %f"
                                        (dired-directory
                                         dired-directory
                                         (revert-buffer-function " %b"
                                                                 ("%b - Dir:  " default-directory)))))))

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

;; re-builder+
;; http://www.emacswiki.org/cgi-bin/emacs/ReBuilder
(require 're-builder+)

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

;; revert all open buffers, useful when VC changes happen in the background
(require 'revbufs)

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



;; Set breadcrumbs in visited buffers for navigation
(require 'breadcrumb)
(key-chord-define-global "bs" 'bc-set)
(key-chord-define-global "bp" 'bc-previous)
(key-chord-define-global "bn" 'bc-next)
(key-chord-define-global "bl" 'bc-list)

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

;; http://www.dr-qubit.org/emacs.php#undo-tree
;; hot damn.....
(require 'undo-tree)
(global-undo-tree-mode)

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

;; edit files as root
(defun sudo-find-file (file-name)
  (interactive "Find file (sudo): ")
  (find-file (concat "/sudo::" file-name)))

;; http://nschum.de/src/emacs/full-ack/
(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)
;(setq ack-executable "~/../../bin/ack")


(autoload 'csv-mode "csv-mode"
   "Major mode for editing comma-separated value files." t)
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-hook 'yaml-mode-hook '(lambda ()
                             (define-key yaml-mode-map
                               (kbd "RET") 'newline-and-indent)))
(add-to-list 'ac-modes 'yaml-mode)

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


(setq ediff-split-window-function 'split-window-horizontally)
(defun command-line-diff (switch)
      (let ((file1 (pop command-line-args-left))
            (file2 (pop command-line-args-left)))
        (ediff file1 file2)))

(add-to-list 'command-switch-alist '("--diff" . command-line-diff))

;;Python

;;pymacs and rope
;; http://pymacs.progiciels-bpi.ca/pymacs.html#install-the-pymacs-proper
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-autoload "pymacs")

(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)
(setq ropemacs-enable-shortcuts t)

;;python.el
(require 'python)
(setq-default indent-tabs-mode nil)    ; use only spaces and no tabs
(setq default-tab-width 4)

;re-bind RET to newline and indent, mode defines C-j for doing this
(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map (kbd "RET") 'newline-and-indent)))

(defun python-add-debug-highlight ()
  "Adds a highlighter for use by `python-pdb-breakpoint-string'"
  (highlight-lines-matching-regexp "## DEBUG ##\\s-*$" 'hi-red-b))

(add-hook 'python-mode-hook 'python-add-debug-highlight)

(defvar python-pdb-breakpoint-string
  "import ipdb,pprint;pp=pprint.PrettyPrinter(width=2,indent=2).pprint;ipdb.set_trace() ## DEBUG ##"
  "Python breakpoint string used by `python-insert-breakpoint'")

(defun python-insert-breakpoint ()
  "Inserts a python breakpoint using `ipdb'"
  (interactive)
  (back-to-indentation)
  ;; this preserves the correct indentation in case the line above
  ;; point is a nested block
  (split-line)
  (insert python-pdb-breakpoint-string)
  (python-indent-line)
  (save-buffer) )
(key-chord-define python-mode-map "dd" 'python-insert-breakpoint)

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

;;some extra functions for Python code completion
;(require 'pycomplete)

;; ;; pyflakes on OS X
;; ;;(add-to-list 'exec-path "/opt/local/bin/")

;; ;; flymake config to enable on the fly error checking for Python
;; ;; http://reinout.vanrees.org/weblog/2010/05/11/pep8-pyflakes-emacs.html

;; (add-to-list 'exec-path "~/../../bin")

;; (when (load "flymake" t)
;;   (defun flymake-pyflakes-init ()
;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-inplace))
;;            (local-file (file-relative-name
;;                         temp-file
;;                         (file-name-directory buffer-file-name))))
;;       (list "pyflakes" (list local-file))))

;;   (add-to-list 'flymake-allowed-file-name-masks
;;                '("\\.py\\'" flymake-pyflakes-init)))

;; (add-hook 'find-file-hook 'flymake-find-file-hook)

;; ;(custom-set-variables
;;   ;; custom-set-variables was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;; ; '(ecb-layout-window-sizes (quote (("left8" (0.15053763440860216 . 0.2727272727272727) (0.15053763440860216 . 0.22727272727272727) (0.15053763440860216 . 0.29545454545454547) (0.15053763440860216 . 0.18181818181818182)))))
;; ; '(ecb-options-version "2.40")
;; ; '(jde-jdk (quote ))
;; ; '(jde-jdk-registry (quote (("1.6.0.24" . "/usr/lib/jvm/java-6-sun/"))))
;; ; '(py-pychecker-command "~/bin/pychecker.sh")
;; ; '(py-pychecker-command-args (quote ("")))
;; ; '(python-check-command "~/bin/pychecker.sh")
;; ; '(safe-local-variable-values (quote ((test-case-name . formless\.test)))))

;; ;(set-default-font "DejaVu Sans Mono 9")

;; CEDT: required for ECB and speedbar
(load-file "~/.emacs.d/vendors/cedet-1.0/common/cedet.el")
(global-ede-mode 1)                      ; Enable the Project management system
(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion
(global-srecode-minor-mode 1)            ; Enable template insertion menu

;; ;; ECB
(require 'ecb)
(require 'ecb-autoloads)
(setq  ecb-new-ecb-frame  1)

;; (custom-set-faces
;;   custom-set-faces was added by Custom.
;;   If you edit it by hand, you could mess it up, so be careful.
;;   Your init file should contain only one such instance.
;;   If there is more than one, they won't work right.
;; )

;; (global-set-key (kbd "<f7>")   'ecb-activate)
;; (global-set-key (kbd "<C-f7>") 'ecb-deactivate)

;; ;; JDE library for Java
;; (setq defer-loading-jde t)
;; (if defer-loading-jde
;;     (progn
;;       (autoload 'jde-mode "jde" "JDE mode." t)
;;       (setq auto-mode-alist
;;          (append
;;           '(("\\.java\\'" . jde-mode))
;;           auto-mode-alist))))
;; (require 'jde)