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

;; start native Emacs server ready for client connections                  .
(add-hook 'after-init-hook 'server-start)

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
;(setq tramp-default-method "ssh")
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
(require 'emacros)
(setq emacros-global-dir "~/.emacs.d")
(global-set-key [f12] 'emacros-auto-execute-named-macro)
;; Load predefined macros
(add-hook 'after-init-hook 'emacros-load-macros)

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

(add-hook 'prog-mode-hook 'hs-minor-mode)

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

(Defun djcb-duplicate-line (&optional commentfirst)
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
(add-to-list 'auto-mode-alist '("\\.prod$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.model$" . yaml-mode))
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

;; ;;pymacs and rope
;; ;; http://pymacs.progiciels-bpi.ca/pymacs.html#install-the-pymacs-proper
;; (require 'pymacs)
;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-exec "pymacs" nil t)
;; (autoload 'pymacs-load "pymacs" nil t)
;; (autoload 'pymacs-autoload "pymacs")

;; (pymacs-load "ropemacs" "rope-")
;; (setq ropemacs-enable-autoimport t)
;; (setq ropemacs-enable-shortcuts t)

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


(define-abbrev-table 'sql-mode-abbrev-table
  '(
  ("absolute" "ABSOLUTE" nil 1)
  ("action" "ACTION" nil 1)
  ("add" "ADD" nil 1)
  ("after" "AFTER" nil 1)
  ("all" "ALL" nil 1)
  ("allocate" "ALLOCATE" nil 1)
  ("alter" "ALTER" nil 1)
  ("and" "AND" nil 1)
  ("any" "ANY" nil 1)
  ("are" "ARE" nil 1)
  ("array" "ARRAY" nil 1)
  ("as" "AS" nil 1)
  ("asc" "ASC" nil 1)
  ("asensitive" "ASENSITIVE" nil 1)
  ("assertion" "ASSERTION" nil 1)
  ("asymmetric" "ASYMMETRIC" nil 1)
  ("at" "AT" nil 1)
  ("atomic" "ATOMIC" nil 1)
  ("authorization" "AUTHORIZATION" nil 1)
  ("avg" "AVG" nil 1)
  ("before" "BEFORE" nil 1)
  ("begin" "BEGIN" nil 1)
  ("between" "BETWEEN" nil 1)
  ("bigint" "BIGINT" nil 1)
  ("binary" "BINARY" nil 1)
  ("bit" "BIT" nil 1)
  ("bitlength" "BITLENGTH" nil 1)
  ("blob" "BLOB" nil 1)
  ("boolean" "BOOLEAN" nil 1)
  ("both" "BOTH" nil 1)
  ("breadth" "BREADTH" nil 1)
  ("by" "BY" nil 1)
  ("call" "CALL" nil 1)
  ("called" "CALLED" nil 1)
  ("cascade" "CASCADE" nil 1)
  ("cascaded" "CASCADED" nil 1)
  ("case" "CASE" nil 1)
  ("cast" "CAST" nil 1)
  ("catalog" "CATALOG" nil 1)
  ("char" "CHAR" nil 1)
  ("char_length" "CHAR_LENGTH" nil 1)
  ("character" "CHARACTER" nil 1)
  ("character_length" "CHARACTER_LENGTH" nil 1)
  ("check" "CHECK" nil 1)
  ("clob" "CLOB" nil 1)
  ("close" "CLOSE" nil 1)
  ("coalesce" "COALESCE" nil 1)
  ("collate" "COLLATE" nil 1)
  ("collation" "COLLATION" nil 1)
  ("column" "COLUMN" nil 1)
  ("commit" "COMMIT" nil 1)
  ("condition" "CONDITION" nil 1)
  ("connect" "CONNECT" nil 1)
  ("connection" "CONNECTION" nil 1)
  ("constraint" "CONSTRAINT" nil 1)
  ("constraints" "CONSTRAINTS" nil 1)
  ("constructor" "CONSTRUCTOR" nil 1)
  ("contains" "CONTAINS" nil 1)
  ("continue" "CONTINUE" nil 1)
  ("convert" "CONVERT" nil 1)
  ("corresponding" "CORRESPONDING" nil 1)
  ("count" "COUNT" nil 1)
  ("create" "CREATE" nil 1)
  ("cross" "CROSS" nil 1)
  ("cube" "CUBE" nil 1)
  ("current" "CURRENT" nil 1)
  ("current_date" "CURRENT_DATE" nil 1)
  ("current_default_transform_group" "CURRENT_DEFAULT_TRANSFORM_GROUP" nil 1)
  ("current_path" "CURRENT_PATH" nil 1)
  ("current_role" "CURRENT_ROLE" nil 1)
  ("current_time" "CURRENT_TIME" nil 1)
  ("current_timestamp" "CURRENT_TIMESTAMP" nil 1)
  ("current_transform_group_for_type" "CURRENT_TRANSFORM_GROUP_FOR_TYPE" nil 1)
  ("current_user" "CURRENT_USER" nil 1)
  ("cursor" "CURSOR" nil 1)
  ("cycle" "CYCLE" nil 1)
  ("data" "DATA" nil 1)
  ("date" "DATE" nil 1)
  ("day" "DAY" nil 1)
  ("deallocate" "DEALLOCATE" nil 1)
  ("dec" "DEC" nil 1)
  ("decimal" "DECIMAL" nil 1)
  ("declare" "DECLARE" nil 1)
  ("default" "DEFAULT" nil 1)
  ("deferrable" "DEFERRABLE" nil 1)
  ("deferred" "DEFERRED" nil 1)
  ("delete" "DELETE" nil 1)
  ("depth" "DEPTH" nil 1)
  ("deref" "DEREF" nil 1)
  ("desc" "DESC" nil 1)
  ("describe" "DESCRIBE" nil 1)
  ("descriptor" "DESCRIPTOR" nil 1)
  ("deterministic" "DETERMINISTIC" nil 1)
  ("diagnostics" "DIAGNOSTICS" nil 1)
  ("disconnect" "DISCONNECT" nil 1)
  ("distinct" "DISTINCT" nil 1)
  ("do" "DO" nil 1)
  ("domain" "DOMAIN" nil 1)
  ("double" "DOUBLE" nil 1)
  ("drop" "DROP" nil 1)
  ("dynamic" "DYNAMIC" nil 1)
  ("each" "EACH" nil 1)
  ("element" "ELEMENT" nil 1)
  ("else" "ELSE" nil 1)
  ("elseif" "ELSEIF" nil 1)
  ("end" "END" nil 1)
  ("equals" "EQUALS" nil 1)
  ("escape" "ESCAPE" nil 1)
  ("except" "EXCEPT" nil 1)
  ("exception" "EXCEPTION" nil 1)
  ("exec" "EXEC" nil 1)
  ("execute" "EXECUTE" nil 1)
  ("exists" "EXISTS" nil 1)
  ("exit" "EXIT" nil 1)
  ("external" "EXTERNAL" nil 1)
  ("extract" "EXTRACT" nil 1)
  ("false" "FALSE" nil 1)
  ("fetch" "FETCH" nil 1)
  ("filter" "FILTER" nil 1)
  ("first" "FIRST" nil 1)
  ("float" "FLOAT" nil 1)
  ("for" "FOR" nil 1)
  ("foreign" "FOREIGN" nil 1)
  ("found" "FOUND" nil 1)
  ("free" "FREE" nil 1)
  ("from" "FROM" nil 1)
  ("full" "FULL" nil 1)
  ("function" "FUNCTION" nil 1)
  ("general" "GENERAL" nil 1)
  ("get" "GET" nil 1)
  ("global" "GLOBAL" nil 1)
  ("go" "GO" nil 1)
  ("goto" "GOTO" nil 1)
  ("grant" "GRANT" nil 1)
  ("group" "GROUP" nil 1)
  ("grouping" "GROUPING" nil 1)
  ("handler" "HANDLER" nil 1)
  ("having" "HAVING" nil 1)
  ("hold" "HOLD" nil 1)
  ("hour" "HOUR" nil 1)
  ("identity" "IDENTITY" nil 1)
  ("if" "IF" nil 1)
  ("immediate" "IMMEDIATE" nil 1)
  ("in" "IN" nil 1)
  ("indicator" "INDICATOR" nil 1)
  ("initially" "INITIALLY" nil 1)
  ("inner" "INNER" nil 1)
  ("inout" "INOUT" nil 1)
  ("input" "INPUT" nil 1)
  ("insensitive" "INSENSITIVE" nil 1)
  ("insert" "INSERT" nil 1)
  ("int" "INT" nil 1)
  ("integer" "INTEGER" nil 1)
  ("intersect" "INTERSECT" nil 1)
  ("interval" "INTERVAL" nil 1)
  ("into" "INTO" nil 1)
  ("is" "IS" nil 1)
  ("isolation" "ISOLATION" nil 1)
  ("iterate" "ITERATE" nil 1)
  ("join" "JOIN" nil 1)
  ("key" "KEY" nil 1)
  ("language" "LANGUAGE" nil 1)
  ("large" "LARGE" nil 1)
  ("last" "LAST" nil 1)
  ("lateral" "LATERAL" nil 1)
  ("leading" "LEADING" nil 1)
  ("leave" "LEAVE" nil 1)
  ("left" "LEFT" nil 1)
  ("level" "LEVEL" nil 1)
  ("like" "LIKE" nil 1)
  ("local" "LOCAL" nil 1)
  ("localtime" "LOCALTIME" nil 1)
  ("localtimestamp" "LOCALTIMESTAMP" nil 1)
  ("locator" "LOCATOR" nil 1)
  ("loop" "LOOP" nil 1)
  ("lower" "LOWER" nil 1)
  ("map" "MAP" nil 1)
  ("match" "MATCH" nil 1)
  ("map" "MAP" nil 1)
  ("member" "MEMBER" nil 1)
  ("merge" "MERGE" nil 1)
  ("method" "METHOD" nil 1)
  ("min" "MIN" nil 1)
  ("minute" "MINUTE" nil 1)
  ("modifies" "MODIFIES" nil 1)
  ("module" "MODULE" nil 1)
  ("month" "MONTH" nil 1)
  ("multiset" "MULTISET" nil 1)
  ("names" "NAMES" nil 1)
  ("national" "NATIONAL" nil 1)
  ("natural" "NATURAL" nil 1)
  ("nchar" "NCHAR" nil 1)
  ("nclob" "NCLOB" nil 1)
  ("new" "NEW" nil 1)
  ("next" "NEXT" nil 1)
  ("no" "NO" nil 1)
  ("none" "NONE" nil 1)
  ("not" "NOT" nil 1)
  ("null" "NULL" nil 1)
  ("nullif" "NULLIF" nil 1)
  ("numeric" "NUMERIC" nil 1)
  ("object" "OBJECT" nil 1)
  ("octet_length" "OCTET_LENGTH" nil 1)
  ("of" "OF" nil 1)
  ("old" "OLD" nil 1)
  ("on" "ON" nil 1)
  ("only" "ONLY" nil 1)
  ("open" "OPEN" nil 1)
  ("option" "OPTION" nil 1)
  ("or" "OR" nil 1)
  ("order" "ORDER" nil 1)
  ("ordinality" "ORDINALITY" nil 1)
  ("out" "OUT" nil 1)
  ("outer" "OUTER" nil 1)
  ("output" "OUTPUT" nil 1)
  ("over" "OVER" nil 1)
  ("overlaps" "OVERLAPS" nil 1)
  ("pad" "PAD" nil 1)
  ("parameter" "PARAMETER" nil 1)
  ("partial" "PARTIAL" nil 1)
  ("partition" "PARTITION" nil 1)
  ("path" "PATH" nil 1)
  ("position" "POSITION" nil 1)
  ("precision" "PRECISION" nil 1)
  ("prepare" "PREPARE" nil 1)
  ("preserve" "PRESERVE" nil 1)
  ("primary" "PRIMARY" nil 1)
  ("prior" "PRIOR" nil 1)
  ("privileges" "PRIVILEGES" nil 1)
  ("procedure" "PROCEDURE" nil 1)
  ("public" "PUBLIC" nil 1)
  ("range" "RANGE" nil 1)
  ("read" "READ" nil 1)
  ("reads" "READS" nil 1)
  ("real" "REAL" nil 1)
  ("recursive" "RECURSIVE" nil 1)
  ("ref" "REF" nil 1)
  ("references" "REFERENCES" nil 1)
  ("referencing" "REFERENCING" nil 1)
  ("relative" "RELATIVE" nil 1)
  ("release" "RELEASE" nil 1)
  ("repeat" "REPEAT" nil 1)
  ("resignal" "RESIGNAL" nil 1)
  ("restrict" "RESTRICT" nil 1)
  ("result" "RESULT" nil 1)
  ("return" "RETURN" nil 1)
  ("returns" "RETURNS" nil 1)
  ("revoke" "REVOKE" nil 1)
  ("right" "RIGHT" nil 1)
  ("role" "ROLE" nil 1)
  ("rollback" "ROLLBACK" nil 1)
  ("rollup" "ROLLUP" nil 1)
  ("routine" "ROUTINE" nil 1)
  ("row" "ROW" nil 1)
  ("rows" "ROWS" nil 1)
  ("savepoint" "SAVEPOINT" nil 1)
  ("schema" "SCHEMA" nil 1)
  ("scope" "SCOPE" nil 1)
  ("scroll" "SCROLL" nil 1)
  ("search" "SEARCH" nil 1)
  ("second" "SECOND" nil 1)
  ("section" "SECTION" nil 1)
  ("select" "SELECT" nil 1)
  ("sensitive" "SENSITIVE" nil 1)
  ("session" "SESSION" nil 1)
  ("session_user" "SESSION_USER" nil 1)
  ("set" "SET" nil 1)
  ("sets" "SETS" nil 1)
  ("signal" "SIGNAL" nil 1)
  ("similar" "SIMILAR" nil 1)
  ("size" "SIZE" nil 1)
  ("smallint" "SMALLINT" nil 1)
  ("some" "SOME" nil 1)
  ("space" "SPACE" nil 1)
  ("specific" "SPECIFIC" nil 1)
  ("specifictype" "SPECIFICTYPE" nil 1)
  ("sql" "SQL" nil 1)
  ("sqlcode" "SQLCODE" nil 1)
  ("sqlerror" "SQLERROR" nil 1)
  ("sqlexception" "SQLEXCEPTION" nil 1)
  ("sqlstate" "SQLSTATE" nil 1)
  ("sqlwarning" "SQLWARNING" nil 1)
  ("start" "START" nil 1)
  ("state" "STATE" nil 1)
  ("static" "STATIC" nil 1)
  ("submultiset" "SUBMULTISET" nil 1)
  ("substring" "SUBSTRING" nil 1)
  ("sum" "SUM" nil 1)
  ("symmetric" "SYMMETRIC" nil 1)
  ("system" "SYSTEM" nil 1)
  ("system_user" "SYSTEM_USER" nil 1)
  ("table" "TABLE" nil 1)
  ("tablesample" "TABLESAMPLE" nil 1)
  ("temporary" "TEMPORARY" nil 1)
  ("then" "THEN" nil 1)
  ("time" "TIME" nil 1)
  ("timestamp" "TIMESTAMP" nil 1)
  ("timezone_hour" "TIMEZONE_HOUR" nil 1)
  ("timezone_minute" "TIMEZONE_MINUTE" nil 1)
  ("to" "TO" nil 1)
  ("trailing" "TRAILING" nil 1)
  ("transaction" "TRANSACTION" nil 1)
  ("translate" "TRANSLATE" nil 1)
  ("translation" "TRANSLATION" nil 1)
  ("treat" "TREAT" nil 1)
  ("trigger" "TRIGGER" nil 1)
  ("trim" "TRIM" nil 1)
  ("true" "TRUE" nil 1)
  ("under" "UNDER" nil 1)
  ("undo" "UNDO" nil 1)
  ("union" "UNION" nil 1)
  ("unique" "UNIQUE" nil 1)
  ("unknown" "UNKNOWN" nil 1)
  ("unnest" "UNNEST" nil 1)
  ("until" "UNTIL" nil 1)
  ("update" "UPDATE" nil 1)
  ("upper" "UPPER" nil 1)
  ("usage" "USAGE" nil 1)
  ("user" "USER" nil 1)
  ("using" "USING" nil 1)
  ("value" "VALUE" nil 1)
  ("values" "VALUES" nil 1)
  ("varchar" "VARCHAR" nil 1)
  ("varying" "VARYING" nil 1)
  ("view" "VIEW" nil 1)
  ("when" "WHEN" nil 1)
  ("whenever" "WHENEVER" nil 1)
  ("where" "WHERE" nil 1)
  ("while" "WHILE" nil 1)
  ("window" "WINDOW" nil 1)
  ("with" "WITH" nil 1)
  ("within" "WITHIN" nil 1)
  ("without" "WITHOUT" nil 1)
  ("work" "WORK" nil 1)
  ("write" "WRITE" nil 1)
  ("year" "YEAR" nil 1)
  ("zone" "ZONE" nil 1)
   ))
