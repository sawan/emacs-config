(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/vendors")
(add-to-list 'load-path "~/.emacs.d/vendors/auto-complete-1.2")
(add-to-list 'load-path "~/.emacs.d/vendors/ecb-2.40/")
(add-to-list 'load-path "~/.emacs.d/vendors/nonsequitur-smex-7d5d797/")
(add-to-list 'load-path "~/.emacs.d/vendors/yasnippet-0.6.1c/")
(add-to-list 'load-path "~/.emacs.d/vendors/DrewsLibraries/")
(add-to-list 'load-path "~/.emacs.d/vendors/exec-abbrev-cmd.el")

; start native Emacs server ready for client connections
(add-hook 'after-init-hook 'server-start)

; auto save desktop as well during buffer auto-save
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

;; enable paren highliting for all files
(add-hook 'find-file-hooks (lambda() (show-paren-mode t)))

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
  kept-new-versions 6
  kept-old-versions 2
  version-control t
  backup-by-copying t)

;; delete trailing whitespace before file is saved
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; switching buffers
(iswitchb-mode 1)
;(setq iswitchb-buffer-ignore '("^ " "*Buffer"))

;; colums
(column-number-mode 1)

;; ido mode
(require 'ido)
(ido-mode t)

(defun iswitchb-local-keys ()
  (mapc (lambda (K)
          (let* ((key (car K)) (fun (cdr K)))
            (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
        '(("<right>" . iswitchb-next-match)
          ("<left>"  . iswitchb-prev-match)
          ("<up>"    . ignore             )
          ("<down>"  . ignore             ))))
(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)

;; use ibuffers for buffer listing
(defalias 'list-buffers 'ibuffer)

;; Split windows horizontally by default
(setq split-width-threshold nil)

;; control how to move between windows
(windmove-default-keybindings 'meta)

;; code folding and hiding shortcuts ....
(global-set-key [f1] 'hs-hide-all)
(global-set-key [f2] 'hs-show-all)

(global-set-key [f3] 'hs-hide-block)
(global-set-key [f4] 'hs-show-block)

;; ..... will work in these modes
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'python-mode-hook     'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)

;; yasnippet
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/vendors/yasnippet-0.6.1c/snippets")

;; python.el
(require 'python)
(setq-default indent-tabs-mode nil)    ; use only spaces and no tabs
(setq default-tab-width 4)
;; re-bind RET to newline and indent, mode defines C-j for doing this
(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map (kbd "RET") 'newline-and-indent)))

;; wrap lines at 80 columns
(setq-default fill-column 80)
(add-hook 'find-file-hook 'turn-on-auto-fill)

;; auto-complete config
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/vendors/auto-complete-1.2/dict")
(ac-config-default)

;; dotmode -- vi like command redo, bound to C-.
(require 'dot-mode)
(add-hook 'find-file-hooks 'dot-mode-on)

;; pymacs and rope
(require 'pymacs)
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)
(setq ropemacs-enable-shortcuts t)

(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

; flymake config to enable on the fly error checking for Python
;http://reinout.vanrees.org/weblog/2010/05/11/pep8-pyflakes-emacs.html

(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))

(add-hook 'find-file-hook 'flymake-find-file-hook)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-layout-window-sizes (quote (("left8" (0.15053763440860216 . 0.2727272727272727) (0.15053763440860216 . 0.22727272727272727) (0.15053763440860216 . 0.29545454545454547) (0.15053763440860216 . 0.18181818181818182)))))
 '(ecb-options-version "2.40")
 '(power-macros-file "~/.emacs.d/power-macros")
 '(py-pychecker-command "~/bin/pychecker.sh")
 '(py-pychecker-command-args (quote ("")))
 '(python-check-command "~/bin/pychecker.sh")
 '(safe-local-variable-values (quote ((test-case-name . formless\.test)))))

;(set-default-font "DejaVu Sans Mono 9")

;; CEDT: required for ECB and speedbar
(load-file "~/.emacs.d/vendors/cedet-1.0pre7/common/cedet.el")
(global-ede-mode 1)                      ; Enable the Project management system
(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion
(global-srecode-minor-mode 1)            ; Enable template insertion menu

;; speedbar
(speedbar 1)
(global-set-key (kbd "<f6>") 'speedbar)

;; ECB
(require 'ecb)
(require 'ecb-autoloads)
;(setq  ecb-new-ecb-frame  1)
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(global-set-key (kbd "<f7>")   'ecb-activate)
(global-set-key (kbd "<C-f7>") 'ecb-deactivate)

;; highlight symbols http://nschum.de/src/emacs/highlight-symbol/
(add-to-list 'load-path "/path/to/highlight-symbol")
(require 'highlight-symbol)

(global-set-key (kbd "<f9>")   'highlight-symbol-at-point)
(global-set-key (kbd "<C-f9>") 'highlight-symbol-next)
(global-set-key (kbd "<S-f9>") 'highlight-symbol-prev)
(global-set-key (kbd "<M-f9>") 'highlight-symbol-remove-all)


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

;; Power Macros http://www.linuxjournal.com/article/3769?page=0,1
(require 'power-macros)
(power-macros-mode)
(pm-load)

;; http://github.com/nonsequitur/smex/
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
;(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(setq smex-save-file "~/.emacs.d/smex-items")

;; http://www.dr-qubit.org/emacs.php#undo-tree
;; hot damn.....
(require 'undo-tree)
(global-undo-tree-mode)

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
(global-set-key (kbd "<C-f12>") 'thing-copy-word)
(global-set-key (kbd "<M-f12>") 'thing-copy-line)
(global-set-key (kbd "<C-f11>") 'thing-copy-to-line-beginning)
(global-set-key (kbd "<M-f11>") 'thing-copy-to-line-end)
