;;{{{ Copyright

;; Copyright (C) 1999 Jesper Kjær Pedersen <blackie@ifad.dk>
;;
;; Author: Jesper Kjær Pedersen <blackie@ifad.dk>
;; Home page: http://www.imada.sdu.dk/~blackie/emacs/
;; Created: 25 May. 1999
;; This release: 25 Aug. 1999
;; Version 1.0
;; Keywords: macros

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;}}}
;;{{{ Commentary

;;
;; Keyboard Macros are a very powerful tool, if you know how to use them the
;; right way!  It is however a bit difficult in Emacs to define and
;; maintain several macros at a time. This problem is solved with this
;; package.
;;
;; When you have loaded this packages, simply press "C-c n" to assign the
;; latest defined keyboard macro to a key. Emacs will then ask you which
;; key you want to assign this macro to, and afterwards it will ask you for
;; a description of the macro.  If something is already bound to this key,
;; Emacs will ask you whether you want to override this binding.  The
;; description for all the defined macros may be obtained by pressing
;; "C-c m". Using this keystroke you can also manage you power-macros.

;; This package has been developed as part of my job on writing the book
;; "Sams Teach yourself Emacs in 24 hours".
;; For more information about this book, please go to the page
;; http://www.imada.sdu.dk/~blackie/emacs/

;;}}}
;;{{{ Installation description

;; To install this package, copy this file to a directory in your load-path
;; and insert the following into your .emacs file:
;; (require 'power-macros)
;; (power-macros-mode)
;; 
;; If you want your latest defined macros loaded when Emacs starts, 
;; then insert the following line into your .emacs file
;; (pm-load)
;;
;; By default two key bindings will be made, namely "C-c n" for defining a new
;; macro, and "C-c m" for managing macros. If you dislike these bindings,
;; you may disable them using the following commands (which must come after
;; the (require 'power-macros) line.
;;   (define-key power-macros-mode-map [(control c) (n)] nil)
;;   (define-key power-macros-mode-map [(control c) (m)] nil)
;; Later you may for example bind these functions to your function keys
;; with the following commands:
;;   (define-key power-macros-mode-map [(f2)] 'pm-define)
;;   (define-key power-macros-mode-map [(f3)] 'pm-manage-macros)

;;}}}
;;{{{ History Information

;; 0.1 First official release.
;; 0.1.1 Fixed a few spelling errors (thanks to
;;       camille.troillard@worldonline.fr)
;; 0.2 Made the package into a real minor mode, added a menu to the
;;     menu-bar, and made it customizable using customize
;; 0.2.1 Added support for XEmacs menus 
;;       thanks to Jan Vroonhof <vroonhof@math.ethz.ch>)
;;       and added a power-macros-hook.
;;       Fixed installation description
;; 0.3a  - Major rewrites.
;;       - Added support for binding keys either global, local, or for a
;;         given mode.
;;       - Added support for saving and loading macros in several files
;;       - Made the search for a possible collision with the key being
;;         bound
;;       - Made managing of macros very easy using a context sensitive
;;         buffer with the description of all the macros.
;; 0.4a  - Major rewrites (again). This time due to realizing that there
;;         is no such thing as a buffer local key map (which is used
;;         together with major-mode map, minor-modes maps, and the global
;;         map.
;;       - lots of code cleanup, this is the last release before a stable
;;         release is made.
;;       - make it work with XEmacs (again).
;; 1.0   - Minor bug-fixes - and of course first stable release

;;}}}
;;{{{ User definitions

(defgroup power-macros nil
  "Power Macros makes it easy for you to bind the latest defined keyboard macro to a key,
describe it and save it to a file"
  :group 'Convenience)

(defcustom power-macros-file "~/.power-macros"
  "File in which the macros are saved by default."
  :type 'file
  :group 'power-macros)

(defcustom power-macros-save-on-change t
  "If this variable is t, then the save function is invoked whenever a change is made"
  :type 'boolean
  :group 'power-macros)

(defcustom power-macros-indication " PM"
  "Indication used in the mode-line, when the power macros are enabled."
  :type 'string
  :group 'power-macros)

(defcustom power-macros-menu-name "PM"
  "The name of the menu-bar item in which the power macros items is located"
  :type 'string
  :group 'power-macros)

(defcustom power-macros-hook nil
  "Hook called when power macros start"
  :type 'hook 
  :group 'power-macros)

(defcustom power-macros-advice-end-kbd-macro nil
  "Should power macros be activated as soon as a macro has been defined?"
  :type 'boolean
  :group 'power-macros)

;;}}}

;;--------------------------------------------------
;;                   CODE
;;--------------------------------------------------

;;{{{ To-do and wish list from Emacs

; TO-DO:
; - It should be possible to select an entry in the managing buffer by
;   pressing enter on it descriptive text.
; - When moving a macro from one file to another when managing the macros,
;   it should be checked for possible collisions with a power macro in the
;   new file.
; - When the user ends edit a keyboard macro (with kbd-edit-kbdmacro) then
;   it should be checked if the power macros should be saved.
; - pm-set-mode-key should check for minor-modes too.
; - It should be possible not to show the menu item.

; Wish List from Emacs:
; - It would be nice if C-h c could list the description for the macro
; - It would be very useful if was possible to continue defining a keyboard
;   macro, which was not the latest defined one.

;;}}}

(require 'cl)
(require 'easymenu)
(require 'advice)

;;{{{ The minor mode function.

(message "Loading power-macros...")

(defvar power-macros-mode nil
  "When non-nil power macros is enabled")

(defvar power-macros-mode-map (make-keymap)
  "Local key map for the power macros.")

(defvar pm-macros '()
  "List of available macro defined with power-macros")

(defvar pm-available-modes '()
  "List of available modes - used for major-mode completion")

(defvar pm-warn-buffer nil
  "Internal variable used when generating the warning about possible
overriding of key bindings.")

;; Add an element to the mode line list
(or (assq 'power-macros-mode minor-mode-alist)
    (setq minor-mode-alist
          (cons `(power-macros-mode ,power-macros-indication) minor-mode-alist)))

;; Key bindings for the minor mode
(define-key power-macros-mode-map [(control c) (n)] 'pm-define)
(define-key power-macros-mode-map [(control c) (m)] 'pm-manage-macros)

;; Add information to the minor mode map.
(or (assq 'power-macros-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
          (cons `(power-macros-mode . ,power-macros-mode-map) minor-mode-map-alist)))

;; Advice for end-kbd-macro
(defadvice  end-kbd-macro (after pm-start-define dis)
  (pm-define))

(defun power-macros-mode (&optional arg)
  "\\<power-macros-mode-map>Defining keyboard macros is very easy, but binding them to keys, and
saving them to files  is much harder with Emacs in its default setup.

This problem is fixed with this minor-mode.
Simply press \\[pm-define] to bind the latest defined keyboard macro to a
key. To see a list of your defined macros and to edit these settings, press
\\[pm-manage-macros]. For more information press \\[pm-help-on-power-macros]"
  (interactive)

  (setq power-macros-mode         
        (if (null arg) (not power-macros-mode)
          (> (prefix-numeric-value arg) 0)))
  (if power-macros-mode
      (progn
        ;; Start the mode
        (if (boundp 'xemacs-logo) 
            (add-submenu nil pm-menu)
          (easy-menu-add pm-menu))
        (if power-macros-advice-end-kbd-macro
            (progn
              (ad-enable-advice 'end-kbd-macro 'after 'pm-start-define)
              (ad-activate 'end-kbd-macro)))
        (run-hooks 'power-macros-hook))
    (progn
      ;; Disable the mode
      (ad-disable-advice 'end-kbd-macro 'after 'pm-start-define)
      (ad-activate 'end-kbd-macro)
      (easy-menu-remove pm-menu))))

;;}}}
;;{{{ Menu binding for the minor mode.

(easy-menu-define
 pm-menu
 power-macros-mode-map
 "power macros menu"
 (list 
  power-macros-menu-name
  ["Bind Macro"     pm-define  t]
  ["Manage Macros" pm-manage-macros t]
  "-----"
  ["Load Macros" pm-load t]
  ["Save Macros" pm-save t]
  ["About Power Macros" pm-help-on-power-macros t]
  "-----"
  "-----"
  ["Start Recording a Macro" start-kbd-macro t]
  ["End Recording a  Macro" end-kbd-macro t]
  ["Execute the Latest Defined Macro" call-last-kbd-macro last-kbd-macro]
  "-----"
  ["Pause Macro at This Point" kbd-macro-query defining-kbd-macro]
  ["Enter Recursive Editing" pm-enter-recursive-edit defining-kbd-macro]
  ))

;;}}}
;;{{{ Definition of macros

(defun pm-define ()
  "Bind the latest defined keyboard macro to a key, and give it a description"
  (interactive)

  ;; Test if a macro exists
  (or last-kbd-macro
      (error "No keyboard macro has been defined yet. Define one with C-x ("))

  (let* ((key (read-key-sequence "Bind last keyboard macro to which key? "))
         (mode (pm-ask-for-mode "How should it be defined?" t))
         (file (pm-read-file-name 
                (concat "File to save macro to (default " power-macros-file "): ")
                nil power-macros-file t))
         (existing-name (pm-get-macros key mode file))
         (macro-name (if existing-name existing-name (pm-new-macro-name))))
          
    ;; Verify if it is ok to bind to the key given.
    (if (catch 'pm-stop-def
          (pm-possible-override t key mode file)
          t)

        ;; Bind and describe the key.
        (pm-describe key mode file macro-name)
      (message "key binding discarded!"))))


(defun pm-describe (key mode file macro-name)
  "This function sets up a buffer for describing a keyboard macro."
  (let* ((desc (pm-get-desc key mode file))
         (buffer (current-buffer)))
    (switch-to-buffer (pm-create-buffer))
    (text-mode)
    (use-local-map (make-keymap))
    (local-set-key [(control c) (control c)] 
                   `(lambda () (interactive) 
                      (pm-end-description ,key ',mode ,file ',macro-name)))
    (insert "Type a short description of the macro below this line, and type C-c C-c\n")
    (insert "-----------------------------------------------------------------------\n")
    (if desc
        (insert desc))))

(defun pm-end-description (key mode file macro-name)
  "This functions sets the description for the macro with the name defined by the input
variable nm."
  (beginning-of-buffer)
  (if (re-search-forward "-------------- *$" nil t)
      (forward-char))

  (pm-set-info mode key (buffer-substring (point) (point-max)) 
               file macro-name)
  (kill-buffer (current-buffer))
  (name-last-kbd-macro macro-name)
  (pm-set-key macro-name)
  (pm-maybe-save)
  (message "Assigned macro to %s" (key-description key)))


(defun pm-ask-for-mode (question offer-current-major-mode)
  "This is an auxiliary function used to ask the user which type of binding 
we are talking about."
  (let ((map (make-keymap))
        tp)
    (suppress-keymap map t)
    (define-key map [(g)] 'self-insert-and-exit)
    (define-key map [(m)]  'self-insert-and-exit)
    (define-key map [(control g)]  'abort-recursive-edit)
    (define-key map [(??)]  'pm-help-on-bindings)
    (if offer-current-major-mode
        (define-key map [(c)]  'self-insert-and-exit))
    (define-key map [(control h)]  'pm-help-on-bindings)

    ;; Ask the user for the type of the binding
    (setq tp 
          (read-from-minibuffer 
           (concat question " (g/m/?)") 
           "" map))
    (if (equal tp "g") 
        'global

        ;; Ask for a mode name
        (let ((mode (completing-read 
                     (concat "Mode name "
                             (if offer-current-major-mode
                                 (concat "(default: " (symbol-name major-mode) ") "))
                             ": ")
                     (pm-available-modes))))
          (if (not (string= mode ""))
              (intern mode)
            (if offer-current-major-mode
                major-mode
              (error "No mode name given!")))))))

(defun pm-new-macro-name ()
  "Generates a new name for a macro."
  (let ((num 1)
        (name 'pm-macro-1))
    (while (fboundp name)
      (setq num (+ num 1))
      (setq name (intern (format "pm-macro-%s" num))))
    name))

;;}}}
;;{{{ Help descriptions

(defun pm-help-on-bindings ()
  "This functions is called when you press '?' when emacs asks you where to bind your macro"
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ "The macro you are currently binding may be made available 
in several different places. You have the following options:
g - The macro will be global for every major mode. 
    That is, it will be accessible everywhere.
m - The macro will be made available only for the given major mode. 
    This might be very useful in case you have a macro, which does only have a meaning
    for say c-mode or emacs-lisp-mode.")
  (save-excursion
    (set-buffer standard-output)
    (help-mode))))

(defun pm-help-on-filename (create)
  "This function is invoked when the user press '?' when emacs asks you for the file 
to save the just defined macro in."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ "Here you must type the filename in which the macro should be saved.\n")
    (if create
        (princ (concat 
                "If you just press enter, then the macro will be saved to the standard\nmacro file: " 
                power-macros-file ".\n")))
    (princ "If you erase the filename - that is enter an empty filename, 
then the macro will not be saved. The names of all your macro-files
are available by pressing either the up or down arrows")
    (save-excursion
      (set-buffer standard-output)
      (help-mode))))


(defun pm-help-on-move-outsite ()
  "This functions is called when you press '?' when emacs tells you that
you have moved the cursor outside the description field."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ "Before you continue editing other macros, you must first end
editing the current description. Normally you end this by pressing enter
(for submit) or control-g (for abort) in the description field. Now,
however, you have typed a key outside the description area. Does this mean
that you are finished editing the current description?

You now have the following choices:
s - Submit current description. That is change the description to the new text
a - Abort edits. That is go back to the description as it was before the
    current edits.
c - Continue editing the description.")
    (save-excursion
      (set-buffer standard-output)
      (help-mode))))


(defun pm-help-on-edit-or-copy ()
  "This function is called when the user press '?' when emacs asks whether
he wants to copy-and-edit the field or just edit it."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ "Emacs asks what you want to do, you have two possibilities:
copy-and-edit - This means that you copy the macro and edit its
                properties. Thus the macro will still be located 
                on the original key, with the original type in the 
                original file, but in addition it will also be copied 
                to the new location you are about to specify.
edit - Just edit the macros properties.")
    (save-excursion
      (set-buffer standard-output)
      (help-mode))))

(defun pm-help-on-power-macros ()
  "This function describes macros and power-macros"
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ (substitute-command-keys "Macros and Power Macros.
========================
Power Macros makes it very easy for you to define and manage several
keyboard macros at a time. If you know everything about defining keyboard
macros, then you may skip to the section labeled \"Power Macros\",
otherwise, here comes a description on macros.

What is a Macro?
================
A macro is a recorded sequence of keystrokes, which you can get executed
over and over again. With macros you may get Emacs to do all you trivial and
monotonic work. 

Conceptually there is two different kind of macros: Those which you use one
time now and then, and those which you use a 100 times in a row and then
never again.

The second kind is often used to translate a number of lines, where the
macro you record does the translation on one of the lines, and as the last
thing, moves to the next line. 

The first kind of macros is used to do a task, which you may do several
times a day, or even several times each minutes. Examples of this may
include the following:
 - Insert the text \"Dear Mother<newline>Everything is well here<newline>
   send more money\"
 - Make a mark at your current location in the buffer, search forward for
   some text, copy the text found to your original location, and return to
   this place.
 - Insert a template for a for-loop

Power Macros will help you with the first kind of macros, that is those
which you record once, and the reuse once in a while.


Recording Macros.
=================
You start recording a macro by pressing \"C-x (\" That is press control
hold it down press the letter x, release both, then press the opening
parenthesis.

Every key you press from now on, will be part of the macro. When you are
done, press \"C-x )\" to end recording the macro.

To make Emacs type the keys, just recorded, for you press \"C-x e\". You
will now see that Emacs behaves just as if you pressed the keys once again.

When you define macros you are allowed to do whatever you want - that is
kill buffers, switch buffers, insert text, invoke commands, etc.

Power Macros
============
It's very easy with Emacs to record a macro, and execute it once again. It
is, however, much harder to have several macros defined at a time, to bind
a macro to a key on the keyboard, and finally to save the macros to
files. (This is of course only true, if you do not use power macros!)

As you might guess power macros makes it much easier to bind the macros to
keys, and to save them to files for use in later Emacs sessions.

When you have defined a macro as described above, simply press
'\\[pm-define]', and Emacs will ask you for the following things:

The key to bind the macro to
----------------------------
  This is simply the key sequence on which you want the macro located.

The type
--------
  The macro may be defined in several different ways:
  global - That is, the macro is available on the given key in every
           buffer.
  mode   - This way the macro is only available in the buffers with the given
           major or minor mode

File to save to
---------------
  This is the name of the file in which you want to define this macro. By
  default you should use the file it suggest.
  You may, however, want to save to a different file, if the macro does
  only apply in certain situations. In the given situations you may then
  load the macro using the command 'pm-load'.

Description
-----------
  Finally you must give your new macro a description. This makes it easier
  for you in the feature to see which macros you have defined, and what
  their purpose it.


Managing Macros
===============
To manage the macros you have defined with power-macros press '\\[pm-manage-macros]'. 
This will bring up a buffer in which you can see all your macros. Here you
may do the following things:

- Copy a macro
- Move a macro from one key to another
- Change the type of the macro (e.g. from global to local to a given 
  major mode)
- Move a macro to another file
- Change the description of a macro.

"))
    (save-excursion
      (set-buffer standard-output)
      (help-mode))))

;;}}}
;;{{{ Managing buffer

;;{{{ Building the buffer.

(defun pm-manage-macros ()
  "This function brings up a managing buffer for power-macros."
  (interactive)
  
  ;; Switch to the buffer and insert the header
  (switch-to-buffer (pm-create-buffer))
  (pm-macro-manage-mode)
  (use-local-map pm-normal-edit-map)

  (insert (substitute-command-keys 
"Descriptions of macros defined with power macros.

In this buffer you have the following possibilities:
- Press enter or the left mouse button on one of the fields to edit them.
  When you are done editing the key, type or file field, you are asked if
  your edits should form a new macro, or just update the existing one.
- Move from field to field with the tabulator key.
- Move from macro to macro by pressing page up / page down.
- Mark a macro for deletion by pressing 'd'.
- Remove the deletion mark by pressing 'u'.
- Delete macros marked for deletion by pressing 'x'.
- Sort the entries by pressing either 's' or 'S' on top of the field you wish to use
  for sorting.
- To edit one of the macros, simply press 'e' on top of the macro.
- Press C-c C-c or \\[kill-buffer] to kill the buffer.

"))

  (let (key desc file macro-name mode oldp tuple-start)
    (setq pm-token-list '())
    (setq pm-tuple-list '())
    (pm-sort)
    (dolist (macro pm-macros)
     (setq  key (get macro 'key)
            desc (get macro 'documentation)
            file (get macro 'file)
            mode (get macro 'mode)
            )

      ;;---------- Separator
      (setq tuple-physical-start (point))
      (insert "--------------------------------------------------\n")
      (setq tuple-logical-start (point))

      ;; Insert a number of spaces equal to the length of the delete text, to avoid
      ;; that all the other fields moves, when a tuple is marked for deletion.
      (insert (make-string (length pm-del-text) ?\ ) "\n")

      ;;---------- Key
      (insert "Key : ")
      (setq oldp (point))
      (insert (key-description key) "\n")
      (push (list 'key oldp (point)) pm-token-list)
      (add-text-properties oldp (point) '(mouse-face highlight))

      ;; ---------- Type
      (insert "Type: ")
      (setq oldp (point))
      (insert (if (eq mode 'global) "global"
                (concat "mode - " (symbol-name mode)))
              "\n")
      (push (list 'type oldp (point)) pm-token-list)
      (add-text-properties oldp (point) '(mouse-face highlight))

      ;; ---------- File
      (insert "File: ")
      (setq oldp (point))
      (insert file "\n")
      (push (list 'file oldp (point)) pm-token-list)
      (add-text-properties oldp (point) '(mouse-face highlight))

      ;; ---------- Description
      (insert "Description: ")
      (setq oldp (point))
      (if (string= desc "")
          (insert "No description")
        (insert desc))
      (add-text-properties oldp (point) '(left-margin 3))
      (fill-region oldp (point))
      (insert "\n")
      (push (list 'desc oldp (point)) pm-token-list)
      (add-text-properties oldp (point) '(mouse-face highlight))

      ;;---------- End of tuple
      (insert "\n\n")
      (push (list tuple-physical-start tuple-logical-start (point) macro) pm-tuple-list))
      

    
    (setq pm-token-list (nreverse pm-token-list))
    (setq pm-tuple-list (nreverse pm-tuple-list))
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    ))

;;}}}
;;{{{ Major mode definition

(define-derived-mode pm-macro-manage-mode fundamental-mode "Macro Manage" 
  "Mode for managing power macros"

  ;; ---------- Normal edit map
  (defvar pm-normal-edit-map (make-keymap)
    "This key map is used when not editing the description field")
  (define-key pm-normal-edit-map [(tab)] 'pm-next-field)
  (define-key pm-normal-edit-map [(meta tab)] 'pm-prev-field)
  (define-key pm-normal-edit-map [(next)] 'pm-next-macro)
  (define-key pm-normal-edit-map [(prior)] 'pm-prev-macro)
  (define-key pm-normal-edit-map [(control c) (control c)] 
    (lambda () (interactive) (kill-buffer (current-buffer))))
  (define-key pm-normal-edit-map [(return)] 'pm-edit-field-return)
  (if (boundp 'xemacs-logo)
      (define-key pm-normal-edit-map [(button1)] 'pm-edit-field-mouse)
    (define-key pm-normal-edit-map [(mouse-1)] 'pm-edit-field-mouse))
  (define-key pm-normal-edit-map [(d)] 'pm-set-delete-mark)
  (define-key pm-normal-edit-map [(u)] 'pm-unset-delete-mark)
  (define-key pm-normal-edit-map [(x)] 'pm-execute-deletion)
  (define-key pm-normal-edit-map [(e)] 'pm-edit-kbd-macro)
  (define-key pm-normal-edit-map [(s)] (lambda () (interactive) (pm-set-sort-func 'string<)))
  (define-key pm-normal-edit-map [(S)] (lambda () (interactive) (pm-set-sort-func 'string>)))

  ;; ---------- Description edit map
  ;; Ordinary movement functions should still exists in the rest of the buffer
  ;; when editing the description field. Therefore only inserting commands are 
  ;; disabled. This does however not disable commands like delete, return and backspace.
  ;; Therefore the rest of the buffer is also made read-only.

  (defvar pm-desc-edit-map (make-keymap)
    "This key map is used when editing the description field")
  (suppress-keymap pm-desc-edit-map)
  (substitute-key-definition 'undefined 'pm-move-outside pm-desc-edit-map)
  (define-key pm-desc-edit-map [(return)] 'pm-move-outside)
  (define-key pm-desc-edit-map [(delete)] 'pm-move-outside)
  (define-key pm-desc-edit-map [(backspace)] 'pm-move-outside)
  (define-key pm-desc-edit-map [(control k)] 'pm-move-outside)
  (define-key pm-desc-edit-map [(control w)] 'pm-move-outside)
  (define-key pm-desc-edit-map [(control g)] 
    (lambda () (interactive) (pm-end-edit-or-abort-desc-field 'abort)))
  
  (defvar pm-token-list nil
    "List of all the locations where info may be changed in the manage buffer")
  (defvar pm-tuple-list nil
    "List of (physical start point, logical start point, end point, macro name)
of each of the tuple in the power-macro buffer")
  (defvar pm-start-point nil
    "Location of the start of the description field. Used when the description is changed")
  (defvar pm-current-end-point nil
    "Location of the end of the description field. Used when the description is changed")
  (defvar pm-old-end-point nil
    "Location of the original end of the description field. 
Used when the description is changed")
  (defvar pm-old-desc nil
    "This variable contain the old description, when the description field is edited
in the manage buffer.")
  (defvar pm-current-macro nil
    "The macro for which the description field is currently being edited.")
  (defvar pm-del-text "Marked for deletion"
    "Text used to indicate that a record has been marked for deletion")
  (defvar pm-sort-function 'pm-sort-file
    "This is the latest function used to sort the content of the management buffer")
 )

;;}}}
;;{{{ Movement functions

(defun pm-next-field ()
  "Jumps to the next field in the \"Macro Manage\" buffer"
  (interactive)
  (pm-jump (lambda (x y) (< x y)) pm-token-list))

(defun pm-prev-field ()
  "Jumps to the previous field in the \"Macro Manage\" buffer"
  (interactive)
  (pm-jump (lambda (x y) (> x y)) (reverse pm-token-list)))

(defun pm-next-macro ()
  "Jumps to the key field of the next macro in the \"Macro Manage\" buffer"
  (interactive)
  (pm-jump (lambda (x y) (< x y)) pm-token-list 'key)  
)

(defun pm-prev-macro ()
  "Jumps to the key field of the previous macro in the \"Macro Manage\" buffer"
  (interactive)
  (pm-jump (lambda (x y) (> x y)) (reverse pm-token-list) 'key)  
)

(defun pm-jump (dirp l &optional type)
  "Jumps to the next or previous field. If the optional type is given, then
the field must be of the given type"
  (let ((p (point))
        (found nil)
        (head (car l)))
    (while l
      (if (and (funcall dirp p (cadar l)) (or (not type) (eq type (caar l))))
          (progn 
            (goto-char (cadar l))
            (setq l '())
            (setq found t))
        (setq l (cdr l)))
      (if (not found)
          (goto-char (cadr head))))))

;;}}}
;;{{{ Editing functions

(defun pm-edit-field-return ()
  "See pm-edit-field"
  (interactive)
  (pm-edit-field nil))

(defun pm-edit-field-mouse (event)
  "See pm-edit-field"
  (interactive "e")
  (if (boundp 'xemacs-logo)
      (goto-char (event-closest-point event)))
  (pm-edit-field t))

(defun pm-edit-field (is-mouse)
  "This function takes care of calling the editing functions
when enter is pressed in the managing buffer."
  (let* ((elm (pm-get-context))
        (widget-type (car elm))
        (start (cadr elm))
        (end (caddr elm))
        (macro (if (not (eq widget-type 'text))
                   (pm-get-macro-at-point)
                 nil))
        (map (make-keymap))
        (key (get macro 'key))
        (file (get macro 'file))
        (mode (get macro 'mode))
        (desc (get macro 'documentation))
        (do-reload nil)
        answer tuple new-name)

    (suppress-keymap map)
    (define-key map [(c)] 'self-insert-and-exit)
    (define-key map [(e)] 'self-insert-and-exit)
    (define-key map [(control g)] 'abort-recursive-edit)
    (define-key map [(??)] 'pm-help-on-edit-or-copy)
    (define-key map [(control h)] 'pm-help-on-edit-or-copy)

    (if (eq widget-type 'text)
        (if (not is-mouse)
            (beep))
      (if (eq widget-type 'desc)
          (pm-edit-desc start end macro)
        (progn
          ;; Ask if the macro should be copied or just edited.
          (setq answer 
                (read-from-minibuffer 
                 "Do you want to copy-and-edit the macro or just edit it? (c/e/?) " "" map))

          ;; Call the specific editing functions.
          (if (eq widget-type 'key)
              (setq key (pm-edit-key start end mode file))
            (if (eq widget-type 'type)
                (setq mode (pm-edit-type start end key file))
              (setq file (pm-edit-file start end))))
        
          ;; Now delete the original macro if there is one on the given key.
          (let ((existing-macro (pm-get-macros key mode file)))
            (if existing-macro
                (progn
                  (pm-delete-macro existing-macro)
                  (setq do-reload t))))

          ;; update the key bindings
          (if (equal answer "c")
              (progn
                ;; Copy the macro and insert the copy into the list of macros
                (setq other-macro (pm-new-macro-name))
                (fset other-macro (symbol-function macro))
                (setplist other-macro (copy-sequence (symbol-plist macro)))
                (push other-macro pm-macros)
                (setq macro other-macro) ; this makes the code below common for both paths
                (setq do-reload t)
                )
            ;; Edit the key
            (pm-unset-key macro)
            )
          
          ;; set the new value for the new macro
          (put macro 'file file)
          (put macro 'key key)
          (put macro 'mode mode)
          (put macro 'documentation desc)

          ;; define the binding for the new key.
          (pm-set-key macro)

          ;; Update the buffer if type is copy
          (if do-reload
              (pm-manage-macros))))) ;; Just like windows: Lets restart, thats the easiest!
    (set-buffer-modified-p nil)))

;;}}}
;;{{{ Specific editing functions

(defun pm-edit-key (start end mode file)
  "This function is invoked when enter is pressed on the key field of the managing buffer"
  (let* ((key (read-key-sequence "Which key: "))
         (key-descs (key-description key)))

    ;; Check if the new binding is ok.
    (pm-possible-override nil key mode file)

    (pm-update-manage-buffer start end key-descs)
    key ; return value.
    ))


(defun pm-edit-type (start end key file)
  "This function is invoked when enter is pressed on the type field of the managing buffer"
  (let ((mode (pm-ask-for-mode "New type: " nil))
        text)
    (if (not (eq mode 'global))
             (setq text (format "mode - %s" (symbol-name mode)))
      (setq text (symbol-name mode)))

    ;; Check if the new binding is ok.
    (pm-possible-override nil key mode file)


    (pm-update-manage-buffer start end text)
    mode ; return value
    ))

(defun pm-edit-file (start end)
  "This function is invoked when enter is pressed on the file field 
of the managing buffer"
  (let* ((old-name (buffer-substring start end))
         (file (pm-read-file-name "New file name: " (file-name-directory old-name) nil nil)))
    (pm-update-manage-buffer start end file)
    file))

(defun pm-read-file-name (prompt dir default create)
  "This functions reads a filename from the mini buffer. Furthermore it
checks if the selected filename is a directory."
  (let ((file-name-history (pm-fetch-active-files))
         (minibuffer-local-completion-map (copy-keymap minibuffer-local-completion-map)))
    (define-key minibuffer-local-completion-map [(control h)] 
      `(lambda () (interactive) (pm-help-on-filename create)))
    (let* ((f (read-file-name prompt dir default))
           (file (if (string= f "") f (expand-file-name f))))
      (if (and file (file-directory-p file))
          (error "Filename is a name of a directory")
        file))))
         


(defun pm-edit-desc (start end macro)
  "This function is invoked when enter is pressed on the description field 
of the managing buffer"

  ;; Make the desc part writable
  (setq buffer-read-only nil)

  ;; Go to start of region and record information about the text
  (goto-char start)
  (setq pm-start-point (point))
  (setq pm-current-end-point (copy-marker end))
  (setq pm-old-end-point end)
  (setq pm-old-desc (buffer-substring start (- end 1)))
  (setq pm-current-macro macro)

  ;; Change the bindings.
  (use-local-map pm-desc-edit-map)
  (let ((keym (copy-keymap global-map)))
    (define-key keym [(control c) (control c)] 
      (lambda () (interactive) (pm-end-edit-or-abort-desc-field 'end)))
    (define-key keym [(control g)] 
      (lambda () (interactive) (pm-end-edit-or-abort-desc-field 'abort)))
    (if (boundp 'xemacs-logo)
        (add-text-properties start end `(keymap ,keym))
      (add-text-properties start end `(local-map ,keym))))

  ;; make it impossible to insert at the beginning of the buffer.
  (add-text-properties 1 2 '(front-sticky (read-only)))
  
  ;; Make it possible to insert at the first point in the description area.
  (add-text-properties (- start 1) start '(rear-nonsticky (face read-only)))

  ;; Now make everything else than the description area read-only.
  ;; This is to ensure that the user has not bound any strange key, which
  ;; is not disabled by the new key map.
  (make-face 'pm-face)
  (set-face-foreground 'pm-face "grey")
  (add-text-properties (point-min) start '(read-only t face pm-face))
  (add-text-properties (- end 1) (point-max) '(read-only t face pm-face))
  (message "Press C-c C-c to submit changes or C-g to abort")
)

(defun pm-end-edit-or-abort-desc-field (type)
  "This functions is called to end editing the description field of the \"Manage Macro\" buffer"
  (setq inhibit-read-only t)
  (remove-text-properties (point-min) (point-max) '(read-only t local-map t keymap t face t))
  (setq inhibit-read-only nil)

  (if (eq type 'abort)
      (progn
        (delete-region pm-start-point pm-current-end-point)
        (goto-char pm-start-point)
        (insert pm-old-desc "\n"))
    (progn
      (pm-update-info-lists pm-start-point (- pm-current-end-point
                                              pm-old-end-point))
      (set-text-properties pm-start-point (point) '(mouse-face highlight))
      (put pm-current-macro 'documentation 
           (buffer-substring-no-properties pm-start-point pm-current-end-point))))
  (setq pm-current-end-point nil)
  (use-local-map pm-normal-edit-map)
  (setq buffer-read-only t))

(defun pm-move-outside (&rest x)
  "This function is called when the user press a key outside the description field,
when he is supposed to edit the description field."
  (interactive)
  (message "%s,%s,%s" (point) pm-start-point pm-current-end-point)
  (let ((map (make-keymap))
        answer)
    (suppress-keymap map)
    (define-key map [(s)] 'self-insert-and-exit)
    (define-key map [(a)] 'self-insert-and-exit)
    (define-key map [(c)] 'self-insert-and-exit)
    (define-key map [(??)] 'pm-help-on-move-outsite)
    (define-key map [(control g)] 'self-insert-and-exit)
    (setq answer
          (read-from-minibuffer "You have moved outside the description area, what to do? (s/a/c/?) " nil map))
    (if (equal answer "s")
        (pm-end-edit-or-abort-desc-field 'end)
      (if (equal answer "a")
          (pm-end-edit-or-abort-desc-field 'abort)
        (progn
          ;; c or control g has been pressed.
          (goto-char pm-start-point)
          (message "Please continue editing the current description. End with either Enter or C-g")
          )))))

;;}}}
;;{{{ Info-list management

(defun pm-get-context ()
  (let ((p (point))
        (l pm-token-list)
        (last-end (point-min))
        type start end res) 
    (if (null l)
        (list 'text (point-min) (point-max))
      (if (< p (cadr (car l)))
          (list 'text (point-min) (cadr (car l)))
        (progn
          (while l
            (setq elm (car l)
                  l (cdr l)
                  type (car elm)
                  start (cadr elm)
                  end (caddr elm))
            (if (< p start)
                (setq res (list 'text start end))
              (if (< p end)
                  (setq res elm)))
            (if (not (null res))
                (setq l '()))
            (setq last-end end))
          
          (if (null res)
              (list 'text last-end (point-max))
            res))))))


(defun pm-update-info-lists (start incr)
  "This functions updates the token-list and the tuple-list."

  ;; update the token list
  (let ((res '())
        tuple-type tuple-start tuple-end)
    (dolist (elm (reverse pm-token-list))
      (setq tuple-type (car elm)
           tuple-start (cadr elm)
           tuple-end (caddr elm))
      (if (> tuple-start start)
          (push (list tuple-type (+ tuple-start incr) (+ tuple-end incr)) res)
        (if (= tuple-start start)
            (push (list tuple-type tuple-start (+ tuple-end incr)) res)
          (push elm res))))
    (setq pm-token-list res))

  ;; update the tuple list
  (let ((res '())
        p-start l-start end name)
    (dolist (elm (reverse pm-tuple-list))
      (setq p-start (car elm)
            l-start (cadr elm)
            end (caddr elm)
            name (cadddr elm))
      (if (> p-start start)
          (push (list (+ p-start incr) (+ l-start incr) (+ end incr) name) res)
        (push elm res)))
    (setq pm-tuple-list res)))


(defun pm-update-manage-buffer (start end text)
  "This function inserts text in place of the content from start to end in
the manage buffer. Furthermore it also makes the text highlight when mouse
is over the appropriate region"

  (setq buffer-read-only nil)
  
  (delete-region start end)
  (goto-char start)
  (insert text "\n")
  
  (add-text-properties start (+ start (length text))
                       '(mouse-face highlight))
  (setq buffer-read-only t)

  ;; Now update the info lists.
  (pm-update-info-lists start (+ 1 (- (length text) (- end start))))
)
    
(defun pm-delete-tuple-in-token-list (start end)
  "This function removes the info from the token-list between start and end"
  (let ((l pm-token-list) 
        elm t-start t-end)
    (setq pm-token-list '())
    (while l
      (setq elm (car l)
            l (cdr l)
            t-start (cadr elm)
            t-end (caddr elm))
      (if (not (and (>= t-start start) (<= t-end end)))
          (push elm pm-token-list)))
    (setq pm-token-list (nreverse pm-token-list))))

(defun pm-get-delete-pos ()
  "This function returns the position where the \"delete\" text may be inserted
for the tuple at point."
  (pm-get-tuple-info 'cadar))

(defun pm-get-macro-at-point ()
  "This function returns the name of the macro at point"
  (pm-get-tuple-info 'cadddar))

(defun pm-get-tuple-info (f)
  "This function returns information from a tuple. The actual information
is determined by the function f given as argument to the function."
  (let ((p (point))
        (l pm-tuple-list)
        (res nil))
    (if (null l)
        (error "No macros exists!")
      (if (< p (cadar l))
          (error "Not located on a macro!"))
      (while l
        (if (< p (caddar l))
            (setq res (funcall f l)
                  l '())
          (setq l (cdr l))))
      (if (eq res nil)
          (error "Not located on a macro!"))
      res)))

;;}}}
;;{{{ Deletion

(defun pm-set-delete-mark ()
  "Mark the current macro in the \"Macro Manage\" buffer for deletion"
  (interactive)
  (pm-set-or-unset-delete-mark 'set))

(defun pm-unset-delete-mark ()
  "Remove the deletion mark for the current macro in \"Macro Manage\" buffer"
  (interactive)
  (pm-set-or-unset-delete-mark 'unset))

(defun pm-set-or-unset-delete-mark (type)
  "Set or unsets the deletion mark in the \"Macro Manage\" buffer"
  (save-excursion
    (let ((pos (pm-get-delete-pos)))
      (setq buffer-read-only nil)
      (goto-char pos)
      (delete-region pos (+ pos (length pm-del-text)))
      (if (eq type 'set)
          (progn
            (insert pm-del-text)
            (set-text-properties pos (point) '(face modeline)))
        (insert (make-string (length pm-del-text) ?\ )))
      (setq buffer-read-only t)
      (set-buffer-modified-p nil))))


(defun pm-execute-deletion ()
  "Delete the macros marked for deletion in \"Macro Manage\" buffer"
  (interactive)
  (setq buffer-read-only nil)
  (save-excursion
    (let ((new-list '())
          elm p-start l-start end name)
    
      (while pm-tuple-list
        (setq elm (car pm-tuple-list)
              pm-tuple-list (cdr pm-tuple-list)
              p-start (car elm)
              l-start (cadr elm)
              end (caddr elm)
              name (cadddr elm))
        (goto-char l-start)
        (if (looking-at pm-del-text)
            (progn
              (delete-region p-start end)
              (pm-delete-tuple-in-token-list p-start end)
              (pm-update-info-lists p-start (- p-start end))
              (pm-delete-macro name))
          (push elm new-list)))
      (setq pm-tuple-list (nreverse new-list))))
  (setq buffer-read-only t)
  (set-buffer-modified-p nil)
  (pm-maybe-save))

(defun pm-delete-macro (macro)
  "This functions delete the macro named macro, and remove its
key-bindings"
  (pm-unset-key macro)
  (unintern macro)
  (setq pm-macros (delete macro pm-macros)))

    ;;}}}
;;{{{ Edit macro definition

; The tricky part with the function below is that edit-kbd-macro does not
; support a back-end to enter the edit buffer. Thus I need to go trough the
; front-end, which requires that I answer a question in the
; mini-buffer. Luckily this can be solved with a macro ;-)

(defun pm-edit-kbd-macro ()
  "This function edits the keyboard macro under point in the managing buffer"
  (interactive)
  (let* ((macro (pm-get-macro-at-point))
         (macro-str (symbol-name macro))
         (new-mac (vconcat [?\M-x] "edit-kbd-macro" [return ?\M-x]
                           macro-str [return])))

    ;; I need to switch buffer to avoid that d,e,x have the meaning
    ;; pm-delete-macro, pm-edit-kbd-macro etc. 
    (switch-to-buffer (get-buffer-create "*scratch*"))
      
    (execute-kbd-macro new-mac)
    ;; Any idea how I can get pm-maybe-save executed when the user finish
    ;; editing the macro? The hook doesn't work when I enter the editing
    ;; the above way.
    ))

;;}}}
;;{{{ Sorting

(defun pm-sort ()
  (setq pm-macros (stable-sort pm-macros pm-sort-function)))

(defun pm-set-sort-func (direction)
  (let* ((elm (pm-get-context))
        (type (car elm)))

    (if (eq type 'text)
        (error "You must press 's' on top of a field")
      (fset pm-sort-function
            `(lambda (x y) (,direction (format "%s" (get x ',type))
                                       (format "%s" (get y ',type))))))
    (pm-manage-macros)))

(defun string> (x y) (not (string< x y)))

(defun pm-sort-file (x y)
  (string< (get x 'file) (get y 'file)))

;;}}}

;;}}}
;;{{{ Saving

(defun pm-maybe-save ()
  "This function is invoked whenever a change is made."
  (if power-macros-save-on-change
      (pm-save)))


(defun pm-save ()
  "Saves the power macros to files"
  (interactive)
  (let (macros buf)
    (dolist (file (pm-fetch-active-files))
      (setq macros (pm-select `(lambda (name) (equal (get name 'file) ,file)) pm-macros))
      (setq buf (create-file-buffer file))
      (princ ";;  Definitions for the power-macros - must be read with pm-read\n\n" buf)
      
      (dolist (macro macros)
        (pm-write-tuple-to-buffer buf file macro))
      
      
      (set-buffer buf) ; needed for write-file
      (write-file file)
      (kill-buffer buf))))

(defun pm-write-tuple-to-buffer (buf file macro)
  "This functions prints out 'macro' to 'file'"
  (let* ((mode (get macro 'mode))
        (key (get macro 'key))
        (desc (get macro 'documentation))
        (file (get macro 'file))
        )
    
    ;; print header
    (princ "\n\n;--------------- " buf)
    (princ key buf)
    (princ " - " buf)
    (princ (if (eq mode 'global)
               "global definition"
             (concat "specific for " (symbol-name mode))) buf)
    (princ " ---------------\n" buf)

    (princ "(pm-def-macro\n\t'" buf)
    (prin1 mode buf) (princ " ;; mode\n\t" buf)
    (prin1 key buf) (princ " ;; key\n\t" buf)
    (prin1 desc buf) (princ " ;; description\n\t" buf)
    (prin1 (format-kbd-macro macro) buf)
    (princ " ;; The macro\n\t" buf)
    (princ ")\n" buf)
))

;;}}}
;;{{{ Loading

(defun pm-load (&optional file)
  "Load macros from the file name given as the first argument. If no file
name is given and the function is not called interactively, then the macros
are loaded from the file named by the variable power-macros-file. Otherwise
the user is asked for a file-name"
  (interactive)
  (let* ((file-name (if (interactive-p)
                        (read-file-name "Which file to load from? " nil power-macros-file)
                      (if (null file)
                          power-macros-file
                        file))))
    (if (file-exists-p file-name)
        (progn
          (setq pm-current-file (expand-file-name file-name))
          (load-file file-name))
      (message "File '%s' does not exist" file-name))))

(defun pm-def-macro (mode key desc macro-def)
  (let* ((existing-name (pm-get-macros key mode pm-current-file))
         (macro-name (if (null existing-name)
                         (pm-new-macro-name)
                       existing-name)))
    ;; Define the macro
    (fset macro-name (read-kbd-macro macro-def))
    
    ;; Set the info about the macro
    (pm-set-info mode key desc file macro-name)

    ;; Bind the key
    (pm-set-key macro-name)))

;;}}}
;;{{{ Database Management

;; These functions takes care of managing the key bindings, descriptions and file information
;; The data is located as properties in the symbol defining the macro. The
;; variable called 'pm-macros is a list of symbol names for each of these macros.
;;
;; The following elements exists:
;;     key           - The key for which this macro is bound
;;     documentation - The description of this macro
;;     file          - The file in which this macro is saved
;;     mode          - The name of the major mode for mode specific macros
;;                     or the symbol global.

(defun pm-get-macros (key mode file &optional all)
  "This function returns the symbol containing the macro bound
to 'key' for mode 'mode' for file 'file'.
Any of the field might be nil, which means that they are wildcards. If the
optional variable 'all' is t, then a list of matches is returned."
  (let ((list pm-macros)
        (res '())
        macro)
    (while list
      (setq macro (car list)
            list (cdr list))
      (if (and (or (null key) (equal (get macro 'key) key))
               (or (null mode) (eq (get macro 'mode) mode))
               (or (null file) (equal (get macro 'file) file)))
          (push macro res)))

    (if all
        res
      (if (> (length res) 1)
          (error "Internal error: More that one tuple matched the query")
        (car res)))))

(defun pm-get-desc (key mode file)
  "This function returns the description for the given key/type/file."
  (let ((name (pm-get-macros key mode file)))
    (if (null name)
        ""
      (get name 'documentation))))

(defun pm-set-info (mode key desc file macro-name)
  (put macro-name 'mode mode)
  (put macro-name 'key key)
  (put macro-name 'documentation desc)
  (put macro-name 'file file)
  (if (not (member macro-name pm-macros))
      (push macro-name pm-macros)))
  

(defun pm-fetch-active-files ()
  "This function returns a list of files from which macros has been loaded"
  (let ((files '())
        file)
    (dolist (macro pm-macros)
      (setq file (get macro 'file))
      (if (not (member file files))
          (setq files (cons file files))))
    (delete "" files)))

;;}}}
;;{{{ Key binding Check

;;{{{ Description

; The function pm-possible-override takes care of warning the user if he
; is about to overwrite a key or shadow key. Below the rules for checking
; this is described.

;--------------- meaning of symbols ---------------
; 
; (+) means that the given test is implemented
; (-) means that the given test is not implemented. This is either because
;     I don't know how to do it, or because I find it very unlikely that it
;     will ever be used.
;
; (key, mode, file) in set PM-bind
;     Check if pm-macros contain a definition for the given
;     key,type,mode,file tuple.
;     A dash means that the given field does have a meaning (only used for
;     mode)
;     An asterix is a wildcard meaning that this field should not be
;     included when searching for tuples.

; key in set map 
;    Check if key is bound in the given map
;
; Override
;    This means that if the given check returns true, then the new
;    definition will override the given match(es).
;
; Shadows
;    This means that if the given check returns true, then the new
;    definition will shadow the given match(es). Which means that the
;    existing definition(s) will not be visible.
;
; Invisible
;    This means that if the given check returns true, then the new
;    definition will have no effect. 
;
; Note: that the two clauses above might only be true for the given 
; mode or the given buffer.
; 
; 1)
;    This means test number one for both new bindings and existing bindings
; 1 new)
;    This means test number one. It should, however, only be done for new bindings.

;--------------- Want to bind a global key ---------------
; (+) 1)     Override  - (key,global,-,file) in set PM-bind
; (-) 2)     Override  - (key,global,-,*)    in set PM-bind
; (+) 3)     Override  - key in set global-map
; (-) 4 new) Invisible - (key,active-modes,*) in set PM-bind
; (+) 5 new) Invisible - key in set *-active-mode-map
; (-) 6)     Invisible - (key,*-mode,*) in set PM-bind
; (-) 7)     Invisible - key in set *-mode-map

;--------------- Want to bind a mode specific key ---------------
; (+) 1)      Override  - (key,mode,file) in set PM-bind
; (-) 2)      Override  - (key,mode,*) in set PM-bind
; (+) 3)      Override  - key in set mode-map
; (-) 4)      Shadows   - (key,global,*) in set PM-bind
; (+) 5)      Shadows   - key in set global-map
; (-) 6 new)  Invisible - (key,active-minor-modes,*) in set PM-bind
; (+) 7 new)  Invisible - key in set active-minor-modes-maps
; (-) 8)      Invisible - (key,*-minor-mode,*) in set PM-bind
; (-) 9)      Invisible - key in set *-minor-modes-map

;;}}}
;;{{{ pm-possible-override

(defun pm-possible-override (new-def key mode file) 
  "This function checks in a number of ways whether the new key binding
violates and existing key binding. For a list of all the different checks,
please see the description in the source code."

  (let* ((buf (pm-create-buffer "*Warning*"))
         (any-output nil)
         main-macro macro-list macro)
    (setq pm-warn-buffer buf)
    (pm-insert "Your new definition might violate an existing definition. Below you
see a report of what will happen if you make your binding.")


    ;; -------------------------------
    ;; Rule no. 1 is the same for both
    ;; -------------------------------
    (setq main-macro (pm-get-macros key mode file))
    (if main-macro
        (progn
          (setq any-output t)
          (pm-insert "\n
----------------------------------------------------------------------

This definition will " (pm-outstand "replace") " an existing power-macro on the same key, 
with the same type and defined in the same file. Its description is:\n")
          (pm-insert (pm-indent (get main-macro 'documentation)))))

      
    ;; -------- The rest of the rules ---------
    (setq any-output (or (cond 
                          ((eq mode 'global) 
                           (pm-override-global new-def key file main-macro))
                          
                          (t
                           (pm-override-mode new-def key file mode main-macro))) 
                         any-output))

    (pm-insert "\n") ; This is to ensure that there is a location, which is
                  ; not with the left-margin property.

    (if any-output
        (save-excursion
          (switch-to-buffer buf)
          (pm-indent-regarding-to-properties)
          (setq buffer-read-only t)
          (if (not (yes-or-no-p "Continue definiting the macro? "))
              ;; Stop defining the macro.
              (progn
                (kill-buffer buf)
                (throw 'pm-stop-def nil)))))
    
      ;; Finally kill the buffer.
    (kill-buffer buf)))

;;}}}
;;{{{ pm-override-global

(defun pm-override-global (new-def key file main-macro)
  "This function checks in a number of ways if the new global key binding violates existing bindings.
See the source code for all the different checks"

  (let ((any-output nil))
        

    ;; ---------------------------------
    ;; Rule no. 3
    ;; Override  - key in set global-map
    ;; ---------------------------------
    (setq any-output 
          (or (pm-check-binding 'global key 'conflict main-macro) any-output))

    ;; ----------------------------------------
    ;; Rule no. 5 (only for new macros)
    ;; Invisible - key in set *-active-mode-map
    ;; ----------------------------------------
    (if new-def
        (setq any-output
              (or (pm-check-binding 'mode key 'invisible  main-macro major-mode) any-output)))

    (if new-def
        (setq any-output
              (or (pm-check-binding 'minor-mode key 'invisible main-macro) any-output)))


    any-output)) ; return value

;;}}}
;;{{{ pm-override-mode

(defun pm-override-mode (new-def key file mode main-macro)
  "This function checks in a number of ways if the new mode key binding violates existing bindings.
See the source code for all the different checks"

  (let ((any-output nil))

    ;; ---------------------------------------
    ;; Rule no. 3
    ;; Override  - key in set current-mode-map
    ;; ---------------------------------------
    (setq any-output
          (or (pm-check-binding 'mode key 'conflict  main-macro mode) any-output))
    

    ;; ---------------------------------
    ;; Rule no. 5
    ;; Shadows   - key in set global-map
    ;; ---------------------------------
    (setq any-output
          (or (pm-check-binding 'global key 'shadows main-macro) any-output))

    ;; ----------------------------------------------
    ;; Rule no. 7 (only for new macros)
    ;; Invisible - key in set active-minor-modes-maps
    ;; ----------------------------------------------
    (if new-def
        (setq any-output
              (or (pm-check-binding 'minor-mode key 'invisible main-macro) any-output)))


    any-output)) ; return value

;;}}}
;;{{{ pm-check-binding

(defun pm-check-binding (type key warn-type main-macro &optional mode)
  "This function verifies if key is bound for the given type, and if so it
will insert a warning of warning-type into the buffer, telling that there
is a problem. where is used to indicate where the problem is.  The function
returns whether there was anything to report.

warn-type is one of: conflict, invisible, shadows
type is one of: global, mode, minor-mode"

  (let* (map-name
         (func (cond ((eq type 'global) (pm-lookup-key global-map key))
                     ((eq type 'mode)
                      (progn
                        (setq map-name (intern (concat (symbol-name mode) "-map")))
                        (if (boundp map-name)
                            (pm-lookup-key (eval map-name) key)
                          nil)))
                     ((eq type 'minor-mode) (pm-get-minor-mode-binding key))
                     (t (error "Internal error: Unknown type"))))
         (where (symbol-name type)))
    (if (and func (not (eq func main-macro)))
        (progn

          (pm-insert "\n----------------------------------------------------------------------\n\n")
          ;; Insert the warning text
          (if (eq warn-type 'conflict)
              (pm-insert "This definition will " (pm-outstand "conflict")
                         " with ")
            (if (eq warn-type 'invisible)
                (pm-insert "This definition will " (pm-outstand "be invisible")
                           " because of ")
              (if (eq warn-type 'shadows)
                  (pm-insert "This definition will " (pm-outstand "shadow")
                             " for the ")
              (error (format "Unknown type %s" warn-type)))))

          ;; Now insert the description for the function
          (if (symbolp func) ;; This is a function name.
              (if (fboundp func) ;; The function exists.

                  (if (string-match "pm-macro-" (format "%s" func))
              
                      ;; This is a power macro.
                      (pm-insert "the " where " power macros loaded from\nthe file '" 
                                 (get func 'file) "'. Its description is:\n"
                                 (pm-indent (get func 'documentation)))
                      
                    ;; This is another existing function
                    (pm-insert "the " where " function '" (symbol-name func) 
                            "'\nwhich has the following description:\n"
                            (pm-indent (documentation func))))

                ;; The function doesn't seem to be defined.
                (pm-insert "the " where " function '" (symbol-name func) "' which,\n"
                        "however, doesn't seem to be defined (At least not at the moment)"))
            
            ;; Hmm it was not a symbol, then it might be a lambda expression
            ;; Does there exists other possibilities?
            (pm-insert "the following " where " definition:\n" (format "%s" func)))


          t) ;; return value
      nil)))

;;}}}
;;{{{ Misc

(defun pm-outstand (text)
  "This function adds bold properties to the string given as argument"
  (let ((new-text (concat text))) ;; we need to copy it to avoid adding
                                  ;; properties to the original widget
    (add-text-properties 0 (length new-text) '(face bold) new-text)
    new-text))

(defun pm-indent (text)
  "This function returns a string equal to its input, with the
exception that it has indentation properties"
  (let ((new-text (concat text))) ;; we need to copy it to avoid adding
                                  ;; properties to the original widget
    (add-text-properties 0 (length new-text) '(left-margin 3) new-text)
    new-text))


(defun pm-indent-regarding-to-properties ()
  "This function runs through the buffer with warnings about overriding
keys etc. and executes fill-region on the locations where the text property
left-margin is set."
  (let ((cont t)
        (end (point-min))
        start)
    (while cont
      (setq cont nil)
      (setq start (next-single-property-change end 'left-margin))
      (if start
          (if (not (get-text-property start 'left-margin))
              (setq cont t
                    end (+ end 1))
            (progn
              (setq end (next-single-property-change start 'left-margin))
              (if end
                  (progn
                    (fill-region start end)
                    (setq end (+ end 1))
                    (setq cont t)))))))))
  


(defun pm-lookup-key (map key)
  "Returns the function definition bound to key if it exists, otherwise
returns nil"
  (let ((func (lookup-key map key)))
    (if (and func (not (numberp func)))
        func
      nil)))



(defun pm-get-minor-mode-binding (key)
  "This function returns the definitions of keys in the active minor modes"
  (let ((maps (current-minor-mode-maps))
        (binding nil))
    (while maps
      (setq binding (lookup-key (car maps) key))
      (if (and binding (not (numberp binding)))
          (setq maps '())
        (setq binding nil))
      (setq maps (cdr maps)))
    binding))

(defun pm-insert (&rest args)
  "This function is just like insert, with the exception that it inserts
the contents into the buffer pm-warn-buffer"
  (save-excursion
    (set-buffer pm-warn-buffer)
    (eval `(insert ,@args))))

;;}}}

;;}}}
;;{{{ Actual Key (un)binding

(defun pm-unset-key (macro)
  "This function removes the binding described in macro."
  (let ((key (get macro 'key))
        (mode (get macro 'mode)))
    (if (eq mode 'global)
        (if (eq (lookup-key global-map key) macro)
            (global-unset-key key))

      ;; Mode specific binding
      (let* ((map (intern (concat (symbol-name mode) "-map"))))
        (if (boundp map)
            (if (eq (lookup-key (eval map) key) macro)
                (define-key (eval map) key nil))

          ;; No map exists, we must then unadvice the function and run
          ;; through all buffers.
          (if (fboundp mode)
              (pm-set-or-unset-mode-key macro mode key 'unset)))))))

(defun pm-set-key (macro)
  "Makes the key binding as described in the given macro"

  (let ((mode (get macro 'mode))
        (key (get macro 'key)))
    (if (eq mode 'global)
        (global-set-key key macro)

      ;; major-mode macro
      (let ((map (intern (concat (symbol-name mode) "-map"))))
        (if (boundp map)
            (define-key (eval map) key macro)

          ;; A mode-map didn't exists so now we need to make the binding
          ;; much more manual.
          (pm-set-or-unset-mode-key macro mode key 'set))))))

(defun pm-set-or-unset-mode-key (macro mode key type)
  "This function does two things:
1) Bind the macro to the key in all buffers with mode
2) Advises mode to bind the key for subsequent buffers for this mode"

  (let ((all-buffers (buffer-list))
        (adv-name (intern (concat (symbol-name macro) "-advice"))))

    ;; Run through all the buffers and bind the key for buffers with the
    ;; given major mode
    (dolist (buf all-buffers)
      (save-excursion 
        (set-buffer buf)
        (if (eq major-mode mode)
            (if (eq type 'set)
                (local-set-key key macro)
              
              ;; Unset the key if it is bound to the macro.
              (if (eq (lookup-key (current-local-map) key) macro) 
                  (local-unset-key key))))))
    
    (if (eq type 'set)
        (eval `(defadvice ,mode (after ,adv-name activate)
                 (local-set-key ,key ',macro)))
      (ad-remove-advice mode 'after adv-name))
    ))

;;}}}
;;{{{ Utility functions

(defun pm-create-buffer (&optional name)
  "This function switches to the named buffer or to the power-macros buffer if no name is give.
The buffer is created if it doesn't exists."
  (let* ((nm (if name name "*Power Macros Description*"))
        (old-buf (get-buffer nm)))
    (if old-buf
        (kill-buffer old-buf))
    (get-buffer-create nm)))


(defun pm-ass-remove (var list)
  "Removes var from assoc list list"
  (if (null list)
      '()
    (if (eq (caar list) var)
        (pm-ass-remove var (cdr list))
      (cons (car list) (pm-ass-remove var (cdr list))))))


;; The following function makes it possible to insert "Enter recursive edit"
;; in the power macro menu item.
;; One could argue that these items does not belong in the PM entry,
;; but I believe they are extremely useful, and have a very hard to remember
;; key binding.
(defun pm-enter-recursive-edit ()
  "This is just like C-u C-x q when defining keyboard macros."
  (interactive)
  (message (substitute-command-keys "Press \\[exit-recursive-edit] to leave recursive edit"))
  (kbd-macro-query 1))


;; Does select not exists in core lisp?
(defun pm-select (func list)
  (let ((res '()))
    (while list
      (if (funcall func (car list))
          (push (car list) res))
      (setq list (cdr list)))
    (nreverse res)))

(defun pm-available-modes ()
  "This function returns the list of available modes"
  (interactive)
  (if (not (null pm-available-modes))
      pm-available-modes

    ;; calculate the available modes ones and for all
    ;; Does anyone know how this list can be extracted more efficiently?
    (progn
      (message "Searching for available modes...")
      (defun pm-extract-modes (x)
        (if (string-match ".*-mode$" (symbol-name x))
            (push (list (symbol-name x)) pm-available-modes)))
      (mapatoms 'pm-extract-modes)
      (message "Searching for available modes...Done")
      pm-available-modes)))

(defun cadddar (l)
  (cadddr (car l)))

;;}}}

(provide 'power-macros)
(message "Loading power-macros...done")
