;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

(add-to-list 'load-path "~/.emacs.d")

;; Setup packages
(require 'setup-package)

;; Install any missing packages
(add-desired-packages
   (cons 'auto-complete melpa)
   (cons 'autopair melpa)
   (cons 'cmake-mode marmalade)
   (cons 'sws-mode marmalade)
   (cons 'jade-mode marmalade)
   (cons 'markdown-mode marmalade)
   (cons 'multi-web-mode melpa)
   (cons 'color-theme melpa)
   (cons 'gitignore-mode melpa)
   (cons 'flymake-jshint melpa)
   (cons 'flymake-cursor melpa)
   (cons 'ctags melpa)
   (cons 'ctags-update melpa)
   (cons 'multiple-cursors melpa))

(if (eq system-type 'windows-nt)
    (add-desired-packages
     (cons 'cygwin-mount marmalade)))

(packages-install-desired)

(if (eq system-type 'windows-nt)
    (progn
      (setenv "PATH" (concat "e:/cygwin/bin;" (getenv "PATH")))
      (setq exec-path (cons "e:/cygwin/bin/" exec-path))
      (require 'cygwin-mount)
      (require 'setup-cygwin)
      (setq w32shell-cygwin-bin "e:\\cygwin\\bin")
      (require 'w32shell)

      (w32shell-add-emacs)
      (w32shell-set-shell "cygwin")

      (defun cygwin-shell ()
        "Run cygwin bash in shell mode."
        (interactive)
        (let ((explicit-shell-file-name "E:/cygwin/bin/bash"))
          (call-interactively 'shell)))
      ))

;;       (cygwin-mount-activate)

;;       (add-to-list 'load-path "~/.emacs.d/cygwin-link")
;;       (require 'cygwin-link)
;;       (require 'cygwin32-symlink)

;;       (add-hook 'comint-output-filter-functions
;;                 'shell-strip-ctrl-m nil t)
;;       (add-hook 'comint-output-filter-functions
;;                 'comint-watch-for-password-prompt nil t)

;;       ;; For subprocesses invoked via the shell
;;       ;; (e.g., "shell -c command")
;;       (setq explicit-shell-file-name "bash.exe")
;;       (setenv "SHELL" explicit-shell-file-name)
;;       (setq shell-file-name explicit-shell-file-name)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(setq locale-coding-system 'utf-8-unix) ; pretty
(set-terminal-coding-system 'utf-8-unix) ; pretty
(set-keyboard-coding-system 'utf-8-unix) ; pretty
(set-selection-coding-system 'utf-8-unix) ; please
(prefer-coding-system 'utf-8-unix) ; with sugar on top

(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

; Disable annoying gui stuff
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

; CUA mode for better rectangle selection
(cua-mode t)
(setq cua-enable-cua-keys nil)
(setq cua-auto-tabify-rectangles nil)

; Highlight selected region
(transient-mark-mode 1)

(set-default-font "Anonymous Pro-10")

(setq inhibit-startup-message t)

; Keys
(defun other-window-reverse ()
  (interactive)
  (other-window -1))

(auto-fill-mode 1)
(setq comment-auto-fill-only-comments t)

; Save place
(require 'saveplace)
(setq-default save-place t)

; Load autocomplete
(require 'auto-complete)
(setq ac-auto-start 2)
(setq ac-ignore-case t)
; (ac-set-trigger-key "RET")

; Load autopair
(require 'autopair)
(setq autopair-autowrap t)
(autopair-global-mode t)

(setq require-final-newline nil)

(setq c-default-style "linux"
          c-basic-offset 2)

; C-k at the end of a line removes spaces from the next line
; (which gets appended to the end of current line)
(defun kill-and-join-forward (&optional arg)
      (interactive "P")
      (if (and (eolp) (not (bolp)))
          (progn (forward-char 1)
                 (just-one-space 0)
                 (backward-char 1)
                 (kill-line arg))
        (kill-line arg)))

(defun set-window-width (n)
  "Set the selected window's width."
  (adjust-window-trailing-edge (selected-window) (- n (window-width)) t))

(defun set-80-columns ()
  "Set the selected window to 80 columns."
  (interactive)
  (set-window-width 80))

(require 'cmake-mode)

; Show parens
(show-paren-mode 1)
(setq show-paren-delay 0)

; Show column number
(column-number-mode)

; Colors
(set-background-color "black")
(set-foreground-color "white")
(set-cursor-color "orange")

; Don't echo command line
(setq explicit-cmd.exe-args '("/q"))
(setq explicit-cmdproxy.exe-args '("/q"))

; Tabs settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default js-indent-level 2)

; On save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

; Ocaml mode
;(add-to-list 'load-path "D:/Utility/emacs-23.4-bin-i386/emacs-23.4/site-lisp/caml-mode")
;(add-to-list 'auto-mode-alist (cons "\\.ml[iylpt]?$" 'caml-mode))

;(autoload 'caml-mode "caml" "Major mode for editing Caml code." t)
;(autoload 'run-caml "inf-caml" "Run an inferior Caml process." t)
;(autoload 'camldebug "camldebug" (interactive) "Debug caml mode")

; Copy line
(defun copy-line (&optional arg)
 "Do a kill-line but copy rather than kill.  This function directly calls
  kill-line, so see documentation of kill-line for how to use it including prefix
  argument and relevant variables.  This function works by temporarily making the
  buffer read-only."
  (interactive "P")
  (let ((buffer-read-only t)
    (kill-read-only-ok t))
    (kill-line arg)))

;(if window-system (require 'caml-font))

; jade mode
(add-to-list 'load-path "~/.emacs.d/vendor/jade-mode")
(require 'sws-mode)
(require 'jade-mode)
(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

; ruby 'slim' processor for jade
(add-to-list 'auto-mode-alist '("\\.slim$" . jade-mode))

(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))

;; Python Hook
(setq python-indent 2)
(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil
                            tab-width 2))))

(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)

(require 'color-theme)

(auto-fill-mode)
(delete-selection-mode nil)

(modify-frame-parameters nil '((wait-for-wm . nil)))

(require 'tramp)
(setq tramp-default-method "ssh")
(setq tramp-debug-buffer t)

(setq auto-mode-alist (cons '(".less" . less-css-mode) auto-mode-alist))

(setq css-indent-offset 2)

(add-to-list 'load-path "~/.emacs.d/dirtree")
(autoload 'dirtree "dirtree" "Add directory to tree view" t)
(require 'dirtree)

(setq ispell-program-name "aspell")
(setq ispell-dictionary "english")

(setq shell-file-name "bash")
(setq explicit-bash.exe-args '("--noediting" "--login" "-i"))
(setenv "SHELL" shell-file-name)
(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(setq scroll-conservatively 5)
(setq scroll-margin 5)

;; allow scroll-down/up-command to move point to buffer end/beginning
(setq scroll-error-top-bottom 'true)

(require 'cmake-mode)

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;; setting the PC keyboard's various keys to
;; Super or Hyper, for emacs running on Windows.
(setq w32-pass-lwindow-to-system nil
      w32-pass-rwindow-to-system nil
      w32-pass-apps-to-system nil
      w32-lwindow-modifier 'super ; Left Windows key
      w32-rwindow-modifier 'super ; Right Windows key
      w32-apps-modifier 'hyper) ; Menu key

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(comint-completion-addsuffix (quote ("\\" . " ")))
 '(comment-style (quote plain))
 '(desktop-save-mode t)
 '(safe-local-variable-values (quote ((c-set-style . "BSD")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defface ac-etags-candidate-face
  '((t (:background "gainsboro" :foreground "deep sky blue")))
  "Face for etags candidate")

(defface ac-etags-selection-face
  '((t (:background "deep sky blue" :foreground "white")))
  "Face for the etags selected candidate.")

(defvar ac-source-etags
  '((candidates . (lambda ()
                    (all-completions ac-target (tags-completion-table))))
    (candidate-face . ac-etags-candidate-face)
    (selection-face . ac-etags-selection-face)
    (requires . 3))
  "Source for etags.")

(iswitchb-mode)

;; Setup key bindings
(require 'key-bindings)

(add-to-list 'load-path "~/.emacs.d/jshint-mode")
(require 'flymake-jshint)
(add-hook 'javascript-mode-hook
     (lambda () (flymake-mode t)))

;; Turns on flymake for all files which have a flymake mode
(add-hook 'find-file-hook 'flymake-find-file-hook)

(require 'flymake-cursor)
(require 'multiple-cursors)
(require 'ctags)
(require 'ctags-update)

;; Show flymake errors (Like 'no makefile found for file') in the
;; minibuffer instead of popping up annoying modal dialog boxes.
(defun flymake-display-warning (warning)
  "Display a warning to the user, using lwarn"
  (message warning))
