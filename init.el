(server-start)

(require 'package)
(setq package-archives
    '(("ELPA" . "http://tromey.com/elpa/")
      ("gnu" . "http://elpa.gnu.org/packages/")
      ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

; Disable annoying gui stuff
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

; Highlight selected region
(transient-mark-mode 1)

(set-default-font "Anonymous Pro-10")

(setq inhibit-startup-message t)

; Keys
(defun other-window-reverse ()
  (interactive)
  (other-window -1))

(global-set-key (kbd "<C-tab>") 'other-window)
(global-set-key (kbd "<C-S-tab>") 'other-window-reverse)

(auto-fill-mode 1)
(setq comment-auto-fill-only-comments t)

; Save place
(require 'saveplace)
(setq-default save-place t)

; Load autocomplete
(add-to-list 'load-path "c:/Program Files/emacs/auto-complete-1.3.1")
(require 'auto-complete)
(global-auto-complete-mode t)
; (ac-set-trigger-key "RET")

; Load autopair
(add-to-list 'load-path "~/.emacs.d/autopair")
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
(global-set-key "\C-k" 'kill-and-join-forward)

(defun set-window-width (n)
  "Set the selected window's width."
  (adjust-window-trailing-edge (selected-window) (- n (window-width)) t))

(defun set-80-columns ()
  "Set the selected window to 80 columns."
  (interactive)
  (set-window-width 80))

(global-set-key "\C-x~" 'set-80-columns)

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
(global-set-key "\M-k" 'copy-line)

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

(auto-fill-mode)
(delete-selection-mode 1)

(modify-frame-parameters nil '((wait-for-wm . nil)))

(require 'tramp)
(setq tramp-default-method "smx")
(setq tramp-debug-buffer t)

(setq auto-mode-alist (cons '(".less" . less-css-mode) auto-mode-alist))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(comint-completion-addsuffix (quote ("\\" . " ")))
 '(comment-style (quote plain))
 '(desktop-save-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
