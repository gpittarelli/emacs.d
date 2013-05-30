;; Why leave emacs with something as easy as C-x C-c?
;; instead, the mnemonic is C-x REALLY QUIT
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'delete-frame)

(global-set-key (kbd "C-<f1>") 'show-file-name)

;; Unbind Pesky Sleep Button
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; C-tab and C-S-tab between windows
(global-set-key (kbd "<C-tab>") 'other-window)
(global-set-key (kbd "<C-S-tab>") 'other-window-reverse)
(global-set-key (kbd "<C-S-iso-lefttab>") 'other-window-reverse)

;; Better line killing
(global-set-key (kbd "C-k") 'kill-and-join-forward)

;; Set buffers to 80 col
(global-set-key (kbd "C-x ~") 'set-80-columns)

;; Sane M-k function (M-k is to C-k what M-w is to C-w)
(global-set-key "\M-k" 'copy-line)

(global-set-key (kbd "C-x C-e") 'eval-and-replace)

(provide 'key-bindings)
