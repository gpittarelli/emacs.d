(require 'package)

;; Fill in this list:
(setq packages-desired '())
;; using (add-desired-packages list)
;; and then call (packages-install-desired)

(defvar marmalade '("marmalade" . "http://marmalade-repo.org/packages/"))
(defvar gnu '("gnu" . "http://elpa.gnu.org/packages/"))
(defvar melpa '("melpa" . "http://melpa.milkbox.net/packages/"))

;; Add marmalade to package repos
(add-to-list 'package-archives marmalade)
(add-to-list 'package-archives melpa t)
(add-to-list 'package-archives gnu t)

(package-initialize)

(unless (and (file-exists-p "~/.emacs.d/elpa/archives/marmalade")
             (file-exists-p "~/.emacs.d/elpa/archives/gnu")
             (file-exists-p "~/.emacs.d/elpa/archives/melpa"))
  (package-refresh-contents))

(defun packages-install (&rest packages)
  (mapc (lambda (package)
          (when (not (package-installed-p package))
            (package-install name)))
  packages))

(defun packages--filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun get-missing-packages ()
  (packages--filter (lambda (x)
                      (not (package-installed-p x)))
                    packages-desired))

(defun packages-install-desired ()
  (let ((needed-packages (get-missing-packages)))
    (message "Needed: %s" needed-packages)
    (when needed-packages
      (message "%s" "Refreshing package index...")
      (package-refresh-contents)
      (message "%s" "Installing packages...")
      (apply 'packages-install packages-desired)
      (message "%s" "Done installing packages."))))

(defun add-desired-packages (packages)
  (message "Packs now: %s" packages-desired)
  (message "add: %s" packages)
  (if packages-desired
      (nconc packages-desired packages)
      (setq packages-desired packages))
  (message "and now: %s" packages-desired))

(provide 'setup-package)
