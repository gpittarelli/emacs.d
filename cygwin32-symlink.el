;;; -*- emacs-lisp -*-
;;; Make Emacs understnd Cygwin-style symlinks
;;; AUTHOR: Adam Schlegel, Thinkage Ltd. <aceschle@uwaterloo.ca>
;;; CREATED: July 21, 2000 14:46:12
;;; MODIFIED: August 25, 2000 16:03:51

;;; INSTRUCTIONS: Just place this file somewhere in your load path and add
;;;		  the line (require 'cygwin32-symlink) to your .emacs file

;;; BUGS: The default directory for a file is changed to the location
;;;	of the destination file. It should remain the location of
;;;	the link file.
;;;
;;;	Symlinkd dirs still complete as files (because they *are* files)
;;;
;;;	Symlinks will not work on a network machine. So don't try. This is
;;;	very hard to fix because you'd have to know the mount table for the
;;;	new computer. Not worth the effort.
;;;
;;;	Doesn't help emacs follow the current directory in shell mode.

;;; BACKGROUND:
;;; AFICT, the filename in a link is in the same format as
;;; it was when it was created (with the ln -s command). This means that
;;; it can be in *any* format acceptable to cygwin.
;;; Cygwin32-mount is required to be able to parse files referred to
;;; by mount points. (Most importantly the slash mount point)
(require 'cygwin-mount)

(defvar cygwin32-follow-symlinks t
  "What to do when visiting a Cygwin symlink.
When non-nil, any Cygwin-style symlink will be followed to the file that
it points to (unless that file is not there). Otherwise, just open the
link file itself.")

(defconst cygwin32-symlink-style-regexp "^!<symlink>\\(.*\\)\0$"
  "A regular expression matching the structure of a symlink file.
A file is considered to be a symlink if its first line matches
this regular expression. Furthermore, the first sub-expression is
considered to be the filename of the file to which it is linked.")

;; I wish I didn't need this, but I don't see any way around it.
;; The right solution is to scan up to the first NULL, but
;; `insert-file-contents' needs to have a number supplied.
(defconst cygwin32-symlink-max-length 500
  "The maximum length of a cygwin symlink file.
This determines how far into a file to look for the *entire* definition of
a cygwin symlink.")

(defconst cygwin32-symlink-drive-root
  "^\\([a-zA-Z]:\\|/\\|/cygdrive/[a-zA-Z]\\|~\\)[/\\\\]?$"
  "Controls when to stop looking for symlinked directories in a path.
How can this be made more 'general'? I want the same regexp to be used for
this and `smart-compile'.")

;; File-Handler Stuff
(or (assoc "" file-name-handler-alist)
    (setq file-name-handler-alist
          (cons '("" . cygwin32-symlink-handler) file-name-handler-alist)))

(defun cygwin32-symlink-handler (operation &rest args)
  (let ((inhibit-file-name-handlers				; directly
         (cons 'cygwin32-symlink-handler		        ; from the
               (and (eq inhibit-file-name-operation operation)	; GNU E-Lisp
                    inhibit-file-name-handlers)))		; manual on
        (inhibit-file-name-operation operation)			; the web
        ;; need to completely disable this handler within itself
        (file-name-handler-alist
         (delete '("" . cygwin32-symlink-handler) file-name-handler-alist)))
    (cond ((and cygwin32-follow-symlinks
                (or (eq operation 'expand-file-name)
                    (eq operation 'substitute-in-file-name)))
           (apply operation
                  (cons (cygwin32-symlink-internal-expand (car args))
                        (cdr args))))
          (t (apply operation args)))))

(defun cygwin32-symlink-p (file)
  "Returns non-nil if file is regognized as a symlink.
Reads the first `cygwin32-symlink-max-length'  characters  of the file and
compares it to known symlink styles, found in `cygwin32-symlink-style-regexp'.
If the format matches the regexp, the function returns the filename found in
the first sub-expression, otherwise, it returns nil."
  (let ((return-value nil)
        (cygwin32-follow-symlinks nil))
    (if (and (file-exists-p file)
             (file-readable-p file)
             (not (file-directory-p file)))
        (with-temp-buffer
          (insert-file-contents-literally
           file nil 0 cygwin32-symlink-max-length)
          (goto-char (point-min))
          (if (looking-at cygwin32-symlink-style-regexp)
              (setq return-value (match-string 1)))))
    return-value))

(defun cygwin32-symlink-expand (file)
  "Return the actual (DOS/Windows) path from a cygwin path."
  (interactive "F") ; so the user can actually call this
  (message "%s" (cygwin32-symlink-internal-expand file nil)))

;; This will always look at the file relative to the current directory.
(defun cygwin32-symlink-internal-expand (file &optional circle)
  "Return the actual (DOS/Windows) path from a cygwin path.
File is the filename to expand.
Circle is a list of links that have already been visited. This checks for
circular references. When initially calling this, pass it nil unless you
have a *very* good reason not to (I can't think of one)."
  ;; Set file to what the user actually wants us to work with
  (setq file (substitute-in-file-name file))
  (let ((my-list (break-file-name file))
        (my-file)
        (dir)
        (result))
    (while my-list
      (setq dir my-file) ;; the last value of my-file
      (setq my-file (concat my-file (car my-list)))
      (setq my-list (cdr my-list))
      (setq result (cygwin32-symlink-p my-file))
      (if result ;; my-file refers to a symlink
          (progn
            (if (string-match
                 ;; drive-root is anchored.. need non-anchored version
                 ;; ASSUMPTION: drive root ends in a $
                 (substring cygwin32-symlink-drive-root 0 -1)
                 result)
                ;; File is 'rooted' into the file-system
                nil ; nothing needed
              ;; otherwise file is a relative name
              ;; interpret relative to the directory we were last in
              (setq result (concat dir "/" result)))
            (if (member result circle)
                (error "Too many symbolic links"))
            (let ((circle (cons result circle)))
              (setq my-file
                    (cygwin32-symlink-internal-expand result circle))))))
    ;; do any necessary post-processing
    (setq result my-file)
    result))

(defun break-file-name (file)
  "Returns a list of components of file's directories."
  (let* ((cygwin32-follow-symlinks nil)
         (my-file file)
         (my-list ()))
    (while (not (or (string= my-file "")
                    (string-match cygwin32-symlink-drive-root my-file)))
      (if (string-match "\\(.*\\)\\([/\\\\].+\\)" my-file)
          (let ((first (match-string 1 my-file))
                (second (match-string 2 my-file)))
            ;; delete any ending /'s or \'s unless its either the / mount
            ;; or the first (leaf) entry [for filename completion]
            (if (and my-list
                     (string-match "\\(.+\\)[/\\\\]" second))
                (setq second (match-string 1 second)))
            (setq my-list (cons second my-list))
            (setq my-file first))
        (progn
          (setq my-list (cons my-file my-list))
          (setq my-file ""))))
    (setq my-list (cons my-file my-list))
    my-list))

(provide 'cygwin32-symlink)
