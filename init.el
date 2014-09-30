(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(package-initialize)

(defvar my-packages '(paredit clojure-mode))

(defun packages-missing-p (packages)
  (let ((package-missing-p nil))
    (while (and (not package-missing-p) packages)
      (if (package-installed-p (car packages))
	  (setq packages (cdr packages))
	(setq package-missing-p t)))
    package-missing-p))

(if (packages-missing-p my-packages)
    (dolist (p my-packages)
      (if (not (package-installed-p p))
	  (package-install p))))


;; UI Customization
(tool-bar-mode -1)

;; Keep backup files in a global location instead of local dir
(setq backup-directory-alist `(("." . "~/.backup")))
(setq backup-by-copying t)



