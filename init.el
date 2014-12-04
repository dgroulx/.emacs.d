(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(setq my-packages '(paredit clojure-mode cider web-mode
			    scss-mode exec-path-from-shell nodejs-repl
			    inf-ruby feature-mode slime yafolding
			    yaml-mode coffee-mode nginx-mode wrap-region
			    org markdown-mode sml-mode))

(defun packages-missing-p (packages)
  (let ((package-missing-p nil))
    (while (and (not package-missing-p) packages)
      (if (package-installed-p (car packages))
	(setq packages (cdr packages))
	(setq package-missing-p t)))
    package-missing-p))

(if (packages-missing-p my-packages)
  (progn
    (package-refresh-contents)
    (dolist (p my-packages)
      (if (not (package-installed-p p))
	  (package-install p)))))


;; Get path from .bashrc instead of the GUI that launched Emacs
(exec-path-from-shell-initialize)


;; UI Customization
(setq inhibit-startup-screen -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(show-paren-mode)
(require 'ido)
(ido-mode t)

;; Appease OSX Terminal.app
;; Requires you to add these maps to your keymaps
(define-key input-decode-map "\e[1;2A" [S-up])
(define-key input-decode-map "\e[1:2B" [S-down])

;; Shift keys for window navigation
(windmove-default-keybindings)

;; Keep backup files in a global location instead of local dir
(setq backup-directory-alist `(("." . "~/.backup")))
(setq backup-by-copying t)


;; Elisp
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'yafolding-mode)


;; Clojure
(require 'clojure-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'yafolding-mode)

(defun clojure-indent (syms indent)
  (mapcar (lambda (sym) (put-clojure-indent sym indent)) syms))

;; Cucumber Indents
(setq cucumber-indents '(Given When Then And But))
(clojure-indent cucumber-indents 2)

;; Compojure Indents
(setq compojure-indents '(context))
(clojure-indent compojure-indents 2)

;; Om Indents
(setq om-indents '(p div span h1 h2 h3 h4 h5 h6 ul li a ))
(clojure-indent om-indents 2)


;; Cider
(add-hook 'cider-repl-mode-hook 'paredit-mode)


;; Web-mode
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html.erb$" . web-mode))
(setq web-mode-markup-indent-offset 2)


;; Ruby
(add-hook 'ruby-mode-hook 'yafolding-mode)
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Vagrantfile$" . ruby-mode))


;; Javascript
(setq js-indent-level 2)


;; SASS
(setq scss-compile-at-save nil)


;; CSS
(setq css-indent-offset 2)


;; nginx
(add-to-list 'auto-mode-alist '("\\nginx.conf$" . nginx-mode))
