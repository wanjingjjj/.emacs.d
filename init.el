;;(setq debug-on-quit t)

;;default directory
(setq default-directory "~/" )

;; load-path
(setq load-path
      (append (list "~/.emacs.d/site-lisp")
                      load-path))
(require 'unicad)

;; melpa need to be the first one
(load "~/.emacs.d/init_melpa.el")
(load "~/.emacs.d/init_face.el")
(load "~/.emacs.d/init_org.el")
(load "~/.emacs.d/init_backup.el")
(load "~/.emacs.d/init_ido.el")
(load "~/.emacs.d/init_helm.el")
(load "~/.emacs.d/init_golang.el")
(load "~/.emacs.d/init_py.el")
(load "~/.emacs.d/init_tex.el")
(load "~/.emacs.d/init_eclim.el")
(load "~/.emacs.d/init_markdown.el")
(load "~/.emacs.d/init_colortheme.el")
(load "~/.emacs.d/init_gnus.el")


;; disable to use daemon
(server-start)

;; use auto-fill-mode for text files
(setq text-mode-hook '(lambda()  
                        (auto-fill-mode t) 
                        )) 

;; ecb
;;'(require 'ecb-autoloads)

;; jsp
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . html-mode))

;; popwin to eliminate all these *help* *message* buffers
;;(require 'popwin)
;;(popwin-mode 1)


;; jsx mode
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
(autoload 'jsx-mode "jsx-mode" "JSX mode" t)
