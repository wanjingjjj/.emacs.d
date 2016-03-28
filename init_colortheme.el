;;====================================
;; color theme
;;====================================
(add-to-list 'custom-theme-load-path "~/.emacs.d/site-lisp")
(when (display-graphic-p)
	(load-theme 'meacupla t))
