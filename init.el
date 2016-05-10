(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
;;(when (< emacs-major-version 24)
;; For important compatibility libraries like cl-lib
;;  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(setq inhibit-splash-screen t)

(setq initial-scratch-message nil)

;;(global-font-lock-mode 0)

;;(split-window-right)

(setq frame-title-format
  '("Emacs - " (buffer-file-name "%f"
    (dired-directory dired-directory "%b"))))

(line-number-mode 1)
(column-number-mode 1)

(load-library "paren")
(show-paren-mode 1)
(transient-mark-mode t)
(require 'paren)

;;(blink-cursor-mode 0)

(scroll-bar-mode 0)
;;(menu-bar-mode 0)
(tool-bar-mode 0)

(setq isearch-highlight t)
(setq search-highlight t)
(setq query-replace-highlight t)

(delete-selection-mode t)

;;(set-frame-parameter (selected-frame) 'alpha '(85 50))

(setq require-final-newline t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;;(global-linum-mode t)
;;(setq linum-format "%d ")

(global-hl-line-mode t)

(setq-default indicate-empty-lines t)

(require 'column-marker)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (nth 5 (file-attributes file))))
                  week))
      (message "%s" file)
      (delete-file file))))

;;(add-to-list 'default-frame-alist '(background-color . "black"))
;;(add-to-list 'default-frame-alist '(foreground-color . "white"))
;;(set-face-background hl-line-face "#000020")

;;(add-to-list 'custom-theme-load-path "~/.emacs.d/site-lisp")
(when (display-graphic-p)
	(load-theme 'misterioso t))

(global-set-key (kbd "C-c i") 
(lambda() (interactive)(org-babel-load-file "~/.emacs.d/init.org")))

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(cond
 ((string-equal system-type "darwin")   ; Mac OS X
  (progn
    (setq
     ns-command-modifier 'meta         ; Apple/Command key is Meta
	 ns-alternate-modifier nil         ; Option is the Mac Option key
	 ns-use-mac-modifier-symbols  nil  ; display standard Emacs (and not standard Mac) modifier symbols
	 ))
  )
 )

(defun jump-mark ()
  (interactive)
  (set-mark-command (point)))
(defun beginning-of-defun-and-mark ()
  (interactive)
  (push-mark (point))
  (beginning-of-defun))
(defun end-of-defun-and-mark ()
  (interactive)
  (push-mark (point))
  (end-of-defun))

(global-set-key "\^c\^b" 'beginning-of-defun-and-mark)
(global-set-key "\^c\^e" 'end-of-defun-and-mark)
(global-set-key "\^c\^j" 'jump-mark)
(global-set-key [S-f6] 'jump-mark)		;; jump from mark to mark

(setq select-active-regions nil)
(setq x-select-enable-primary t)
(setq x-select-enable-clipboard t)
(setq mouse-drag-copy-region t)

;;  (if(string-equal system-type "gnu/linux")   ; Linux!
;;      (
       (require (quote xclip))
       (xclip-mode 1)
;;      )()
;;        )

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
;; C-x C-0 restores the default font size

;; Inspired from http://tex.stackexchange.com/questions/166681/changing-language-of-flyspell-emacs-with-a-shortcut
;; (defun spell (choice)
;;    "Switch between language dictionaries."
;;    (interactive "cChoose:  (a) American | (f) Francais")
;;     (cond ((eq choice ?1)
;;            (setq flyspell-default-dictionary "american")
;;            (setq ispell-dictionary "american")
;;            (ispell-kill-ispell))
;;           ((eq choice ?2)
;;            (setq flyspell-default-dictionary "francais")
;;            (setq ispell-dictionary "francais")
;;            (ispell-kill-ispell))
;;           (t (message "No changes have been made."))) )

(define-key global-map (kbd "C-c s a") (lambda () (interactive) (ispell-change-dictionary "american")))
(define-key global-map (kbd "C-c s f") (lambda () (interactive) (ispell-change-dictionary "francais")))
(define-key global-map (kbd "C-c s r") 'flyspell-region)
(define-key global-map (kbd "C-c s b") 'flyspell-buffer)
(define-key global-map (kbd "C-c s s") 'flyspell-mode)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(custom-set-variables
 '(org-agenda-files (quote ("~/todo.org"))))

(require 'ox-latex)
(add-to-list 'org-latex-classes
             '("beamer"
               "\\documentclass\[presentation\]\{beamer\}"
               ("\\section\{%s\}" . "\\section*\{%s\}")
               ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
               ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))

;;(setq org-ctrl-k-protect-subtree t)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-hide-emphasis-markers t)

(font-lock-add-keywords 'org-mode 
			'(("^+\\([-*]\\) "
			   (0 (prog1 () (compose-region (match-beginning 1)(match-end 1) "•"))))))

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;;(let* ((variable-tuple (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
;;                             ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
;;                             ((x-list-fonts "Verdana")         '(:font "Verdana"))
;;                             ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
;;                             (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
;;       (headline           `(:inherit default :weight bold )))
;;
;;  (custom-theme-set-faces 'user
;;                          `(org-level-8 ((t (,@headline ,@variable-tuple))))
;;                          `(org-level-7 ((t (,@headline ,@variable-tuple))))
;;                          `(org-level-6 ((t (,@headline ,@variable-tuple))))
;;                          `(org-level-5 ((t (,@headline ,@variable-tuple))))
;;                          `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
;;                          `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
;;                          `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
;;                          `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
;;                          `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))

(setq org-export-babel-evaluate nil)
(setq org-confirm-babel-evaluate nil)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (sh . t)
     (python . t)
     (R . t)
     (ruby . t)
     (ocaml . t)
     (ditaa . t)
     (dot . t)
     (octave . t)
     (sqlite . t)
     (perl . t)
     (screen . t)
     (plantuml . t)
     (lilypond . t)
     (org . t)
     (makefile . t)
     ))
  (setq org-src-preserve-indentation t)

(add-to-list 'org-structure-template-alist
        '("S" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>"))

(add-to-list 'org-structure-template-alist
        '("m" "#+begin_src emacs-lisp\n\n#+end_src" "<src lang=\"emacs-lisp\">\n\n</src>"))

(add-to-list 'org-structure-template-alist
        '("r" "#+begin_src R :results output :session *R* :exports both\n\n#+end_src" "<src lang=\"R\">\n\n</src>"))

(add-to-list 'org-structure-template-alist
        '("R" "#+begin_src R :results output graphics :file (org-babel-temp-file \"figure\" \".png\") :exports both :width 600 :height 400 :session *R* \n\n#+end_src" "<src lang=\"R\">\n\n</src>"))

(add-to-list 'org-structure-template-alist
        '("RR" "#+begin_src R :results output graphics :file  (org-babel-temp-file (concat (file-name-directory (or load-file-name buffer-file-name)) \"figure-\") \".png\") :exports both :width 600 :height 400 :session *R* \n\n#+end_src" "<src lang=\"R\">\n\n</src>"))

(add-to-list 'org-structure-template-alist
        '("p" "#+begin_src python :results output :exports both\n\n#+end_src" "<src lang=\"python\">\n\n</src>"))

(add-to-list 'org-structure-template-alist
        '("P" "#+begin_src python :results output :session *python* :exports both\n\n#+end_src" "<src lang=\"python\">\n\n</src>"))

(add-to-list 'org-structure-template-alist
        '("b" "#+begin_src sh :results output :exports both\n\n#+end_src" "<src lang=\"sh\">\n\n</src>"))

(add-to-list 'org-structure-template-alist
        '("B" "#+begin_src sh :session foo :results output :exports both \n\n#+end_src" "<src lang=\"sh\">\n\n</src>"))

(add-to-list 'org-structure-template-alist
        '("g" "#+begin_src dot :results output graphics :file \"/tmp/graph.pdf\" :exports both
   digraph G {
      node [color=black,fillcolor=white,shape=rectangle,style=filled,fontname=\"Helvetica\"];
      A[label=\"A\"]
      B[label=\"B\"]
      A->B
   }\n#+end_src" "<src lang=\"dot\">\n\n</src>"))

(global-set-key (kbd "C-c S-t") 'org-babel-execute-subtree)

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images) 
(add-hook 'org-mode-hook 'org-display-inline-images)
(add-hook 'org-mode-hook 'org-babel-result-hide-all)

;;(custom-set-variables
;;  '(eclim-eclipse-dirs '("~/eclipse"))
;;  '(eclim-executable "~/eclipse/eclim")
;;  '(eclimd-executable "~/eclipse/eclimd"))
;;(require 'eclim)
;;(require 'eclimd)
;;(global-eclim-mode)
;;(setq help-at-pt-display-when-idle t)
;;(setq help-at-pt-timer-delay 0.1)
;;(help-at-pt-set-timer)

;;(eval-when-compile (require 'cl))
;;
;;(defun set-font (english chinese english-size chinese-size)
;;(set-face-attribute 'default nil :font
;;(format "%s:pixelsize=%d" english english-size))
;;(dolist (charset '(kana han symbol cjk-misc bopomofo))
;;(set-fontset-font (frame-parameter nil 'font) charset
;;(font-spec :family chinese :size chinese-size))))
;;;;
;;;;(ecase system-type
;;;;(gnu/linux
;;(set-face-bold-p 'bold nil)
;;(set-face-underline-p 'bold nil)
;;(set-font "DejaVu Sans Mono" "Noto Sans Mono CJK SC" 15 14)
;;)
;;(darwin
;;(set-font "monofur" "STHeiti" 20 20)))

;;================================================================
;; use single font for all
;;================================================================
;;(set-face-attribute 'default nil :font "Noto Sans Mono CJK SC-13" )
(set-frame-font "DejaVu Sans Mono-11" nil t)

;;(setq gnus-select-method
;;      '(nnimap "imap.exmail.qq.com"
;;               (nnimap-stream ssl)))
;;(setq send-mail-function    'smtpmail-send-it
;;      smtpmail-smtp-server  "smtp.exmail.qq.com"
;;      smtpmail-stream-type  'ssl
;;      smtpmail-smtp-service 465)

(require 'go-mode-autoloads)
(require 'go-direx)

(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
(require 'golint)

(add-hook 'go-mode-hook 
          (lambda()
            ;; tab size is 4
            (setq tab-width 4)
            (setq gofmt-command "goimports")
            (add-hook 'before-save-hook 'gofmt-before-save)
            ;; C-c c compile
            (setq compile-command "go generate && go build -v && go test -v && go vet")
            ;;(define-key go-mode-map "\C-cc" 'compile)
            (define-key go-mode-map "<f8>" 'compile)
            ;; C-c C-c 
            (define-key go-mode-map "\C-c\C-c" 'comment-region)
            ;; C-u C-c C-c 
            (define-key go-mode-map "\C-u\C-c\C-c" 'uncomment-region)
            (load-file (concat (getenv "GOPATH") "/src/golang.org/x/tools/cmd/oracle/oracle.el"))
            (define-key go-mode-map "<f5>" 'godef-jump)
            (define-key go-mode-map "<f6>" 'godef-jump-other-window)
            ))

(require 'go-eldoc)
(add-hook 'go-mode-hook 'go-eldoc-setup)
(set-face-attribute 'eldoc-highlight-function-argument nil
                    :underline t :foreground "darkgreen"
                    :weight 'bold)

;;(setq go-oracle-command "/usr/bin/oracle")

(require 'go-rename)

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat (getenv "GOPATH") "/src/github.com/auto-complete/auto-complete/dict"))
(require 'go-autocomplete)
(ac-config-default)
(ac-set-trigger-key "TAB")

(require 'yasnippet)
(add-to-list 'yas-snippet-dirs (concat (getenv "GOPATH") "/src/github.com/atotto/yasnippet-golang"))
(yas-reload-all)
(add-hook 'go-mode-hook #'yas-minor-mode)
;;(yas-global-mode 1)

(require 'autoinsert)
(setq auto-insert-directory "~/.emacs.d/_templates/")
(define-auto-insert "\\.go\\'" "T.go")
(define-auto-insert "\\test.go\\'" "T_test.go")
(add-hook 'find-file-hooks 'auto-insert)

;;(require 'helm-config)
;;(helm-mode 1)
;;
;;(global-set-key (kbd "M-x")                          'undefined)
;;(global-set-key (kbd "M-x")                          'helm-M-x)
;;(global-set-key (kbd "M-y")                          'helm-show-kill-ring)
;;(global-set-key (kbd "C-c f")                        'helm-recentf)
;;(global-set-key (kbd "C-x C-f")                      'helm-find-files)
;;(global-set-key (kbd "C-c <SPC>")                    'helm-all-mark-rings)
;;(global-set-key (kbd "C-x r b")                      'helm-filtered-bookmarks)
;;(global-set-key (kbd "C-h r")                        'helm-info-emacs)
;;(global-set-key (kbd "C-:")                          'helm-eval-expression-with-eldoc)
;;(global-set-key (kbd "C-,")                          'helm-calcul-expression)
;;(global-set-key (kbd "C-h i")                        'helm-info-at-point)
;;(global-set-key (kbd "C-x C-d")                      'helm-browse-project)
;;(global-set-key (kbd "<f1>")                         'helm-resume)
;;(global-set-key (kbd "C-h C-f")                      'helm-apropos)
;;(global-set-key (kbd "C-h a")                        'helm-apropos)
;;(global-set-key (kbd "<f5> s")                       'helm-find)
;;(global-set-key (kbd "<f2>")                         'helm-execute-kmacro)
;;(global-set-key (kbd "C-c g")                        'helm-gid)
;;(global-set-key (kbd "C-c i")                        'helm-imenu-in-all-buffers)
;;;;(global-set-key (kbd "<f11> o")                      'helm-org-agenda-files-headings)
;;(global-set-key (kbd "C-s")                          'helm-occur)
;;(define-key global-map [remap jump-to-register]      'helm-register)
;;(define-key global-map [remap list-buffers]          'helm-buffers-list)
;;(define-key global-map [remap dabbrev-expand]        'helm-dabbrev)
;;(define-key global-map [remap find-tag]              'helm-etags-select)
;;(define-key global-map [remap xref-find-definitions] 'helm-etags-select)
;;(define-key global-map (kbd "M-g a")                 'helm-do-grep-ag)
;;(define-key global-map (kbd "M-g g")                 'helm-grep-do-git-grep)

(ido-mode 1)
(ido-everywhere 1)
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(require 'smex)

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)                 ; optional

;; auctex
;; (load "auctex.el" nil t t)
;; (load "preview-latex.el" nil t t)
;;(setq TeX-output-view-style
;;      (quote
;;       (("^pdf$" "." "evince -f %o")
;;        ("^html?$" "." "google-chrome-stable %o"))))

(setq load-path
      (append (list "~/.emacs.d/site-lisp")
                      load-path))
(require 'unicad)

(server-start)

(setq text-mode-hook '(lambda()  
                        (auto-fill-mode t) 
                        )) 

;;'(require 'ecb-autoloads)

(add-to-list 'auto-mode-alist '("\\.jsp\\'" . html-mode))

;;(require 'popwin)
;;(popwin-mode 1)

(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
(autoload 'jsx-mode "jsx-mode" "JSX mode" t)

(setq tramp-default-method "ssh")

(setq json-reformat:pretty-string? 1)
