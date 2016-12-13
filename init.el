(require 'package)
;;(add-to-list 'package-archives
;;             '("melpa" . "https://melpa.org/packages/"))
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
;;(when (< emacs-major-version 24)
;; For important compatibility libraries like cl-lib
;;  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(display-time-mode 1)

(setq inhibit-splash-screen t)

(setq initial-major-mode 'text-mode)
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

;;(global-hl-line-mode t)

(setq-default indicate-empty-lines t)

;;(require 'column-marker)

(setq make-backup-files nil)
;;(setq backup-directory-alist
;;      `((".*" . ,temporary-file-directory)))
;;(setq auto-save-file-name-transforms
;;      `((".*" ,temporary-file-directory t)))
;;(message "Deleting old backup files...")
;;(let ((week (* 60 60 24 7))
;;      (current (float-time (current-time))))
;;  (dolist (file (directory-files temporary-file-directory t))
;;    (when (and (backup-file-name-p file)
;;               (> (- current (float-time (nth 5 (file-attributes file))))
;;                  week))
;;      (message "%s" file)
;;      (delete-file file))))

;;(add-to-list 'default-frame-alist '(background-color . "black"))
;;(add-to-list 'default-frame-alist '(foreground-color . "#99CF96"))
;;(set-face-background hl-line-face "#505050")

(add-to-list 'custom-theme-load-path "~/.emacs.d/site-lisp")
(when (display-graphic-p)
	(load-theme 'misterioso t))

;;(defun random-color-theme () (interactive)
;;  (let ((chosen-theme
;;         (nth
;;          (random
;;           (length (mapcar 'symbol-name (custom-available-themes))))
;;          (custom-available-themes))))
;;    (message "Theme: %s" chosen-theme)
;;    (load-theme chosen-theme t nil)))
;;
;;(defun show-me-the-colors ()  (interactive) (loop do (random-color-theme) (sit-for 3)))
;;(random-color-theme)
;;(setq color-theme-is-cumulative 'false)

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
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cfs--current-profile "profile2" t)
 '(custom-safe-themes
   (quote
    ("45dd0598e9413dc34c020bd928245f89995024df8d032c0fa671148fef21a1e0" default)))
 '(org-agenda-files (quote ("~/todo.org"))))

;;(setq org-log-done 'time)

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
        '("p" "#+begin_src python :results output :exports both\n\n#+end_src" "<src lang=\"python\">\n\n</src>"))

(add-to-list 'org-structure-template-alist
        '("P" "#+begin_src python :results output :session *python* :exports both\n\n#+end_src" "<src lang=\"python\">\n\n</src>"))

(add-to-list 'org-structure-template-alist
        '("b" "#+begin_src sh :results output :exports both\n\n#+end_src" "<src lang=\"sh\">\n\n</src>"))

(add-to-list 'org-structure-template-alist
        '("B" "#+begin_src sh :session foo :results output :exports both \n\n#+end_src" "<src lang=\"sh\">\n\n</src>"))

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
;;(set-font "DejaVu Sans Mono" "WenQuanYi Zen Hei Mono" 15 16)
;;)
;;(darwin
;;(set-font "monofur" "STHeiti" 20 20)))
(require 'chinese-fonts-setup)
;; 让 chinese-fonts-setup 随着 emacs 自动生效。
(chinese-fonts-setup-enable)

;;================================================================
;; use single font for all
;;================================================================
;;(set-face-attribute 'default nil :font "Noto Sans Mono CJK SC-13" )
;;(set-frame-font "Consolas-13" nil t)

;;(setq gnus-select-method
;;      '(nnimap "imap.exmail.qq.com"
;;               (nnimap-stream ssl)))
;;(setq send-mail-function    'smtpmail-send-it
;;      smtpmail-smtp-server  "smtp.exmail.qq.com"
;;      smtpmail-stream-type  'ssl
;;      smtpmail-smtp-service 465)

(defvar GO_PKG_PATH "/home/wanjing/gopkg")
(setenv "PATH" (concat (getenv "PATH") ":" GO_PKG_PATH "/bin"))
(setq exec-path (cons (concat GO_PKG_PATH "/bin") exec-path ))

(require 'go-mode)
(require 'go-direx)

(add-to-list 'load-path (concat GO_PKG_PATH "/src/github.com/golang/lint/misc/emacs"))
(require 'golint)

(add-hook 'go-mode-hook 
          (lambda()
            ;; tab size is 4
            (setq tab-width 4)
            (setq gofmt-command "goimports")
            (add-hook 'before-save-hook 'gofmt-before-save)
            ;; C-c c compile
            (setq compile-command "go build -v && go vet")
            (define-key go-mode-map "\C-cc" 'compile)
            ;; C-c C-c 
            (define-key go-mode-map "\C-c\C-c" 'comment-region)
            ;; C-u C-c C-c 
            (define-key go-mode-map "\C-u\C-c\C-c" 'uncomment-region)
            (load-file (concat GO_PKG_PATH "/src/golang.org/x/tools/cmd/oracle/oracle.el"))
            ))

(require 'go-eldoc)
(add-hook 'go-mode-hook 'go-eldoc-setup)
(set-face-attribute 'eldoc-highlight-function-argument nil
                    :underline t :foreground "darkgreen"
                    :weight 'bold)

(setq go-oracle-command "/home/wanjing/gopkg/bin/oracle")

(require 'go-rename)

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat GO_PKG_PATH "/src/github.com/auto-complete/auto-complete/dict"))
(require 'go-autocomplete)
(ac-config-default)
(ac-set-trigger-key "TAB")

(require 'helm-config)
(helm-mode 1)
;;
(global-set-key (kbd "M-x")                          'helm-M-x)
(global-set-key (kbd "C-x C-F")                      'helm-find-files)
;; auto resize the completion window based on the candidates number
;;(helm-autoresize-mode 1)

;;(ido-mode 1)
;;(ido-everywhere 1)
;;(require 'ido-ubiquitous)
;;(ido-ubiquitous-mode 1)

;;(smex-initialize)
;;(global-set-key (kbd "M-x") 'smex)
;;(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;;(require 'smex)

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

(require 'multiple-cursors)
;;(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;;(global-set-key (kbd "C->") 'mc/mark-next-like-this)
;;(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;;(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;(load "mime-setup")

(require 'mu4e)

;; default
(setq mu4e-maildir (expand-file-name "~/mail"))

;;(setq mu4e-drafts-folder "/[Gmail].Drafts")
;;(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
;;(setq mu4e-trash-folder  "/[Gmail].Trash")

;; don't save message to Sent Messages, GMail/IMAP will take care of this
;;(setq mu4e-sent-messages-behavior 'delete)

;; setup some handy shortcuts
;;(setq mu4e-maildir-shortcuts
;;      '(("/INBOX"             . ?i)
;;        ("/[Gmail].Sent Mail" . ?s)
;;        ("/[Gmail].Trash"     . ?t)))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "getmail")

;; something about ourselves
;; I don't use a signature...
(setq
 user-mail-address "wanjing@creditx.com"
 user-full-name  "万晶"
 ;; message-signature
 ;;  (concat
 ;;    "Foo X. Bar\n"
 ;;    "http://www.example.com\n")
)

;; sending mail -- replace USERNAME with your gmail username
;; also, make sure the gnutls command line utils are installed
;; package 'gnutls-bin' in Debian/Ubuntu, 'gnutls' in Archlinux.

(require 'smtpmail)

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.exmail.qq.com"
      smtpmail-stream-type 'ssl
      smtpmail-smtp-service 465
)

(require 'cal-china-x)
(setq mark-holidays-in-calendar t)
(setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
(setq calendar-holidays cal-china-x-important-holidays)

(autoload 'chinese-wbim-use-package "chinese-wbim" "Another emacs input method")
;; Tooltip 暂时还不好用
(setq chinese-wbim-use-tooltip nil)

(register-input-method
 "chinese-wbim" "euc-cn" 'chinese-wbim-use-package
 "五笔" "汉字五笔输入法" "wb.txt")

;; 用 ; 暂时输入英文
(require 'chinese-wbim-extra)
(global-set-key ";" 'chinese-wbim-insert-ascii)

;设置默认输入法
(setq default-input-method 'chinese-wbim)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
