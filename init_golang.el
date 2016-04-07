;; go mode
(require 'go-mode-autoloads)
(require 'go-direx)

;; golint like gofmt, but for checking style
(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
(require 'golint)

(add-hook 'go-mode-hook 
          (lambda()
            ;; tab size is 4
            (setq tab-width 4)
            (setq gofmt-command "goimports")
            (add-hook 'before-save-hook 'gofmt-before-save)
            ;; C-c c compile
            (setq compile-command "go test -v")
            ;;(define-key go-mode-map "\C-cc" 'compile)
            (define-key go-mode-map "<f5>" 'godef-jump)
            (define-key go-mode-map "<f6>" 'godef-jump-other-window)
            (define-key go-mode-map "<f8>" 'compile)
            ;; C-c C-c 
            (define-key go-mode-map "\C-c\C-c" 'comment-region)
            ;; C-u C-c C-c 
            (define-key go-mode-map "\C-u\C-c\C-c" 'uncomment-region)
            (load-file (concat (getenv "GOPATH") "/src/golang.org/x/tools/cmd/oracle/oracle.el"))
            ))

(require 'go-eldoc)
(add-hook 'go-mode-hook 'go-eldoc-setup)
(set-face-attribute 'eldoc-highlight-function-argument nil
                    :underline t :foreground "darkgreen"
                    :weight 'bold)

(setq go-oracle-command "/usr/bin/oracle")

(require 'go-rename)

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat (getenv "GOPATH") "/src/github.com/auto-complete/auto-complete/dict"))
(require 'go-autocomplete)
(ac-config-default)
(ac-set-trigger-key "TAB")


(require 'yasnippet)
(add-to-list 'yas-snippet-dirs (concat (getenv "GOPATH") "/src/github.com/atotto/yasnippet-golang"))
(yas-global-mode 1)
;; just don't mess up the term-mode
(add-hook 'term-mode-hook (lambda () (yas-minor-mode -1)))

(require 'autoinsert)
(setq auto-insert-directory "~/.emacs.d/_templates/")
(define-auto-insert "\\.go\\'" "T.go")
(define-auto-insert "\\test.go\\'" "T_test.go")
(add-hook 'find-file-hooks 'auto-insert)

(setq compilation-window-height 10)
