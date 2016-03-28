;; ====================================================================
;;                                       Face
;; ====================================================================
;;(add-to-list 'default-frame-alist '(foreground-color . "black"))
;;(add-to-list 'default-frame-alist '(background-color . "white smoke"))

(set-default-font "DejaVu Sans Mono 11")
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset (font-spec :family "Noto Sans Mono CJK SC")))

;; stop syntax highlight
;; (global-font-lock-mode 0)

;; startup with window split
;; (split-window-right)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(ecb-options-version "2.40")
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(menu-bar-mode nil)
 '(tool-bar-mode nil))
;; highlight matches from searches
(setq isearch-highlight t)
(setq search-highlight t)

;; replace highlighted text with what I type rather than just
;; inserting at a point
(delete-selection-mode t)

;; highlight during searching
(setq query-replace-highlight t)

;;transparent
;;(set-frame-parameter (selected-frame) 'alpha '(85 50))

;; add a newline character at the end of the file
(setq require-final-newline t)

(setq-default indent-tabs-mode nil) ; always replace tabs with spaces
(setq-default tab-width 8) ; set tab width to 8 for all buffers

;;(global-linum-mode t)
;;(setq linum-format "%d ")
;; highlight the current line
(global-hl-line-mode t)

;; indicator for empty lines
(setq-default indicate-empty-lines t)

;;eliminate noise
;; disable startup message
;; (setq inhibit-startup-message t)
;; remove the noisy from scratch buffer
;; (setq initial-scratch-message nil)

;; column marker
(require 'column-marker)

