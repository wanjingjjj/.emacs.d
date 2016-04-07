;;; init_font.el --- font config                     -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Wan Jing

;; Author: Wan Jing <wanjing@laputa.lan>
;; Keywords: faces

(eval-when-compile (require 'cl))

(defun set-font (english chinese english-size chinese-size)
(set-face-attribute 'default nil :font
(format "%s:pixelsize=%d" english english-size))
(dolist (charset '(kana han symbol cjk-misc bopomofo))
(set-fontset-font (frame-parameter nil 'font) charset
(font-spec :family chinese :size chinese-size))))

;;(ecase system-type
;;(gnu/linux
(set-face-bold-p 'bold nil)
(set-face-underline-p 'bold nil)
(set-font "DejaVu Sans Mono" "Noto Sans Mono CJK SC" 15 18)
;;)
;;(darwin
;;(set-font "monofur" "STHeiti" 20 20)))


