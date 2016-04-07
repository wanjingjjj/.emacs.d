;;; init_remap_key.el --- remap key under emacs      -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Wan Jing

;; Author: Wan Jing <wanjing@laputa.lan>
;; Keywords: extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(global-set-key (kbd "<f1>") 'find-file)
(global-set-key (kbd "<f2>") 'ido-switch-buffer)
(global-set-key (kbd "<f3>") 'replace-regexp)
(global-set-key (kbd "<f4>") 'kill-buffer)



;;; init_remap_key.el ends here
