;;;; ORG MODE
(custom-set-variables
 '(org-agenda-files (quote ("~/todo.org"))))

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
;;(setq org-ctrl-k-protect-subtree t)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
