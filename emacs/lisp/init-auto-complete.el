(require-package 'company)
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(require-package 'company-anaconda)
(require 'company-anaconda)
(add-to-list 'company-backends 'company-anaconda)
(add-hook 'python-mode-hook 'anaconda-mode)


(after-load 'company
  (setq
   ;; never start auto-completion unless I ask for it
   company-idle-delay 0.1
   ;; autocomplete right after '.'
   company-minimum-prefix-length 0
   ;; remove echo delay
   company-echo-delay 0
   ;; don't complete in certain modes
   company-global-modes '(not git-commit-mode)))

(require-package 'slime-company)
(require 'slime-company)
(slime-setup '(slime-company))

(define-key company-active-map (kbd "\C-n") 'company-select-next)
(define-key company-active-map (kbd "\C-p") 'company-select-previous)
(define-key company-active-map (kbd "\C-q") 'company-show-doc-buffer)
(define-key company-active-map (kbd "<tab>") 'company-complete)

;; tern-mode
(require-package 'company-tern)
(require 'company-tern)
(add-to-list 'company-backends 'company-tern)

(require-package 'company-auctex)
(require 'company-auctex)
(company-auctex-init)

(require-package 'company-math)
(add-to-list 'company-backends 'company-math-symbols-unicode)

(provide 'init-auto-complete)
