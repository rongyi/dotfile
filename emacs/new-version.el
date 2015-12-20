;; basic
(setq inhibit-startup-message t)
(setq initial-scratch-message "happy hacking, ry")
(setq inhibit-startup-echo-area-message "rongyi")
;; custom to a seprate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; no backup file
(setq make-backup-files nil)
;; remember the cursor position
(setq save-place-file (expand-file-name "cursor.save" user-emacs-directory))
(setq-default save-place t)
(require 'saveplace)
;; smooth scrolling
(setq scroll-margin 5
scroll-conservatively 9999
scroll-step 1)
;; full screen if needed
;;(toggle-frame-fullscreen) 

(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
;; yes-or-no-p ==> y-or-n
(fset 'yes-or-no-p 'y-or-no-p)
;; auto indent
(define-key global-map (kbd "RET") 'newline-and-indent)


;; font
(set-frame-font "Source Code Pro for Powerline 10")
(add-to-list 'default-frame-alist '(font . "Source Code Pro for Powerline 10"))
(add-hook 'after-make-frame-functions
	  (lambda (new-frame)
	    (set-fontset-font "fontset-default" 'han '("方正清刻本悦宋简体" . "unicode-bmp"))
	    ))
(set-fontset-font "fontset-default" 'han '("方正清刻本悦宋简体" . "unicode-bmp"))

(global-set-key (kbd "M-/") 'hippie-expand)
(load-theme 'leuven)



;; install package
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(require 'package)
(package-initialize)

;; install evil mode

(defun require-install-nessary (pkg)
  (condition-case nil
      (require pkg)
    (error
     (package-refresh-contents)
     (package-install pkg))))

;; evil setting
(require-install-nessary 'evil)
(evil-mode 1)

(eval-after-load 'evil
  '(progn
     (define-key evil-insert-state-map "\C-c" 'evil-normal-state)
     (define-key evil-visual-state-map "\C-c" 'evil-normal-state)
     (define-key evil-normal-state-map "\C-e" 'evil-end-of-line)
     (define-key evil-normal-state-map "\C-a" 'evil-beginning-of-line)
     (define-key evil-insert-state-map "\C-a" 'evil-beginning-of-line)
     (define-key evil-insert-state-map "\C-e" 'end-of-line)
     (define-key evil-insert-state-map "\C-s" 'save-buffer)
     (define-key evil-insert-state-map "\C-k" 'kill-line)
     (define-key evil-normal-state-map "\C-s" 'save-buffer)
     (define-key evil-visual-state-map "\C-e" 'evil-end-of-line)
     (define-key evil-motion-state-map "\C-e" 'evil-end-of-line)
     (define-key evil-normal-state-map "\C-f" 'evil-scroll-page-down)
     (define-key evil-insert-state-map "\C-f" 'forward-char)
     (define-key evil-insert-state-map "\C-f" 'evil-forward-char)
     (define-key evil-normal-state-map "\C-b" 'evil-scroll-page-up)
     (define-key evil-insert-state-map "\C-b" 'backward-char)
     (define-key evil-visual-state-map "\C-b" 'evil-backward-char)
     (define-key evil-normal-state-map "\C-d" 'evil-delete-char)
     (define-key evil-insert-state-map "\C-d" 'evil-delete-char)
     (define-key evil-visual-state-map "\C-d" 'evil-delete-char)
     (define-key evil-normal-state-map "\C-n" 'evil-next-line)
     (define-key evil-insert-state-map "\C-n" 'evil-next-line)
     (define-key evil-visual-state-map "\C-n" 'evil-next-line)
     (define-key evil-normal-state-map "\C-p" 'evil-previous-line)
     (define-key evil-insert-state-map "\C-p" 'evil-previous-line)
     (define-key evil-visual-state-map "\C-p" 'evil-previous-line)
     (define-key evil-normal-state-map "\C-w" 'evil-delete)
     (define-key evil-insert-state-map "\C-w" 'evil-delete)
     (define-key evil-visual-state-map "\C-w" 'evil-delete)
     (setq evil-emacs-state-cursor '("red" box))
     (setq evil-normal-state-cursor '("DarkGoldenrod2" box))
     (setq evil-visual-state-cursor '("gray" box))
     (setq evil-insert-state-cursor '("chartreuse3" bar))
     (setq evil-replace-state-cursor '("red" bar))
     (setq evil-operator-state-cursor '("red" hollow))))

;; evil leader
(require-install-nessary 'evil-leader)
(setq evil-leader/in-all-states 1)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
;; avy
(require-install-nessary 'avy)
(evil-leader/set-key "f" 'avy-goto-word-or-subword-1)

;; expand-region
(require-install-nessary 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
;; helm
(require-install-nessary 'helm)
(require-install-nessary 'helm-config)
(require-install-nessary 'helm-misc)
(require-install-nessary 'helm-locate)
(require-install-nessary 'projectile)
(setq helm-quick-update t)
(setq helm-bookmark-show-location t)
(setq helm-buffers-fuzzy-matching t)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; magit
(require-install-nessary 'magit)
(evil-leader/set-key "g" 'magit-status)


;; powerline
(require-install-nessary 'powerline)
(setq powerline-default-separator 'wave)
(powerline-center-evil-theme)
(display-time-mode t)

;; flycheck
(require-install-nessary 'flycheck)
(require-install-nessary 'flycheck-pos-tip)
(add-hook 'after-init-hook #'global-flycheck-mode)
(global-flycheck-mode t)

(with-eval-after-load 'flycheck
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))
  (setq flycheck-checkers (delq 'html-tidy flycheck-checkers))
  (setq flycheck-standard-error-navigation nil))

;; flycheck errors on a tooltip (doesnt work on console)
(when (display-graphic-p (selected-frame))
  (with-eval-after-load 'flycheck
    (custom-set-variables
     '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))))


;; silver searcher
(require-install-nessary 'ag)
(evil-leader/set-key "s" 'ag)


;; color variable
(require-install-nessary 'color-identifiers-mode)
(global-color-identifiers-mode)

;; rainbow delimeters
(require-install-nessary 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; smartpare

(require-install-nessary 'smartparens-config)
(add-hook 'prog-mode-hook #'smartparens-mode)

;; company
(require-install-nessary 'company)
(require-install-nessary 'company-statistics)
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'after-init-hook 'company-statistics-mode)
(with-eval-after-load 'company
  '(setq company-idle-delay 0.2
	company-minimum-prefix-length 2
	company-require-match nil
	company-dabbrev-ignore-case nil
	company-dabbrev-downcase nil
	(defvar-local company-fci-mode-on-p nil)))
 
;; python auto complete
(require-install-nessary 'company-anaconda)
(add-to-list 'company-backends 'company-anaconda)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'eldoc-mode)
;; js
(require-install-nessary 'js2-mode)
(require-install-nessary 'company-tern)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(add-to-list 'company-backends 'company-tern)
(setq company-tern-property-marker "")
(setq company-tern-meta-as-single-line t)


