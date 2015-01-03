;;; package --Summary
;;; Code:
;;; Commentary
(require-package 'evil)
(require 'evil)
;; use emacs as a vim way
(evil-mode 1)

(setq evil-leader/in-all-states 1)
(require-package 'evil-leader)
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")


(define-key evil-insert-state-map "\C-c" 'evil-normal-state)
(define-key evil-visual-state-map "\C-c" 'evil-normal-state)
(define-key evil-normal-state-map "\C-e" 'evil-end-of-line)
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

(require-package 'evil-search-highlight-persist)
(require 'evil-search-highlight-persist)
(global-evil-search-highlight-persist t)
(evil-leader/set-key "q" 'evil-search-highlight-persist-remove-all)

(evil-leader/set-key "e" 'evil-ace-jump-word-mode) ; ,e for Ace Jump (word)
(evil-leader/set-key "l" 'evil-ace-jump-line-mode) ; ,l for Ace Jump (line)
(evil-leader/set-key "x" 'evil-ace-jump-char-mode) ; ,x for Ace Jump (char)



;; steal from spacemacs
(defun spacemacs/state-color-face (state)
  "Return the symbol of the face for the given STATE."
  (intern (format "spacemacs-%s-face" (symbol-name state))))

(defun spacemacs/defface-state-color (state color)
  "Define a face for the given STATE and background COLOR."
  (eval `(defface ,(spacemacs/state-color-face state) '((t ()))
           ,(format "%s state face." (symbol-name state))
           :group 'spacemacs))
  (set-face-attribute (spacemacs/state-color-face state) nil
                      :background color
                      :foreground (face-background 'mode-line)
                      :box (face-attribute 'mode-line :box)
                      :inherit 'mode-line))

(defun spacemacs/state-color (state)
  "Return the color string associated to STATE."
  (face-background (spacemacs/state-color-face state)))

(defun spacemacs/current-state-color ()
  "Return the color string associated to the current state."
  (face-background (spacemacs/state-color-face evil-state)))

(defun spacemacs/state-face (state)
  "Return the face associated to the STATE."
  (spacemacs/state-color-face state))

(defun spacemacs/current-state-face ()
  "Return the face associated to the current state."
  (let ((state (if (eq evil-state 'operator)
                   evil-previous-state
                 evil-state)))
    (spacemacs/state-color-face state)))

(defun spacemacs/set-state-faces ()
  "Define or set the state faces."
  (mapcar (lambda (x) (spacemacs/defface-state-color (car x) (cdr x)))
          '((normal . "DarkGoldenrod2")
            (insert . "chartreuse3")
            (emacs  . "SkyBlue2")
            (visual . "gray")
            (motion . "plum3")
            (lisp   . "HotPink1"))))
(spacemacs/set-state-faces)

(defun set-default-evil-emacs-state-cursor ()
  (setq evil-emacs-state-cursor `(,(spacemacs/state-color 'emacs) box)))
(defun set-default-evil-normal-state-cursor ()
  (setq evil-normal-state-cursor `(,(spacemacs/state-color 'normal) box)))
(defun set-default-evil-insert-state-cursor ()
  (setq evil-insert-state-cursor `(,(spacemacs/state-color 'insert) (bar . 2))))
(defun set-default-evil-visual-state-cursor ()
  (setq evil-visual-state-cursor `(,(spacemacs/state-color 'visual) (hbar . 2))))
(defun set-default-evil-motion-state-cursor ()
  (setq evil-motion-state-cursor `(,(spacemacs/state-color 'motion) box)))
(defun set-default-evil-lisp-state-cursor ()
  (setq evil-lisp-state-cursor `(,(spacemacs/state-color 'lisp) box)))
(defun evil-insert-state-cursor-hide ()
  (setq evil-insert-state-cursor `(,(spacemacs/state-color 'insert) (hbar . 0))))
(set-default-evil-emacs-state-cursor)
(set-default-evil-normal-state-cursor)
(set-default-evil-insert-state-cursor)
(set-default-evil-visual-state-cursor)
(set-default-evil-motion-state-cursor)
(set-default-evil-lisp-state-cursor)

(provide 'init-evil)
