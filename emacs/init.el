;;; package -- Summary
;;; Commentary:

;;; Code:


;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; a newbie user should keep
(menu-bar-mode -1)
;; Turn on the menu bar for exploring new modes
(global-set-key (kbd "C-<f10>") 'menu-bar-mode)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(defconst *is-a-mac* (eq system-type 'darwin))
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-proxies)

;; snippet
(require-package 'yasnippet)
(yas-global-mode 1)
;; enhance M-x
(require-package 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; auto complete
(require-package 'auto-complete)
(require 'auto-complete-config)
(set-default 'ac-sources
             '(ac-source-imenu
               ac-source-dictionary
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-words-in-all-buffer))
(ac-config-default)
(global-auto-complete-mode t)

;; helm
(require-package 'helm)
(require 'helm-config)
(setq helm-ff-smart-completion t)
(helm-mode 1)

;; smartparens
(require-package 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode t)

;; autopair, not comfortable with
;; (require-package 'autopair)
;; (autopair-global-mode)

;; evil mode, uncomment if you like
;; (add-to-list 'load-path "~/.emacs.d/github/evil")
;; (require 'evil)
;; (evil-mode 1)

;; expand region
(require-package 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
;; ace jumpp mode
(require-package 'ace-jump-mode)
(autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
;; you can select the key you prefer to
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
;; enable a more powerful jump back function from ace jump mode
(autoload 'ace-jump-mode-pop-mark "ace-jump-mode" "Ace jump back:-)" t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;;If you use viper mode :
;; (define-key viper-vi-global-user-map (kbd "SPC") 'ace-jump-mode)
;;If you use evil
;; (define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)

;; ag front
(require-package 'ag)

;; flycheck
(require-package 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
;; popup items
(require 'popup)

;; slime
(require-package 'slime)
(setq inferior-lisp-program "/usr/bin/clisp") ; your Lisp system
(slime-setup '(slime-fancy))
;; rainbow delimeter
(require-package 'rainbow-delimiters)
;;(global-rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; web-mode
(require-package 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

;; jedi
(require-package 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)                      ; optional
(setq jedi:complete-on-dot t)                 ; optional


;; ==============ui conf==================
;; (setq-default cursor-type 'bar)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)


;; disable startup screen
(setq inhibit-startup-screen t)
;; disable echo area message
(setq inhibit-startup-echo-area-message t)
;; disable start up message
(setq inhibit-startup-message t)
(setq-default inhibit-scratch-message ";; Hello master ry")

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; make the fringe (gutter) smaller
;; the argument is a width in pixels (the default is 8)
(if (fboundp 'fringe-mode)
    (fringe-mode 4))

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)
;; just create file if the file does not exist
(setq confirm-nonexistent-file-or-buffer nil)

;; highlight the current line
(global-hl-line-mode +1)
(scroll-bar-mode -1)
(require-package 'volatile-highlights)
;; (volatile-highlights-mode t)
;; (diminish 'volatile-highlights-mode)
;; ==============ui conf end==============
;; Death to the tabs!  However, tabs historically indent to the next
;; 8-character offset; specifying anything else will cause *mass*
;; confusion, as it will change the appearance of every existing file.
;; In some cases (python), even worse -- it will change the semantics
;; (meaning) of the program.
;;
;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)            ;; but maintain correct appearance
(show-paren-mode 1)
;; delete the selection with a keypress
(delete-selection-mode t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)


;; diminish keeps the modeline tidy
(require-package 'diminish)

;; use shift + arrow keys to switch between visible buffers
(require-package 'windmove)
(windmove-default-keybindings)

;; clean up obsolete buffers automatically
(require-package 'midnight)
;; smarter kill-ring navigation
(require-package 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
(global-set-key (kbd "s-y") 'browse-kill-ring)
;; whitespace-mode config
(require-package 'whitespace-cleanup-mode)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face tabs empty trailing lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)
;;----------------------------------------------------------------------------
;; Some basic preferences
;;----------------------------------------------------------------------------
(setq-default
 blink-cursor-delay 0
 blink-cursor-interval 0.4
 bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
 buffers-menu-max-size 30
 case-fold-search t
 column-number-mode t
 compilation-scroll-output t
 delete-selection-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 grep-highlight-matches t
 grep-scroll-output t
 indent-tabs-mode nil
 line-spacing 0.2
 make-backup-files nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 show-trailing-whitespace t
 tooltip-delay 0.5
 truncate-lines t
 truncate-partial-width-windows nil
 visible-bell t)

(require 'whitespace-cleanup-mode)
(global-whitespace-cleanup-mode t)
(add-hook 'before-save-hook 'whitespace-cleanup)

(transient-mark-mode t)

(require-package 'undo-tree)
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") 'hippie-expand)
(require 'init-utils)
(require 'init-css)
(require 'init-javascript)

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
(require 'init-themes)
;; set font
;(setq default-frame-alist '((font . "Source Code Pro for Powerline 12")))
;(setq default-frame-alist '((font . "Consolas for Powerline 12")))

(set-frame-font "Source Code Pro for Powerline 12")
(set-fontset-font "fontset-default" 'han '("Microsoft JhengHei" . "unicode-bmp"))


;; replace default buffer list with the excellent ibuffer
(require-package 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; mock some IDE shift-enter action
(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode"
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(global-set-key (kbd "M-o") 'smart-open-line)

(defun smart-open-line-above ()
  "Insert an empty line before the current line.
Position the cursor at its beginning, according to the current mode"
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))
(global-set-key (kbd "M-O") 'smart-open-line-above)

;; copy current file name(with absolute path) to clipboard
(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;; open current file with external program
(defun open-with (arg)
  "Open visited file in default external program.
With a preifx ARG always prompt for command to use."
  (interactive "p")
  (when buffer-file-name
    (shell-command (concat
                    (cond
                     ((and (not arg) (eq system-type 'darwin)) "open")
                     ((and (not arg) (member system-type '(gnu gnu/linux gnu/kfreebsd))) "xdg-open")
                     (t (read-shell-command "Open current file with: ")))
                    " "
                    (shell-quote-argument buffer-file-name)))))
(global-set-key (kbd "C-c o") 'open-with)

(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))
;; indent region or the whole buffer if you does not select any region
(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (indent-buffer)
        (message "Indented buffer.")))))
(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)

(defun indent-defun ()
  "Indent the current defun."
  (interactive)
  (save-excursion
    (mark-defu)
    (indent-region (region-beginning) (region-end))))
(global-set-key (kbd "C-M-z") 'indent-defun)

;; search google
(defun google ()
  "Google the selected region if any, display a  query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://203.208.46.147/search?ie=utf-8&oe=utf08&q=" ;; using google stable
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Google: "))))))
(global-set-key (kbd "C-c g") 'google)
;; auto indent
(electric-indent-mode +1)

(defun visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer "*ansi-term*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (ansi-term (getenv "SHELL")))
    (switch-to-buffer-other-window "*ansi-term*")))

(global-set-key (kbd "C-c t") 'visit-term-buffer)

;; kill other buffer
(defun kill-other-buffers ()
  "Kill all buffers but the current one.
Don't mess with special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))
(global-set-key (kbd "C-c k") 'kill-other-buffers)

;;goto previous window
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1)))
;; change font size
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-+") 'text-scale-increase)

;; move line up and down
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key [(control shift up)] 'move-line-up)
(global-set-key [(control shift down)] 'move-line-down)
;; delete file and this file's buffer
(defun delete-file-and-buffer ()
  "Kill the current buffer and deltetes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))
(global-set-key (kbd "C-c D") 'delete-file-and-buffer)
;; keep recent open file list
(require 'recentf)
(setq recentf-max-saved-items 100)
(setq recentf-max-menu-items 15)
(recentf-mode +1)

;; mock Vim '%' function
(defun forward-or-backward-sexp (&optional arg)
  "Go to the matching parenthesis character if one is adjacent to point."
  (interactive "^p")
  (cond ((looking-at "\\s(") (forward-sexp arg))
        ((looking-back "\\s)" 1) (backward-sexp arg))
        ;; Now, try to succeed from inside of a bracket
        ((looking-at "\\s)") (forward-char) (backward-sexp arg))
        ((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))
(global-set-key (kbd "C-%") 'forward-or-backward-sexp)

;; set frame title with file name
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
;; kill line backward
(global-set-key (kbd "M-<backspace>") (lambda ()
                                        (interactive)
                                        (kill-line 0)
                                        (indent-according-to-mode)))
;; smart kill whole line
(global-set-key [remap kill-whole-line] (lambda (&optional arg)
                                          (interactive "p")
                                          (kill-whole-line)
                                          (back-to-indentation)))
;; CamelCase aware
(add-hook 'prog-mode-hook 'subword-mode)
;; fast way to edit init.el
(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))
;; Vim '^'
(defun smart-move-beginning-of-line (arg)
  "Move point to the first non-whitespace character on this line.
If ARG is not nil or 1, move forward ARG - 1 lines first.If point
reaches the beginning or end of the buffer, stop there"
  (interactive "^p")
  (setq arg (or arg 1))
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
(global-set-key [remap move-beginning-of-line] 'smart-move-beginning-of-line)
;; Vim 'J'
(defun join-line-below ()
  "Join the line below."
  (interactive)
  (forward-line 1)
  (delete-indentation))
(global-set-key (kbd "C-x j") 'join-line-below)

(defun eval-and-replace ()
  "Rplace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))
(global-set-key (kbd "C-c e") 'eval-and-replace)

(defun byte-compile-init-dir ()
  "Byte-compile all el file in dot dir."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))

;; Highlight Comment Annotation
(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotation.
This functions should be added to the hooks of major modes for porgramming."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))
(add-hook 'prog-mode-hook 'font-lock-comment-annotations)

;; fast open shell init file
(defun find-shell-init-file ()
  "Edit the shell init file in another window."
  (interactive)
  (let* ((shell (car (reverse (split-string (getenv "SHELL") "/"))))
         (shell-init-file (cond
                           ((string-equal "zsh" shell) ".zshrc")
                           ((string-equal "bash" shell) ".bashrc")
                           (t (error "Unknown shell")))))
    (find-file-other-window (expand-file-name shell-init-file (getenv "HOME")))))
(global-set-key (kbd "C-c s") 'find-shell-init-file)

(require-package 'magit)

;; using ido not helm
(ido-mode +1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
;; do not confirm newly created file
(setq ido-create-new-buffer 'always)

(defun untabify-buffer ()
  "Untabify buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
          "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))

(defun insert-date ()
  "Insert a timestamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

;; set default browser
(setq browse-url-browser-function 'browse-url-firefox)

(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)
(setq use-file-dialog nil)
(setq use-dialog-box nil)

;; opacity setting
(defun adjust-opacity (frame incr)
  "Adjust FRAME opacity by INCR."
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

(when (and *is-a-mac* (fboundp 'toggle-frame-fullscreen))
  (global-set-key (kbd "M-C-8") '(lambda ()
                                   (interactive)
                                   (adjust-opacity nil -5)))
  (global-set-key (kbd "M-C-9") '(lambda ()
                                   (interactive)
                                   (adjust-opacity nil 5)))
  (global-set-key (kbd "M-C-0") '(lambda ()
                                   (interactive)
                                   (modify-frame-parameters nil
                                                            '((alpha . 100))))))
;; http://stackoverflow.com/questions/9688748/emacs-comment-uncomment-current-line
(defun toggle-comment-current-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-c c") 'toggle-comment-current-line)

;; Tern setting
(add-to-list 'load-path "/usr/lib/node_modules/tern/emacs/")
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

(require-package 'markdown-mode)
(setq auto-mode-alist
      (cons '("\\.\\(md\\|markdown\\)\\'" . markdown-mode) auto-mode-alist))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/org/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/org/contrib/lisp" t))



(provide 'init)
;;; init.el ends here
