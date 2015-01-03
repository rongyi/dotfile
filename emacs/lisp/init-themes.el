(defcustom window-system-color-theme 'leuven
  "Color theme to use in window-system frames.
If Emacs' native theme support is available, this setting is
ignored: use `custom-enabled-themes' instead."
  :type 'symbol)

(require-package 'leuven-theme)
(load-theme window-system-color-theme t)


(provide 'init-themes)
