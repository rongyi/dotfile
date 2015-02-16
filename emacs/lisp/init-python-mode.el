(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
		("SConscript\\'" . python-mode))
              auto-mode-alist))

(require-package 'pip-requirements)


(add-hook 'python-mode-hook 'anaconda-mode)

(provide 'init-python-mode)
