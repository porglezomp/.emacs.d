;;; python-config --- Configure Python

;;; Commentary:
;;; Code:

(defun run-python-file ()
  "Run the current buffer as 'python <filename>'."
  (interactive)
  (when (buffer-file-name (current-buffer))
    (compile (format "python %s" (buffer-file-name (current-buffer))))))

(global-set-key (kbd "C-c p") 'run-python-file)

(require 'elpy)
(elpy-enable)
(setq elpy-rpc-backend "jedi")
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode 'flycheck-mode))

(provide 'python-config)
;;; python-config.el ends here
