;;; python-config --- Configure Python

;;; Commentary:
;;; Code:

(defun run-python-file ()
  "Run the current buffer as 'python <filename>'."
  (interactive)
  (when (buffer-file-name (current-buffer))
    (compile (format "python %s" (buffer-file-name (current-buffer))))))

(global-set-key (kbd "C-c p") 'run-python-file)

(provide 'python-config)
;;; python-config.el ends here
