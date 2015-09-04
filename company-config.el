;;; company-config --- Configure the company autocompletion
;;; Commentary:

;;; Code:

;; Company
(require 'company)
(global-company-mode)

;; Reduce the time for company autocompletion to appear
(setq company-idle-delay 0.2)
;; Reduce the number of characters before company appears
(setq company-minimum-prefix-length 1)

(define-key company-active-map [tab] 'company-complete-common-or-cycle)
(define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)

(provide 'company-config)
;;; company-config.el ends here
