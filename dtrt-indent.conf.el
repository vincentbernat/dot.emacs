;;; Code:

;; Remove the dtrt indent stuff from modeline, no need for it.
(diminish 'dtrt-indent-mode)

;; Add more offset variables
(add-to-list 'dtrt-indent-hook-mapping-list '(groovy-mode c/c++/java groovy-indent-offset))

;;; dtrt-indent.conf.el ends here
