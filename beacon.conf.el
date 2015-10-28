;;; Code:

(setq beacon-lighter ""          ; Don't display anything in mode-line
      beacon-blink-when-focused nil
      beacon-blink-duration 0.3
      beacon-color "#f57900")

;;; Disable on some modes
(add-to-list 'beacon-dont-blink-major-modes 'erc-mode)
(add-to-list 'beacon-dont-blink-major-modes 'gnus-group-mode)
(add-to-list 'beacon-dont-blink-major-modes 'gnus-summary-mode)

;;; beacon.conf.el ends here
