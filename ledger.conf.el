;; Fonts for ledger are for light background, modify them

(set-face-attribute 'ledger-font-xact-highlight-face nil
                    :background "grey10")
(set-face-attribute 'ledger-occur-xact-face nil
                    :background "grey10")
(set-face-attribute 'ledger-font-payee-cleared-face nil
                    :foreground "grey70")
(set-face-attribute 'ledger-font-other-face nil
                    :foreground "violet")

;; Align amounts automatically
(setq ledger-post-auto-adjust-amounts t)

