(require 'nnir)
(require 'google-contacts-gnus)
(require 'google-contacts-message)

(setq epa-file-cache-passphrase-for-symmetric-encryption t
      gnus-ignored-from-addresses "nafai"
      ;; Disable archiving, since sending with smtp to gmail automatically adds it to sent mail
      gnus-message-archive-group nil
      gnus-posting-styles '(((header "to" "nafai@travishartwell.net")
                             (address "nafai@travishartwell.net")))
      gnus-select-method '(nntp "news.gmane.org")
      gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject
      gnus-thread-hide-subtree t
      gnus-thread-ignore-subject t
      gnus-use-cache t
      message-send-mail-function 'smtpmail-send-it
      mm-text-html-renderer 'w3m
      mm-discouraged-alternatives '("text/html" "text/richtext")
      mml2015-signers '("23562D0B")
      mml2015-encrypt-to-self t
      smtpmail-auth-credentials "~/.authinfo.gpg"
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-local-domain "travishartwell.net"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil)))

(setq imap-shell-program (expand-file-name ".dovecot-wrapper" tbh-home-dir))

(add-to-list 'gnus-secondary-select-methods '(nnimap "travishartwell.net"
                                                     (nnimap-address "localhost")
                                                     (nnimap-stream shell)
                                                     (nnir-search-engine imap)))
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(add-hook 'message-setup-hook 'mml-secure-message-encrypt)

;; Exclude Gnus files from the recentf list
(add-to-list 'recentf-exclude ".*/\\.news.*")
