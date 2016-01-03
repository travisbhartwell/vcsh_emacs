;; Local configuration only for shedemei.
(defun tbh/dotspacemacs/layers ()
  "Local configuration layers declaration"
  (let ((local-configuration-layers
         '(
           ansible
           (c-c++ :variables
                  c-c++-enable-clang-support t)
           erlang
           eyebrowse
           finance
           go
           gnus
           (haskell :variables
                    haskell-enable-ghc-mod-support nil)
           javascript
           lua
           pandoc
           purescript
           (ranger :variables
                   ranger-show-preview t)
           rust
           semantic
           tbh-org
           vagrant
           ;; weechat
           )))
    (dolist (layer local-configuration-layers)
      (add-to-list 'dotspacemacs-configuration-layers layer)))

  (let ((local-additional-packages '(google-contacts visual-fill-column w3m)))
    (dolist (package local-additional-packages)
      (add-to-list 'dotspacemacs-additional-packages package))))

(defun tbh/dotspacemacs/user-config ()
  "Local configuration function.
This function is called at the very end of Spacemacs initialization after
layers configuration, after the general dotspacemacs/user-config."
  (setq find-function-C-source-directory (expand-file-name "Third-Party/emacs/emacs/src" tbh-home-dir)
        google-contacts-message-use-primary nil
        ledger-post-amount-alignment-column 68
        oauth2-token-file (expand-file-name "oauth2.plstore" spacemacs-cache-directory)
        ranger-cleanup-on-disable t)

  (add-to-list 'org-agenda-files (expand-file-name "Documents/Planning/Work/Basho/work-inbox.org" tbh-home-dir))
  (spacemacs/declare-prefix "ow" "Org Work Inbox")
  (spacemacs/set-leader-keys
    "owt" (lambda () (interactive) (bookmark-jump "Work Tasks Inbox"))
    "own" (lambda () (interactive) (bookmark-jump "Work Notes Inbox")))
  (which-key-add-key-based-replacements
    (concat dotspacemacs-leader-key " owt") "Work Tasks Inbox"
    (concat dotspacemacs-leader-key " own") "Work Notes Inbox")

  (add-hook 'visual-line-mode-hook 'visual-fill-column-mode)
  (tbh-add-magit-repo-dirs '("Documents/" "Third-Party/" "Work/")))
