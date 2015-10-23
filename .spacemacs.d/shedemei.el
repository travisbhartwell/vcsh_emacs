;; Local configuration only for shedemei.
(defun tbh/dotspacemacs/layers ()
  "Local configuration layers declaration"
  (let ((local-configuration-layers
         '(
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
           ocaml
           pandoc
           purescript
           (ranger :variables
                   ranger-show-preview t)
           rust
           tbh-org
           weechat
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
  (setq google-contacts-message-use-primary nil)
  (setq ledger-post-amount-alignment-column 68)
  (setq oauth2-token-file (expand-file-name "oauth2.plstore" spacemacs-cache-directory))
  (setq ranger-cleanup-on-disable t)
  (setq find-function-C-source-directory (expand-file-name "Third-Party/emacs/emacs/src" tbh-home-dir))
  (add-hook 'visual-line-mode-hook 'visual-fill-column-mode)
  (tbh-add-magit-repo-dirs '("Documents/" "Third-Party/" "Work/")))
