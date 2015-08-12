;; Local configuration only for shedemei.
(defun tbh/dotspacemacs/layers ()
  "Local configuration layers declaration"
  (let ((local-configuration-layers
         '(
           (c-c++ :variables
                  c-c++-enable-clang-support t)
           eyebrowse
           finance
           gnus
           (haskell :variables
                    haskell-enable-ghc-mod-support nil)
           javascript
           lua
           purescript
           (ranger :variables
                   ranger-show-preview t)
           ruby
           weechat
           )))
    (dolist (layer local-configuration-layers)
      (add-to-list 'dotspacemacs-configuration-layers layer)))

  (let ((local-additional-packages '(google-contacts visual-fill-column w3m)))
    (dolist (package local-additional-packages)
      (add-to-list 'dotspacemacs-additional-packages package))))

(defun tbh/dotspacemacs/config ()
  "Local configuration function.
This function is called at the very end of Spacemacs initialization after
layers configuration, after the general dotspacemacs/config."
  (setq google-contacts-message-use-primary nil)
  (setq ledger-post-amount-alignment-column 68)
  (setq ranger-cleanup-on-disable t)
  (setq find-function-C-source-directory (expand-file-name "Third-Party/emacs/emacs/src" tbh-home-dir))
  (add-hook 'visual-line-mode-hook 'visual-fill-column-mode)
  (tbh-add-magit-repo-dirs '("Documents/" "Third-Party/" "Work/")))
