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
           haskell
           javascript
           lua
           purescript
           ruby
           semantic
           )))
    (dolist (layer local-configuration-layers)
      (add-to-list 'dotspacemacs-configuration-layers layer)))

  (let ((local-additional-packages '(emr google-contacts w3m)))
    (dolist (package local-additional-packages)
      (add-to-list 'dotspacemacs-additional-packages package))))

(defun tbh/dotspacemacs/init ()
  "Local initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration, after the general dotspacemacs/init."
  (tbh-push-theme-to-front 'misterioso))

(defun tbh/dotspacemacs/config ()
  "Local configuration function.
This function is called at the very end of Spacemacs initialization after
layers configuration, after the general dotspacemacs/config."
  (setq ledger-post-amount-alignment-column 68)
  (add-hook 'prog-mode-hook (lambda ()
                              (emr-initialize)
                              (evil-leader/set-key-for-mode major-mode
                                "or"
                                'emr-show-refactor-menu)))
  (tbh-add-magit-repo-dirs '("Documents/" "Third-Party/" "Work/")))
