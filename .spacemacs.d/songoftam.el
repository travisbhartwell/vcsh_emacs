;; Local configuration only for shedemei.
(defun tbh/dotspacemacs/layers ()
  "Local configuration layers declaration"
  (let ((local-configuration-layers
         '(
           dockerfile
           finance
           (go :variables go-use-gometalinter t)
           html
           nixos
           (ranger :variables
                   ranger-show-preview t)
           semantic
           (shell :variables
                  shell-default-height 30
                  shell-default-position 'bottom
                  shell-default-term-shell "/run/current-system/sw/bin/bash"
                  shell-default-shell 'ansi-term)
           tbh-org
           vagrant
           yaml
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

  (add-hook 'visual-line-mode-hook 'visual-fill-column-mode)
  (tbh-add-magit-repo-dirs '("Documents/" "Third-Party/" "Work/")))

(with-eval-after-load 'ledger-mode
  (defun tbh-generate-financial-dashboard-html ()
    (interactive)
    (with-current-buffer (find-file-noselect "/home/nafai/Documents/Planning/Org/financial-dashboard.org")
      (setq-local org-confirm-babel-evaluate nil)
      (org-babel-execute-buffer)
      (org-html-export-to-html)
      (save-buffer)))
  (add-hook 'ledger-mode-hook
            (lambda ()
              (add-hook 'after-save-hook 'tbh-generate-financial-dashboard-html nil 'make-it-local))))
