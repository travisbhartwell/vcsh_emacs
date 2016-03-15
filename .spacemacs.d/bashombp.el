;; Local configuration only for bashombp.
(defun tbh/dotspacemacs/layers ()
  "Local configuration layers declaration"
  (let ((local-configuration-layers
         '(
           ansible
           dockerfile
           erlang
           go
           javascript
           osx
           (ranger :variables
                   ranger-show-preview t)
           semantic
           tbh-org
           vagrant
           ;; weechat
           )))
    (dolist (layer local-configuration-layers)
      (add-to-list 'dotspacemacs-configuration-layers layer)))

  (let ((local-additional-packages '(visual-fill-column w3m)))
    (dolist (package local-additional-packages)
      (add-to-list 'dotspacemacs-additional-packages package))))

(defun tbh/dotspacemacs/init ()
    (setq-default
     dotspacemacs-fullscreen-at-startup t))

(defun tbh/dotspacemacs/user-config ()
  "Local configuration function.
This function is called at the very end of Spacemacs initialization after
layers configuration, after the general dotspacemacs/user-config."
  (setq find-function-C-source-directory (expand-file-name "Third-Party/emacs/emacs/src" tbh-home-dir)
        oauth2-token-file (expand-file-name "oauth2.plstore" spacemacs-cache-directory)
        ranger-cleanup-on-disable t
        user-mail-address "thartwell@contractor.basho.com")

  (add-to-list 'org-agenda-files (expand-file-name "Documents/Planning/Work/Basho/work-inbox.org" tbh-home-dir))
  (spacemacs/declare-prefix "ow" "Org Work Inbox")
  (evil-leader/set-key
    "owt" (lambda () (interactive) (bookmark-jump "Work Tasks Inbox"))
    "own" (lambda () (interactive) (bookmark-jump "Work Notes Inbox")))
  (which-key-add-key-based-replacements
    (concat dotspacemacs-leader-key " owt") "Work Tasks Inbox"
    (concat dotspacemacs-leader-key " own") "Work Notes Inbox")

  (add-hook 'visual-line-mode-hook 'visual-fill-column-mode)
  (tbh-add-magit-repo-dirs '("Documents/" "Third-Party/" "Work/"))
  (spacemacs|define-custom-layout "@Planning"
    :binding "p"
    :body
    (let*
        ((timeclock-buffer (find-file "~/Documents/Planning/Org/timeclock.org"))
         (inbox-buffer (find-file "~/Documents/Planning/Org/inbox.org")))
      (switch-to-buffer timeclock-buffer)
      (split-window-right)
      (switch-to-buffer-other-window inbox-buffer)
      (select-window (get-buffer-window timeclock-buffer))
      (goto-char (point-min))
      (re-search-forward "* Reports")
      (split-window-below)
      (org-tree-to-indirect-buffer)
      (goto-char (point-min)))))
