;;; packages.el --- tbh-org Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Travis B. Hartwell & Contributors
;;
;; Author: Travis B. Hartwell <nafai@travishartwell.net>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq tbh-org-packages
      '(
        (org :location built-in)
        (ox-latex :location built-in)
        ))

;; List of packages to exclude.
(setq tbh-org-excluded-packages '())

(defun tbh-org-get-clock-time ()
  (if (and (fboundp 'org-clocking-p)
             (org-clocking-p))
      (substring-no-properties (funcall spaceline-org-clock-format-function))
    ""))

(defun tbh-org/post-init-org ()
  "Do further personal initialization of org mode."
  (let ((planning-dir (expand-file-name "Documents/Planning/" tbh-home-dir)))
    (setq diary-file (expand-file-name "Calendar/diary" planning-dir))
    (setq org-directory (expand-file-name "Org" planning-dir)))

  (add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
  (add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files)

  (require 'find-lisp)
  (setq org-agenda-files (when (file-directory-p org-directory)
                           (find-lisp-find-files org-directory "\.org$")))

  (spacemacs/declare-prefix "oi" "Org Inbox")
  (spacemacs/set-leader-keys
    "oit" (lambda () (interactive) (bookmark-jump "Tasks Inbox"))
    "oin" (lambda () (interactive) (bookmark-jump "Notes Inbox")))
  (which-key-add-key-based-replacements
    (concat dotspacemacs-leader-key " oit") "Tasks Inbox"
    (concat dotspacemacs-leader-key " oin") "Notes Inbox")

  (setq org-agenda-sorting-strategy
        '((agenda time-up todo-state-down)
          (todo time-up todo-state-down category-keep)
          (tags time-up category-keep)
          (search category-keep)))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (ledger . t)
     (shell . t)))
  (setq org-confirm-babel-evaluate t)

  (setq org-clock-mode-line-total 'today)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "STARTED(s!)" "|" "DONE(d!/!)")
          (sequence "WAITING(w@/!)" "SOMEDAY(S!)" "OPEN(O@)" "|"
                    "CANCELLED(c@/!)")))

  ;; TODO: Update these faces to match with Spacemacs Dark theme
  (setq org-todo-keyword-faces
        '(("TODO" :foreground "red" :weight bold)
          ("STARTED" :foreground "blue" :weight bold)
          ("DONE" :foreground "forest green" :weight bold)
          ("WAITING" :foreground "orange" :weight bold)
          ("SOMEDAY" :foreground "magenta" :weight bold)
          ("CANCELED" :foreground "forest green" :weight bold)
          ("OPEN" :foreground "blue" :weight bold)))

  (setq org-log-done 'note)

  (setq org-agenda-start-on-weekday 0 ;; Start on Sunday
        org-agenda-include-diary t
        org-agenda-skip-scheduled-if-done t
        org-agenda-span 'day)

  ;; Refiling
  (setq org-outline-path-complete-in-steps nil
        org-refile-targets '((org-agenda-files :maxlevel . 5)
                             (nil :maxlevel . 5))
        org-refile-use-outline-path 'file)

  (setq org-default-notes-file (expand-file-name "inbox.org" org-directory))

  (setq org-capture-templates
        '(("a" "Appointment" entry
           (file+headline
            (expand-file-name "important-dates.org" org-directory)
            "Miscellaneous Appointments")
           "* %^{description} %^{Date and Time}T")
          ("t" "TODO" entry
           (file+headline
            (expand-file-name "inbox.org" org-directory)
            "Tasks")
           "* TODO %?")
          ("n" "Notes" entry
           (file+headline
            (expand-file-name "inbox.org" org-directory)
            "Notes")
           "* %U                   :NOTE:\n\n  %?")
          ("j" "Journal" entry
           (file+datetree
            (expand-file-name "journal.org" org-directory)
            "Entries")
           "* %U\n\n  %?\n")
          ("l" "Log" entry
           (file+headline
            (expand-file-name "log.org" org-directory)
            "Log")
           "* %U %^{What are you doing}\n")))

  (setq org-clock-in-resume t
        org-clock-in-switch-to-state "STARTED"
        org-clock-into-drawer t
        org-clock-out-remove-zero-time-clocks t
        org-clock-persist t)

  (setq org-columns-default-format
        "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")
  (setq org-global-properties
        '(("Effort_ALL" . "0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 8:00"))))

(defun tbh-org/post-init-ox-latex ()
  (use-package ox-latex
    :config
    (progn
      ;; Use the listings package to syntax highlight code
      (setq org-latex-listings t)
      (add-to-list 'org-latex-packages-alist '("" "listings"))
      (add-to-list 'org-latex-packages-alist '("" "color"))

      (setq org-latex-classes
            (cons '("tbharticle"
                    "\\documentclass[10pt,letterpaper]{article}
\\usepackage[letterpaper,includeheadfoot,top=0.5in,bottom=0.5in,left=0.75in,right=0.75in]{geometry}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\setlength{\\parindent}{0pt}
\\setlength{\\parskip}{10pt}
\\usepackage[scaled]{helvet}
\\renewcommand*\\familydefault{\\sfdefault}
\\usepackage{lastpage}
\\usepackage{fancyhdr}
\\pagestyle{fancy}
\\usepackage{hyperref}
\\hypersetup{colorlinks,linkcolor=blue}
\\renewcommand{\\headrulewidth}{1pt}
\\renewcommand{\\footrulewidth}{0.5pt}
% Default footer
\\fancyfoot[L]{\\small Basho Technologies \\\\ \\today}
\\fancyfoot[C]{\\small Page \\thepage\\ of \\pageref{LastPage}}"
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                nil)))))
;; TODO:
;; Add support for org-gcal
;; Make agenda view show day planner view
;; Handle org-capture better with emacsclient
