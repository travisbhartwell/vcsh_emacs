;;; packages.el --- weechat Layer packages File for Spacemacs
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
(setq weechat-packages
      '(
        tracking
        weechat
        ))

;; List of packages to exclude.
(setq weechat-excluded-packages '())

(defun weechat/init-tracking ()
  "Initialize tracking"
  (use-package tracking
    :defer t
    :init
    (evil-leader/set-key
      "ai <SPC>" 'tracking-next-buffer)))

(defun weechat/init-weechat ()
  "Initialize weechat-el"
  (use-package weechat
    :defer t
    :init
    (evil-leader/set-key
      "aiw" 'weechat-connect)
    (setq weechat-modules '(
                            weechat-button
                            weechat-color
                            weechat-tracking
                            ))
    (when (configuration-layer/layer-usedp 'spell-checking)
      (push 'weechat-spelling weechat-modules))
    (when (configuration-layer/layer-usedp 'auto-completion)
      (push 'weechat-complete weechat-modules))
    (setq weechat-auto-monitor-buffers t
          weechat-sync-active-buffer t
          weechat-sync-buffer-read-status t
          weechat-host-default "localhost"
          weechat-port-default 8001
          weechat-tracking-types '(:highlight :message))

    ;; TODO: Have predefined color schemes for spacemacs light
    ;;       and dark themes and set appropriately
    ;; If the highest precedence enabled theme is spacemacs-dark,
    ;; pick colors that fit better with the theme
    (when (eq 'spacemacs-dark (car custom-enabled-themes))
      ;; TODO: Finish tweaking the color list based on the theme
      (setq weechat-color-list '(unspecified
                                 "black"
                                 "dark gray"
                                 "dark red"
                                 "#d70000" ;; "red"
                                 "#67b11d" ;; "dark green"
                                 "#5faf00" ;; "light green"
                                 "brown"
                                 "#875f00" ;; "yellow"
                                 "#268bd2" ;; "dark blue"
                                 "light blue"
                                 "dark magenta"
                                 "magenta"
                                 "dark cyan"
                                 "light cyan"
                                 "gray"
                                 "white")))

    ;; show-smartparens doesn't interact well with this mode
    (add-hook 'weechat-mode-hook 'turn-off-show-smartparens-mode)
    (push 'weechat-mode evil-insert-state-modes)
    :config
    ;; Does this belong here or in the :config of weechat/init-tracking?
    (tracking-mode)))
