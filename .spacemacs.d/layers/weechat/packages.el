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

;; TODO: Should I unset the tracking-mode-map?
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
    (when (configuration-layer/layer-usedp 'syntax-checking)
      (push 'weechat-spelling weechat-modules))
    (when (configuration-layer/layer-usedp 'auto-completion)
      (push 'weechat-complete weechat-modules))
    (setq weechat-auto-monitor-buffers t
          weechat-sync-active-buffer t
          weechat-host-default "localhost"
          weechat-port-default 8001)
    (add-hook 'weechat-mode-hook 'visual-line-mode)
    (push 'weechat-mode evil-insert-state-modes)
    :config
    (setq weechat-tracking-types '(:message :hilight))
    ;; Does this belong here or in the :config of weechat/init-tracking?
    (tracking-mode)))
