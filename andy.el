;; use zenburn color theme
(zenburn)

;; Linux/X11 settings:
;;   font, frame size
(if (eq window-system 'x)
    (progn (set-default-font "Consolas-11")
           (add-to-list 'default-frame-alist '(width . 132))
           (add-to-list 'default-frame-alist '(height . 66))))
  
;; NextStep/Mac OSX settings:
;;   modifier keys, font, frame size
(if (eq window-system 'ns)
    (progn (setq ns-alternate-modifier (quote super))
           (setq ns-command-modifier (quote meta))
           (set-default-font "Monaco-12")
           (add-to-list 'default-frame-alist '(width . 179))
           (add-to-list 'default-frame-alist '(height . 49))
           (setenv "PATH" (concat (getenv "PATH") ":/usr/local/git/bin"))
           (setq exec-path (append exec-path '("/usr/local/git/bin")))))


;; add my ruby rspec and test::unit support and keys
(require 'rspec-mode)

(require 'testunit-mode)
(add-hook 'testunit-mode-hook
	  (lambda ()
	    (define-key testunit-mode-map (kbd "M-s") 'run-focused-test)
	    (define-key testunit-mode-map (kbd "M-S") 'run-tests)))

;; use browse-kill-ring on raw M-y if available, uses browse-kill-ring-mode
(when (require 'browse-kill-ring nil 'noerror)
  (browse-kill-ring-default-keybindings))

;; remap other-window to M-o, delete-other-windows to M-O
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-O") 'delete-other-windows)

;; fullscreen toggle
(defun fullscreen-toggle ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
  (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

;; grep-find ignores bundle dir
(grep-compute-defaults)
(add-to-list 'grep-find-ignored-directories "vendor/bundle")
