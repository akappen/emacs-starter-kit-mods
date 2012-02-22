;; Linux/X11 settings:
;;   font, frame size
(if (eq window-system 'x)
    (progn (set-default-font "Inconsolata-12")
           (add-to-list 'default-frame-alist '(width . 100))
           (add-to-list 'default-frame-alist '(height . 62))))

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


;; use twilight color theme
;;(color-theme-twilight)

;; use solaized theme
;; (load "color-theme-solarized.el")
;; (color-theme-solarized-dark)

;; use zenburn theme
(color-theme-zenburn)

;; use tangotango theme
;;(load "color-theme-tangotango.el")
;;(color-theme-tangotango)

;; use browse-kill-ring on raw M-y if available, uses browse-kill-ring-mode
(when (require 'browse-kill-ring nil 'noerror)
  (browse-kill-ring-default-keybindings))

;; remap other-window to M-o
(global-set-key (kbd "M-o") 'other-window)

;; starter kit has C-M-h as backward-kill-word, also remap C-h
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)

;; ido mode
(setq ido-use-virtual-buffers t) ; C-x b suggests recently closed buffers

;; fullscreen toggle
(defun fullscreen-toggle ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
  (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

;; grep-find ignores
(grep-compute-defaults)
(add-to-list 'grep-find-ignored-directories "vendor")
(add-to-list 'grep-find-ignored-directories "log")
(add-to-list 'grep-find-ignored-directories "tmp")
(add-to-list 'grep-find-ignored-files "*.dump")

;; Only auto-fill comment blocks in non-text modes
(setq comment-auto-fill-only-comments t)

;; Add buffer or associated filename to window frame
(setq frame-title-format
      '("" invocation-name ": "(:eval (if (buffer-file-name)
                                          (abbreviate-file-name (buffer-file-name))
                                        "%b"))))

;; delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; allow scroll-down/up-command to move point to buffer end/beginning
(setq scroll-error-top-bottom 'true)

;; diasable all automagic window splitting
;;(setq pop-up-windows nil)

;; auto-complete-mode
;;(add-to-list 'load-path "~/.emacs.d/andy/ac-mode")
;;(require 'auto-complete-config)
;;(add-to-list 'ac-dictionary-directories "~/.emacs.d/andy/ac-mode/ac-dict")
;;(ac-config-default)
;;(add-to-list 'ac-modes 'rhtml-mode)

;; configure org-mode
(setq org-directory "~/org")
(setq org-agenda-files
      (quote
       ("~/org/self.org" "~/org/home.org" "~/org/trading.org" "~/org/richarddawkins-net.org" "~/org/net_promoter_score.org" "~/org/biotestlabs.org" "~/org/comics-app.org" "~/org/writing.org")))
(setq org-agenda-clockreport-parameter-plist (quote (:fileskip0 t)))
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 1))))
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-remember-default-headline "Notes")
(org-remember-insinuate)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cr" 'org-remember)

;; Rinari
(add-to-list 'load-path "~/.emacs.d/andy/rinari")
(require 'rinari)
(require 'ruby-compilation-rspec)
(setenv "RUBYLIB" "spec")

;; load my ELPA packages
(defvar andy-packages (list 'haml-mode 'sass-mode 'coffee-mode 'ruby-electric 'textmate 'ecb_snap 'browse-kill-ring)
  "Libraries that should be installed for andy.")

(defun andy-elpa-install ()
  "Install all andy's packages that aren't installed."
  (interactive)
  (dolist (package andy-packages)
    (unless (or (member package package-activated-list)
                (functionp package))
      (message "Installing %s" (symbol-name package))
      (package-install package))))

(andy-elpa-install)

;; ECB settings
(custom-set-variables '(ecb-options-version "2.40"))
(setq stack-trace-on-error nil)
(setq ecb-windows-width 32)
(setq ecb-fix-window-size 'auto)
(setq ecb-tip-of-the-day nil)
(setq ecb-layout-name "left7")
(setq ecb-compile-window-height 12)
(setq ecb-source-path (list (getenv "PWD")))
(setq ecb-primary-secondary-mouse-buttons 'mouse-1--C-mouse-1)
(setq ecb-tree-make-parent-node-sticky nil)
(eval-after-load 'ecb
  '(progn
     (set-face-background 'ecb-default-highlight-face "dim gray")))

;; rhtml mode
(add-to-list 'load-path "~/.emacs.d/andy/rhtml")
(require 'rhtml-mode)

;; extra snippets
(yas/load-directory "~/.emacs.d/andy/yasnippets")

;; coffeescript mode
(defun coffee-custom ()
  "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 2))

(add-hook 'coffee-mode-hook
          '(lambda() (coffee-custom)))
