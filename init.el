;;; init.el -- My init file

;;; Commentary:

;;; Code:

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(require 'package)
(require 'utils)

;; ----- CUSTOM FILE -----
(defvar custom-file-path
  (concat user-emacs-directory "custom.el"))

;; Create the file if it does not exist
(unless (file-readable-p custom-file-path)
    (write-region "" nil custom-file-path))

(setq custom-file custom-file-path)
(load custom-file-path)

;; ----- BACKUP FILES -----
(defvar tom/backup-dir (concat user-emacs-directory "backup/"))
(setq backup-directory-alist
      `(("." . ,tom/backup-dir)))

;; ----- PACKAGES -----
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(defvar my-packages
  '(ag
    company
    company-jedi  ; python
    diminish
    evil
    evil-commentary
    evil-leader
    evil-surround
    flycheck
    flycheck-typescript-tslint
    gotham-theme
    gruvbox-theme
    helm
    helm-projectile
    magit
    projectile
    py-autopep8
    python-mode
    tide  ; typescript
    yaml-mode
    yasnippet))

(dolist (pak my-packages)
  (package-install pak))

;; ----- MISC -----

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(column-number-mode 1)
(setq require-final-newline t)
(setq inhibit-splash-screen t)

(defvar whitespace-style '(face tabs trailing))
(setq tab-always-indent 'complete)
(setq async-shell-command-buffer 'new-buffer)
(setq-default indent-tabs-mode nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(load-theme 'gotham)
(set-face-attribute 'default nil
                    :family "Inconsolata"
                    :height 110)

;; ----- EVIL ------

(global-evil-leader-mode 1)
(evil-leader/set-leader "»")
(evil-leader/set-key "ag" 'projectile-ag)
(evil-leader/set-key "bb" 'list-buffers)
(evil-leader/set-key "bmc" 'bookmark-set)
(evil-leader/set-key "bmg" 'bookmark-bmenu-list)
(evil-leader/set-key "o" 'helm-find-files)
(evil-leader/set-key "git" 'magit-status)
(evil-leader/set-key "ln" 'flycheck-next-error)
(evil-leader/set-key "lp" 'flycheck-previous-error)
(evil-leader/set-key "ff" 'helm-projectile)
(evil-leader/set-key "t"
  (lambda ()
    (interactive)
    (tom/call-terminal)))
(evil-leader/set-key "w" 'save-buffer)

(evil-mode 1)
(diminish 'undo-tree-mode)
(evil-define-key 'normal global-map (kbd "é") 'evil-ex)
(evil-define-key 'insert global-map (kbd "C-l") 'evil-normal-state)

(evil-commentary-mode 1)

;; Mode-specific evil bindings
(evil-define-key 'motion help-mode-map (kbd "TAB") 'forward-button)
(evil-leader/set-key-for-mode 'emacs-lisp-mode "e" 'eval-last-sexp)
(evil-leader/set-key-for-mode 'lisp-interaction-mode "e" 'eval-last-sexp)
(evil-define-key 'normal python-mode-map (kbd "gd") 'jedi:goto-definition)
(evil-define-key 'normal python-mode-map (kbd "gs") 'jedi:show-doc)
(evil-define-key 'normal tide-mode-map (kbd "gd") 'tide-jump-to-definition)
(evil-define-key 'normal tide-mode-map (kbd "C-o") 'tide-jump-back)
(evil-define-key 'normal tide-mode-map (kbd "gs") 'tide-documentation-at-point)

;; ----- MAGIT -----
(evil-set-initial-state 'magit-mode 'normal)
(evil-set-initial-state 'magit-status-mode 'normal)
(evil-set-initial-state 'magit-diff-mode 'normal)
(evil-set-initial-state 'magit-log-mode 'normal)
(evil-define-key 'normal magit-status-mode-map
  "q" 'magit-mode-bury-buffer)
(evil-define-key 'normal magit-log-mode-map
  "j" 'magit-section-forward
  "k" 'magit-section-backward)
(evil-define-key 'normal magit-diff-mode-map
  "j" 'magit-section-forward
  "k" 'magit-section-backward)

;; ----- COMPANY -----
(global-company-mode 1)
(diminish 'company-mode)

;; ----- FLYCHECK -----
(defvar flycheck-emacs-lisp-load-path 'inherit)
(defvar flycheck-check-syntax-automatically '(save))
(add-hook 'flycheck-mode-hook #'flycheck-typescript-tslint-setup)

;; ----- HELM -----
(global-set-key (kbd "M-x") 'helm-M-x)

;; ----- PROJECTILE -----
(projectile-global-mode 1)
(diminish 'projectile-mode)

;; ----- MODES -----
(add-hook 'prog-mode-hook
          (lambda ()
            (linum-mode 1)
            (whitespace-mode 1)
            (diminish 'whitespace-mode)
            (flycheck-mode 1)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (eldoc-mode 1)))

(add-hook 'python-mode-hook
          (lambda ()
            (add-to-list 'company-backends 'company-jedi)
            (pyvenv-mode 1)
            (run-python 1)
            (eldoc-mode 1)
            (py-autopep8-enable-on-save)))

(add-to-list 'auto-mode-alist '("\\.tsx" . typescript-mode))
(add-hook 'typescript-mode-hook
          (lambda ()
            (tide-mode)
            (tide-start-server-if-required)))

(provide 'init)
;;; init.el ends here
