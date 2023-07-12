(tool-bar-mode -1) ;; remove toolbar
;;(toggle-scroll-bar -1) ;; remove scrollbar
;;(setq inhibit-startup-screen t) ;; no startup screen
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; mouse scroll one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate mouse scrolling
(setq
   backup-by-copying t
   backup-directory-alist
    '(("." . "~/.config/emacs/backups/"))
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)
(add-hook 'prog-mode-hook 'linum-mode) ;; Line numbers
(setq-default indent-tabs-mode t) ;; Tabs as indentation
(setq-default tab-width 2) ;; Tabs have a width of 2 spaces
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; start every frame maximized
(setq column-number-mode t) ;; Display column numbers
(setq package-enable-at-startup nil)

;; Bootstrap straight.el (do not modify)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
												 user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)

;; Multiple cursors
(use-package multiple-cursors
	:straight t
	:ensure t
	:bind (("C-c m c" . 'mc/edit-lines)
				 ("C-c m n" . 'mc/mark-next-like-this)
				 ("C-c m p" . 'mc/mark-previous-like-this)
				 ("C-c m a" . 'mc/mark-all-like-this)))

;; Comment hiding
(use-package hide-comnt
	:straight (:host github :repo "emacsmirror/hide-comnt")
	:ensure t
	)

;; GitHub Copilot
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t)
(add-hook 'prog-mode-hook 'copilot-mode)
(with-eval-after-load 'company
  ;; disable inline previews
  (delq 'company-preview-if-just-one-frontend company-frontends))
  
(define-key copilot-completion-map (kbd "C-c c a") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "C-c c l") 'copilot-accept-completion-by-line)
(define-key copilot-completion-map (kbd "C-c c w") 'copilot-accept-completion-by-word)
(define-key copilot-completion-map (kbd "C-c c n") 'copilot-next-completion)
(define-key copilot-completion-map (kbd "C-c c p") 'copilot-previous-completion)

;; Highlight TODO, FIXME, etc.
(use-package hl-todo
  :straight (:host github :repo "tarsius/hl-todo" :files ("*.el"))
	:ensure t
  :hook (prog-mode . hl-todo-mode)
	)

;; CUDA Mode

(use-package cuda-mode
  :straight t
	:ensure t)

;; CMake Mode

(use-package cmake-mode
	:straight (:host github :repo "Kitware/CMake" :files ("Auxiliary/cmake-mode.el"))
	:ensure t)
(require 'cmake-mode)
(use-package cmake-font-lock
	:straight t
	:ensure t)
(require 'cmake-font-lock)
  
;; Haskell Mode

;; (use-package haskell-mode
;;   :ensure t)

;; OCaml Mode

;; (use-package tuareg
;;   :ensure t
;;   :config
;;   (add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu))

;; (use-package merlin
;;   :ensure t
;;   :after tuareg
;;   :config
;;   (add-hook 'tuareg-mode-hook 'merlin-mode))

;; Rust Mode

;; (use-package rust-mode
;;   :ensure t)

;; Lua Mode

;; (use-package lua-mode
;; 	:ensure t)

;; Ivy and Counsel (better completion)

(use-package ivy
  :straight t
	:ensure t
  :config (ivy-mode))

(use-package counsel
  :straight t
	:ensure t
  :after ivy
  :config (counsel-mode))

;; Generally useful stuff for working on projects

(load "/usr/share/emacs/site-lisp/clang-format/clang-format.el")
(global-set-key (kbd "C-M-i") 'clang-format-buffer)

(use-package magit
  :straight t
	:ensure t)

(use-package company
  :straight t
	:ensure t
  :init
  (setq company-idle-delay 0)
  :config
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (global-company-mode))

(add-hook ;; Disable company mode in ssh to avoid latency
 'prog-mode-hook
 (lambda () (when (file-remote-p default-directory) (company-mode -1))))

;; (use-package company-box
;;   :straight (:host github :repo "sebastiencs/company-box" :files ("*.el"))
;;   :ensure t)
;; (add-hook 'company-mode-hook 'company-box-mode)

(use-package projectile
  :straight t
	:ensure t
  :after ivy
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (use-package counsel-projectile
    :after projectile
		:straight t
    :ensure t
    :config (counsel-projectile-mode)))

;; Eglot 
(use-package eglot
  :straight t
	:ensure t)

(require 'eglot)
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'cuda-mode-hook 'eglot-ensure)
(add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1))) 

;; ;; LSP-Mode and DAP-Mode
;; (use-package lsp-mode
;; 	:straight t
;;   :init
;;   ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;;   (setq lsp-keymap-prefix "C-c l")
;;   :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
;;          (c-mode . lsp)
;;          (c++-mode . lsp)
;;          (cuda-mode . lsp)
;;          ;; if you want which-key integration
;;          (lsp-mode . lsp-enable-which-key-integration))
;;   :commands lsp)

;; ;; optionally
;; (use-package lsp-ui
;; 	:straight t
;; 	:commands lsp-ui-mode)
;; ;; if you are helm user
;; (use-package helm-lsp 
;; 	:straight t
;; 	:commands helm-lsp-workspace-symbol)
;; ;; if you are ivy user
;; (use-package lsp-ivy 
;; 	:straight t
;; 	:commands lsp-ivy-workspace-symbol)
;; (use-package lsp-treemacs 
;; 	:straight t
;; 	:commands lsp-treemacs-errors-list)

;; ;; optionally if you want to use debugger
;; (use-package dap-mode
;; 	:straight t)
;; ;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; ;; optional if you want which-key integration
;; (use-package which-key
;; 	:straight t
;; 	:config
;;     (which-key-mode))
