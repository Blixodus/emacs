(setq gc-cons-threshold 10000000)
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

(tool-bar-mode -1) ;; remove toolbar
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; mouse scroll one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate mouse scrolling
(add-hook 'prog-mode-hook 'display-line-numbers-mode) ;; Line numbers
(setq-default tab-width 2) ;; Tabs have a width of 2 spaces
(setq column-number-mode t) ;; Display column numbers
(setq package-enable-at-startup nil)

(setq
   backup-by-copying t
   backup-directory-alist
    '(("." . "~/.config/emacs/backups/"))
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)

;; Tree-sitter configuration
(setq treesit-language-source-alist
			'((c "https://github.com/tree-sitter/tree-sitter-c")
				(cpp "https://github.com/tree-sitter/tree-sitter-cpp")))

(mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
;;Some weird bug with C++ ts mode
;;(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
;;(add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))

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

;; Magic hack for garbage collector
;; (use-package gcmh
;; 	:straight t
;; 	:ensure t)
;; (gcmh-mode 1)

;; Guix package manager utility for Emacs
(use-package geiser
	:straight t
	:ensure t
	:config
	(setq geiser-guile-binary (executable-find "guile-3.0"))
	(setq geiser-default-implementation '(guile)))

(use-package geiser-guile
	:straight t
	:ensure t)

(use-package guix
	:straight t
	:ensure t)

;; Package to explore assembly (Compiler explorer style)
(use-package rmsbolt
	:straight t
	:ensure t)

;; Multiple cursors
(use-package multiple-cursors
	:straight t
	:ensure t
	:bind (("C-c m c" . 'mc/edit-lines)
				 ("C-c m n" . 'mc/mark-next-like-this)
				 ("C-c m p" . 'mc/mark-previous-like-this)
				 ("C-c m a" . 'mc/mark-all-like-this)))

;; Set regex mode for re-builder to string
(require 're-builder)
(setq reb-re-syntax 'string)

;; GitHub Copilot
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
	:bind (("C-c c c" . 'copilot-complete)
				 ("C-c c a" . 'copilot-accept-completion)
				 ("C-c c l" . 'copilot-accept-completion-by-line)
				 ("C-c c w" . 'copilot-accept-completion-by-word)
				 ("C-c c n" . 'copilot-next-completion)
				 ("C-c c p" . 'copilot-previous-completion))
	:hook ((prog-mode . copilot-mode)
				 (cuda-mode . copilot-mode))
	:config (setq copilot-idle-delay 10000.0))
(with-eval-after-load 'company
  ;; disable inline previews
  (delq 'company-preview-if-just-one-frontend company-frontends))

;; LLM Client
(use-package gptel
	:straight t)

(setq auth-sources '("~/.authinfo"))

;; Highlight TODO, FIXME, etc.
(use-package hl-todo
  :straight (:host github :repo "tarsius/hl-todo" :files ("*.el"))
	:ensure t
  :hook (prog-mode . hl-todo-mode))

;; ;; CUDA Mode
(use-package cuda-mode
  :straight t
	:ensure t)
(add-to-list 'auto-mode-alist '("\\.cu\\'" . cuda-mode))

;; CMake Mode
(use-package cmake-mode
	:straight (:host github :repo "Kitware/CMake" :files ("Auxiliary/cmake-mode.el"))
	:ensure t)
(require 'cmake-mode)

(use-package cmake-font-lock
	:straight t
	:ensure t)
(require 'cmake-font-lock)

;; ParEdit
(use-package paredit
	:straight t
	:ensure t
	:hook ((emacs-lisp-mode . paredit-mode)
				 (lisp-mode . paredit-mode)
				 (lisp-interaction-mode . paredit-mode)
				 (scheme-mode . paredit-mode)
				 (clojure-mode . paredit-mode)
				 (cider-repl-mode . paredit-mode)
				 (eval-expression-minibuffer-setup . paredit-mode)
				 (ielm-mode . paredit-mode)
				 (lisp-mode . enable-paredit-mode)
				 (racket-mode . enable-paredit-mode)
				 (racket-repl-mode . enable-paredit-mode)
				 (scheme-mode . enable-paredit-mode)
				 (clojure-mode . enable-paredit-mode)
				 (cider-repl-mode . enable-paredit-mode)
				 (eval-expression-minibuffer-setup . enable-paredit-mode)
				 (ielm-mode . enable-paredit-mode)))

;; Haskell Mode
(use-package haskell-mode
  :ensure t)

;; ;; OCaml Mode
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
(use-package rust-mode
  :ensure t)

;; Lua Mode
(use-package lua-mode
	:ensure t)

(use-package emacs
	:hook
  ;; Auto parenthesis matching
  ((prog-mode . electric-pair-mode)))

(use-package envrc
	:straight t
  :bind (:map envrc-mode-map
							("C-c e" . envrc-command-map))
  ;:config (which-key-add-key-based-replacements "C-c e" "envrc")
  :init (envrc-global-mode))

;; Motion aid
(use-package avy
	:straight t
  :demand t
  :bind (("C-c j" . avy-goto-line)
         ("s-j"   . avy-goto-char-timer)))

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
;; (use-package eglot
;;   :straight t
;; 	:ensure t
;; 	:hook ((c-mode . eglot-ensure)
;; 				 (c++-mode . eglot-ensure)
;; 				 (c-ts-mode . eglot-ensure)
;; 				 (c++-ts-mode . eglot-ensure)
;; 				 (cuda-mode . eglot-ensure)
;; 				 (eglot-managed-mode . (lambda () (eglot-inlay-hints-mode -1)))))

;; (require 'eglot)
;; (add-to-list 'eglot-server-programs '((c++-ts-mode c-ts-mode c++-mode c-mode cuda-mode) "clangd"))

;; LSP-Mode and DAP-Mode
(use-package lsp-mode
	:straight t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook ((c-mode . lsp)
         (c++-mode . lsp)
         (c-or-c++-mode . lsp)
				 (c-ts-mode . lsp)
         (c++-ts-mode . lsp)
         (c-or-c++-ts-mode . lsp)
         (cuda-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui
	:straight t
	:commands lsp-ui-mode)
;; if you are helm user
(use-package helm-lsp
	:straight t
	:commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy
	:straight t
	:commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs
	:straight t
	:commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode
	:straight t)
(use-package dap-gdb-lldb)
(require 'dap-gdb-lldb)

;; optional if you want which-key integration
(use-package which-key
	:straight t
	:config
  (which-key-mode))


;; Set font last to make sure it stays
(set-frame-font "Ubuntu Mono Medium" nil t)
(set-face-attribute 'default nil :height 120)
(put 'narrow-to-region 'disabled nil)
