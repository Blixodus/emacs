(setq gc-cons-threshold 10000000)
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

(tool-bar-mode -1)                                    ;; remove toolbar
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))   ;; mouse scroll one line at a time
(setq mouse-wheel-progressive-speed nil)              ;; don't accelerate mouse scrolling
(add-hook 'prog-mode-hook 'display-line-numbers-mode) ;; Line numbers
(setq-default tab-width 2)                            ;; Tabs have a width of 2 spaces
(setq column-number-mode t)                           ;; Display column number on mode line

;; Do not litter with backups
(setq
 backup-by-copying t
 backup-directory-alist
 '(("." . "~/.config/emacs/backups/"))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bootstrap elpaca (do not modify manually) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End bootstrap of elpaca ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Elpaca configuration
;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

;; Tree-sitter configuration
(setq treesit-language-source-alist
			'((c "https://github.com/tree-sitter/tree-sitter-c")
				(cpp "https://github.com/tree-sitter/tree-sitter-cpp")))

(mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
;;Some weird bug with C++ ts mode
;;(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
;;(add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))

;; Org-mode configuration
(use-package org
	:ensure t
	:bind (("C-c o l" . 'org-store-link)
				 ("C-c o a" . 'org-agenda)
				 ("C-c o c" . 'org-capture))
	:config
	(setq org-directory      "~/org"
				org-agenda-files   (list "~/org/" "~/org/dailies/")
				org-log-done 'time)
	(setq org-todo-keywords
				'((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)")
					(sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
					(sequence "WAITING(w)" "|" "POSTPONE" "CANCELED(c)")))
	(setq org-todo-keyword-faces
				'(("STARTED" . "orange")
					("WAITING" . "magenta")
					("POSTPONE" . "blue"))))

(use-package org-roam
	:ensure t
	:bind (("C-c o r f" . 'org-roam-node-find)
				 ("C-c o r l" . 'org-roam-node-insert)
				 ("C-c o r c" . 'org-roam-capture)
				 ("C-c o d t" . 'org-roam-dailies-capture-today)
				 ("C-c o d g" . 'org-roam-dailies-goto-today)
				 ("C-c o d r" . 'org-roam-dailies-capture-tomorrow)
				 ("C-c o d f" . 'org-roam-dailies-goto-tomorrow)
				 ("C-c o d y" . 'org-roam-dailies-capture-yesterday)
				 ("C-c o d h" . 'org-roam-dailies-goto-yesterday)
				 ("C-c o d e" . 'org-roam-dailies-capture-date)
				 ("C-c o d d" . 'org-roam-dailies-goto-date)
				 ("C-c o d n" . 'org-roam-dailies-goto-next-note)
				 ("C-c o d p" . 'org-roam-dailies-goto-previous-note))
	:config
	(setq org-roam-directory (file-truename "~/org"))
	(setq org-roam-dailies-directory "dailies")
	(setq org-roam-dailies-capture-templates
				'(("d" "default" entry
					 "* %?"
					 :target (file+head "%<%Y-%m-%d>.org"
															"#+title: %<%Y-%m-%d>\n"))))
	(org-roam-db-autosync-mode))

;; Guix package manager utility for Emacs
(use-package geiser
	:ensure t
	:config
	(setq geiser-guile-binary (executable-find "guile-3.0"))
	(setq geiser-default-implementation '(guile)))

(use-package geiser-guile
	:ensure t)

(use-package guix
	:ensure t)

;; Package to explore assembly (Compiler explorer style)
(use-package rmsbolt
	:ensure t)

;; Multiple cursors
(use-package multiple-cursors
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
  :ensure (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
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
	:ensure t)

;; Highlight TODO, FIXME, etc.
(use-package hl-todo
  :ensure (:host github :repo "tarsius/hl-todo" :files ("*.el"))
  :hook (prog-mode . hl-todo-mode))

;; ;; CUDA Mode
(use-package cuda-mode
	:ensure t)
(add-to-list 'auto-mode-alist '("\\.cu\\'" . cuda-mode))

;; CMake Mode
(use-package cmake-mode
	:ensure (:host github :repo "Kitware/CMake" :files ("Auxiliary/cmake-mode.el")))

(use-package cmake-font-lock
	:ensure t)

;; ParEdit
(use-package paredit
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

(use-package envrc
	:ensure t
  :bind (:map envrc-mode-map
							("C-c e" . envrc-command-map))
  ;:config (which-key-add-key-based-replacements "C-c e" "envrc")
  :init (envrc-global-mode))

;; Motion aid
(use-package avy
	:ensure t
  :demand t
  :bind (("C-c j" . avy-goto-line)
         ("s-j"   . avy-goto-char-timer)))

;; Ivy and Counsel (better completion)
(use-package ivy
	:ensure t
  :config (ivy-mode))

(use-package counsel
	:ensure t
  :after ivy
  :config (counsel-mode))

;; Generally useful stuff for working on projects

(load "/usr/share/emacs/site-lisp/clang-format/clang-format.el")
(global-set-key (kbd "C-M-i") 'clang-format-buffer)

(elpaca transient)
(elpaca git-commit)
(elpaca magit)

(use-package company
	:ensure t
  :init
  (setq company-idle-delay 0)
	:hook ((prog-mode . (lambda () (when (file-remote-p default-directory) (company-mode -1)))))
  :config
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (global-company-mode))

;; (use-package company-box
;;   :straight (:host github :repo "sebastiencs/company-box" :files ("*.el"))
;;   :ensure t)
;; (add-hook 'company-mode-hook 'company-box-mode)

(use-package projectile
	:ensure t
  :after ivy
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package counsel-projectile
  :after projectile
  :ensure t
  :config (counsel-projectile-mode))

;; Eglot
(use-package eglot
	:ensure nil
	:hook ((c-mode . eglot-ensure)
				 (c++-mode . eglot-ensure)
				 (c-ts-mode . eglot-ensure)
				 (c++-ts-mode . eglot-ensure)
				 (cuda-mode . eglot-ensure)
				 (eglot-managed-mode . (lambda () (eglot-inlay-hints-mode -1)))))

(require 'eglot)
(add-to-list 'eglot-server-programs '((c++-ts-mode c-ts-mode c++-mode c-mode cuda-mode) "clangd"))

;; ;; LSP-Mode and DAP-Mode
;; (use-package lsp-mode
;; 	:ensure t
;;   :init
;;   ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;;   (setq lsp-keymap-prefix "C-c l")
;;   :hook ((c-mode . lsp)
;;          (c++-mode . lsp)
;;          (c-or-c++-mode . lsp)
;; 				 (c-ts-mode . lsp)
;;          (c++-ts-mode . lsp)
;;          (c-or-c++-ts-mode . lsp)
;;          (cuda-mode . lsp)
;;          ;; if you want which-key integration
;;          (lsp-mode . lsp-enable-which-key-integration))
;;   :commands lsp)

;; ;; optionally
;; (use-package lsp-ui
;; 	:ensure t
;; 	:commands lsp-ui-mode)
;; ;; if you are helm user
;; (use-package helm-lsp
;; 	:ensure t
;; 	:commands helm-lsp-workspace-symbol)
;; ;; if you are ivy user
;; (use-package lsp-ivy
;; 	:ensure t
;; 	:commands lsp-ivy-workspace-symbol)
;; (use-package lsp-treemacs
;; 	:ensure t
;; 	:commands lsp-treemacs-errors-list)

;; ;; optionally if you want to use debugger
;; (use-package dap-mode
;; 	:ensure t)
;; ;; (use-package dap-gdb-lldb
;; ;; 	:ensure t)

;; optional if you want which-key integration
(use-package which-key
	:ensure t
	:config
  (which-key-mode))

;; libvterm intergation
(use-package vterm
    :ensure t)

;; Emacs configuration
(use-package emacs
	:ensure nil
	:hook
  ;; Auto parenthesis matching
  ((prog-mode . electric-pair-mode)))

;; Set font last to make sure it stays
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes '(modus-vivendi))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka Nerd Font Mono" :foundry "UKWN" :slant normal :weight regular :height 120 :width normal)))))
