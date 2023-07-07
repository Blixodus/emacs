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

(defun split-3-windows-horizontally-evenly ()
  (interactive)
  (command-execute 'split-window-horizontally)
  (command-execute 'split-window-horizontally)
  (command-execute 'balance-windows)
)

(global-set-key (kbd "C-x 4") 'split-3-windows-horizontally-evenly)

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

;; Customize variables (generated)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
	 '(lua-mode eglot treemacs-tab-bar treemacs-persp treemacs-magit treemacs-icons-dired treemacs-projectile treemacs-evil treemacs cmake-font-lock cuda-mode use-package))
 '(safe-local-variable-values
	 '((c-offsets-alist
			(inexpr-class . +)
			(inexpr-statement . +)
			(lambda-intro-cont . +)
			(inlambda . c-lineup-inexpr-block)
			(template-args-cont c-lineup-template-args +)
			(incomposition . +)
			(inmodule . +)
			(innamespace . +)
			(inextern-lang . +)
			(composition-close . 0)
			(module-close . 0)
			(namespace-close . 0)
			(extern-lang-close . 0)
			(composition-open . 0)
			(module-open . 0)
			(namespace-open . 0)
			(extern-lang-open . 0)
			(objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +)
			(objc-method-args-cont . c-lineup-ObjC-method-args)
			(objc-method-intro .
												 [0])
			(friend . 0)
			(cpp-define-intro c-lineup-cpp-define +)
			(cpp-macro-cont . +)
			(cpp-macro .
								 [0])
			(inclass . +)
			(stream-op . c-lineup-streamop)
			(arglist-cont-nonempty c-lineup-gcc-asm-reg c-lineup-arglist)
			(arglist-cont c-lineup-gcc-asm-reg 0)
			(arglist-intro . +)
			(catch-clause . 0)
			(else-clause . 0)
			(do-while-closure . 0)
			(label . 2)
			(access-label . -)
			(substatement-label . 2)
			(substatement . +)
			(statement-case-open . 0)
			(statement-case-intro . +)
			(statement-block-intro . +)
			(statement-cont . +)
			(statement . 0)
			(brace-entry-open . 0)
			(brace-list-entry . 0)
			(brace-list-intro . +)
			(brace-list-close . 0)
			(brace-list-open . 0)
			(block-close . 0)
			(inher-cont . c-lineup-multi-inher)
			(inher-intro . +)
			(member-init-cont . c-lineup-multi-inher)
			(member-init-intro . +)
			(annotation-var-cont . +)
			(annotation-top-cont . 0)
			(topmost-intro-cont . c-lineup-topmost-intro-cont)
			(topmost-intro . 0)
			(knr-argdecl . 0)
			(func-decl-cont . +)
			(inline-close . 0)
			(inline-open . +)
			(class-close . 0)
			(class-open . 0)
			(defun-block-intro . +)
			(defun-close . 0)
			(defun-open . 0)
			(string . c-lineup-dont-change)
			(arglist-close . c-lineup-arglist)
			(substatement-open . 0)
			(case-label . 0)
			(block-open . 0)
			(c . 1)
			(comment-intro . 0)
			(knr-argdecl-intro . -))
		 (c-cleanup-list scope-operator brace-else-brace brace-elseif-brace brace-catch-brace empty-defun-braces list-close-comma defun-close-semi)
		 (c-hanging-semi&comma-criteria c-semi&comma-no-newlines-before-nonblanks)
		 (c-hanging-colons-alist
			(member-init-intro before)
			(inher-intro)
			(case-label after)
			(label after)
			(access-label after))
		 (c-hanging-braces-alist
			(substatement-open after)
			(brace-list-open after)
			(brace-entry-open)
			(defun-open after)
			(class-open after)
			(inline-open after)
			(block-open after)
			(block-close . c-snug-do-while)
			(statement-case-open after)
			(substatement after))
		 (c-comment-only-line-offset . 0)
		 (c-tab-always-indent . t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
