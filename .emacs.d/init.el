(require 'package)
(require 'cl)
(add-to-list 'package-archives
       '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(defun is-gui()
  (display-graphic-p))

(defmacro when-gui(&rest body)
  `(when (is-gui) ,@body))

(defun is-term()
  (not (display-graphic-p)))

(defmacro when-term(&rest body)
  `(when (is-term),@body))

;get rid of all distractions. No fear baby
(setq inhibit-startup-message t)
;emacs bugs up if you fullscreen immediately
(run-with-idle-timer 0.1 nil 'toggle-frame-fullscreen)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq default-directory "~/")

(add-to-list 'default-frame-alist
             '(font . "Fira Mono"))

;consolidate all auto-save files and backups
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(package-selected-packages
   (quote
    (auctex js2-mode fill-column-indicator auto-complete web-mode jinja2-mode geiser racket-mode smart-tab paredit parinfer ace-jump-mode avy w3m slime flycheck ample-theme))))

;if necessary, create the directory for autosaving
(make-directory "~/.emacs.d/autosaves/" t)

(setq frame-title-format "emacs ~ %b")
(fset 'yes-or-no-p 'y-or-n-p)
(global-linum-mode 1)
;give spacing between line number and text in terminal
(when-term
 (setq linum-format "%d "))
(require 'ido)
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(define-key global-map (kbd "RET") 'newline-and-indent)
(setq next-line-add-newlines t)
(setq-default indent-tabs-mode nil)

(dolist (command '(yank yank-pop))
   (eval `(defadvice ,command (after indent-region activate)
            (and (not current-prefix-arg)
                 (member major-mode '(emacs-lisp-mode lisp-mode
                                                      clojure-mode    scheme-mode
                                                      haskell-mode    ruby-mode
                                                      rspec-mode      python-mode
                                                      c-mode          c++-mode
                                                      objc-mode       latex-mode
                                                      plain-tex-mode))
                 (let ((mark-even-if-inactive transient-mark-mode))
                   (indent-region (region-beginning) (region-end) nil))))))

(when-gui
 (load-theme 'ample t t)
 (enable-theme 'ample))
;(load-theme 'ample-flat t t)
;(load-theme 'ample-light t t)

;;;LISP settings;;;
;;parinfer
(use-package parinfer
    :ensure t
    :bind
    (("C-," . parinfer-toggle-mode))
    :init
    (progn
      (setq parinfer-extensions
            '(defaults       ; should be included.
              pretty-parens  ; different paren styles for different modes.
              paredit        ; Introduce some paredit commands.
              smart-tab))      ; C-b & C-f jump positions and smart shift with tab & S-tab.
;      (setq parinfer-auto-switch-indent-mode t) ;; automatically switch to indent-mode
;     (setq parinfer-auto-switch-indent-mode-when-closing t) ;; likewise as above
      (add-hook 'clojure-mode-hook #'parinfer-mode)
      (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
      (add-hook 'common-lisp-mode-hook #'parinfer-mode)
      (add-hook 'scheme-mode-hook #'parinfer-mode)
      (add-hook 'lisp-mode-hook #'parinfer-mode)))

(when-gui
 (load (expand-file-name  "~/quicklisp/slime-helper.el"))
 (setq inferior-lisp-program "/bin/sbcl")
 (setq slime-contribs '(slime-fancy))
 (setq slime-protocol-version 'ignore)
 (add-hook 'slime-repl-mode-hook #'delete-other-windows))
; (slime))

;;geiser for scheme
(setq geiser-active-implementations '(racket))
(setq geiser-repl-use-other-window nil)
(setq geiser-mode-start-repl-p t)

;;;Python settings;;;
(add-hook 'python-mode-hook '(lambda ()
                              (setq python-indent 4)))

;;;C settings;;;
;;set default indentation to 4 spaces
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "k&r")))
(setq-default c-basic-offset 4)
;; Run C programs directly from within emacs

(defun execute-c-program ()
  (interactive)
  (gcc-program :cpp nil))

(defun execute-cpp-program ()
  (interactive)
  (gcc-program :cpp t))

(defun gcc-program (&key cpp)
  (save-buffer)
  (delete-other-windows)
  (split-window-right)
  (other-window 1)
  (let* ((sans-extension (file-name-sans-extension buffer-file-name))
         (compiler (if cpp "g++" "gcc -std=c11"))
         (foo (concat compiler " -Wall -o " sans-extension " " buffer-file-name " && " sans-extension)))
    (async-shell-command foo)))

(defun my-c-initialization-hook ()
  (local-set-key (kbd "C-c C-c") 'execute-c-program))
;;c-initialization-hook evaluated when c major mode first starts
(add-hook 'c-mode-hook 'my-c-initialization-hook)

(defun my-cpp-initialization-hook ()
  (local-set-key (kbd "C-c C-c") 'execute-cpp-program))
(add-hook 'c++-mode-hook 'my-cpp-initialization-hook)

; j2s-mode for javascript
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;(add-hook 'web-mode-hook 'js2-minor-mode)

;;change default browser for 'browse-url'  to w3m
(setq browse-url-browser-function 'w3m-goto-url-new-session)

;;change w3m user-agent to android
(setq w3m-user-agent "Mozilla/5.0 (Linux; U; Android 2.3.3; zh-tw; HTC_Pyramid Build/GRI40) AppleWebKit/533.1 (KHTML, like Gecko) Version/4.0 Mobile Safari/533.")

;;when I want to enter the web address all by hand
(defun w3m-open-site (site)
  "Opens site in new w3m session with 'http://' appended"
  (interactive
   (list (read-string "Enter website address(default: w3m-home):" nil nil w3m-home-page nil)))
  (w3m-goto-url-new-session
   (concat "http://" site)))

(setq w3m-default-display-inline-images t)

;;
;; tramp settings
;;
(require 'tramp)
(setq tramp-default-method "ssh")

;; Many thanks go to Andy Sloane at https://www.a1k0n.net for the following tramp settings (with my own edits, of course)
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))

(setq lisp-indent-function 'common-lisp-indent-function
      slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

(defvar *my-box-tramp-path*
  "/ssh:desmond@desmondcheong.com:")

(defvar *current-tramp-path* nil)
(defun connect-to-host (path)
  (setq *current-tramp-path* path)
  (setq slime-translate-from-lisp-filename-function
        (lambda (f)
          (concat *current-tramp-path* f)))
  (setq slime-translate-to-lisp-filename-function
        (lambda (f)
          (substring f (length *current-tramp-path*))))
  ;establish shh tunnel with server to connect slime
  (shell-command "ssh -f -N slime-tunnel")
  (slime-connect "localhost" 4005))

(defun my-slime ()
  (interactive)
  (connect-to-host *my-box-tramp-path*))

(defun my-box ()
  (interactive)
  (find-file *my-box-tramp-path*))

;;
;; ace jump mode major function
;; 
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;;only use lower case characters
(setq ace-jump-mode-move-keys
      (loop for i from ?a to ?z collect i))
;; you can select the key you prefer to
(define-key global-map (kbd "C-c C-f") 'ace-jump-mode)
(define-key global-map (kbd "C-c C-l") 'ace-jump-line-mode)


;; 
;; enable a more powerful jump back function from ace jump mode
;;
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-c C-b") 'ace-jump-mode-pop-mark)

;;magit settings
(global-set-key (kbd "C-x g") 'magit-status)

;;auto-complete
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)

;;web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(defun my-web-mode-hook ()
  "Hook for web-mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(add-hook 'web-mode-hook 'my-web-mode-hook)

;;shell shortcut
(global-set-key [f1] 'shell)

;;control h to backspace
(global-set-key (kbd "C-? k") 'describe-key)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-region)
(global-set-key (kbd "C-.") 'other-window)
(global-set-key (kbd "C-j") 'back-to-indentation)

;;start flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;;always start a shell in emacs
;;but in the same window
(push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)
;;same goes for shell commands
(push (cons "\\*Async Shell Command\\*" display-buffer--same-window-action) display-buffer-alist)
(push (cons "\\*Shell Command\\*" display-buffer--same-window-action) display-buffer-alist)
;(when-gui
; (shell))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
