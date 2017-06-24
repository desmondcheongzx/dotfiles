(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;get rid of all distractions. No fear baby
(setq inhibit-startup-message t)
;emacs bugs up if you fullscreen immediately
(run-with-idle-timer 0.1 nil 'toggle-frame-fullscreen)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq default-directory "~/")

;consolidate all auto-save files and backups
(custom-set-variables
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))

;if necessary, create the directory for autosaving
(make-directory "~/.emacs.d/autosaves/" t)

(setq frame-title-format "emacs ~ %b")
(fset 'yes-or-no-p 'y-or-n-p)
(global-linum-mode 1)
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

(load-theme 'ample t t)
;(load-theme 'ample-flat t t)
;(load-theme 'ample-light t t)
(enable-theme 'ample)

;;;LISP settings;;;
;;paredit

(load (expand-file-name  "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy))
(setq slime-protocol-version 'ignore)
(require 'slime)
(add-hook 'slime-repl-mode-hook #'delete-other-windows)
;(slime)

;;geiser for scheme
(setq geiser-active-implementations '(racket))
(setq geiser-repl-use-other-window nil)
(setq geiser-mode-start-repl-p t)

;;;Python settings;;;
(add-hook 'python-mode-hook '(lambda ()
                               (setq python-indent 2)))

;;;C settings;;;
;;set default indentation to 4 spaces
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "k&r")))
(setq-default c-basic-offset 4)
;; Run C programs directly from within emacs
(defun execute-c-program ()
  (interactive)
  (save-buffer)
  (delete-other-windows)
  (split-window-right)
  (other-window 1)
  (let* ((sans-extension (file-name-sans-extension buffer-file-name))
         (foo (concat "gcc -std=c11 -Wall -o " sans-extension " " buffer-file-name " && " sans-extension)))
    (async-shell-command foo)))

(defun my-c-initialization-hook ()
  (local-set-key (kbd "C-c C-c") 'execute-c-program))
;;c-initialization-hook evaluated when c major mode first starts
(add-hook 'c-initialization-hook 'my-c-initialization-hook)

;;change default browser for 'browse-url'  to w3m
(setq browse-url-browser-function 'w3m-goto-url-new-session)

;;change w3m user-agent to android
(setq w3m-user-agent "Mozilla/5.0 (Linux; U; Android 2.3.3; zh-tw; HTC_Pyramid Build/GRI40) AppleWebKit/533.1 (KHTML, like Gecko) Version/4.0 Mobile Safari/533.")

;;when I want to enter the web address all by hand
(defun w3m-open-site (site)
  "Opens site in new w3m session with 'http://' appended"
  (interactive
   (list (read-string "Enter website address(default: w3m-home):" nil nil w3m-home-page nil )))
  (w3m-goto-url-new-session
   (concat "http://" site)))

(setq w3m-default-display-inline-images t)

;;
;; tramp settings
;;
(setq tramp-default-method "ssh")


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
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)


;;magit settings
(global-set-key (kbd "C-x g") 'magit-status)

;;shell shortcut
(global-set-key [f1] 'shell)

;;control h to backspace
(global-set-key (kbd "C-? k") 'describe-key)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-region)
(global-set-key (kbd "C-.") 'other-window)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (ace-jump-mode avy w3m slime flycheck ample-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "gray13" :foreground "#bdbdb3" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "PfEd" :family "Source Code Pro Medium")))))

;;start flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;;always start a shell in emacs
;;but in the same window
(push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)
;;same goes for shell commands
(push (cons "\\*Async Shell Command\\*" display-buffer--same-window-action) display-buffer-alist)
(push (cons "\\*Shell Command\\*" display-buffer--same-window-action) display-buffer-alist)
(shell)
