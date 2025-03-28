;;;; PvE's minimal Emacs configuration.

;;; To use it, run:
;;; $ emacs -q --load .emacs
;;;
;;; For "dark" mode, run:
;;; $ emacs -q -rv --load .emacs

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Libs
;;;

(require 'package)
(require 'cl-lib)
(require 'dired-x)
(require 'ibuffer)
(require 'shell)
(require 'windmove)

;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;; (package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Load path
;;;

(when load-file-name
  (let ((here (file-name-directory (file-truename load-file-name))))
    (add-to-list 'load-path here)))

;; Ibs is included
(let ((ibs (locate-library "ibs")))
  (when ibs
    ;; Set cycling key like this:
    ;; (setq ibs-cycling-key "<C-tab>")
    (load ibs)))

(add-to-list 'load-path "~/emacs")
(add-to-list 'load-path "~/emacs/elisp")
(add-to-list 'load-path "~/Emacs")
(add-to-list 'load-path "~/Emacs/elisp")

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Modes
;;;

(menu-bar-mode 1)
(tool-bar-mode 0)
(show-paren-mode 1)
(global-font-lock-mode 1)
(column-number-mode 1)
(windmove-default-keybindings)
(indent-tabs-mode -1)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Variables etc.
;;;

(setq inhibit-startup-screen t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      scroll-bar-mode nil
      dabbrev-case-fold-search t
      dired-dwim-target t
      dired-listing-switches "-alh"
      dired-recursive-copies 'always
      calendar-date-style 'iso
      calendar-week-start-day 1
      european-calendar-style t)

(fset 'yes-or-no-p #'y-or-n-p)

(add-hook 'emacs-startup-hook
          (lambda ()
            (when (and (< 1 (length (window-list)))
                       (equal "*scratch*" (buffer-name (current-buffer))))
              (delete-window))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Keys
;;;

(global-set-key (kbd "C-x C-b") 'bs-show)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "§") 'dabbrev-expand) ; §
(global-set-key (kbd "M-1") "(")
(global-set-key (kbd "M-2") ")")
(global-set-key (kbd "C-<delete>") 'fixup-whitespace)
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-<") 'other-window)
(global-set-key (kbd "<insert>") 'other-window)
(global-set-key (kbd "M-<end>") 'other-window)
(global-set-key (kbd "M-<up>") 'backward-sexp)
(global-set-key (kbd "M-<down>") 'forward-sexp)

(define-key emacs-lisp-mode-map '[S-iso-lefttab]
  'completion-at-point)

(define-key emacs-lisp-mode-map (kbd "M-r") 'raise-sexp)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Dark window borders
;;;

(defun pve-set-frame-dark (frame)
  (let ((id (frame-parameter frame 'outer-window-id)))
    (call-process-shell-command
     (concat "xprop -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT \"dark\" -id \""
             id
             "\""))))

(defun pve-set-selected-frame-dark ()
  (interactive)
  (pve-set-frame-dark (selected-frame)))

(defun pve-ensure-all-frames-dark (&optional frame)
  (dolist (frame (frame-list))
    (pve-set-frame-dark frame)))

(when (window-system)
  (pve-ensure-all-frames-dark))

;; (add-hook 'after-make-frame-functions 'pve-set-frame-dark)
;; (add-hook 'desktop-after-read-hook 'pve-ensure-all-frames-dark)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; pve-query-replace
;;;

(defun pve-region-as-string ()
  (buffer-substring-no-properties (mark) (point)))

(defun pve-query-replace (prefix)
  (interactive "P")
  (if (region-active-p)
      (let* ((from (pve-region-as-string))
             (to (read-from-minibuffer
                  (format "Query replace: %s -> " from)
                  from)))
        (if prefix
            (goto-char (min (mark) (point)))
          (goto-char 1))
        (deactivate-mark)
        (query-replace from to))
    (call-interactively 'query-replace)))

(global-set-key (kbd "C-M-5") 'pve-query-replace)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Dired
;;;

(defun pve-dired-kill-filename-at-point ()
  (interactive)
  (kill-new (file-truename (dired-file-name-at-point))))

(define-key dired-mode-map (kbd "C-c C-f") 'pve-dired-kill-filename-at-point)

(defun pve-dired-open-terminal-here ()
  (interactive)
  (call-process "gnome-terminal"))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Emacs lisp
;;;

(defun pve-emacs-lisp-hook ()
  (eldoc-mode 1))

(add-hook 'emacs-lisp-mode-hook 'pve-emacs-lisp-hook)
