;;; voyager.el --- DAP debugger  -*- lexical-binding: t -*-

;; Filename: voyager.el
;; Description: DAP debugger
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-06-15 14:10:12
;; Version: 0.5
;; Last-Updated: 2023-08-12 21:08:48
;;           By: Andy Stewart
;; URL: https://github.com/manateelazycat/voyager
;; Keywords:
;; Compatibility: emacs-version >= 28
;; Package-Requires: ((emacs "28") (posframe "1.1.7") (markdown-mode "2.6"))
;;
;; Features that might be required by this library:
;;
;; Please check README
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Voyager
;;

;;; Installation:
;;
;; Please check README
;;

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET voyager RET
;;

;;; Change log:
;;
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Code:
(require 'cl-lib)
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'voyager-epc)

(defgroup voyager nil
  "Voyager group."
  :group 'applications)

(defvar voyager-server nil
  "The Voyager Server.")

(defvar voyager-python-file (expand-file-name "voyager.py" (if load-file-name
                                                               (file-name-directory load-file-name)
                                                             default-directory)))

(defvar voyager-server-port nil)

(defun voyager--start-epc-server ()
  "Function to start the EPC server."
  (unless (process-live-p voyager-server)
    (setq voyager-server
          (voyager-epc-server-start
           (lambda (mngr)
             (let ((mngr mngr))
               (voyager-epc-define-method mngr 'eval-in-emacs 'voyager--eval-in-emacs-func)
               (voyager-epc-define-method mngr 'get-emacs-var 'voyager--get-emacs-var-func)
               (voyager-epc-define-method mngr 'get-emacs-vars 'voyager--get-emacs-vars-func)
               (voyager-epc-define-method mngr 'get-user-emacs-directory 'voyager--user-emacs-directory)
               ))))
    (if voyager-server
        (setq voyager-server-port (process-contact voyager-server :service))
      (error "[Voyager] voyager-server failed to start")))
  voyager-server)

(defun voyager--eval-in-emacs-func (sexp-string)
  (eval (read sexp-string))
  ;; Return nil to avoid epc error `Got too many arguments in the reply'.
  nil)

(defun voyager--get-emacs-var-func (var-name)
  (let* ((var-symbol (intern var-name))
         (var-value (symbol-value var-symbol))
         ;; We need convert result of booleanp to string.
         ;; Otherwise, python-epc will convert all `nil' to [] at Python side.
         (var-is-bool (prin1-to-string (booleanp var-value))))
    (list var-value var-is-bool)))

(defun voyager--get-emacs-vars-func (&rest vars)
  (mapcar #'voyager--get-emacs-var-func vars))

(defvar voyager-epc-process nil)

(defvar voyager-internal-process nil)
(defvar voyager-internal-process-prog nil)
(defvar voyager-internal-process-args nil)

(defcustom voyager-name "*voyager*"
  "Name of Voyager buffer."
  :type 'string)

(defcustom voyager-python-command (if (memq system-type '(cygwin windows-nt ms-dos)) "python.exe" "python3")
  "The Python interpreter used to run voyager.py."
  :type 'string)

(defcustom voyager-enable-debug nil
  "If you got segfault error, please turn this option.
Then Voyager will start by gdb, please send new issue with `*voyager*' buffer content when next crash."
  :type 'boolean)

(defcustom voyager-enable-log nil
  "Enable this option to print log message in `*voyager*' buffer, default only print message header."
  :type 'boolean)

(defcustom voyager-enable-profile nil
  "Enable this option to output performance data to ~/voyager.prof."
  :type 'boolean)

(defun voyager--user-emacs-directory ()
  "Get lang server with project path, file path or file extension."
  (expand-file-name user-emacs-directory))

(defun voyager-call-async (method &rest args)
  "Call Python EPC function METHOD and ARGS asynchronously."
  (if (voyager-epc-live-p voyager-epc-process)
      (voyager-deferred-chain
        (voyager-epc-call-deferred voyager-epc-process (read method) args))
    (setq voyager-first-call-method method)
    (setq voyager-first-call-args args)
    ))

(defvar voyager-first-call-method nil)
(defvar voyager-first-call-args nil)

(defun voyager-restart-process ()
  "Stop and restart Voyager process."
  (interactive)
  (voyager-kill-process)
  (voyager-start-process)
  (message "[Voyager] Process restarted."))

(defun voyager-start-process ()
  "Start Voyager process if it isn't started."
  (if (voyager-epc-live-p voyager-epc-process)
      (remove-hook 'post-command-hook #'voyager-start-process)
    ;; start epc server and set `voyager-server-port'
    (voyager--start-epc-server)
    (let* ((voyager-args (append
                          (list voyager-python-file)
                          (list (number-to-string voyager-server-port))
                          (when voyager-enable-profile
                            (list "profile"))
                          )))

      ;; Set process arguments.
      (if voyager-enable-debug
          (progn
            (setq voyager-internal-process-prog "gdb")
            (setq voyager-internal-process-args (append (list "-batch" "-ex" "run" "-ex" "bt" "--args" voyager-python-command) voyager-args)))
        (setq voyager-internal-process-prog voyager-python-command)
        (setq voyager-internal-process-args voyager-args))

      ;; Start python process.
      (let ((process-connection-type t))
        (setq voyager-internal-process
              (apply 'start-process
                     voyager-name voyager-name
                     voyager-internal-process-prog voyager-internal-process-args)))
      (set-process-query-on-exit-flag voyager-internal-process nil))))

(defvar voyager-stop-process-hook nil)

(defun voyager-kill-process ()
  "Stop Voyager process and kill all Voyager buffers."
  (interactive)

  ;; Run stop process hooks.
  (run-hooks 'voyager-stop-process-hook)

  ;; Kill process after kill buffer, make application can save session data.
  (voyager--kill-python-process))

(add-hook 'kill-emacs-hook #'voyager-kill-process)

(defun voyager--kill-python-process ()
  "Kill Voyager background python process."
  (when (voyager-epc-live-p voyager-epc-process)
    ;; Cleanup before exit Voyager server process.
    (voyager-call-async "cleanup")
    ;; Delete Voyager server process.
    (voyager-epc-stop-epc voyager-epc-process)
    ;; Kill *voyager* buffer.
    (when (get-buffer voyager-name)
      (kill-buffer voyager-name))
    (setq voyager-epc-process nil)
    (message "[Voyager] Process terminated.")))

(defun voyager--first-start (voyager-epc-port)
  "Call `voyager--open-internal' upon receiving `start_finish' signal from server."
  ;; Make EPC process.
  (setq voyager-epc-process (make-voyager-epc-manager
                             :server-process voyager-internal-process
                             :commands (cons voyager-internal-process-prog voyager-internal-process-args)
                             :title (mapconcat 'identity (cons voyager-internal-process-prog voyager-internal-process-args) " ")
                             :port voyager-epc-port
                             :connection (voyager-epc-connect "127.0.0.1" voyager-epc-port)
                             ))
  (voyager-epc-init-epc-layer voyager-epc-process)

  (when (and voyager-first-call-method
             voyager-first-call-args)
    (voyager-deferred-chain
      (voyager-epc-call-deferred voyager-epc-process
                                 (read voyager-first-call-method)
                                 voyager-first-call-args)
      (setq voyager-first-call-method nil)
      (setq voyager-first-call-args nil)
      )))

(defun voyager-start ()
  (interactive)
  (voyager-call-async "start" (voyager-get-buffer-file-name)))

(defun voyager-set-function-breakpoint ()
  (interactive)
  (let* ((function-name (voyager-get-function-name))
         (name (read-string (if (string-empty-p function-name)
                                "Set function breakpoint: "
                              (format "Set function breakpoint: (%s) " function-name))
                            nil nil function-name)))
    (if (string-empty-p name)
        (message "Please input function name.")
      (voyager-call-async "set_function_breakpoint" (voyager-get-buffer-file-name) name))))

(defun voyager-configure-done ()
  (interactive)
  (voyager-call-async "configure_done" (voyager-get-buffer-file-name)))

(defun voyager-get-buffer-file-name ()
  (file-truename (buffer-file-name)))

(defun voyager-get-match-nodes (query)
  (ignore-errors
    (mapcar #'(lambda (range)
                (treesit-node-at (car range)))
            (treesit-query-range
             (treesit-node-language (treesit-buffer-root-node))
             query))))

(defun voyager-get-function-name ()
  (let* ((function-nodes (append (voyager-get-match-nodes '((function_definition name: (symbol) @x)))
                                 (voyager-get-match-nodes '((function_definition name: (identifier) @x)))
                                 (voyager-get-match-nodes '((method_declaration name: (identifier) @x)))
                                 ))
         (function-name (catch 'found
                          (dolist (function-node function-nodes)
                            (when (and (> (point) (treesit-node-start (treesit-node-parent function-node)))
                                       (< (point) (treesit-node-end (treesit-node-parent function-node))))
                              (throw 'found (treesit-node-text function-node t))))
                          (throw 'found ""))))
    function-name))

(defun voyager-enable ()
  (add-hook 'post-command-hook #'voyager-start-process))

(provide 'voyager)

;;; voyager.el ends here
