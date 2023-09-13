;;; epcs.el --- EPC Server              -*- lexical-binding: t -*-

;; Copyright (C) 2011,2012,2013  Masashi Sakurai

;; Author: Masashi Sakurai <m.sakurai at kiwanami.net>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; deferred
(cl-defmacro voyager-deferred-chain (&rest elements)
  "Anaphoric function chain macro for deferred chains."
  (declare (debug (&rest form))
           (indent 0))
  `(let (it)
     ,@(cl-loop for i in elements
                collect
                `(setq it ,i))
     it))

;; Debug
(defvar voyager-deferred-debug nil
  "Debug output switch.")

(defvar voyager-deferred-debug-count 0
  "[internal] Debug output counter.")

(defun voyager-deferred-log (&rest args)
  "[internal] Debug log function."
  (when voyager-deferred-debug
    (with-current-buffer (get-buffer-create "*voyager-deferred-log*")
      (save-excursion
        (goto-char (point-max))
        (insert (format "%5i %s\n\n\n" voyager-deferred-debug-count (apply #'format args)))))
    (cl-incf voyager-deferred-debug-count)))

(defvar voyager-deferred-debug-on-signal nil
  "If non nil, the value `debug-on-signal' is substituted this
value in the `condition-case' form in deferred
implementations. Then, Emacs debugger can catch an error occurred
in the asynchronous tasks.")

(cl-defmacro voyager-deferred-condition-case (var protected-form &rest handlers)
  "[internal] Custom condition-case. See the comment for
`voyager-deferred-debug-on-signal'."
  (declare (debug condition-case)
           (indent 1))
  `(let ((debug-on-signal
          (or debug-on-signal voyager-deferred-debug-on-signal)))
     (condition-case ,var
         ,protected-form
       ,@handlers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Back end functions of deferred tasks

(defvar voyager-deferred-tick-time 0.001
  "Waiting time between asynchronous tasks (second).
The shorter waiting time increases the load of Emacs. The end
user can tune this parameter. However, applications should not
modify it because the applications run on various environments.")

(defvar voyager-deferred-queue nil
  "[internal] The execution queue of deferred objects.
See the functions `voyager-deferred-post-task' and `voyager-deferred-worker'.")

(defun voyager-deferred-post-task (d which &optional arg)
  "[internal] Add a deferred object to the execution queue
`voyager-deferred-queue' and schedule to execute.
D is a deferred object. WHICH is a symbol, `ok' or `ng'. ARG is
an argument value for execution of the deferred task."
  (let ((pack `(,d ,which . ,arg)))
    (push pack voyager-deferred-queue)
    (voyager-deferred-log "QUEUE-POST [%s]: %s" (length voyager-deferred-queue) pack)
    (run-at-time voyager-deferred-tick-time nil 'voyager-deferred-worker)
    d))

(defun voyager-deferred-worker ()
  "[internal] Consume a deferred task.
Mainly this function is called by timer asynchronously."
  (when voyager-deferred-queue
    (let* ((pack (car (last voyager-deferred-queue)))
           (d (car pack))
           (which (cadr pack))
           (arg (cddr pack)) value)
      (setq voyager-deferred-queue (nbutlast voyager-deferred-queue))
      (condition-case err
          (setq value (voyager-deferred-exec-task d which arg))
        (error
         (voyager-deferred-log "ERROR : %s" err)
         (message "deferred error : %s" err)))
      value)))

;; Struct: voyager-deferred-object
;;
;; callback    : a callback function (default `identity')
;; errorback   : an errorback function (default `voyager-deferred-resignal')
;; cancel      : a canceling function (default `voyager-deferred-default-cancel')
;; next        : a next chained deferred object (default nil)
;; status      : if 'ok or 'ng, this deferred has a result (error) value. (default nil)
;; value       : saved value (default nil)
;;
(cl-defstruct voyager-deferred-object
  (callback 'identity)
  (errorback 'voyager-deferred-resignal)
  (cancel 'voyager-deferred-default-cancel)
  next status value)

(defun voyager-deferred-resignal (err)
  "[internal] Safely resignal ERR as an Emacs condition.

If ERR is a cons (ERROR-SYMBOL . DATA) where ERROR-SYMBOL has an
`error-conditions' property, it is re-signaled unchanged. If ERR
is a string, it is signaled as a generic error using `error'.
Otherwise, ERR is formatted into a string as if by `print' before
raising with `error'."
  (cond ((and (listp err)
              (symbolp (car err))
              (get (car err) 'error-conditions))
         (signal (car err) (cdr err)))
        ((stringp err)
         (error "%s" err))
        (t
         (error "%S" err))))

(defun voyager-deferred-default-cancel (d)
  "[internal] Default canceling function."
  (voyager-deferred-log "CANCEL : %s" d)
  (setf (voyager-deferred-object-callback d) 'identity)
  (setf (voyager-deferred-object-errorback d) 'voyager-deferred-resignal)
  (setf (voyager-deferred-object-next d) nil)
  d)

(defun voyager-deferred-exec-task (d which &optional arg)
  "[internal] Executing deferred task. If the deferred object has
next deferred task or the return value is a deferred object, this
function adds the task to the execution queue.
D is a deferred object. WHICH is a symbol, `ok' or `ng'. ARG is
an argument value for execution of the deferred task."
  (voyager-deferred-log "EXEC : %s / %s / %s" d which arg)
  (when (null d) (error "voyager-deferred-exec-task was given a nil."))
  (let ((callback (if (eq which 'ok)
                      (voyager-deferred-object-callback d)
                    (voyager-deferred-object-errorback d)))
        (next-deferred (voyager-deferred-object-next d)))
    (cond
     (callback
      (voyager-deferred-condition-case err
                                       (let ((value (funcall callback arg)))
                                         (cond
                                          ((voyager-deferred-object-p value)
                                           (voyager-deferred-log "WAIT NEST : %s" value)
                                           (if next-deferred
                                               (voyager-deferred-set-next value next-deferred)
                                             value))
                                          (t
                                           (if next-deferred
                                               (voyager-deferred-post-task next-deferred 'ok value)
                                             (setf (voyager-deferred-object-status d) 'ok)
                                             (setf (voyager-deferred-object-value d) value)
                                             value))))
                                       (error
                                        (cond
                                         (next-deferred
                                          (voyager-deferred-post-task next-deferred 'ng err))
                                         (t
                                          (voyager-deferred-log "ERROR : %S" err)
                                          (message "deferred error : %S" err)
                                          (setf (voyager-deferred-object-status d) 'ng)
                                          (setf (voyager-deferred-object-value d) err)
                                          err)))))
     (t                                 ; <= (null callback)
      (cond
       (next-deferred
        (voyager-deferred-exec-task next-deferred which arg))
       ((eq which 'ok) arg)
       (t                               ; (eq which 'ng)
        (voyager-deferred-resignal arg)))))))

(defun voyager-deferred-set-next (prev next)
  "[internal] Connect deferred objects."
  (setf (voyager-deferred-object-next prev) next)
  (cond
   ((eq 'ok (voyager-deferred-object-status prev))
    (setf (voyager-deferred-object-status prev) nil)
    (let ((ret (voyager-deferred-exec-task
                next 'ok (voyager-deferred-object-value prev))))
      (if (voyager-deferred-object-p ret) ret
        next)))
   ((eq 'ng (voyager-deferred-object-status prev))
    (setf (voyager-deferred-object-status prev) nil)
    (let ((ret (voyager-deferred-exec-task next 'ng (voyager-deferred-object-value prev))))
      (if (voyager-deferred-object-p ret) ret
        next)))
   (t
    next)))

(defun voyager-deferred-new (&optional callback)
  "Create a deferred object."
  (if callback
      (make-voyager-deferred-object :callback callback)
    (make-voyager-deferred-object)))

(defun voyager-deferred-callback (d &optional arg)
  "Start deferred chain with a callback message."
  (voyager-deferred-exec-task d 'ok arg))

(defun voyager-deferred-errorback (d &optional arg)
  "Start deferred chain with an errorback message."
  (declare (indent 1))
  (voyager-deferred-exec-task d 'ng arg))

(defun voyager-deferred-callback-post (d &optional arg)
  "Add the deferred object to the execution queue."
  (declare (indent 1))
  (voyager-deferred-post-task d 'ok arg))

(defun voyager-deferred-next (&optional callback arg)
  "Create a deferred object and schedule executing. This function
is a short cut of following code:
 (voyager-deferred-callback-post (voyager-deferred-new callback))."
  (let ((d (if callback
               (make-voyager-deferred-object :callback callback)
             (make-voyager-deferred-object))))
    (voyager-deferred-callback-post d arg)
    d))

(defun voyager-deferred-nextc (d callback)
  "Create a deferred object with OK callback and connect it to the given deferred object."
  (declare (indent 1))
  (let ((nd (make-voyager-deferred-object :callback callback)))
    (voyager-deferred-set-next d nd)))

(defun voyager-deferred-error (d callback)
  "Create a deferred object with errorback and connect it to the given deferred object."
  (declare (indent 1))
  (let ((nd (make-voyager-deferred-object :errorback callback)))
    (voyager-deferred-set-next d nd)))

(defvar voyager-epc-debug nil)

(defun voyager-epc-log (&rest args)
  (when voyager-epc-debug
    (with-current-buffer (get-buffer-create "*voyager-epc-log*")
      (buffer-disable-undo)
      (goto-char (point-max))
      (insert (apply 'format args) "\n\n\n"))))

(defun voyager-epc-make-procbuf (name)
  "[internal] Make a process buffer."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (set (make-local-variable 'kill-buffer-query-functions) nil)
      (erase-buffer) (buffer-disable-undo))
    buf))

(defvar voyager-epc-uid 1)

(defun voyager-epc-uid ()
  (cl-incf voyager-epc-uid))

(defvar voyager-epc-accept-process-timeout 150
  "Asynchronous timeout time. (msec)")

(put 'epc-error 'error-conditions '(error epc-error))
(put 'epc-error 'error-message "EPC Error")

(cl-defstruct voyager-epc-connection
  "Set of information for network connection and event handling.

name    : Connection name. This name is used for process and buffer names.
process : Connection process object.
buffer  : Working buffer for the incoming data.
channel : Event channels for incoming messages."
  name process buffer channel)

(defun voyager-epc-connect (host port)
  "[internal] Connect the server, initialize the process and
return voyager-epc-connection object."
  (voyager-epc-log ">> Connection start: %s:%s" host port)
  (let* ((connection-id (voyager-epc-uid))
         (connection-name (format "voyager-epc con %s" connection-id))
         (connection-buf (voyager-epc-make-procbuf (format "*%s*" connection-name)))
         (connection-process
          (open-network-stream connection-name connection-buf host port))
         (channel (list connection-name nil))
         (connection (make-voyager-epc-connection
                      :name connection-name
                      :process connection-process
                      :buffer connection-buf
                      :channel channel)))
    (voyager-epc-log ">> Connection establish")
    (set-process-coding-system  connection-process 'binary 'binary)
    (set-process-filter connection-process
                        (lambda (p m)
                          (voyager-epc-process-filter connection p m)))
    (set-process-sentinel connection-process
                          (lambda (p e)
                            (voyager-epc-process-sentinel connection p e)))
    (set-process-query-on-exit-flag connection-process nil)
    connection))

(defun voyager-epc-process-sentinel (connection process msg)
  (voyager-epc-log "!! Process Sentinel [%s] : %S : %S"
                   (voyager-epc-connection-name connection) process msg)
  (voyager-epc-disconnect connection))

(defun voyager-epc-net-send (connection sexp)
  (let* ((msg (encode-coding-string
               (concat (voyager-epc-prin1-to-string sexp) "\n") 'utf-8-unix))
         (string (concat (format "%06x" (length msg)) msg))
         (proc (voyager-epc-connection-process connection)))
    (voyager-epc-log ">> SEND : [%S]" string)
    (process-send-string proc string)))

(defun voyager-epc-disconnect (connection)
  (let ((process (voyager-epc-connection-process connection))
        (buf (voyager-epc-connection-buffer connection))
        (name (voyager-epc-connection-name connection)))
    (voyager-epc-log "!! Disconnect [%s]" name)
    (when process
      (set-process-sentinel process nil)
      (delete-process process)
      (when (get-buffer buf) (kill-buffer buf)))
    (voyager-epc-log "!! Disconnected finished [%s]" name)))

(defun voyager-epc-process-filter (connection process message)
  (voyager-epc-log "INCOMING: [%s] [%S]" (voyager-epc-connection-name connection) message)
  (with-current-buffer (voyager-epc-connection-buffer connection)
    (goto-char (point-max))
    (insert message)
    (voyager-epc-process-available-input connection process)))

(defun voyager-epc-signal-connect (channel event-sym &optional callback)
  "Append an observer for EVENT-SYM of CHANNEL and return a deferred object.
If EVENT-SYM is `t', the observer receives all signals of the channel.
If CALLBACK function is given, the deferred object executes the
CALLBACK function asynchronously. One can connect subsequent
tasks to the returned deferred object."
  (let ((d (if callback
               (voyager-deferred-new callback)
             (voyager-deferred-new))))
    (push (cons event-sym d)
          (cddr channel))
    d))

(defun voyager-epc-signal-send (channel event-sym &rest args)
  "Send a signal to CHANNEL. If ARGS values are given,
observers can get the values by following code:

  (lambda (event)
    (destructuring-bind
     (event-sym (args))
     event ... ))
"
  (let ((observers (cddr channel))
        (event (list event-sym args)))
    (cl-loop for i in observers
             for name = (car i)
             for d = (cdr i)
             if (or (eq event-sym name) (eq t name))
             do (voyager-deferred-callback-post d event))))

(defun voyager-epc-process-available-input (connection process)
  "Process all complete messages that have arrived from Lisp."
  (with-current-buffer (process-buffer process)
    (while (voyager-epc-net-have-input-p)
      (let ((event (voyager-epc-net-read-or-lose process))
            (ok nil))
        (voyager-epc-log "<< RECV [%S]" event)
        (unwind-protect
            (condition-case err
                (progn
                  (apply 'voyager-epc-signal-send
                         (cons (voyager-epc-connection-channel connection) event))
                  (setq ok t))
              ('error (voyager-epc-log "MsgError: %S / <= %S" err event)))
          (unless ok
            (voyager-epc-process-available-input connection process)))))))

(defun voyager-epc-net-have-input-p ()
  "Return true if a complete message is available."
  (goto-char (point-min))
  (and (>= (buffer-size) 6)
       (>= (- (buffer-size) 6) (voyager-epc-net-decode-length))))

(defun voyager-epc-net-read-or-lose (_process)
  (condition-case error
      (voyager-epc-net-read)
    (error
     (debug 'error error)
     (error "net-read error: %S" error))))

(defun voyager-epc-net-read ()
  "Read a message from the network buffer."
  (goto-char (point-min))
  (let* ((length (voyager-epc-net-decode-length))
         (start (+ 6 (point)))
         (end (+ start length))
         _content)
    (cl-assert (cl-plusp length))
    (prog1 (save-restriction
             (narrow-to-region start end)
             (read (decode-coding-string
                    (buffer-string) 'utf-8-unix)))
      (delete-region (point-min) end))))

(defun voyager-epc-net-decode-length ()
  "Read a 24-bit hex-encoded integer from buffer."
  (string-to-number (buffer-substring-no-properties (point) (+ (point) 6)) 16))

(defun voyager-epc-prin1-to-string (sexp)
  "Like `prin1-to-string' but don't octal-escape non-ascii characters.
This is more compatible with the CL reader."
  (with-temp-buffer
    (let (print-escape-nonascii
          print-escape-newlines
          print-length
          print-level)
      (prin1 sexp (current-buffer))
      (buffer-string))))

(cl-defstruct voyager-epc-manager
  "Root object that holds all information related to an EPC activity.

`voyager-epc-start-epc' returns this object.

title          : instance name for displaying on the `voyager-epc-controller' UI
server-process : process object for the peer
commands       : a list of (prog . args)
port           : port number
connection     : voyager-epc-connection instance
methods        : alist of method (name . function)
sessions       : alist of session (id . deferred)
exit-hook      : functions for after shutdown EPC connection"
  title server-process commands port connection methods sessions exit-hooks)

(cl-defstruct voyager-epc-method
  "Object to hold serving method information.

name       : method name (symbol)   ex: 'test
task       : method function (function with one argument)
arg-specs  : arg-specs (one string) ex: \"(A B C D)\"
docstring  : docstring (one string) ex: \"A test function. Return sum of A,B,C and D\"
"
  name task docstring arg-specs)

(defvar voyager-epc-live-connections nil
  "[internal] A list of `voyager-epc-manager' objects.
those objects currently connect to the epc peer.
This variable is for debug purpose.")

(defun voyager-epc-server-process-name (uid)
  (format "voyager-epc-server:%s" uid))

(defun voyager-epc-server-buffer-name (uid)
  (format " *%s*" (voyager-epc-server-process-name uid)))

(defun voyager-epc-stop-epc (mngr)
  "Disconnect the connection for the server."
  (let* ((proc (voyager-epc-manager-server-process mngr))
         (buf (and proc (process-buffer proc))))
    (voyager-epc-disconnect (voyager-epc-manager-connection mngr))
    (when proc
      (accept-process-output proc 0 voyager-epc-accept-process-timeout t))
    (when (and proc (equal 'run (process-status proc)))
      (kill-process proc))
    (when buf  (kill-buffer buf))
    (setq voyager-epc-live-connections (delete mngr voyager-epc-live-connections))
    ))

(defun voyager-epc-args (args)
  "[internal] If ARGS is an atom, return it. If list, return the cadr of it."
  (cond
   ((atom args) args)
   (t (cadr args))))

(defun voyager-epc-init-epc-layer (mngr)
  "[internal] Connect to the server program and return an voyager-epc-connection instance."
  (let* ((mngr mngr)
         (conn (voyager-epc-manager-connection mngr))
         (channel (voyager-epc-connection-channel conn)))
    ;; dispatch incoming messages with the lexical scope
    (cl-loop for (method . body) in
             `((call
                . (lambda (args)
                    (voyager-epc-log "SIG CALL: %S" args)
                    (apply 'voyager-epc-handler-called-method ,mngr (voyager-epc-args args))))
               (return
                . (lambda (args)
                    (voyager-epc-log "SIG RET: %S" args)
                    (apply 'voyager-epc-handler-return ,mngr (voyager-epc-args args))))
               (return-error
                . (lambda (args)
                    (voyager-epc-log "SIG RET-ERROR: %S" args)
                    (apply 'voyager-epc-handler-return-error ,mngr (voyager-epc-args args))))
               (epc-error
                . (lambda (args)
                    (voyager-epc-log "SIG EPC-ERROR: %S" args)
                    (apply 'voyager-epc-handler-epc-error ,mngr (voyager-epc-args args))))
               (methods
                . (lambda (args)
                    (voyager-epc-log "SIG METHODS: %S" args)
                    (voyager-epc-handler-methods ,mngr (caadr args))))
               ) do
             (voyager-epc-signal-connect channel method body))
    (push mngr voyager-epc-live-connections)
    mngr))

(defun voyager-epc-manager-send (mngr method &rest messages)
  "[internal] low-level message sending."
  (let* ((conn (voyager-epc-manager-connection mngr)))
    (voyager-epc-net-send conn (cons method messages))))

(defun voyager-epc-manager-get-method (mngr method-name)
  "[internal] Return a method object. If not found, return nil."
  (cl-loop for i in (voyager-epc-manager-methods mngr)
           if (eq method-name (voyager-epc-method-name i))
           do (cl-return i)))

(defun voyager-epc-handler-methods (mngr uid)
  "[internal] Return a list of information for registered methods."
  (let ((info
         (cl-loop for i in (voyager-epc-manager-methods mngr)
                  collect
                  (list
                   (voyager-epc-method-name i)
                   (or (voyager-epc-method-arg-specs i) "")
                   (or (voyager-epc-method-docstring i) "")))))
    (voyager-epc-manager-send mngr 'return uid info)))

(defun voyager-epc-handler-called-method (mngr uid name args)
  "[internal] low-level message handler for peer's calling."
  (let ((mngr mngr) (uid uid))
    (let* ((_methods (voyager-epc-manager-methods mngr))
           (method (voyager-epc-manager-get-method mngr name)))
      (cond
       ((null method)
        (voyager-epc-log "ERR: No such method : %s" name)
        (voyager-epc-manager-send mngr 'epc-error uid (format "EPC-ERROR: No such method : %s" name)))
       (t
        (condition-case err
            (let* ((f (voyager-epc-method-task method))
                   (ret (apply f args)))
              (cond
               ((voyager-deferred-object-p ret)
                (voyager-deferred-nextc ret
                                        (lambda (xx) (voyager-epc-manager-send mngr 'return uid xx))))
               (t (voyager-epc-manager-send mngr 'return uid ret))))
          (error
           (voyager-epc-log "ERROR : %S" err)
           (voyager-epc-manager-send mngr 'return-error uid err))))))))

(defun voyager-epc-manager-remove-session (mngr uid)
  "[internal] Remove a session from the epc manager object."
  (cl-loop with ret = nil
           for pair in (voyager-epc-manager-sessions mngr)
           unless (eq uid (car pair))
           do (push pair ret)
           finally
           do (setf (voyager-epc-manager-sessions mngr) ret)))

(defun voyager-epc-handler-return (mngr uid args)
  "[internal] low-level message handler for normal returns."
  (let ((pair (assq uid (voyager-epc-manager-sessions mngr))))
    (cond
     (pair
      (voyager-epc-log "RET: id:%s [%S]" uid args)
      (voyager-epc-manager-remove-session mngr uid)
      (voyager-deferred-callback (cdr pair) args))
     (t                                 ; error
      (voyager-epc-log "RET: NOT FOUND: id:%s [%S]" uid args)))))

(defun voyager-epc-handler-return-error (mngr uid args)
  "[internal] low-level message handler for application errors."
  (let ((pair (assq uid (voyager-epc-manager-sessions mngr))))
    (cond
     (pair
      (voyager-epc-log "RET-ERR: id:%s [%S]" uid args)
      (voyager-epc-manager-remove-session mngr uid)
      (voyager-deferred-errorback (cdr pair) (format "%S" args)))
     (t                                 ; error
      (voyager-epc-log "RET-ERR: NOT FOUND: id:%s [%S]" uid args)))))

(defun voyager-epc-handler-epc-error (mngr uid args)
  "[internal] low-level message handler for epc errors."
  (let ((pair (assq uid (voyager-epc-manager-sessions mngr))))
    (cond
     (pair
      (voyager-epc-log "RET-EPC-ERR: id:%s [%S]" uid args)
      (voyager-epc-manager-remove-session mngr uid)
      (voyager-deferred-errorback (cdr pair) (list 'epc-error args)))
     (t                                 ; error
      (voyager-epc-log "RET-EPC-ERR: NOT FOUND: id:%s [%S]" uid args)))))

(defun voyager-epc-call-deferred (mngr method-name args)
  "Call peer's method with args asynchronously. Return a deferred
object which is called with the result."
  (let ((uid (voyager-epc-uid))
        (sessions (voyager-epc-manager-sessions mngr))
        (d (voyager-deferred-new)))
    (push (cons uid d) sessions)
    (setf (voyager-epc-manager-sessions mngr) sessions)
    (voyager-epc-manager-send mngr 'call uid method-name args)
    d))

(defun voyager-epc-define-method (mngr method-name task &optional arg-specs docstring)
  "Define a method and return a deferred object which is called by the peer."
  (let* ((method (make-voyager-epc-method
                  :name method-name :task task
                  :arg-specs arg-specs :docstring docstring))
         (methods (cons method (voyager-epc-manager-methods mngr))))
    (setf (voyager-epc-manager-methods mngr) methods)
    method))

(defun voyager-epc-sync (mngr d)
  "Wrap deferred methods with synchronous waiting, and return the result.
If an exception is occurred, this function throws the error."
  (let ((result 'voyager-epc-nothing))
    (voyager-deferred-chain
     d
     (voyager-deferred-nextc it
                             (lambda (x) (setq result x)))
     (voyager-deferred-error it
                             (lambda (er) (setq result (cons 'error er)))))
    (while (eq result 'voyager-epc-nothing)
      (save-current-buffer
        (accept-process-output
         (voyager-epc-connection-process (voyager-epc-manager-connection mngr))
         0 voyager-epc-accept-process-timeout t)))
    (if (and (consp result) (eq 'error (car result)))
        (error (cdr result)) result)))

(defun voyager-epc-call-sync (mngr method-name args)
  "Call peer's method with args synchronously and return the result.
If an exception is occurred, this function throws the error."
  (voyager-epc-sync mngr (voyager-epc-call-deferred mngr method-name args)))

(defun voyager-epc-live-p (mngr)
  "Return non-nil when MNGR is an EPC manager object with a live
connection."
  (let ((proc (ignore-errors
                (voyager-epc-connection-process (voyager-epc-manager-connection mngr)))))
    (and (processp proc)
         ;; Same as `process-live-p' in Emacs >= 24:
         (memq (process-status proc) '(run open listen connect stop)))))

;; epcs
(defvar voyager-epc-server-client-processes nil
  "[internal] A list of ([process object] . [`voyager-epc-manager' instance]).
When the server process accepts the client connection, the
`voyager-epc-manager' instance is created and stored in this variable
`voyager-epc-server-client-processes'. This variable is used for the management
purpose.")

;; voyager-epc-server
;;   name    : process name (string)   ex: "EPC Server 1"
;;   process : server process object
;;   port    : port number
;;   connect-function : initialize function for `voyager-epc-manager' instances
(cl-defstruct voyager-epc-server name process port connect-function)

(defvar voyager-epc-server-processes nil
  "[internal] A list of ([process object] . [`voyager-epc-server' instance]).
This variable is used for the management purpose.")

(defun voyager-epc-server-get-manager-by-process (proc)
  "[internal] Return the voyager-epc-manager instance for the PROC."
  (cl-loop for (pp . mngr) in voyager-epc-server-client-processes
           if (eql pp proc)
           do (cl-return mngr)
           finally return nil))

(defun voyager-epc-server-accept (process)
  "[internal] Initialize the process and return voyager-epc-manager object."
  (voyager-epc-log "VOYAGER-EPC-SERVER- >> Connection accept: %S" process)
  (let* ((connection-id (voyager-epc-uid))
         (connection-name (format "voyager-epc con %s" connection-id))
         (channel (list connection-name nil))
         (connection (make-voyager-epc-connection
                      :name connection-name
                      :process process
                      :buffer (process-buffer process)
                      :channel channel)))
    (voyager-epc-log "VOYAGER-EPC-SERVER- >> Connection establish")
    (set-process-coding-system process 'binary 'binary)
    (set-process-filter process
                        (lambda (p m)
                          (voyager-epc-process-filter connection p m)))
    (set-process-query-on-exit-flag process nil)
    (set-process-sentinel process
                          (lambda (p e)
                            (voyager-epc-process-sentinel connection p e)))
    (make-voyager-epc-manager :server-process process :port t
                              :connection connection)))

(defun voyager-epc-server-sentinel (process message connect-function)
  "[internal] Process sentinel handler for the server process."
  (voyager-epc-log "VOYAGER-EPC-SERVER- SENTINEL: %S %S" process message)
  (let ((mngr (voyager-epc-server-get-manager-by-process process)))
    (cond
     ;; new connection
     ((and (string-match "open" message) (null mngr))
      (condition-case err
          (let ((mngr (voyager-epc-server-accept process)))
            (push (cons process mngr) voyager-epc-server-client-processes)
            (voyager-epc-init-epc-layer mngr)
            (when connect-function (funcall connect-function mngr))
            mngr)
        ('error
         (voyager-epc-log "VOYAGER-EPC-SERVER- Protocol error: %S" err)
         (voyager-epc-log "VOYAGER-EPC-SERVER- ABORT %S" process)
         (delete-process process))))
     ;; ignore
     ((null mngr) nil )
     ;; disconnect
     (t
      (let ((pair (assq process voyager-epc-server-client-processes)) _d)
        (when pair
          (voyager-epc-log "VOYAGER-EPC-SERVER- DISCONNECT %S" process)
          (voyager-epc-stop-epc (cdr pair))
          (setq voyager-epc-server-client-processes
                (assq-delete-all process voyager-epc-server-client-processes))
          ))
      nil))))

(defun voyager-epc-server-start (connect-function &optional port)
  "Start TCP Server and return the main process object."
  (let*
      ((connect-function connect-function)
       (name (format "VOYAGER EPC Server %s" (voyager-epc-uid)))
       (buf (voyager-epc-make-procbuf (format " *%s*" name)))
       (main-process
        (make-network-process
         :name name
         :buffer buf
         :family 'ipv4
         :server t
         :host "127.0.0.1"
         :service (or port t)
         :noquery t
         :sentinel
         (lambda (process message)
           (voyager-epc-server-sentinel process message connect-function)))))
    (push (cons main-process
                (make-voyager-epc-server
                 :name name :process main-process
                 :port (process-contact main-process :service)
                 :connect-function connect-function))
          voyager-epc-server-processes)
    main-process))

(provide 'voyager-epc)
;;; voyager-epc.el ends here
