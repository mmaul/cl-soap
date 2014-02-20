;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: http-client.lisp,v 1.7 2005-10-05 13:24:38 scaekenberghe Exp $
;;;;
;;;; A basic HTTP client, somewhat API compatible with portableaserve's do-http-request
;;;; Copied from another project (basic authorization support removed)
;;;;
;;;; Copyright (C) 2005 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :cl-soap)

#+lispworks (require "comm")

;; data structures for state management

(defclass http-client-state () 
  ((data :initform nil)))

(defun make-http-client-state ()
  "Make a new HTTP client state object to hold open (keepalive) connections"
  (make-instance 'http-client-state))

(defvar *default-http-client-state* (make-http-client-state))

(defclass http-server-state ()
  ((scheme-host-port :accessor get-scheme-host-port :initarg :scheme-host-port)
   (socket :accessor get-socket :initarg :socket :initform nil)))

;; low level output

(defun write-http-request-line (string &optional (stream *standard-output*))
  (write-string string stream)
  (write-char #\return stream)
  (write-char #\linefeed stream))

(defun format-http-request-line (stream format-string &rest args)
  (write-http-request-line (apply #'format nil format-string args) stream))

;; some HTTP protocol defaults

(defvar *http-client-agent* 
  (format nil "HTTP Client ~a ~a" (lisp-implementation-type) (lisp-implementation-version)))

(defvar *http-client-accept* 
  "*/*")

;; low level HTTP input

(defun response-read-code (stream)
  (let* ((line (read-line stream))
         (first-space (position #\Space line)))
    (parse-integer line :start (1+ first-space) :junk-allowed t)))

(defun response-read-headers (stream)
  (loop :for line = (read-line stream nil)
        :until (or (null line)
                   (zerop (length line))
                   (char= (elt line 0) #\return)
                   (char= (elt line 0) #\linefeed))
        :collect (let ((colon (position #\: line)))
                   (cons (intern (string-upcase (subseq line 0 colon)) :keyword)
                         (string-trim (list #\space #\return #\linefeed)
                                      (subseq line (1+ colon)))))))

(defun response-read-chunked-body (stream)
  (let ((buffer (make-string 4096))
        (total-size 0)
        line
        chunk-size)
    (with-output-to-string (out)
      (loop
       (setf line (read-line stream nil))
       (setf chunk-size (parse-integer line :radix 16 :junk-allowed t))
       (incf total-size chunk-size)
       (if (zerop chunk-size)
           (return)
         (let ((total-chunk-size 0))
           (loop (let ((size (read-sequence buffer stream 
                                            :end (min (length buffer) 
                                                      (- chunk-size total-chunk-size)))))
                   (incf total-chunk-size size)
                   (write-sequence buffer out :end size)
                   (when (= total-chunk-size chunk-size)
                     (return))))
           (read-line stream))))
      (read-line stream))))

(defun response-read-body (stream &optional length)
  (let ((buffer (make-string 4096))
        (total-size 0))
    (with-output-to-string (out)
      (loop
       (let ((size (read-sequence buffer stream 
                                  :end (when length
                                         (min (length buffer) 
                                              (- length total-size))))))
         (incf total-size size)
         (write-sequence buffer out :end size)
         (when (or (and length (= total-size length))
                   (< size (length buffer)))
           (return)))))))

;; connection / server state management

(defmethod get-http-server-state ((http-client-state http-client-state) scheme-host-port)
  (with-slots (data) 
      http-client-state
    (let ((server-state (find scheme-host-port data 
                              :key #'get-scheme-host-port :test #'string-equal)))
      (unless server-state
        (push (setf server-state (make-instance 'http-server-state 
                                                :scheme-host-port scheme-host-port))
              data))
      server-state)))
                
(defmethod close-all-connections ((http-client-state http-client-state) &key abort)
  "Close all open connections in http-client-state (optionaly aborting them)"
  (with-slots (data) 
      http-client-state
    (dolist (http-server-state data)
      (let ((connection (get-socket http-server-state)))
        (when connection
          (ignore-errors (close connection :abort abort)))))))

(defun open-socket-stream (scheme host port)
  #+lispworks (ecase scheme
                (:http (comm:open-tcp-stream host port))
                (:https (comm:open-tcp-stream host port :ssl-ctx t)))
  #+openmcl (when (eql scheme :http)
              (ccl:make-socket :remote-host host :remote-port port))
  #+clisp (when (eql scheme :http)
            (socket:socket-connect port host))
  #+cmu (when (eql scheme :http)
          (sys:make-fd-stream (ext:connect-to-inet-socket host port) 
                              :input t :output t :buffering :none))
  #+sbcl (when (eql scheme :http)
           (let ((socket (make-instance 'sb-bsd-sockets:inet-socket 
                                        :type :stream :protocol :tcp)))
             (sb-bsd-sockets:socket-connect socket 
                                            (car 
                                             (sb-bsd-sockets:host-ent-addresses 
                                              (sb-bsd-sockets:get-host-by-name host))) 
                                            port)
             (sb-bsd-sockets:socket-make-stream socket 
                                                :element-type 'character 
                                                :input t :output t :buffering :none))))

(defun get-open-connection (scheme host port state &key force-new)
  (if state
      (let* ((scheme-host-port (format nil "~a://~a:~d" scheme host port))
             (server-state (get-http-server-state state scheme-host-port))
             (connection (get-socket server-state)))
        (if (and connection (open-stream-p connection) (not force-new))
            (values connection :keep-alive)
          (progn 
            (when connection (ignore-errors (close connection)))
            (values (setf (get-socket server-state) (open-socket-stream scheme host port)) 
                    :new))))
    (values (open-socket-stream scheme host port) :new)))

;; high level HTTP protocol

(defun read-response (stream)
  "Read an HTTP response, headers and content, from stream"
  (let (response-code response-headers response-body)
    (setf response-code (response-read-code stream)
          response-headers (response-read-headers stream))
    (let* ((content-length-header (cdr (find :content-length response-headers :key #'car)))
           (content-length (when content-length-header 
                             (ignore-errors (parse-integer content-length-header))))
           (transfer-encoding-header (cdr (find :transfer-encoding response-headers :key #'car)))
           (chunked-p (when transfer-encoding-header
                        (search "chunked" transfer-encoding-header :test #'char-equal))))
      (setf response-body (if chunked-p
                              (response-read-chunked-body stream)
                            (response-read-body stream content-length)))
      (values response-body
              response-code
              response-headers))))

(defun write-request (stream 
                      uri method
                      &key 
                      content content-type 
                      headers)
  "Write an HTTP request, full header and body, to stream"
  (format-http-request-line stream 
                            "~a ~a~@[?~a~] HTTP/1.1" 
                            method (if (puri:uri-path uri) 
                                       (puri:uri-path uri) "/") 
                            (puri:uri-query uri))
  (format-http-request-line stream "Host: ~a:~d" (puri:uri-host uri) (puri:uri-port uri))
  (format-http-request-line stream "User-Agent: ~a" *http-client-agent*)
  (format-http-request-line stream "Accept: ~a" *http-client-accept*)
  (when (and content content-type) 
    (format-http-request-line stream "Content-Length: ~d" (length content))
    (format-http-request-line stream "Content-Type: ~a" content-type))
  ;; maybe custom headers should be able to override (some of) the above ?
  (loop :for (header-name . header-value) :in headers
        :do (format-http-request-line stream "~a: ~a" header-name header-value))
  (write-http-request-line "" stream)
  (when (and content content-type)
    (write-sequence content stream))
  (finish-output stream))

(defun do-one-request-response (connection 
                                uri method
                                &key 
                                content content-type 
                                headers)
  "Do one HTTP request and response on stream"
  (write-request connection uri method 
                 :content content :content-type content-type 
                 :headers headers)
  (values-list `(,@(multiple-value-list (read-response connection)) ,uri)))

;; the user level API 

(defun do-http-request (uri 
                        &key 
                        (method :get) 
                        content content-type 
                        headers
                        proxy
                        state)
  "Execute an HTTP request, returns (values body code headers uri kept-alive-p)"
  (declare (ignore proxy))
  (assert (member method '(:get :put :post :delete :head)))
  (setf uri (puri:parse-uri uri))
  (let* ((scheme (puri:uri-scheme uri))
         (host (puri:uri-host uri))
         (port (or (puri:uri-port uri) 
                   (setf (puri:uri-port uri) (ecase scheme
                                               (:http  80)
                                               (:https 443))))))
    (multiple-value-bind (connection keep-alive)
        ;; state could hold an open (kept alive) connection to host:port
        (get-open-connection scheme host port state)
      (flet ((execute-request-response ()
               (values-list `(,@(multiple-value-list 
                                 (do-one-request-response connection uri method 
                                                          :content content 
                                                          :content-type content-type 
                                                          :headers headers)) 
                              ,keep-alive))))
      (unwind-protect
          ;; if there is no state, always close the connection
          (handler-case (execute-request-response)
            ;; the first stream/socket error on the re-used (kept-alive) connection is interpreted 
            ;; as a timeout on the keep-alive, so we close the connection and retry once
            ((or stream-error #+lispworks comm:socket-error) ()
             (when keep-alive
               (setf keep-alive :new
                     connection (get-open-connection scheme host port state :force-new t))
               (execute-request-response))))
        (unless state (close connection)))))))

;;;; eof