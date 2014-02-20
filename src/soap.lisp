;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: soap.lisp,v 1.11 2005-10-06 08:07:58 scaekenberghe Exp $
;;;;
;;;; The basic SOAP protocol
;;;;
;;;; Copyright (C) 2005 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :cl-soap)

;;; Globals

(defvar *debug-stream* nil
  "If non-nil, a stream to write debugging output to")

;;; SOAP Fault
 
(define-condition standard-soap-fault (error)
  ((code :initarg :code :reader fault-code :initform nil)
   (string :initarg :string :reader fault-string :initform nil)
   (actor :initarg :actor :reader fault-actor :initform nil)
   (detail :initarg :detail :reader fault-detail :initform nil))
  (:report (lambda (condition stream)
	     (format stream
		     "SOAP Fault~@[ ~s~]~@[ code ~s~]~@[ actor ~s~]~@[ (detail ~s)~]"
                     (fault-string condition)
                     (fault-code condition)
                     (fault-actor condition)
                     (fault-detail condition))))
  (:documentation "Thrown by CL-SOAP when a standard SOAP Fault is read"))

(defun lxml->standard-soap-fault (xml)
  (let ((code (lxml-get-contents (lxml-find-tag :|faultcode| (rest xml))))
        (string (lxml-get-contents (lxml-find-tag :|faultstring| (rest xml))))
        (actor (lxml-get-contents (lxml-find-tag :|faultactor| (rest xml))))
        (detail (lxml-get-contents (lxml-find-tag :|detail| (rest xml)))))
    (make-condition 'standard-soap-fault
                    :code code
                    :string string
                    :actor actor
                    :detail detail)))

;;; SOAP Server End Point

(defclass soap-end-point ()
  ((url :accessor get-url :initarg :url))
  (:documentation "Object representing a SOAP Server end point"))

(defun make-soap-end-point (url)
  "Create a new SOAP Server end point"
  (make-instance 'soap-end-point :url url))

;;; SOAP content generation support

(defun soap-header (header-lxml &optional header-attributes)
  (when header-lxml
    (if header-attributes
        `(((soapenv:|Header| ,@header-attributes) ,@header-lxml)) 
      `((soapenv:|Header| ,@header-lxml)))))

(defun soap-body (body-lxml &optional body-attributes)
  (if body-attributes
      `((soapenv:|Body| ,@body-attributes) ,body-lxml)
    `(soapenv:|Body| ,body-lxml)))

(defun soap-envelope (header body &key envelope-attributes header-attributes body-attributes)
  `((soapenv:|Envelope|
     :|xmlns:soapenv| ,+soapenv-ns-uri+
     :|xmlns:xsd| ,+xsd-ns-uri+
     :|xmlns:xsi| ,+xsi-ns-uri+
     ,@envelope-attributes)
    ,@(soap-header header header-attributes)
    ,(soap-body body body-attributes)))

;;; Call Interface

(defvar *last-soap-call-xml* nil 
  "When *debug-stream* is non-nil, will contain the last SOAP call's parsed XML")

(defvar *last-soap-result-xml* nil
  "When *debug-stream* is non-nil, will contain the last SOAP result's parsed XML")

(defun report-soap-call ()
  (format t "~&;; CL-SOAP last call and result envelopes:~%")
  (pprint *last-soap-call-xml*)
  (terpri)
  (pprint *last-soap-result-xml*))

(defun soap-call (server-end-point 
                  header 
                  body 
                  &key 
                  soap-action 
                  envelope-attributes 
                  header-attributes 
                  body-attributes)
  "Make a SOAP Call to server-end-point using headers and body"
  ;; only one single body element is allowed/supported for now
  (let* ((call-soap-envelope (soap-envelope header body 
                                            :envelope-attributes envelope-attributes
                                            :header-attributes header-attributes
                                            :body-attributes body-attributes))
         (call-xml (s-xml:print-xml-string call-soap-envelope :pretty t))
         result-soap-envelope)
    (when *debug-stream*
      (setf *last-soap-call-xml* call-soap-envelope)
      (format *debug-stream* ";; SOAP CALL sending: ~a~%" call-xml))
    (multiple-value-bind (result-xml code)
        (do-http-request (get-url server-end-point)
                         :method :POST
                         :headers `(("SOAPAction" . ,(or soap-action "")))
                         :content-type "text/xml"
                         :content call-xml)
      (declare (ignore code))
      (when *debug-stream*
        (format *debug-stream* ";; SOAP CALL receiving: ~a~%" result-xml))
      (setf result-soap-envelope (s-xml:parse-xml-string result-xml))
      (when *debug-stream*
        (setf *last-soap-result-xml* result-soap-envelope))
      (if (eql (lxml-get-tag result-soap-envelope) 'soapenv:|Envelope|)
          (let ((headers (lxml-find-tag 'soapenv:|Header| (rest result-soap-envelope)))
                (body (lxml-find-tag 'soapenv:|Body| (rest result-soap-envelope))))
            ;; simply return header key/value pairs as an alist
            (setf headers (mapcar #'(lambda (x) (list (lxml-get-tag x) (lxml-get-contents x))) (rest headers)))
            ;; only the first child of the body is returned, unless it is a fault
            (if body
                (let ((fault (lxml-find-tag 'soapenv:|Fault| (rest body))))
                  (if fault
                      (error (lxml->standard-soap-fault fault))
                    (values (second body) headers)))
              (error "No body found in SOAP Envelope")))
        (error "No SOAP Envelope found")))))
 
;;;; eof
