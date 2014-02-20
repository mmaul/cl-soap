;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: package.lisp,v 1.3 2005-09-26 11:17:42 scaekenberghe Exp $
;;;;
;;;; Definition of the CL-SOAP package
;;;;
;;;; Copyright (C) 2005 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :cl-user)

(defpackage :cl-soap
  (:use :common-lisp)
  (:export 
   ;; standard soap
   #:make-soap-end-point #:soap-call 
   standard-soap-fault #:fault-code #:fault-string #:fault-actor #:fault-detail
   #:*debug-stream* *last-soap-call-xml* *last-soap-result-xml* #:report-soap-call
   +soap-enc-ns-uri+
   ;; wsdl
   #:parse-wsdl #:parse-wsdl-file #:parse-wsdl-url #:describe-wsdl-soap
   #:wsdl-cache-get #:wsdl-cache-clear #:wsdl-cache-clear-all
   #:wsdl-soal-call
   ;; xsd
   #:parse-xsd #:parse-xsd-file #:parse-xsd-url
   ;; lxml
   #:lxml-get-tag #:lxml-get-attributes #:lxml-find-tag
   ;; html-client
   #:do-http-request #:make-http-client-state #:close-all-connections)
  (:documentation "The Common Lisp SOAP package"))

;;;; eof
