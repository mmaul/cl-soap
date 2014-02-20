;;;; -*- Mode: LISP -*-
;;;;
;;;; $Id: cl-soap.asd,v 1.4 2005-09-26 11:17:41 scaekenberghe Exp $
;;;;
;;;; The CL-SOAP ASDF system definition
;;;;
;;;; Copyright (C) 2005 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :asdf)

(defsystem :cl-soap
  :name "CL-SOAP"
  :author "Sven Van Caekenberghe <svc@mac.com>"
  :version "1"
  :maintainer "Sven Van Caekenberghe <svc@mac.com>"
  :licence "Lesser Lisp General Public License (LLGPL)"
  :description "Common Lisp SOAP Package"
  :long-description "CL-SOAP is an implementation of the SOAP 1.1, WSDL 1.1 and XML Schema Definition procotols"

  :components
  ((:module
    :src 
    :components ((:file "package")
                 (:file "http-client" :depends-on ("package"))
                 (:file "lxml" :depends-on ("package"))
                 (:file "namespaces" :depends-on ("package"))
                 (:file "xsd" :depends-on ("package" "namespaces" "lxml"))
                 (:file "soap" :depends-on ("package" "http-client" "namespaces" "lxml"))
                 (:file "wsdl" :depends-on ("package" "xsd" "namespaces" "lxml" "soap")))))
  :depends-on (:s-xml :puri))

;;;; eof
