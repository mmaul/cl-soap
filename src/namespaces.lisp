;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: namespaces.lisp,v 1.9 2005-10-06 11:06:56 scaekenberghe Exp $
;;;;
;;;; Definition of some standard XML namespaces commonly needed for SOAP
;;;;
;;;; Copyright (C) 2005 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :cl-soap)

;;; SOAP Envelope

(defconstant +soapenv-ns-uri+ "http://schemas.xmlsoap.org/soap/envelope/")

(defpackage :soapenv
  (:nicknames "soapenv")
  (:export "Envelope" "Header" "Body" "Fault" "encodingStyle")
  (:documentation "Package for symbols in the SOAP Envelope XML Namespace"))

(defparameter *soapenv-ns* (s-xml:register-namespace +soapenv-ns-uri+ "soapenv" :soapenv))

;;; XSD

(defconstant +xsd-ns-uri+ "http://www.w3.org/1999/XMLSchema")

;; http://www.w3.org/2000/10/XMLSchema

(defpackage :xsd
  (:nicknames "xsd")
  (:export "schema" "element" "simpleType" "complexType" "complexContent" 
           "sequence" "choice" "all" "attribute"
           "restriction" "extension" "maxLength" "pattern" "list" "union" "enumeration")
  (:documentation "Package for symbols in the XML Schema Definition XML Namespace"))

(defparameter *xsd-ns* (s-xml:register-namespace +xsd-ns-uri+ "xsd" :xsd))

;;; XSI

(defconstant +xsi-ns-uri+ "http://www.w3.org/1999/XMLSchema-instance") 

;; "http://www.w3.org/2000/10/XMLSchema-instance"
;; "http://www.w3.org/2001/XMLSchema"

(defpackage :xsi
  (:nicknames "xsi")
  (:export "null" "type")
  (:documentation "Package for symbols in the XML Schema Instance XML Namespace"))

(defparameter *xsi-ns* (s-xml:register-namespace +xsi-ns-uri+ "xsi" :xsi))

;;; SOAP Encoding

(defconstant +soap-enc-ns-uri+ "http://schemas.xmlsoap.org/soap/encoding/")

;;; WSDL

(defconstant +wsdl-ns-uri+ "http://schemas.xmlsoap.org/wsdl/")

(defpackage :wsdl
  (:nicknames "wsdl")
  (:export 
   "definitions" "documentation"
   "portType" "message" "operation" "port" "service" "binding" "part" "input" "output" "fault" "types")
  (:documentation "Package for symbols in the WSDL XML Namespace"))

(defparameter *wsdl-ns* (s-xml:register-namespace +wsdl-ns-uri+ "wsdl" :wsdl))

;;; WSDL Soap Binding

(defconstant +wsdl-soap-ns-uri+ "http://schemas.xmlsoap.org/wsdl/soap/")

(defpackage :wsdl-soap
  (:nicknames "wsdl-soap")
  (:export "address" "binding" "operation" "body" "header" "fault" "headerfault")
  (:documentation "Package for symbols in the WSDL Soap Bindings XML Namespace"))

(defparameter *wsdl-soap-ns* (s-xml:register-namespace +wsdl-soap-ns-uri+ "soap" :wsdl-soap))

;;;; eof
