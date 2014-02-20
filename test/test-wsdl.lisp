;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: test-wsdl.lisp,v 1.2 2005-09-15 13:31:54 scaekenberghe Exp $
;;;;
;;;; Some test for the WSDL structured SOAP protocol
;;;;
;;;; Copyright (C) 2005 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :cl-soap)

;; accessing some external services
;; http://xmethods.net

(defun xmethods-get-quote (symbol)
  ;; doesn't work: response element incorrectly described in wsdl wrt wire usage ??
  (wsdl-soap-call "http://services.xmethods.net/soap/urn:xmethods-delayed-quotes.wsdl"
                  "getQuote"
                  :input `("symbol" ,symbol)))
 
(defun xmethods-get-temperature (zipcode)
  (wsdl-soap-call "http://www.xmethods.net/sd/2001/DemoTemperatureService.wsdl"
                  "getTemp"
                  :input `("zipcode" ,zipcode)))

(defun xmethods-get-rate (country1 country2)
  (wsdl-soap-call "http://www.xmethods.net/sd/2001/CurrencyExchangeService.wsdl"
                  "getRate"
                  :input `("country1" ,country1 "country2" ,country2)))

;; contributed by carlos.ungil@bluewin.ch
;; http://www.random.org/soap.html
;; http://www.random.org/RandomDotOrg.wsdl
;; http://www.random.org/clients/soap/

(defun get-random-number (&key unsigned)
  ;; doesn't work: response element incorrectly described in wsdl wrt wire usage ??
  "Returns a true random  number in the interval [2^31, 2^31) or [0, 2^31)"
  (wsdl-soap-call "http://www.random.org/RandomDotOrg.wsdl"
                  (if unsigned "lrand48" "mrand48")))

;; accessing local AXIS services
      
(defun axis-example3-echo (string)
  ;; doesn't work: different input/output namespaces currently not supported
  ;; furthermore namespaces seem to be described inconsistently in wsdl wrt wire usage ??
  (wsdl-soap-call "http://localhost:8080/axis/services/MyService?wsdl"
                  "serviceMethod"
                  :input `("arg" ,string)))

;;;; eof
