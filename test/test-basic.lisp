;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: test-basic.lisp,v 1.4 2005-09-12 14:28:41 scaekenberghe Exp $
;;;;
;;;; Some test for the basic SOAP protocol
;;;;
;;;; Copyright (C) 2005 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :cl-soap)

(defpackage :ns1)

;; accessing some external services

(defun xmethods-get-quote (symbol)
  "Calling http://services.xmethods.net/soap/urn:xmethods-delayed-quotes.wsdl"
  (let ((ns "urn:xmethods-delayed-quotes"))
    (s-xml:register-namespace ns "ns1" :ns1)
    (let* ((xmethods (make-soap-end-point "http://64.124.140.30:9090/soap"))
           (result (soap-call xmethods 
                              '() 
                              `((ns1::|getQuote| 
                                 soapenv:|encodingStyle| ,+soap-enc-ns-uri+
                                 :|xmlns:ns1| ,ns) 
                                ((:|symbol| xsi::|type| "xsd:string") 
                                 ,symbol)) 
                              :soap-action "urn:xmethods-delayed-quotes#getQuote")))
      (if (eql (lxml-get-tag result) 'ns1::|getQuoteResponse|)
          (let ((contents (lxml-find-tag :|Result| (rest result))))
            (if contents
                (coerce (read-from-string (second contents)) 'float)
              (error "Expected a <Result> element")))
        (error "Expected a <getQuoteResponse> element")))))
 
(defun xmethods-get-temperature (zipcode)
  "Calling http://www.xmethods.net/sd/2001/DemoTemperatureService.wsdl"
  (let ((ns "urn:xmethods-Temperature-Demo"))
    (s-xml:register-namespace ns "ns1" :ns1)
    (let* ((xmethods (make-soap-end-point "http://services.xmethods.net:80/soap/servlet/rpcrouter"))
           (result (soap-call xmethods 
                              '() 
                              `((ns1::|getTemp| 
                                 soapenv:|encodingStyle| ,+soap-enc-ns-uri+
                                 :|xmlns:ns1| ,ns) 
                                ((:|zipcode| xsi::|type| "xsd:string") 
                                 ,zipcode)))))
      (if (eql (lxml-get-tag result) 'ns1::|getTempResponse|)
          (let ((contents (lxml-find-tag :|return| (rest result))))
            (if contents
                (coerce (read-from-string (second contents)) 'float)
              (error "Expected a <return> element")))
        (error "Expected a <getTempResponse> element")))))

(defun xmethods-get-rate (country1 country2)
  "Calling http://www.xmethods.net/sd/2001/CurrencyExchangeService.wsdl"
  (let ((ns "urn:xmethods-CurrencyExchange"))
    (s-xml:register-namespace ns "ns1" :ns1)
    (let* ((xmethods (make-soap-end-point "http://services.xmethods.net:80/soap"))
           (result (soap-call xmethods 
                              '() 
                              `((ns1::|getRate| 
                                 soapenv:|encodingStyle| ,+soap-enc-ns-uri+
                                 :|xmlns:ns1| ,ns) 
                                ((:|country1| xsi::|type| "xsd:string") 
                                 ,country1)
                                ((:|country2| xsi::|type| "xsd:string") 
                                 ,country2)))))
      (if (eql (lxml-get-tag result) 'ns1::|getRateResponse|)
          (let ((contents (lxml-find-tag :|Result| (rest result))))
            (if contents
                (coerce (read-from-string (second contents)) 'float)
              (error "Expected a <Result> element")))
        (error "Expected a <getRateResponse> element")))))

;; contributed by carlos.ungil@bluewin.ch
;; http://www.random.org/soap.html
;; http://www.random.org/RandomDotOrg.wsdl
;; http://www.random.org/clients/soap/

(defun get-random-number (&key unsigned)
  "Returns a true random  number in the interval [2^31, 2^31) or [0, 2^31)"
  (let ((ns "urn:RandomDotOrg"))
    (s-xml:register-namespace ns "ns1" :ns1)
    (let* ((rng (make-soap-end-point "http://www.random.org/cgi-bin/Random.cgi"))
	   (result (soap-call rng
			      nil
			      (if unsigned
				  `((ns1::|lrand48| :|xmlns:ns1| ,ns))
                                `((ns1::|mrand48| :|xmlns:ns1| ,ns))))))
      (if (or (eql (lxml-get-tag result) 'ns1::|lrand48Response|)
	      (eql (lxml-get-tag result) 'ns1::|mrand48Response|))
	  (values (parse-integer (second (second result))))
        (error "Expected a <lrand48Response> or <mrand48Response> element")))))

;; accessing local AXIS services
      
(defun axis-example3-echo (string)
  "Calling Axis's User Guide Example 3 'echo'"
  (let ((ns "http://example3.userguide.samples"))
    (s-xml:register-namespace ns "ns1" :ns1)
    (let* ((local (make-soap-end-point "http://localhost:8080/axis/services/MyService"))
           (result (soap-call local
                              '()
                              `((ns1::|serviceMethod| 
                                 soapenv:|encodingStyle| ,+soap-enc-ns-uri+ 
                                 :|xmlns:ns1| ,ns) 
                                ((:|arg| xsi::|type| "xsd:string") 
                                 ,string)))))
      (if (eql (lxml-get-tag result) 'ns1::|serviceMethodResponse|)
          (let ((contents (lxml-find-tag :|serviceMethodReturn| (rest result))))
            (if contents
                (second contents)
              (error "Expected a <serviceMethodReturn> element")))
        (error "Expected a <serviceMethodResponse> element")))))

;;;; eof
