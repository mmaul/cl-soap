;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: wsdl.lisp,v 1.23 2008-04-14 13:41:47 scaekenberghe Exp $
;;;;
;;;; The basic WSDL protocol: we parse the generic and soap specific parts
;;;;
;;;; Copyright (C) 2005 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :cl-soap)

;;; Generic WSDL Model

(defclass abstract-wsdl-definition ()
  ((name :accessor get-name :initarg :name :initform nil)
   (documentation :accessor get-documentation :initarg :documentation :initform nil)))

(defmethod print-object ((object abstract-wsdl-definition) out)
  (print-unreadable-object (object out :type t :identity t)
    (prin1 (or (get-name object) "anonymous") out)))

(defclass wsdl-document-definitions (abstract-wsdl-definition)
  ((target-namespace :accessor get-target-namespace :initarg :target-namespace :initform nil)
   (types :accessor get-types :initarg :types :initform nil)
   (messages :accessor get-messages :initarg :messages :initform nil)
   (port-types :accessor get-port-types :initarg :port-types :initform nil)
   (bindings :accessor get-bindings :initarg :bindings :initform nil)
   (services :accessor get-services :initarg :bindings :initform nil)))

(defclass wsdl-service (abstract-wsdl-definition)
  ((ports :accessor get-ports :initarg :ports :initform nil)))

(defclass wsdl-port (abstract-wsdl-definition)
  ((binding :accessor get-binding :initarg :binding :initform nil)
   (extension :accessor get-extension :initarg :extension :initform nil)))

(defclass wsdl-extensions-mixin ()
  ((extensions :accessor get-extensions :initarg :extensions :initform nil)))

(defclass wsdl-binding (abstract-wsdl-definition wsdl-extensions-mixin)
  ((type :accessor get-type :initarg :type :initform nil)
   (operations :accessor get-operations :initarg :operations :initform nil)))

(defclass wsdl-port-type (abstract-wsdl-definition)
  ((operations :accessor get-operations :initarg :operations :initform nil)))

(defclass wsdl-operation-element (wsdl-extensions-mixin)
  ((message :accessor get-message :initarg :message :initform nil)))

(defmethod print-object ((object wsdl-operation-element) out)
  (print-unreadable-object (object out :type t :identity t)
    (prin1 (get-message object) out)))

(defclass wsdl-input (wsdl-operation-element)
  ())

(defclass wsdl-output (wsdl-operation-element)
  ())

(defclass wsdl-fault (wsdl-operation-element)
  ())

(defclass wsdl-operation (abstract-wsdl-definition wsdl-extensions-mixin)
  ((elements :accessor get-elements :initarg :elements :initform nil)))

(defclass wsdl-part ()
  ((name :accessor get-name :initarg :name :initform nil)
   (element :accessor get-element :initarg :element :initform nil)
   (type :accessor get-type :initarg :type :initform nil)))

(defmethod print-object ((object wsdl-part) out)
  (print-unreadable-object (object out :type t :identity t)
    (prin1 (or (get-name object) "anonymous") out)))

(defclass wsdl-message (abstract-wsdl-definition)
  ((parts :accessor get-parts :initarg :parts :initform nil)))

;;; WSDL SOAP Model Extension Elements

(defclass wsdl-soap-address ()
  ((location :accessor get-location :initarg :location :initform "http://localhost")))

(defmethod print-object ((object wsdl-soap-address) out)
  (print-unreadable-object (object out :type t :identity t)
    (prin1 (or (get-location object) "unknown") out)))

(defclass wsdl-soap-binding ()
  ((style :accessor get-style :initarg :style :initform "document")
   (transport :accessor get-transport :initarg :transport :initform "http://schemas.xmlsoap.org/soap/http")))

(defclass wsdl-soap-operation ()
  ((soap-action :accessor get-soap-action :initarg :soap-action :initform nil)
   (style :accessor get-style :initarg :style :initform nil)))

(defclass wsdl-soap-operation-element ()
  ((use :accessor get-use :initarg :use :initform nil)
   (encoding-style :accessor get-encoding-style :initarg :encoding-style :initform nil)
   (namespace :accessor get-namespace :initarg :namespace :initform nil)))

(defclass wsdl-soap-body (wsdl-soap-operation-element)
  ((parts :accessor get-parts :initarg :parts :initform nil)))

(defclass wsdl-soap-fault (wsdl-soap-operation-element)
  ((name :accessor get-name :initarg :name :initform nil)))

(defclass wsdl-soap-header (wsdl-soap-operation-element)
  ((message :accessor get-message :initarg :message :initform nil)
   (part :accessor get-part :initarg :part :initform nil)))

(defclass wsdl-soap-header-fault (wsdl-soap-header)
  ())

;; Parsing

;; one day we should handle <import> statements ;-)

(defun lxml->types (lxml)
  (let (types)
    (loop :for element :in (rest lxml) :do
          (if (eql (lxml-get-tag element) 'xsd:|schema|)
              (push (lxml->schema-definition element) types)))
    (nreverse types)))

(defun lxml->operation-element (lxml)
  (let* ((attributes (lxml-get-attributes lxml))
         (message (getf attributes :|message|))
         (class (ecase (lxml-get-tag lxml)
                  (wsdl:|input| 'wsdl-input)
                  (wsdl:|output| 'wsdl-output)
                  (wsdl:|fault| 'wsdl-fault)))
         (operation-element (make-instance class :message message)))
    (loop :for element :in (rest lxml)
          :do (case (lxml-get-tag element)
                (wsdl:|documentation| (setf (get-documentation operation-element)
                                            (rest element)))
                (wsdl-soap:|body| (let ((attributes (lxml-get-attributes element)))
                                    (push (make-instance 'wsdl-soap-body
                                                         :use (getf attributes :|use|)
                                                         :encoding-style (getf attributes :|encodingStyle|)
                                                         :namespace (getf attributes :|namespace|)
                                                         :parts (getf attributes :|parts|))
                                          (get-extensions operation-element))))
                (wsdl-soap:|fault| (let ((attributes (lxml-get-attributes element)))
                                    (push (make-instance 'wsdl-soap-fault
                                                         :use (getf attributes :|use|)
                                                         :encoding-style (getf attributes :|encodingStyle|)
                                                         :namespace (getf attributes :|namespace|)
                                                         :name (getf attributes :|name|))
                                          (get-extensions operation-element))))
                (wsdl-soap:|header| (let ((attributes (lxml-get-attributes element)))
                                    (push (make-instance 'wsdl-soap-header
                                                         :use (getf attributes :|use|)
                                                         :encoding-style (getf attributes :|encodingStyle|)
                                                         :namespace (getf attributes :|namespace|)
                                                         :part (getf attributes :|part|)
                                                         :message (getf attributes :|message|))
                                          (get-extensions operation-element))))
                (wsdl-soap:|headerfault| (let ((attributes (lxml-get-attributes element)))
                                    (push (make-instance 'wsdl-soap-header-fault
                                                         :use (getf attributes :|use|)
                                                         :encoding-style (getf attributes :|encodingStyle|)
                                                         :namespace (getf attributes :|namespace|)
                                                         :part (getf attributes :|part|)
                                                         :message (getf attributes :|message|))
                                          (get-extensions operation-element))))))
    operation-element))

(defun lxml->operation (lxml)
  (let* ((attributes (lxml-get-attributes lxml))
         (name (getf attributes :|name|))
         (wsdl-operation (make-instance 'wsdl-operation :name name)))
    (loop :for element :in (rest lxml)
          :do (case (lxml-get-tag element)
                (wsdl:|documentation| (setf (get-documentation wsdl-operation)
                                            (rest element)))
                (wsdl-soap:|operation| (let ((attributes (lxml-get-attributes element)))
                                         (push (make-instance 'wsdl-soap-operation
                                                              :style (getf attributes :|style|)
                                                              :soap-action (getf attributes :|soapAction|))
                                               (get-extensions wsdl-operation))))
                ((wsdl:|input| wsdl:|output| wsdl:|fault|)  (push (lxml->operation-element element)
                                                                  (get-elements wsdl-operation)))))
    wsdl-operation))

(defun lxml->port-type (lxml)
  (let* ((attributes (lxml-get-attributes lxml))
         (name (getf attributes :|name|))
         (wsdl-port-type (make-instance 'wsdl-port-type :name name)))
    (loop :for element :in (rest lxml)
          :do (case (lxml-get-tag element)
                (wsdl:|documentation| (setf (get-documentation wsdl-port-type)
                                            (rest element)))
                (wsdl:|operation| (push (lxml->operation element)
                                        (get-operations wsdl-port-type)))))
    wsdl-port-type))

(defun lxml->part (lxml)
  (let* ((attributes (lxml-get-attributes lxml))
         (name (getf attributes :|name|))
         (element (getf attributes :|element|))
         (type (getf attributes :|type|))
         (wsdl-part (make-instance 'wsdl-part 
                                   :name name
                                   :element element
                                   :type type)))
    wsdl-part))

(defun lxml->message (lxml)
  (let* ((attributes (lxml-get-attributes lxml))
         (name (getf attributes :|name|))
         (wsdl-message (make-instance 'wsdl-message :name name)))
    (loop :for element :in (rest lxml)
          :do (case (lxml-get-tag element)
                (wsdl:|documentation| (setf (get-documentation wsdl-message)
                                            (rest element)))
                (wsdl:|part| (push (lxml->part element) 
                                   (get-parts wsdl-message)))))
    wsdl-message))    

(defun lxml->binding (lxml)
  (let* ((attributes (lxml-get-attributes lxml))
         (name (getf attributes :|name|))
         (type (getf attributes :|type|))
         (wsdl-binding (make-instance 'wsdl-binding :name name :type type)))
    (loop :for element :in (rest lxml)
          :do (case (lxml-get-tag element)
                (wsdl:|documentation| (setf (get-documentation wsdl-binding)
                                            (rest element)))
                (wsdl-soap:|binding| (let ((attributes (lxml-get-attributes element)))
                                       (push (make-instance 'wsdl-soap-binding
                                                            :style (getf attributes :|style|)
                                                            :transport (getf attributes :|transport|))
                                             (get-extensions wsdl-binding))))
                (wsdl:|operation| (push (lxml->operation element) 
                                        (get-operations wsdl-binding)))))
    wsdl-binding))

(defun lxml->port (lxml)
  (let* ((attributes (lxml-get-attributes lxml))
         (name (getf attributes :|name|))
         (binding (getf attributes :|binding|))
         (wsdl-port (make-instance 'wsdl-port :name name :binding binding)))
    (loop :for element :in (rest lxml)
          :do (case (lxml-get-tag element)
                (wsdl:|documentation| (setf (get-documentation wsdl-port)
                                            (rest element)))
                (wsdl-soap:|address| (setf (get-extension wsdl-port)
                                           (make-instance 'wsdl-soap-address
                                                          :location (getf (lxml-get-attributes element) 
                                                                          :|location|))))))
    wsdl-port))

(defun lxml->service (lxml)
  (let* ((attributes (lxml-get-attributes lxml))
         (name (getf attributes :|name|))
         (wsdl-service (make-instance 'wsdl-service :name name)))
    (loop :for element :in (rest lxml)
          :do (case (lxml-get-tag element)
                (wsdl:|documentation| (setf (get-documentation wsdl-service)
                                            (rest element)))
                (wsdl:|port| (push (lxml->port element) 
                                   (get-ports wsdl-service)))))
    wsdl-service))

(defun parse-wsdl (in)
  (let ((lxml (s-xml:parse-xml in)))
    (if (eql (lxml-get-tag lxml) 'wsdl:|definitions|)
        (let* ((attributes (lxml-get-attributes lxml))
               (name (getf attributes :|name|))
               (target-namespace (getf attributes :|targetNamespace|))
               (wsdl-document-definitions (make-instance 'wsdl-document-definitions
                                                         :name name
                                                         :target-namespace target-namespace)))
          (loop :for element :in (rest lxml)
                :do (case (lxml-get-tag element)
                      (wsdl:|documentation| (setf (get-documentation wsdl-document-definitions)
                                                  (rest element)))
                      (wsdl:|types| (setf (get-types wsdl-document-definitions)
                                          (lxml->types element)))
                      (wsdl:|message| (push (lxml->message element) 
                                            (get-messages wsdl-document-definitions)))
                      (wsdl:|portType| (push (lxml->port-type element)
                                             (get-port-types wsdl-document-definitions)))
                      (wsdl:|binding| (push (lxml->binding element)
                                            (get-bindings wsdl-document-definitions)))
                      (wsdl:|service| (push (lxml->service element)
                                            (get-services wsdl-document-definitions)))))
          wsdl-document-definitions)
      (error "Expected a WSDL <definitions> element"))))

(defun parse-wsdl-file (pathname)
  (with-open-file (in pathname)
    (parse-wsdl in)))

(defun parse-wsdl-url (url)
  (multiple-value-bind (buffer code)
      (do-http-request url)
    (if (eql code 200)
        (with-input-from-string (in buffer)
          (parse-wsdl in))
      (error "Could not retrieve URL ~s, got a ~s code" url code))))

;; A simple caching mechanism for WSDL's by URL

(defvar *wsdl-cache* (make-hash-table :test #'equal))

(defun wsdl-cache-get (url)
  "Access a possibly cached WSDL url, downloading and parsing it if necessary"
  (let ((cached (gethash url *wsdl-cache*)))
    (if cached
        cached
      (setf (gethash url *wsdl-cache*) (parse-wsdl-url url)))))

(defun wsdl-cache-clear (url)
  "Clear the WSDL caching of url"
  (remhash url *wsdl-cache*))

(defun wsdl-cache-clear-all ()
  "Clear the whole WSDL cache"
  (clrhash *wsdl-cache*))

;; Interpreting the WSDL model

(defmethod get-service-named ((wsdl-document-definitions wsdl-document-definitions) service-name)
  (find-item-named service-name (get-services wsdl-document-definitions)))

(defmethod get-port-named ((wsdl-service wsdl-service) port-name)
  (find-item-named port-name (get-ports wsdl-service)))

(defmethod get-binding-named ((wsdl-document-definitions wsdl-document-definitions) binding-name)
  (find-item-named binding-name (get-bindings wsdl-document-definitions)))

(defmethod get-port-type-named ((wsdl-document-definitions wsdl-document-definitions) port-type-name)
  (find-item-named port-type-name (get-port-types wsdl-document-definitions)))
 
(defmethod get-message-named ((wsdl-document-definitions wsdl-document-definitions) message-name)
  (find-item-named message-name (get-messages wsdl-document-definitions)))
 
(defmethod get-operation-named ((wsdl-binding wsdl-binding) operation-name)
  (find-item-named operation-name (get-operations wsdl-binding)))

(defmethod get-operation-named ((wsdl-port-type wsdl-port-type) operation-name)
  (find-item-named operation-name (get-operations wsdl-port-type)))

(defmethod get-part-named ((wsdl-message wsdl-message) part-name)
  (find-item-named part-name (get-parts wsdl-message)))
 
(defun find-item-of-class (class-name sequence)
  (let ((class (find-class class-name)))
    (find-if #'(lambda (c) (eql c class)) sequence :key #'class-of)))

(defmethod get-operation-element ((wsdl-operation wsdl-operation) operation-element-type)
  (find-item-of-class operation-element-type (get-elements wsdl-operation)))

(defmethod get-extension-of-class ((wsdl-extensions-mixin wsdl-extensions-mixin) extension-type)
  (find-item-of-class extension-type (get-extensions wsdl-extensions-mixin)))

(defmethod get-extensions-of-class ((wsdl-extensions-mixin wsdl-extensions-mixin) extension-type)
  (let ((class (find-class extension-type)))
    (remove-if-not #'(lambda (c) (eql c class)) (get-extensions wsdl-extensions-mixin) :key #'class-of)))

(defmethod get-element-named ((wsdl-document-definitions wsdl-document-definitions) element-name)
  (get-element-named (first (get-types wsdl-document-definitions)) element-name))

(defmethod get-xml-schema-definition ((wsdl-document-definitions wsdl-document-definitions))
  (let ((xsd (first (get-types wsdl-document-definitions))))
    (when (typep xsd 'xml-schema-definition)
      xsd)))

;; Describing WSDL

(defun describe-wsdl-soap-part (part xml-schema-definition &key (stream *standard-output*) style)
  (when (equal style "rpc")
    (format stream "          Part: ~a" (get-name part)))
  (cond ((get-type part) 
         (format stream " of type: ~a~%" (get-type part)))
        ((get-element part)
         (describe-xsd-element (get-element part) xml-schema-definition stream 5))))

(defun describe-wsdl-soap (wsdl-document-definitions &key (stream *standard-output*))
  "Print a high-level description of the services/ports/operations in wsdl-document-definitions"
  (format stream "WSDL Document Definitions~@[ named ~a~]~%" (get-name wsdl-document-definitions))
  (loop :for service :in (get-services wsdl-document-definitions) :do 
        (format stream "  Service: ~a~%" (get-name service))
        (loop :for port :in (get-ports service) :do 
              (format stream "    Port: ~a~%" (get-name port))
              (format stream "    SOAP Address Location ~s~%" (get-location (get-extension port)))
              (let* ((binding-name (get-binding port))
                     (binding (get-binding-named wsdl-document-definitions binding-name))
                     (soap-binding (get-extension-of-class binding 'wsdl-soap-binding))
                     (style (get-style soap-binding))
                     (port-type-name (get-type binding))
                     (port-type (get-port-type-named wsdl-document-definitions port-type-name))
                     (xml-schema-definition (first (get-types wsdl-document-definitions))))
                (format stream "    Binding: ~a SOAP style [~a]~%" binding-name style)
                (loop :for operation :in (get-operations binding) :do
                      (format stream "      Operation: ~a~%" (get-name operation))
                      (let* ((operation-details (get-operation-named port-type (get-name operation)))
                             (input-element (get-operation-element operation-details 'wsdl-input))
                             (output-element (get-operation-element operation-details 'wsdl-output))
                             (input-message (get-message-named wsdl-document-definitions 
                                                               (get-message input-element)))
                             (output-message (get-message-named wsdl-document-definitions 
                                                                (get-message output-element))))
                        (format stream "        Input: ~a~%" (get-name input-message))
                        (loop :for part :in (get-parts input-message) :do
                              (describe-wsdl-soap-part part xml-schema-definition 
                                                       :stream stream :style style))
                        (format stream "        Output: ~a~%" (get-name output-message))
                        (loop :for part :in (get-parts output-message) :do
                              (describe-wsdl-soap-part part xml-schema-definition 
                                                       :stream stream :style style)))))))
  (values))

;; Using WSDL to make structured SOAP calls

(defun bind-input-parts (input-message input wsdl-document-definitions)
  (let ((namespace (s-xml:find-namespace (get-target-namespace wsdl-document-definitions)))
        (actual-input-parameters '()))
    (loop :for part :in (get-parts input-message) :do
          (let ((part-element (get-element part))
                (part-type (get-type part)))
            (cond ((xsd-primitive-type-name-p part-type)
                   (let ((value (get-name-binding (get-name part) input)))
                     (if value
                         (push `((,(intern (get-name part) :keyword) ;; default namespace!
                                  xsi::|type| ,part-type)
                                 ,(lisp->xsd-primitive value (intern-xsd-type-name part-type)))
                               actual-input-parameters)
                       (unless (is-optional-p part-element)
                         (error "No input binding found for ~a:~a" (get-name input-message) (get-name part))))))
                  (part-element
                   (push (bind-element part-element 
                                       input 
                                       (get-xml-schema-definition wsdl-document-definitions) 
                                       namespace)
                         actual-input-parameters))
                  (t (error "Cannot resolve input binding ~a:~a" (get-name input-message) (get-name part))))))
    (nreverse actual-input-parameters)))

(defun bind-input-headers (soap-input-headers headers wsdl-document-definitions)
  (let ((actual-headers '()))
    (loop :for part :in soap-input-headers :do
          (let* ((element (get-element part))
                 (namespace (s-xml:find-namespace (get-target-namespace wsdl-document-definitions)))
                 (xml-schema-definition (get-xml-schema-definition wsdl-document-definitions))
                 (binding (bind-element element headers xml-schema-definition namespace)))
            (when binding
              (push binding actual-headers))))
    (nreverse actual-headers)))

(defun resolve-output-parts (result output-message output wsdl-document-definitions)
  (declare (ignore output))
  (let ((namespace (s-xml:find-namespace (get-target-namespace wsdl-document-definitions)))
        (result-values '()))
    (loop :for part :in (get-parts output-message) :do
          (let ((part-type (get-type part))
                (part-element (get-element part)))
            (cond ((xsd-primitive-type-name-p part-type)
                   (let* ((tag-name (intern (get-name part) :keyword)) ;; default namespace!
                          (part-tag (lxml-find-tag tag-name (rest result)))
                          (part-value (second part-tag))) ;; part-tag might have a type attribute as well
                     (push (xsd-primitive->lisp part-value (intern-xsd-type-name part-type))
                           result-values)))
                  (part-element
                   (let ((part-value (resolve-element part-element 
                                                      (list result) 
                                                      (get-xml-schema-definition wsdl-document-definitions) 
                                                      namespace)))
                     (push part-value result-values)))
                  (t (error "Cannot resolve output binding ~a:~a" (get-name output-message) (get-name part))))))
    ;; make the common case more handy
    (if (= (length result-values) 1)
        (first result-values)
      (nreverse result-values))))

(defun resolve-output-headers (soap-output-headers headers wsdl-document-definitions)
  (let ((resolved-headers '()))
    (loop :for part :in soap-output-headers :do
          (let* ((element (get-element part))
                 (namespace (s-xml:find-namespace (get-target-namespace wsdl-document-definitions)))
                 (xml-schema-definition (get-xml-schema-definition wsdl-document-definitions))
                 (binding (resolve-element element headers xml-schema-definition namespace)))
            (when binding
              (push binding resolved-headers))))
    (nreverse resolved-headers)))

(defun wsdl-soap-document-call (wsdl-document-definitions
                                soap-end-point 
                                soap-action
                                input-message
                                output-message
                                soap-input-body
                                soap-input-headers
                                soap-output-body
                                soap-output-headers
                                input
                                output
                                headers)
  (let ((input-namespace-uri (or (get-namespace soap-input-body) 
                                 (get-target-namespace wsdl-document-definitions)))
        (output-namespace-uri (or (get-namespace soap-output-body) 
                                  (get-target-namespace wsdl-document-definitions)))
        namespace)
    (if (equal input-namespace-uri output-namespace-uri)
        (setf namespace (or (s-xml:find-namespace input-namespace-uri)
                            (s-xml:register-namespace input-namespace-uri "ns1" :ns1)))
      (error "The case where input and output namespaces differ is not yet supported"))
    (multiple-value-bind (result headers)
        (soap-call soap-end-point
                   (bind-input-headers soap-input-headers headers wsdl-document-definitions)
                   ;; we assume there is only one parameter
                   (first (bind-input-parts input-message input wsdl-document-definitions))
                   :soap-action soap-action
                   :envelope-attributes `(,(intern (format nil "xmlns:~a" (s-xml:get-prefix namespace))
                                                   :keyword)
                                          ,input-namespace-uri
                                          :|xmlns|
                                          ,input-namespace-uri))
      ;; we assume there is only one result
      (values (resolve-output-parts result output-message output wsdl-document-definitions)
              (resolve-output-headers soap-output-headers headers wsdl-document-definitions)))))

(defun wsdl-soap-rpc-call (wsdl-document-definitions
                           soap-end-point 
                           soap-action
                           binding-operation
                           input-message
                           output-message
                           soap-input-body
                           soap-output-body
                           input
                           output)
  (let ((input-namespace-uri (get-namespace soap-input-body))
        (output-namespace-uri (get-namespace soap-output-body)))
    (if (equal input-namespace-uri output-namespace-uri)
        (s-xml:register-namespace input-namespace-uri "ns1" :ns1)
      (error "The case where input and output namespaces differ is not yet supported"))
    (let ((input-wrapper (intern (get-name binding-operation) :ns1)))
      (multiple-value-bind (result headers)
          (soap-call soap-end-point
                     '()
                     `((,input-wrapper
                        soapenv:|encodingStyle| ,+soap-enc-ns-uri+
                        :|xmlns:ns1| ,input-namespace-uri)
                       ,@(bind-input-parts input-message input wsdl-document-definitions))
                     :soap-action soap-action)
        (let ((output-wrapper (intern (get-name output-message) :ns1)))
          (if (eql (lxml-get-tag result) output-wrapper)
              (values (resolve-output-parts result output-message output wsdl-document-definitions)
                      headers)
            (error "Expected <~a> element" output-wrapper)))))))

(defun wsdl-soap-input-headers (wsdl-document-definitions binding-operation-input)
  (let ((soap-input-headers (get-extensions-of-class binding-operation-input 'wsdl-soap-header)))
    (loop :for soap-input-header :in soap-input-headers 
          :collect (let* ((part-name (get-part soap-input-header))
                          (message-name (get-message soap-input-header))
                          (header-message (get-message-named wsdl-document-definitions message-name)))
                     (get-part-named header-message part-name)))))

(defun wsdl-soap-output-headers (wsdl-document-definitions binding-operation-output)
  (let ((soap-output-headers (get-extensions-of-class binding-operation-output 'wsdl-soap-header)))
    (loop :for soap-output-header :in soap-output-headers 
          :collect (let* ((part-name (get-part soap-output-header))
                          (message-name (get-message soap-output-header))
                          (header-message (get-message-named wsdl-document-definitions message-name)))
                     (get-part-named header-message part-name)))))

(defun wsdl-soap-call-internal (wsdl-document-definitions
                                port
                                operation-name
                                input
                                output
                                headers
                                &optional endpoint-address)
  (let* ((address-location-url (or endpoint-address (get-location (get-extension port))))
         (soap-end-point (make-soap-end-point address-location-url))
         (binding (get-binding-named wsdl-document-definitions (get-binding port)))
         (soap-binding (get-extension-of-class binding 'wsdl-soap-binding))
         (port-type (get-port-type-named wsdl-document-definitions (get-type binding)))
         (binding-operation (get-operation-named binding operation-name))
         (soap-operation (get-extension-of-class binding-operation 'wsdl-soap-operation))
         (soap-action (get-soap-action soap-operation))
         (binding-operation-input (get-operation-element binding-operation 'wsdl-input))
         (soap-input-body (get-extension-of-class binding-operation-input 'wsdl-soap-body))
         (soap-input-headers (wsdl-soap-input-headers wsdl-document-definitions binding-operation-input))
         (binding-operation-output (get-operation-element binding-operation 'wsdl-output))
         (soap-output-headers (wsdl-soap-output-headers wsdl-document-definitions binding-operation-output))
         (binding-operation-output (get-operation-element binding-operation 'wsdl-output))
         (soap-output-body (get-extension-of-class binding-operation-output 'wsdl-soap-body))
         (port-type-operation (get-operation-named port-type operation-name))
         (input-message (get-message-named wsdl-document-definitions 
                                           (get-message (get-operation-element port-type-operation 'wsdl-input))))
         (output-message (get-message-named wsdl-document-definitions
                                            (get-message (get-operation-element port-type-operation 'wsdl-output)))))
    (if (string-equal (get-transport soap-binding) "http://schemas.xmlsoap.org/soap/http")
        (cond ((and (string-equal (get-style soap-binding) "rpc")
                    (string-equal (get-use soap-input-body) "encoded")
                    (string-equal (get-use soap-output-body) "encoded")
                    (string-equal (get-encoding-style soap-input-body) "http://schemas.xmlsoap.org/soap/encoding/")
                    (string-equal (get-encoding-style soap-output-body) "http://schemas.xmlsoap.org/soap/encoding/"))
               (wsdl-soap-rpc-call wsdl-document-definitions
                                   soap-end-point 
                                   soap-action
                                   binding-operation
                                   input-message
                                   output-message
                                   soap-input-body
                                   soap-output-body
                                   input
                                   output))
              ((and (string-equal (get-style soap-binding) "document")
                    (string-equal (get-use soap-input-body) "literal")
                    (string-equal (get-use soap-output-body) "literal"))
               (wsdl-soap-document-call wsdl-document-definitions
                                        soap-end-point 
                                        soap-action
                                        input-message
                                        output-message
                                        soap-input-body
                                        soap-input-headers
                                        soap-output-body
                                        soap-output-headers
                                        input
                                        output
                                        headers))
              (t (error "Only standard SOAP RPC and Document style currently supported as binding")))
      (error "Only standard SOAP HTTP transport currently supported as binding"))))

;; wsdl: either an instance of wsdl-document-definitions, a string url, a stream to parse, a pathname
;; operation-name: string naming the operation to invoke
;; service-name: string of service to use (if nil, use first service found)
;; port-name: string of port of service to use (if nil, use first port found)
;; input: plist ("name1" value1 "name2" value2 ...) of actual parameters to use
;; output: what to do with the result (if nil: use the contents of the first part of the output message, if possible)

(defun wsdl-soap-call (wsdl 
                       operation-name
                       &key
                       service-name
                       port-name
                       input
                       output
                       headers
                       endpoint)
  "Use WSDL to make a SOAP call of operation/port/service using input/output/headers"
  (let* ((wsdl-document-definitions (etypecase wsdl
                                      (wsdl-document-definitions wsdl)
                                      (string (parse-wsdl-url wsdl))
                                      (pathname (parse-wsdl-file wsdl))))
         (service (if service-name 
                      (get-service-named wsdl-document-definitions service-name)
                    (first (get-services wsdl-document-definitions))))
         (port (if port-name
                   (get-port-named service port-name)
                 (first (get-ports service)))))
    (wsdl-soap-call-internal wsdl-document-definitions
                             port
                             operation-name
                             input
                             output
                             headers
                             endpoint)))

;;;; eof
