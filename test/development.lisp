;;;; -*- Mode: LISP -*-
;;;;
;;;; $Id: development.lisp,v 1.2 2005-10-01 08:48:50 scaekenberghe Exp $
;;;;
;;;; Development scratch pad
;;;;
;;;; Copyright (C) 2005 Sven Van Caekenberghe, Beta Nine BVBA.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :cl-soap)

;;; The Google AdWords API 

(defconstant +google-adwords-ns-uri+ "https://adwords.google.com/api/adwords/v2")

(defpackage :google
  (:nicknames "google")
  (:export)
  (:documentation "Package for symbols in the Google AdWords API XML Namespace"))

(defparameter *google-adwords-ns* (s-xml:register-namespace +google-adwords-ns-uri+ "google" :google))

;;; Older Manual Google AdWords Calls

(export 
   '(;; headers
     "email" "password" "useragent" "token" "clientEmail"
     ;; info service
     "getUsageQuotaThisMonth" "getUsageQuotaThisMonthResponse" "getUsageQuotaThisMonthReturn"
     "getCampaigns" "getCampaign" "getBillingAddress"
     ;; optionally add more exports, but this is not really needed for wsdl-soap-call's
   ))

(defun get-usage-quota-this-month ()
  (multiple-value-bind (result headers)
      (soap-call (make-soap-end-point "https://adwords.google.com:443/api/adwords/v2/InfoService")
                 `((google:|email| ,*google-adwords-email*)
                   (google:|password| ,*google-adwords-password*)
                   (google:|useragent| ,*google-adwords-user-agent*)
                   (google:|token| ,*google-adwords-token*))
                 `(google:|getUsageQuotaThisMonth|)
                 :envelope-attributes `(:|xmlns| ,+google-adwords-ns-uri+))
    (if (eql (lxml-get-tag result) 'google:|getUsageQuotaThisMonthResponse|)
        (let ((contents (lxml-find-tag 'google:|getUsageQuotaThisMonthReturn| (rest result))))
          (if contents
              (values (parse-integer (second contents)) headers)
            (error "Expected a <getUsageQuotaThisMonthReturn> element")))
      (error "Expected a <getUsageQuotaThisMonthResponse> element"))))

(defun get-method-cost (service method &optional (date (ut)))
  (multiple-value-bind (result headers)
      (soap-call (make-soap-end-point "https://adwords.google.com:443/api/adwords/v2/InfoService")
                 `((google:|email| ,*google-adwords-email*)
                   (google:|password| ,*google-adwords-password*)
                   (google:|useragent| ,*google-adwords-user-agent*)
                   (google:|token| ,*google-adwords-token*))
                 `(google:|getMethodCost| 
                   (google:|service| ,service)
                   (google:|method| ,method)
                   (google:|date| ,(lisp->xsd-date date)))
                 :envelope-attributes `(:|xmlns| ,+google-adwords-ns-uri+))
    (if (eql (lxml-get-tag result) 'google:|getMethodCostResponse|)
        (let ((contents (lxml-find-tag 'google:|getMethodCostReturn| (rest result))))
          (if contents
              (values (parse-integer (second contents)) headers)
            (error "Expected a <getMethodCostReturn> element")))
      (error "Expected a <getMethodCostResponse> element"))))

(defun get-billing-address (client-email)
  (multiple-value-bind (result headers)
      (soap-call (make-soap-end-point "https://adwords.google.com:443/api/adwords/v2/AccountService")
                 `((google:|email| ,*google-adwords-email*)
                   (google:|password| ,*google-adwords-password*)
                   (google:|useragent| ,*google-adwords-user-agent*)
                   (google:|token| ,*google-adwords-token*)
                   (google:|clientEmail| ,client-email))
                 `(google:|getBillingAddress|)
                 :envelope-attributes `(:|xmlns| ,+google-adwords-ns-uri+))
    (if (eql (lxml-get-tag result) 'google:|getBillingAddressResponse|)
        (values (rest result) headers)
      (error "Expected a <getBillingAddressResponse> element"))))

(defun get-all-adwords-campaigns (client-email)
  (multiple-value-bind (result headers)
      (soap-call (make-soap-end-point "https://adwords.google.com:443/api/adwords/v2/CampaignService")
                 `((google:|email| ,*google-adwords-email*)
                   (google:|password| ,*google-adwords-password*)
                   (google:|useragent| ,*google-adwords-user-agent*)
                   (google:|token| ,*google-adwords-token*)
                   (google:|clientEmail| ,client-email))
                 `(google:|getAllAdWordsCampaigns|
                   (google:|dummy| "1"))
                 :envelope-attributes `(:|xmlns| ,+google-adwords-ns-uri+))
    (values result headers)))

(defun estimate-keyword-list (keywords)
  "((<text> <type> <max-cpc>)*) where type is Broad|Phrase|Exact"
  (multiple-value-bind (result headers)
      (soap-call (make-soap-end-point "https://adwords.google.com:443/api/adwords/v2/TrafficEstimatorService")
                 `((google:|email| ,*google-adwords-email*)
                   (google:|password| ,*google-adwords-password*)
                   (google:|useragent| ,*google-adwords-user-agent*)
                   (google:|token| ,*google-adwords-token*))
                 `(google::|estimateKeywordList|
                   ,@(mapcar #'(lambda (keyword)
                                 (destructuring-bind (text type max-cpc)
                                     keyword
                                   `(google::|keywordRequest|
                                     (google::|text| ,text)
                                     (google::|type| ,type)
                                     (google::|maxCpc| ,max-cpc))))
                             keywords))
                 :envelope-attributes `(:|xmlns| ,+google-adwords-ns-uri+))
    (values result headers)))

(defun get-campaign (id client-email)
  (multiple-value-bind (result headers)
      (soap-call (make-soap-end-point "https://adwords.google.com:443/api/adwords/v2/CampaignService")
                 `((google:|email| ,*google-adwords-email*)
                   (google:|password| ,*google-adwords-password*)
                   (google:|useragent| ,*google-adwords-user-agent*)
                   (google:|token| ,*google-adwords-token*)
                   (google:|clientEmail| ,client-email))
                 `(google:|getCampaign|
                   (google:|id| ,(princ-to-string id)))
                 :envelope-attributes `(:|xmlns| ,+google-adwords-ns-uri+))
    (values result headers)))

;; Moved code

(defun binding-primitive-value (name type bindings)
  (let ((value (get-name-binding name bindings)))
    (when value
      (lisp->xsd-primitive value (intern-xsd-type-name type)))))

(defun bind-primitive (element type-name bindings namespace)
  (let ((value (binding-primitive-value (get-name element) type-name bindings)))
    (if value
        `(,(intern (get-name element) (s-xml:get-package namespace)) ,value)
      (if (is-optional-p element)
          nil
        (error "Cannot find binding for ~a" (get-name element))))))
  
(defun bind-type (type-spec bindings super-element xml-schema-definition namespace)
  (let* ((type-element (if (stringp type-spec) (get-element-named xml-schema-definition type-spec) type-spec))
         (type (get-element-type xml-schema-definition type-element)))
    (if (typep type 'xsd-complex-type)
        (let ((members (get-members type xml-schema-definition))
              (members-actual-bindings '()))
          (loop :for member :in members :do
                (let* ((member-name (get-name member))
                       (member-type (get-type member))
                       (sub-tag-name (intern member-name (s-xml:get-package namespace))))
                  (if (is-plural-p member)
                      (let ((count 0))
                        (loop :for sub-binding :in (get-name-binding member-name bindings) :do
                              (if (xsd-primitive-type-name-p member-type)
                                  (let ((member-binding (bind-primitive member member-type 
                                                                        sub-binding namespace)))
                                    (when member-binding
                                      (incf count)
                                      (push member-binding members-actual-bindings)))
                                (multiple-value-bind (member-binding bound)
                                    (bind-type member-type sub-binding member 
                                               xml-schema-definition namespace)
                                  (when bound
                                    (incf count)
                                    (push `(,sub-tag-name ,@member-binding) members-actual-bindings)))))
                        (if (zerop count)
                            (unless (or (is-optional-p member) (get-nillable member))
                              (error "Required element <~a> not found" member-name))))
                    (let ((sub-binding (get-name-binding member-name bindings)))
                      (cond ((xsd-primitive-type-name-p member-type)
                             (let ((member-binding (bind-primitive member member-type 
                                                                   bindings namespace)))
                               (when member-binding
                                 (push member-binding members-actual-bindings))))
                            (t
                             (multiple-value-bind (member-binding bound)
                                 (bind-type member-type sub-binding member 
                                            xml-schema-definition namespace)
                               (if bound
                                   (push `(,sub-tag-name ,@member-binding) members-actual-bindings)
                                 (unless (or (is-optional-p member) (get-nillable member))
                                   (error "Required member ~a not bound" member-name))))))))))
          (values (nreverse members-actual-bindings) t))
      (if (xsd-primitive-type-name-p type)
          (let ((value (binding-primitive-value (get-name super-element) type bindings)))
            (if value (values (list value) t) (values nil nil)))
        (error "Unexpected type")))))

(defun bind-element (element bindings xml-schema-definition namespace)
  (let* ((element (if (stringp element) (get-element-named xml-schema-definition element) element))
         (element-type (get-element-type xml-schema-definition element)))
    (cond ((xsd-primitive-type-name-p element-type)
           (bind-primitive element element-type bindings namespace))
          ((typep element-type 'xsd-complex-type)
           (let ((sub-bindings (get-name-binding (get-name element) bindings))
                 (tag-name (intern (get-name element) (s-xml:get-package namespace))))
             (if sub-bindings
                 (multiple-value-bind (members-binding bound)
                     (bind-type element-type sub-bindings element xml-schema-definition namespace)
                   (when bound
                     `(,tag-name ,@members-binding)))
               (if (or (is-optional-p element) (null (get-members element-type xml-schema-definition)))
                   tag-name
                 (error "Element ~a not bound" (get-name element))))))
          (t (error "Cannot bind element ~s of type ~s" element element-type)))))

(defun lxml-primitive-value (name type lxml namespace)
  (let ((tag-name (intern name (s-xml:get-package namespace))))
    (if (eql (lxml-get-tag lxml) tag-name)
        (values (xsd-primitive->lisp (first (lxml-get-children lxml)) (intern-xsd-type-name type)) t)
      (values nil nil))))

(defun resolve-primitive (element type-name lxml namespace)
  (multiple-value-bind (value present)
      (lxml-primitive-value (get-name element) type-name lxml namespace)
    (if present
        (values value t)
      (if (is-optional-p element)
          (values nil nil)
        (error "Expected a <~a> element" (get-name element))))))

(defun resolve-type (type-name lxml super-element xml-schema-definition namespace)
  (let* ((type-element (get-element-named xml-schema-definition type-name))
         (type (get-element-type xml-schema-definition type-element)))
    (if (typep type 'xsd-complex-type)
        (let ((members (get-members type xml-schema-definition))
              (resolved-members '()))
          (loop :for member :in members :do
                (let* ((member-name (get-name member))
                       (member-type (get-type member))
                       (sub-tag-name (intern member-name (s-xml:get-package namespace))))
                  (if (is-plural-p member)
                      (let ((count 0))
                        (loop :for item-lxml :in (lxml-find-tags sub-tag-name (lxml-get-children lxml)) :do
                              (if (xsd-primitive-type-name-p member-type)
                                  (multiple-value-bind (member-value required)
                                      (resolve-primitive member member-type item-lxml namespace)
                                    (when required
                                      (incf count)
                                      (push (list member-name member-value) resolved-members)))
                                (multiple-value-bind (member-value required)
                                    (resolve-type member-type item-lxml member 
                                                  xml-schema-definition namespace)
                                  (when required
                                    (incf count)
                                    (push (list member-name member-value) resolved-members)))))
                        (if (zerop count)
                            (unless (or (is-optional-p member) (get-nillable member))
                              (error "Required element <~a> not found" member-name))))
                    (let ((member-lxml (lxml-find-tag sub-tag-name lxml)))
                      (if (xsd-primitive-type-name-p member-type)
                          (multiple-value-bind (member-value required)
                              (resolve-primitive member member-type member-lxml namespace)
                            (when required
                              (push member-name resolved-members)
                              (push member-value resolved-members)))
                        (multiple-value-bind (member-value required)
                            (resolve-type member-type member-lxml member 
                                          xml-schema-definition namespace)
                          (when required
                            (push member-name resolved-members)
                            (push member-value resolved-members))))))))
          (values (nreverse resolved-members) t))
      (if (xsd-primitive-type-name-p type)
          (lxml-primitive-value (get-name super-element) type lxml namespace)
        (error "Unexpected type")))))

(defun resolve-element (element lxml xml-schema-definition namespace)
  (let* ((element (if (stringp element) (get-element-named xml-schema-definition element) element))
         (element-type (get-element-type xml-schema-definition element)))
    (cond ((xsd-primitive-type-name-p element-type)
           (resolve-primitive element element-type lxml namespace))
          ((typep element-type 'xsd-complex-type)
           (let ((tag-name (intern (get-name element) (s-xml:get-package namespace))))
             (if (eql (lxml-get-tag lxml) tag-name)
                 (let ((sub-lxml (lxml-get-children lxml))
                       (members (get-members element-type xml-schema-definition))
                       (resolved-members '()))
                   (loop :for member :in members :do
                         (let* ((member-name (get-name member))
                                (member-type (get-type member))
                                (sub-tag-name (intern member-name (s-xml:get-package namespace))))
                           (if (is-plural-p member)
                               (let ((count 0))
                                 (loop :for item-lxml :in sub-lxml :do
                                       (if (eql (lxml-get-tag item-lxml) sub-tag-name)
                                           (if (xsd-primitive-type-name-p member-type)
                                               (multiple-value-bind (member-value required)
                                                   (resolve-primitive member member-type item-lxml namespace)
                                                 (when required
                                                   (incf count)
                                                   (push (list member-name member-value) resolved-members)))
                                             (multiple-value-bind (member-value required)
                                                 (resolve-type member-type item-lxml member 
                                                               xml-schema-definition namespace)
                                               (when required
                                                 (incf count)
                                                 (push (list member-name member-value) resolved-members))))
                                         (error "Expected a <~a> element" sub-tag-name)))
                                 (if (zerop count)
                                     (unless (or (is-optional-p member) (get-nillable member))
                                       (error "Required element <~a> not found" member-name))))
                             (let ((member-lxml (lxml-find-tag sub-tag-name sub-lxml)))
                               (if (xsd-primitive-type-name-p member-type)
                                   (multiple-value-bind (member-value required)
                                       (resolve-primitive member member-type member-lxml namespace)
                                     (when required
                                       (push member-name resolved-members)
                                       (push member-value resolved-members)))
                                 (multiple-value-bind (member-value required)
                                     (resolve-type member-type member-lxml member 
                                                   xml-schema-definition namespace)
                                   (when required
                                     (push member-name resolved-members)
                                     (push member-value resolved-members))))))))
                   (values (list (get-name element) (nreverse resolved-members)) t))
               (if (is-optional-p element)
                   (values nil nil)
                 (error "Expected a <~a> element" tag-name)))))
          (t (error "Cannot resolve element ~s of type ~s" element element-type)))))

;;; Describing XSD (with pre-rendering of XML)

(defmethod describe-multiplicity ((xml-schema-element xml-schema-element))
  (with-slots (min-occurs max-occurs)
      xml-schema-element
    (cond ((and (zerop min-occurs) (eql max-occurs 1)) "optional")
          ((and (eql min-occurs 1) (eql max-occurs 1)) "required")
          ((and (eql min-occurs 1) (eql max-occurs :unbounded)) "one or more")
          ((and (zerop min-occurs) (eql max-occurs :unbounded)) "zero or more")
          (t (format nil "min:~d-max:~d" min-occurs max-occurs)))))

(defmethod multiplicity-suffix ((xml-schema-element xml-schema-element))
  (with-slots (min-occurs max-occurs)
      xml-schema-element
    (cond ((and (zerop min-occurs) (eql max-occurs 1)) "?")
          ((and (eql min-occurs 1) (eql max-occurs 1)) "")
          ((and (eql min-occurs 1) (eql max-occurs :unbounded)) "+")
          ((and (zerop min-occurs) (eql max-occurs :unbounded)) "*")
          (t (format nil "~d:~d" min-occurs max-occurs)))))

(defun pre-render-xsd-type (xml-schema-definition type-name &key (level 0) (stream *standard-output*))
  (let* ((type-element (get-element-named xml-schema-definition type-name))
         (type (get-element-type xml-schema-definition type-element)))
    (if (typep type 'xsd-complex-type)
        (let ((members (get-members type xml-schema-definition)))
          (loop :for member :in members :do
                (let ((member-name (get-name member))
                      (member-type (get-type member)))
                  (indent level stream)
                  (if (xsd-primitive-type-name-p member-type)
                      (format stream "    <~a>~a</~a>~a~%" 
                              member-name member-type member-name (multiplicity-suffix member)) 
                    (progn
                      (format stream "    <~a>~%" member-name)
                      (pre-render-xsd-type xml-schema-definition member-type 
                                           :level (1+ level) :stream stream)
                      (indent level stream)
                      (format stream "    </~a>~a~%" member-name (multiplicity-suffix member)))))))
      (if (xsd-primitive-type-name-p type)
          (progn
            (indent level stream)
            (format stream "  ~a~%" type))
        (error "unexpected type")))))

(defun describe-xsd-type (xml-schema-definition type-name &key (level 0) (stream *standard-output*))
  (let* ((type-element (get-element-named xml-schema-definition type-name))
         (type (get-element-type xml-schema-definition type-element)))
    (if (typep type 'xsd-complex-type)
        (let ((members (get-members type xml-schema-definition)))
          (loop :for member :in members :do
                (let ((member-name (get-name member))
                      (member-type (get-type member)))
                  (indent level stream)
                  (if (xsd-primitive-type-name-p member-type)
                      (format stream "  Member ~s of primitive type ~s [~a]~@[ nillable~]~%" 
                              member-name member-type (describe-multiplicity member) (get-nillable member)) 
                    (progn
                      (format stream "  Member ~s [~a]~@[ nillable~]~%" member-name 
                              (describe-multiplicity member) (get-nillable member))
                      (describe-xsd-type xml-schema-definition member-type 
                                         :level (1+ level) :stream stream))))))
      (if (xsd-primitive-type-name-p type)
          (progn
            (indent level stream)
            (format stream "  primitive type ~a~%" type))
        (error "unexpected type")))))

(defun describe-xsd-element (xml-schema-definition element &key (level 0) (stream *standard-output*))
  (unless (typep element 'xml-schema-element)
    (setf element (get-element-named xml-schema-definition element)))
  (let* ((element-type (get-element-type xml-schema-definition element))
         (element-name (get-name element)))
    (if (xsd-primitive-type-name-p element-type)
        (progn
          (indent level stream)
          (format stream "Element ~s of primitive type ~s [~a]~@[ nillable~]~%" 
                  element-name element-type (describe-multiplicity element) (get-nillable element))
          (indent level stream)
          (format stream "  <~a>~a</~a>~a~%" 
                  element-name element-type element-name (multiplicity-suffix element)))
      (let ((members (get-members element-type xml-schema-definition)))
        (indent level stream)
        (format stream "Element ~s [~a]~@[ nillable~]~%" element-name 
                (describe-multiplicity element) (get-nillable element))
        (loop :for member :in members :do
              (let ((member-name (get-name member))
                    (member-type (get-type member)))
                (indent level stream)
                (if (xsd-primitive-type-name-p member-type)
                    (format stream "  Member ~s of primitive type ~s [~a]~@[ nillable~]~%" 
                            member-name member-type (describe-multiplicity member) (get-nillable member)) 
                  (progn
                    (format stream "  Member ~s [~a]~@[ nillable~]~%" member-name 
                            (describe-multiplicity member) (get-nillable member))
                    (describe-xsd-type xml-schema-definition member-type 
                                       :level (1+ level) :stream stream)))))
        (indent level stream)
        (format stream "  <~a>~%" element-name)
        (loop :for member :in members :do
              (let ((member-name (get-name member))
                    (member-type (get-type member)))
                (indent level stream)
                (if (xsd-primitive-type-name-p member-type)
                    (format stream "    <~a>~a</~a>~a~%" 
                            member-name member-type member-name (multiplicity-suffix member)) 
                  (progn
                    (format stream "    <~a>~%" member-name)
                    (pre-render-xsd-type xml-schema-definition member-type :level (1+ level) :stream stream)
                    (indent level stream)
                    (format stream "    </~a>~a~%" member-name (multiplicity-suffix member))))))
        (indent level stream)
        (format stream "  </~a>~a~%" element-name (multiplicity-suffix element))))))
  
(defun describe-xsd (xml-schema-definition &key (stream *standard-output*))
  "Print a high-level description of the top-level elements in xml-schema-definition"
  (format stream "XML Schema Definition with target-namespace URI ~s~%" 
          (get-target-namespace xml-schema-definition))
  (loop :for element :in (get-elements xml-schema-definition) :do
        (when (typep element 'xml-schema-element)
          (describe-xsd-element xml-schema-definition element 
                                :level 1 :stream stream)))
  (values))

;;;; eof
