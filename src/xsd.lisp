;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: xsd.lisp,v 1.26 2005-10-06 11:09:39 scaekenberghe Exp $
;;;;
;;;; A partial implementation of the XML Schema Definition standard
;;;;
;;;; Copyright (C) 2005 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :cl-soap)

;;; Model

(defclass xml-schema-definition ()
  ((target-namespace :accessor get-target-namespace :initarg :target-namespace :initform nil)
   (elements :accessor get-elements :initarg :elements :initform nil)))

(defclass children-mixin ()
  ((children :accessor get-children :initarg :children :initform nil)))

(defclass xml-schema-element (children-mixin)
  ((name :accessor get-name :initarg :name :initform nil)
   (type :accessor get-type :initarg :type :initform nil)
   (min-occurs :accessor get-min-occurs :initarg :min-occurs :initform 1)
   (max-occurs :accessor get-max-occurs :initarg :max-occurs :initform 1)
   (nillable :accessor get-nillable :initarg :nillable :initform nil)))

(defmethod print-object ((object xml-schema-element) out)
  (print-unreadable-object (object out :type t :identity t)
    (prin1 (or (get-name object) "anonymous") out)))

(defclass xsd-type (children-mixin)
  ((name :accessor get-name :initarg :name :initform nil)))

(defmethod print-object ((object xsd-type) out)
  (print-unreadable-object (object out :type t :identity t)
    (prin1 (or (get-name object) "anonymous") out)))

(defclass xsd-simple-type (xsd-type)
  ())

(defclass xsd-complex-type (xsd-type)
  ())

(defclass xsd-complex-content (children-mixin)
  ())

(defclass xsd-compositor (children-mixin)
  ())

(defclass xsd-sequence (xsd-compositor)
  ())

(defclass xsd-choice (xsd-compositor)
  ())

(defclass xsd-all (xsd-compositor)
  ())

(defclass xsd-restriction ()
  ((base :accessor get-base :initarg :base :initform nil)))

(defclass xsd-extension (children-mixin)
  ((base :accessor get-base :initarg :base :initform nil)))

(defmethod print-object ((object xsd-restriction) out)
  (print-unreadable-object (object out :type t :identity t)
    (prin1 (or (get-base object) "unknown") out)))

;;; Parsing

(defun handle-lxml-schema-elements (children-mixin lxml)
  (loop :for child :in (lxml-get-children lxml) 
        :do (push (lxml->schema-element child) 
                  (get-children children-mixin)))
  (setf (get-children children-mixin) (nreverse (get-children children-mixin))))

(defun lxml->schema-element (lxml)
  (let ((attributes (lxml-get-attributes lxml)))
    (case (lxml-get-tag lxml)
      (xsd:|element| 
       (let* ((name (getf attributes :|name|))
              (type (getf attributes :|type|))
              (min-occurs (getf attributes :|minOccurs|))
              (max-occurs (getf attributes :|maxOccurs|))
              (nillable (getf attributes :|nillable|))
              (xml-schema-element (make-instance 'xml-schema-element 
                                                 :name name 
                                                 :type type
                                                 :min-occurs (if min-occurs (parse-integer min-occurs) 1)
                                                 :max-occurs (if max-occurs 
                                                                 (if (equal max-occurs "unbounded")
                                                                     :unbounded
                                                                   (parse-integer max-occurs))
                                                               1)
                                                 :nillable (equal nillable "true"))))
         (handle-lxml-schema-elements xml-schema-element lxml)
         xml-schema-element))
      (xsd:|simpleType|
       (let* ((name (getf attributes :|name|))
              (xsd-type (make-instance 'xsd-simple-type :name name)))
         (handle-lxml-schema-elements xsd-type lxml)
         xsd-type))
      (xsd:|complexType|
       (let* ((name (getf attributes :|name|))
              (xsd-type (make-instance 'xsd-complex-type :name name)))
         (handle-lxml-schema-elements xsd-type lxml)
         xsd-type))
      (xsd:|complexContent|
       (let ((xsd-complex-content (make-instance 'xsd-complex-content)))
         (handle-lxml-schema-elements xsd-complex-content lxml)
         xsd-complex-content))
      (xsd:|restriction|
       (let* ((base (getf attributes :|base|))
              (xsd-restriction (make-instance 'xsd-restriction :base base)))
         xsd-restriction))
      (xsd:|extension|
       (let* ((base (getf attributes :|base|))
              (xsd-extension (make-instance 'xsd-extension :base base)))
         (handle-lxml-schema-elements xsd-extension lxml)
         xsd-extension))
      (xsd:|sequence|
       (let ((xsd-sequence (make-instance 'xsd-sequence)))
         (handle-lxml-schema-elements xsd-sequence lxml)
         xsd-sequence)))))

(defun lxml->schema-definition (lxml)
  (if (eql (lxml-get-tag lxml) 'xsd:|schema|)
      (let* ((attributes (lxml-get-attributes lxml))
             (target-namespace (getf attributes :|targetNamespace|))
             (xml-schema-definition (make-instance 'xml-schema-definition
                                                   :target-namespace target-namespace)))
        (loop :for element :in (rest lxml) :do
              (push (lxml->schema-element element)
                    (get-elements xml-schema-definition)))
        xml-schema-definition)
    (error "Expected a XSD <schema> element")))

(defun parse-xsd (in)
  (let ((lxml (s-xml:parse-xml in)))
    (lxml->schema-definition lxml)))

(defun parse-xsd-file (pathname)
  (with-open-file (in pathname)
    (parse-xsd in)))

(defun parse-xsd-url (url)
  (multiple-value-bind (buffer code)
      (do-http-request url)
    (if (eql code 200)
        (with-input-from-string (in buffer)
          (parse-xsd in))
      (error "Could not retrieve URL ~s, got a ~s code" url code))))

;;; Interpreting the XSD model

(defmethod get-element-named ((xml-schema-definition xml-schema-definition) element-name)
  (find-item-named element-name (get-elements xml-schema-definition)))

(defmethod get-type-in-context ((xsd-simple-type xsd-simple-type) elements)
  "For now: return the base type of the restriction child of the simple-type, if any"
  (declare (ignore elements))
  (let ((first-child (first (get-children xsd-simple-type))))
    (when (and first-child
               (typep first-child 'xsd-restriction))
      (get-base first-child))))

(defmethod get-type-in-context ((xsd-complex-type xsd-complex-type) elements)
  "A complex type cannot be reduced further"
  (declare (ignore elements))
  xsd-complex-type)

(defmethod get-type-in-context ((xml-schema-element xml-schema-element) elements)
  "Resolve the type of element to the most primitive one, in the context of elements"
  (let ((type (get-type xml-schema-element)))
    (cond (type
           (if (xsd-primitive-type-name-p type)
               type
             (get-type-in-context (find-item-named type elements) elements)))
          (t
           (let ((first-child (first (get-children xml-schema-element))))
             (when first-child
               (get-type-in-context first-child elements)))))))

(defmethod get-element-type ((xml-schema-definition xml-schema-definition) element)
  "Resolve the type of element to the most primitive one, in the context of elements"
  (let ((element (if (stringp element) 
                     (find-item-named element (get-elements xml-schema-definition))
                   element)))
    (when element
      (get-type-in-context element (get-elements xml-schema-definition)))))

(defmethod get-members ((xsd-complex-type xsd-complex-type) (xml-schema-definition xml-schema-definition))
  "Return the list of members of xsd-complex-type, provided it is a sequence or a complex-content (for now)"
  (let ((first-child (first (get-children xsd-complex-type))))
    (cond ((and first-child (typep first-child 'xsd-sequence))
           (get-children first-child))
          ((and first-child (typep first-child 'xsd-complex-content))
           (get-members first-child xml-schema-definition)))))

(defmethod get-members ((xsd-complex-content xsd-complex-content) (xml-schema-definition xml-schema-definition))
  "Return the list of members of xsd-complex-content, provided it is a base type sequence extension (for now)"
  (let ((first-child (first (get-children xsd-complex-content))))
    (when (and first-child (typep first-child 'xsd-extension))
      (let* ((base-type-name (get-base first-child))
             (base-type-element (get-element-named xml-schema-definition base-type-name))
             (base-members (get-members base-type-element xml-schema-definition))
             (first-child (first (get-children first-child))))
        (if (and first-child (typep first-child 'xsd-sequence))
            (append base-members (get-children first-child))
          base-members)))))

(defmethod get-multiplicity ((xml-schema-element xml-schema-element))
  (with-slots (min-occurs max-occurs)
      xml-schema-element
    (cond ((and (zerop min-occurs) (eql max-occurs 1)) :optional)
          ((and (eql min-occurs 1) (eql max-occurs 1)) :required)
          ((and (eql min-occurs 1) (eql max-occurs :unbounded)) :one-or-more)
          ((and (zerop min-occurs) (eql max-occurs :unbounded)) :zero-or-more)
          (t :complex))))

(defmethod is-optional-p ((xml-schema-element xml-schema-element))
  (zerop (get-min-occurs xml-schema-element)))

(defmethod is-plural-p ((xml-schema-element xml-schema-element))
  (eql (get-max-occurs xml-schema-element) :unbounded))

;;; Template Generation (converting the XSD model to something simpler ;-)

;; an XSD element template looks like this: 
;; ELT = ( <multiplicity> "element-name" [ :primitive | ELT* ] )
;; where <multiplicity> is 1, ?, + or * and :primitive is a XSD primitive type keyword
;; all element types are resolved into primitives or sequences of sub elements
;; elements without contents are also possible

(defmethod get-xsd-template-multiplicity ((xsd-type xsd-type))
  :xsd-type)

(defmethod get-xsd-template-multiplicity ((xml-schema-element xml-schema-element))
  (with-slots (min-occurs max-occurs)
      xml-schema-element
    (cond ((and (zerop min-occurs) (eql max-occurs 1)) '?)
          ((and (eql min-occurs 1) (eql max-occurs 1)) (if (get-nillable xml-schema-element) '? 1))
          ((and (eql min-occurs 1) (eql max-occurs :unbounded)) (if (get-nillable xml-schema-element) '* '+))
          ((and (zerop min-occurs) (eql max-occurs :unbounded)) '*)
          (t :complex))))

(defun generate-xsd-template (xml-schema-element xml-schema-definition)
  (when (stringp xml-schema-element)
    (setf xml-schema-element (or (get-element-named xml-schema-definition xml-schema-element)
                                 (error "Cannot find element named ~s" xml-schema-element))))
  (let ((multiplicity (get-xsd-template-multiplicity xml-schema-element))
        (type (get-element-type xml-schema-definition xml-schema-element))
        (element-name (get-name xml-schema-element)))
    (unless (xsd-primitive-type-name-p type)
      ;; make sure simple types are resolved to their base primitive type
      (setf type (get-element-type xml-schema-definition type)))
    (if (xsd-primitive-type-name-p type)
        (let ((primitive-type-name (intern-xsd-type-name type)))
          `(,multiplicity ,element-name ,primitive-type-name))
      (let ((members (loop :for member :in (get-members type xml-schema-definition)
                           :collect (generate-xsd-template member xml-schema-definition))))
        `(,multiplicity ,element-name ,@members)))))

(defun generate-xsd-templates (xml-schema-definition)
  (loop :for element :in (get-elements xml-schema-definition)
        :when (typep element 'xml-schema-element)
        :collect (generate-xsd-template element xml-schema-definition)))

;;; Binding Templates (combining a template with an s-expr to generate an lxml list of tags)

(defun get-name-binding (name bindings)
  (let ((name-binding (member (actual-name name) bindings :test #'equal)))
    (if name-binding
        (values (second name-binding) t)
      (values nil nil))))

(defun bind-xsd-template-primitive (tag primitive-type value)
  (let ((primitive-value (lisp->xsd-primitive value primitive-type)))
    `(,tag ,primitive-value)))

(defun bind-xsd-template-members (tag members bindings schema namespace)
  (let ((xsi-type (get-name-binding 'xsi::|type| bindings))
        (bound-members '()))
    (cond (xsi-type
           (let ((type-template (generate-xsd-template xsi-type schema)))
             (if (eql (first type-template) :xsd-type)
                 (loop :for member :in (rest (rest type-template)) :do
                       (let ((member-binding (bind-xsd-template member bindings schema namespace)))
                         (when member-binding 
                           (push member-binding bound-members))))
               (error "Could not resolve explicit (sub)type ~s" xsi-type))
             `((,tag xsi::|type| ,xsi-type) ,@(reduce #'append (nreverse bound-members)))))
          (t
           (loop :for member :in members :do
                 (let ((member-binding (bind-xsd-template member bindings schema namespace)))
                   (when member-binding 
                     (push member-binding bound-members))))
           `(,tag ,@(reduce #'append (nreverse bound-members)))))))

(defun bind-xsd-template (template bindings schema namespace)
  (destructuring-bind (multiplicity element-name &rest contents)
      template
    (let ((tag (intern element-name (s-xml:get-package namespace))))
      (multiple-value-bind (value boundp)
          (get-name-binding element-name bindings)
        (cond ((null contents) `(,tag))
              ((symbolp (first contents))
               (let ((primitive-type (first contents)))
                 (case multiplicity
                   ((1 ?) (if boundp
                              `(,(bind-xsd-template-primitive tag primitive-type value))
                            (when (eql multiplicity 1)
                              (error "Required element ~s not bound" element-name))))
                   ((+ *) (if (and boundp value)
                              (loop :for elt-value :in value
                                    :collect (bind-xsd-template-primitive tag primitive-type elt-value)) 
                            (when (eql multiplicity +)
                              (error "Required repeating element ~s not bound correctly" element-name)))))))
              (t
               (case multiplicity
                 ((1 ?) (if boundp
                            `(,(bind-xsd-template-members tag contents value schema namespace))
                          (when (eql multiplicity 1)
                            (error "Required element ~s not bound" element-name))))
                 ((+ *) (if (and boundp value)
                            (loop :for elt-value :in value
                                  :collect (bind-xsd-template-members tag contents elt-value schema namespace))
                          (when (eql multiplicity +)
                            (error "Required repeating element ~s not bound correctly" element-name)))))))))))
                
(defun bind-element (element bindings xml-schema-definition namespace)
  (let ((template (generate-xsd-template element xml-schema-definition)))
    (reduce #'append (bind-xsd-template template bindings xml-schema-definition namespace))))
                  
;;; Resolving Templates (combining a template with an lxml list to generate an s-expr)

(defun resolve-xsd-template-primitive (element-name primitive-type string)
  (let ((value (xsd-primitive->lisp string primitive-type)))
    `(,element-name ,value)))

(defun resolve-xsd-template-members (members lxml namespace)
  (let ((resolved-members '()))
    (loop :for member :in members :do
          (let ((member-binding (resolve-xsd-template member lxml namespace)))
            (when member-binding 
              (push member-binding resolved-members))))
    (reduce #'append (nreverse resolved-members))))

(defun resolve-xsd-template (template lxml namespace)
  (destructuring-bind (multiplicity element-name &rest contents)
      template
    (let* ((tag (intern element-name (s-xml:get-package namespace)))
           (children (lxml-find-tags tag lxml)))
      (cond ((null contents) `(,element-name))
            ((symbolp (first contents))
             (let ((primitive-type (first contents)))
               (case multiplicity
                 ((1 ?) (if children
                            (resolve-xsd-template-primitive element-name primitive-type 
                                                            (lxml-get-contents (first children)))
                          (when (eql multiplicity 1)
                            (error "Required element ~s not bound" element-name))))
                 ((+ *) (if children
                            (loop :for child :in children
                                  :collect (resolve-xsd-template-primitive element-name primitive-type 
                                                                           (lxml-get-contents child)))
                          (when (eql multiplicity +)
                            (error "Required repeating element ~s not bound correctly" element-name)))))))
            (t
             (case multiplicity
               ((1 ?) (if children
                          `(,element-name ,(resolve-xsd-template-members contents (first children) namespace))
                        (when (eql multiplicity 1)
                          (error "Required element ~s not bound" element-name))))
               ((+ *) (if children
                          `(,element-name
                            ,(loop :for child :in children
                                   :collect (resolve-xsd-template-members contents child namespace)))
                        (when (eql multiplicity +)
                          (error "Required repeating element ~s not bound correctly" element-name))))))))))

(defun resolve-element (element lxml xml-schema-definition namespace)
  (let ((template (generate-xsd-template element xml-schema-definition)))
    (resolve-xsd-template template lxml namespace)))

;;; Describing XSD (print the 'sexpr' format with multiplicity indicators using in input/output binding)

(defun indent (n &optional (stream *standard-output*))
  (format stream "~&")
  (loop :repeat n 
        :do (write-char #\space stream) (write-char #\space stream)))

(defun describe-xsd-template-members (members &optional (stream *standard-output*) (level 0))
  (loop :for member :in members :do
        (describe-xsd-template member stream (1+ level))))

(defun describe-xsd-template (template &optional (stream *standard-output*) (level 0))
  (destructuring-bind (multiplicity element-name &rest contents)
      template
    (cond ((null contents) 
           (indent level)
           (format stream "(~s) ~a" element-name multiplicity))
          ((symbolp (first contents))
           (let ((primitive-type (first contents)))
             (case multiplicity
               ((1 ?) 
                (indent level)
                (format stream "(~s ~s) ~a " element-name primitive-type multiplicity))
               ((+ *) 
                (indent level)
                (format stream "(~s (~s) ~a )" element-name primitive-type multiplicity)))))
          (t
           (case multiplicity
             ((1 ?) 
              (indent level)
              (format stream "(~a" element-name)
              (describe-xsd-template-members contents stream level)
              (format stream ") ~a " multiplicity))
             ((+ *) 
              (indent level)
              (format stream "(~a (" element-name)
              (describe-xsd-template-members contents stream level)
              (format stream ") ~a )" multiplicity)))))))

(defun describe-xsd-element (element xml-schema-definition &optional (stream *standard-output*) (level 0))
  (let ((template (generate-xsd-template element xml-schema-definition)))
    (describe-xsd-template template stream level))
  (format stream "~&")
  (values))

(defun describe-xsd (xml-schema-definition &optional (stream *standard-output*))
  "Print a high-level description of the top-level elements in xml-schema-definition"
  (format stream "XML Schema Definition with target-namespace URI ~s~%" 
          (get-target-namespace xml-schema-definition))
  (loop :for element :in (get-elements xml-schema-definition) :do
        (when (typep element 'xml-schema-element)
          (describe-xsd-element element xml-schema-definition stream 1)))
  (format stream "~&")
  (values))

;;; Primitive Types/Values (types are identified :keywords)

(defconstant +known-primitive-type-names+
  '("string" 
    "normalizedString" "token"
    "Name" "QName" "NCName" "anyURI"
    "integer"
    "positiveInteger" "negativeInteger" "nonPositiveInteger" "nonNegativeInteger"
    "long" "unsignedLong" "int" "unsignedInt" "short" "unsignedShort"
    "byte" "decimal"
    "float" "double"
    "boolean"
    "duration" "date" "time" "dateTime"
    "base64Binary" "hexBinary"))

(defun xsd-primitive-type-name-p (name)
  (and (stringp name)
       (member (actual-name name) +known-primitive-type-names+ :test #'string-equal)))

(defun intern-xsd-type-name (name)
  (intern (string-upcase (actual-name name)) :keyword))

;;; Date, Time and DateTime conversions

(defvar *xsd-timezone* nil)

(defun ut (&optional year month date (hours 0) (minutes 0) (seconds 0))
  "Convenience function to create Common Lisp universal times"
  (when (or (null year) (null month) (null date))
    (multiple-value-bind (second minute hour current-date current-month current-year)
        (if *xsd-timezone*
            (decode-universal-time (get-universal-time) *xsd-timezone*)
          (decode-universal-time (get-universal-time)))
      (declare (ignore second minute hour))
      (unless year (setf year current-year))
      (unless month (setf month current-month))
      (unless date (setf date current-date))))
  (if *xsd-timezone*
      (encode-universal-time seconds minutes hours date month year *xsd-timezone*)
    (encode-universal-time seconds minutes hours date month year)))

(defun lisp->xsd-datetime (universal-time)
  "1999-05-31T13:20:00.000-05:00"
  (multiple-value-bind (second minute hour date month year day daylight-p timezone)
      (if *xsd-timezone*
          (decode-universal-time universal-time *xsd-timezone*)
        (decode-universal-time universal-time))
    (declare (ignore day daylight-p))
    (let ((sign (if (minusp timezone) #\- #\+))
          (timezone-hour (floor (* (abs timezone) 60) 60))
          (timezone-minute (rem (* (abs timezone) 60) 60)))
      (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d.000~c~2,'0d:~2,'0d" 
              year month date hour minute second sign timezone-hour timezone-minute))))

(defun xsd-datetime->lisp (string)
  "1999-05-31T13:20:00.000-05:00"
  (let* ((contains-millis (position #\. string))
         (contains-timezone (or (position #\: string :start 18) (position #\Z string)))
         (year (parse-integer string :start 0 :end 4))
         (month (parse-integer string :start 5 :end 7))
         (date (parse-integer string :start 8 :end 10))
         (hour (parse-integer string :start 11 :end 13))
         (minute (parse-integer string :start 14 :end 16))
         (second (parse-integer string :start 17 :end 19))
         timezone-sign
         timezone-hour
         timezone-minute)
    (when contains-timezone
      (if (position #\Z string)
          (setf timezone-sign 1 
                timezone-hour 0
                timezone-minute 0)
        (if contains-millis
            (setf timezone-sign (ecase (char string 23) (#\- -1) (#\+ +1))
                  timezone-hour (parse-integer string :start 24 :end 26)
                  timezone-minute (parse-integer string :start 27 :end 29))
          (setf timezone-sign (ecase (char string 19) (#\- -1) (#\+ +1))
                timezone-hour (parse-integer string :start 20 :end 22)
                timezone-minute (parse-integer string :start 23 :end 25)))))
    (if (or *xsd-timezone* contains-timezone)
        (encode-universal-time second minute hour date month year 
                               (if contains-timezone 
                                   (* timezone-sign (+ timezone-hour (/ timezone-minute 60)))
                                 *xsd-timezone*))
      (encode-universal-time second minute hour date month year))))

(defun lisp->xsd-date (universal-time)
  "1999-05-31"
  (multiple-value-bind (second minute hour date month year)
      (if *xsd-timezone*
          (decode-universal-time universal-time *xsd-timezone*)
        (decode-universal-time universal-time))
    (declare (ignore second minute hour))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month date)))

(defun xsd-date->lisp (string)
  "1999-05-31"
  (let ((year (parse-integer string :start 0 :end 4))
        (month (parse-integer string :start 5 :end 7))
        (date (parse-integer string :start 8 :end 10)))
    (if *xsd-timezone*
        (encode-universal-time 0 0 0 date month year *xsd-timezone*)
      (encode-universal-time 0 0 0 date month year))))

(defun lisp->xsd-time (universal-time)
  "13:20:00.000-05:00"
  (multiple-value-bind (second minute hour date month year day daylight-p timezone)
      (if *xsd-timezone*
          (decode-universal-time universal-time *xsd-timezone*)
        (decode-universal-time universal-time))
    (declare (ignore year month date day daylight-p))
    (let ((sign (if (minusp timezone) #\- #\+))
          (timezone-hour (floor (* (abs timezone) 60) 60))
          (timezone-minute (rem (* (abs timezone) 60) 60)))
      (format nil "~2,'0d:~2,'0d:~2,'0d.000~c~2,'0d:~2,'0d" 
              hour minute second sign timezone-hour timezone-minute))))

(defun xsd-time->lisp (string)
  "13:20:00.000-05:00"
  (let* ((contains-millis (position #\. string))
         (contains-timezone (position #\: string :start 7))
         (hour (parse-integer string :start 0 :end 2))
         (minute (parse-integer string :start 3 :end 5))
         (second (parse-integer string :start 6 :end 8))
         timezone-sign
         timezone-hour
         timezone-minute)
    (when contains-timezone
      (if contains-millis
          (setf timezone-sign (ecase (char string 12) (#\- -1) (#\+ +1))
                timezone-hour (parse-integer string :start 13 :end 15)
                timezone-minute (parse-integer string :start 16 :end 18))
        (setf timezone-sign (ecase (char string 8) (#\- -1) (#\+ +1))
              timezone-hour (parse-integer string :start 9 :end 11)
              timezone-minute (parse-integer string :start 12 :end 14))))
    (if (or *xsd-timezone* contains-timezone)
        (encode-universal-time second minute hour 1 1 0
                               (if contains-timezone 
                                   (* timezone-sign (+ timezone-hour (/ timezone-minute 60)))
                                 *xsd-timezone*))
      (encode-universal-time second minute hour 1 1 0))))

;;; Primitive Types/Values Conversions

(defun xsd-primitive->lisp (value type)
  "Convert the XSD string value to a Common Lisp value, interpreting it as type"
  (ecase type
    ((:string :normalizedString :token) 
     (if (null value) "" value))
    ((:Name :QName :NCName :anyURI) 
     (if (null value) "" value))
    ((:integer 
      :positiveInteger :negativeInteger :nonPositiveInteger :nonNegativeInteger
      :long :unsignedLong :int :unsignedInt :short :unsignedShort 
      :byte :decimal) 
     (parse-integer value))
    (:float
     (coerce (read-from-string value) 'float))
    (:double
     (coerce (read-from-string value) 'double))
    (:boolean
     (cond ((string-equal value "true") t)
           ((string-equal value "false") nil)
           (t (= (parse-integer value) 1))))
    (:duration value)
    (:date (xsd-date->lisp value))
    (:time (xsd-time->lisp value))
    (:dateTime (xsd-datetime->lisp value))
    ((:base64Binary :hexBinary) 
     (error "~a not yet supported as primitive type" type))))

(defun lisp->xsd-primitive (value type)
  "Convert the Common Lisp value to a XSD string value, interpreting it as type" 
  (ecase type
    ((:string :normalizedString :token) 
     value)
    ((:Name :QName :NCName :anyURI) 
     value)
    ((:integer 
      :positiveInteger :negativeInteger :nonPositiveInteger :nonNegativeInteger
      :long :unsignedLong :int :unsignedInt :short :unsignedShort 
      :byte :decimal) 
     (princ-to-string value))
    (:float
     (princ-to-string value))
    (:double
     (princ-to-string value))
    (:boolean 
     (if value "true" "false"))
    (:duration value)
    (:date (lisp->xsd-date value))
    (:time (lisp->xsd-time value))
    (:dateTime (lisp->xsd-datetime value))
    ((:base64Binary :hexBinary) 
     (error "~a not yet supported as primitive type" type))))

;;;; eof
