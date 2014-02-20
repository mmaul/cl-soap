;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: lxml.lisp,v 1.10 2005-10-06 11:07:29 scaekenberghe Exp $
;;;;
;;;; Common utilities (mostly lxml) and some internal/experimental stuff
;;;;
;;;; Copyright (C) 2005 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :cl-soap)

;;; external

(defun lxml-get-tag (lxml)
  "Return the XML tag symbol of the lxml XML DOM"
  (cond ((symbolp lxml) lxml)
        ((stringp lxml) '())
        ((symbolp (first lxml)) (first lxml))
        (t (first (first lxml)))))

(defun lxml-get-attributes (lxml)
  "Return the XML attributes plist of the lxml XML DOM" 
  (cond ((or (symbolp lxml) 
             (stringp lxml) 
             (symbolp (first lxml))) '())
        (t (rest (first lxml)))))

(defun lxml-get-children (lxml)
  "Return the XML children list of the lxml XML DOM"
  (cond ((or (symbolp lxml) 
             (stringp lxml)) '())
        (t (rest lxml))))

(defun lxml-get-contents (lxml)
  "Return the contents (first child) of the lxml XML DOM"
  (first (lxml-get-children lxml)))

(defun lxml-find-tag (tag lxml)
  "Find a specific tag in a lxml XML DOM list"
  (find tag lxml :key #'lxml-get-tag))

(defun lxml-find-tags (tag lxml)
  "Find all elements of a specific tag in a lxml XML DOM list"
  (remove-if-not #'(lambda (x) (eql (lxml-get-tag x) tag)) lxml))

;;; internal shared/common code

(defun actual-name (qname)
  "For now we ignore prefixes ;-) - symbols are untouched"
  (if (stringp qname)
      (multiple-value-bind (prefix identifier)
          (s-xml:split-identifier qname)
        (declare (ignore prefix))
        identifier)
    qname))

(defun find-item-named (item-name sequence)
  (find (actual-name item-name) sequence :test #'string-equal :key #'get-name))

;;; manipulating sexpr (structured/nested plists with string keys)

(defun sexpr-getf (sexpr key &optional default)
  "Find the value of key in sexpr (returning default if not found)"
  (cond ((null sexpr) default)
        ((consp sexpr) (let ((current-key (first sexpr)))
                         (if (stringp current-key)
                             (if (string-equal current-key key)
                                 (second sexpr)
                               (sexpr-getf (rest (rest sexpr)) key default))
                           (error "Illegal key in sexpr: ~s" current-key))))
        (t (error "Not an sexpr: ~s" sexpr))))

(defun (setf sexpr-getf) (value sexpr key)
  "Destructively modify the value of key in sexpr to value (add at tail if not found)"
  (cond ((null sexpr) (error "Cannot destructively add to the empty list"))
        ((consp sexpr) (let ((current-key (first sexpr)))
                         (if (stringp current-key)
                             (if (string-equal current-key key)
                                 (setf (second sexpr) value)
                               (if (null (rest (rest sexpr)))
                                   (setf (rest (rest sexpr)) (list key value))
                                 (setf (sexpr-getf (rest (rest sexpr)) key) value)))
                           (error "Illegal key in sexpr: ~s" current-key))
                         sexpr))
        (t (error "Not an sexpr: ~s" sexpr))))

(defun sexpr-select (sexpr keys)
  "Return a new sexpr with keys and their values retained"
  (cond ((null sexpr) '())
        ((consp sexpr) (let ((current-key (first sexpr)))
                         (if (stringp current-key)
                             (if (member current-key keys :test #'string-equal)
                                 `(,current-key ,(second sexpr) ,@(sexpr-select (rest (rest sexpr)) keys))
                               (sexpr-select (rest (rest sexpr)) keys))
                           (error "Illegal key in sexpr: ~s" current-key))))
        (t (error "Not an sexpr: ~s" sexpr))))

(defun sexpr-remove (sexpr keys)
  "Return a new sexpr with keys and their values not retained"
  (cond ((null sexpr) '())
        ((consp sexpr) (let ((current-key (first sexpr)))
                         (if (stringp current-key)
                             (if (member current-key keys :test #'string-equal)
                                 (sexpr-remove (rest (rest sexpr)) keys)
                               `(,current-key ,(second sexpr) ,@(sexpr-remove (rest (rest sexpr)) keys)))
                           (error "Illegal key in sexpr: ~s" current-key))))
        (t (error "Not an sexpr: ~s" sexpr))))

;;;; eof
