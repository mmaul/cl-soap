;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: test-google-adwords.lisp,v 1.14 2005-10-06 11:09:40 scaekenberghe Exp $
;;;;
;;;; Some tests on the Google AdWords API (not publically available)
;;;;
;;;; Copyright (C) 2005 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :cl-soap)

;;; XML namespaces setup

(defconstant +google-adwords-ns-uri+ "https://adwords.google.com/api/adwords/v2")

(defpackage :google
  (:nicknames "google")
  (:documentation "Package for symbols in the Google Adwords API XML Namespace"))

(defparameter *google-adwords-ns* (s-xml:register-namespace +google-adwords-ns-uri+ "google" :google))

;;; ***************************************************************************************
;;; apparently there are different XML Schema Defintion namespace URIs, Google is using 
(s-xml:register-namespace "http://www.w3.org/2001/XMLSchema" "xsd" :xsd)
;;; ***************************************************************************************

;;; basic WSDL parsing

(defparameter *google-adwords-api-wsdl-urls*
  (loop :for service :in '("CreativeService"
                           "KeywordService"
                           "AdGroupService"
                           "CampaignService"
                           "TrafficEstimatorService"
                           "ReportService"
                           "InfoService"
                           "AccountService")
        :collect (format nil "https://adwords.google.com/api/adwords/v2/~a?wsdl" service)))

(defun parse-all-wsdl ()
  (mapcar #'parse-wsdl-url *google-adwords-api-wsdl-urls*))

;;; account parameters

(defvar *google-adwords-email*)
(defvar *google-adwords-password*)
(defvar *google-adwords-user-agent*)
(defvar *google-adwords-token*)
(defvar *google-client-email*)

(defun make-google-headers (&key 
                            (email *google-adwords-email*)
                            (password *google-adwords-password*)
                            (user-agent *google-adwords-user-agent*)
                            (token *google-adwords-token*)
                            (client-email *google-client-email*))
  `("email" ,email 
    "password" ,password 
    "useragent" ,user-agent
    "token" ,token
    "clientEmail" ,client-email))

;;; some test calls

(defun get-usage-quota-this-month ()
  (wsdl-soap-call (wsdl-cache-get "https://adwords.google.com:443/api/adwords/v2/InfoService?wsdl")
                  "getUsageQuotaThisMonth"
                  :headers (make-google-headers)))

(defun get-method-cost (service method &optional (date (ut)))
  (wsdl-soap-call (wsdl-cache-get "https://adwords.google.com:443/api/adwords/v2/InfoService?wsdl")
                  "getMethodCost"
                  :input `("getMethodCost" ("service" ,service "method" ,method "date" ,date))
                  :headers (make-google-headers)))

#+nil (get-method-cost "InfoService" "getMethodCost")

(defun get-unit-count (&optional (start-date (ut)) (end-date start-date))
  (wsdl-soap-call (wsdl-cache-get "https://adwords.google.com:443/api/adwords/v2/InfoService?wsdl")
                  "getUnitCount"
                  :input `("getUnitCount" ("startDate" ,start-date "endDate" ,end-date))
                  :headers (make-google-headers)))

(defun get-all-adwords-campaigns (&optional (client-email *google-client-email*))
  (wsdl-soap-call (wsdl-cache-get "https://adwords.google.com:443/api/adwords/v2/CampaignService?wsdl")
                  "getAllAdWordsCampaigns"
                  :input '("getAllAdWordsCampaigns" ("dummy" 1))
                  :headers (make-google-headers :client-email client-email)))

(defun estimate-keyword-list (keywords)
  "((<text> <type> <max-cpc>)*) where type is Broad|Phrase|Exact"
  (wsdl-soap-call (wsdl-cache-get "https://adwords.google.com:443/api/adwords/v2/TrafficEstimatorService?wsdl")
                  "estimateKeywordList"
                  :input `("estimateKeywordList"
                           ("keywordRequests"
                            ,(mapcar #'(lambda (keyword)
                                         (destructuring-bind (text type max-cpc)
                                             keyword
                                           `("text" ,text "type" ,type "maxCpc" ,max-cpc)))
                                     keywords)))
                  :headers (make-google-headers)))

#+nil (estimate-keyword-list '(("flowers" "Broad" 50000) ("trees" "Broad" 50000)))
 
(defun get-all-adgroups (campaign-id)
  (wsdl-soap-call (wsdl-cache-get "https://adwords.google.com:443/api/adwords/v2/AdGroupService?wsdl")
                  "getAllAdGroups"
                  :input `("getAllAdGroups" ("campaignID" ,campaign-id))
                  :headers (make-google-headers)))

(defun add-campaign (client-email daily-budget status &key name language-targeting geo-targeting)
  ;; to be completed
  (wsdl-soap-call (wsdl-cache-get "https://adwords.google.com:443/api/adwords/v2/CampaignService?wsdl")
                  "addCampaign"
                  ;; input placeholder !!
                  :input `(,daily-budget ,status name ,name ,language-targeting ,geo-targeting)
                  :headers (make-google-headers :client-email client-email)))

(defun schedule-report-job (client-email report-job)
  ;; to be completed
  (wsdl-soap-call (wsdl-cache-get "https://adwords.google.com:443/api/adwords/v2/ReportService?")
                  "scheduleReportJob"
                  ;; input placeholder !!
                  :input `(,report-job)
                  :headers (make-google-headers :client-email client-email)))

(defun make-ad-group (&key max-cpc max-cpm name (status "Enabled") (id -1) (campaign-id -1))
  (let (fields)
    (when max-cpc
      (push `("maxCpc" ,max-cpc) fields))
    (when max-cpm
      (push `("maxCpm" ,max-cpm) fields))
    (push `("name" ,name) fields)
    (push `("status" ,status) fields)
    (push `("id" ,id) fields)
    (push `("campaignId" ,campaign-id) fields)
    (apply #'append fields)))

(defun add-ad-group (campaign-id new-data)
  (wsdl-soap-call (wsdl-cache-get "https://adwords.google.com:443/api/adwords/v2/AdGroupService?wsdl")
                  "addAdGroup"
                  :input
                  `("addAdGroup" ("campaignID" ,campaign-id "newData" ,new-data))
                  :headers
                  (make-google-headers)))

(defun update-ad-group (changed-data)
  (wsdl-soap-call (wsdl-cache-get "https://adwords.google.com:443/api/adwords/v2/AdGroupService?wsdl")
                  "updateAdGroup"
                  :input
                  `("updateAdGroup" ("changedData" ,changed-data))
                  :headers
                  (make-google-headers)))

(defun get-ad-group (id)
  (wsdl-soap-call (wsdl-cache-get "https://adwords.google.com:443/api/adwords/v2/AdGroupService?wsdl")
                  "getAdGroup"
                  :input
                  `("getAdGroup" ("adGroupId" ,id))
                  :headers
                  (make-google-headers)))

;;;; eof
