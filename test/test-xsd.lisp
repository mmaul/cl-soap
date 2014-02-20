;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: test-xsd.lisp,v 1.4 2005-10-06 11:09:41 scaekenberghe Exp $
;;;;
;;;; Some (internal) test on the implementatin of the XML Schema Definition
;;;;
;;;; Copyright (C) 2005 Sven Van Caekenberghe, Beta Nine BVBA. All Rights Reserved.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;

(in-package :cl-soap)

(let ((schema (get-xml-schema-definition 
               (wsdl-cache-get "https://adwords.google.com:443/api/adwords/v2/CampaignService?wsdl")))
      (lxml '((GOOGLE::|getAllAdWordsCampaignsResponse| :|xmlns| "https://adwords.google.com/api/adwords/v2")
              (GOOGLE::|getAllAdWordsCampaignsReturn|
               (GOOGLE::|dailyBudget| "200000000")
               (GOOGLE::|id| "3871365")
               (GOOGLE::|name| "Campaign #2")
               (GOOGLE::|status| "Ended")
               (GOOGLE::|startDate| "2004-12-20T04:20:46.000Z")
               (GOOGLE::|endDate| "2005-03-29T07:59:59.000Z")
               (GOOGLE::|optInSearchNetwork| "true")
               (GOOGLE::|optInContentNetwork| "false")
               (GOOGLE::|languageTargeting| (GOOGLE::|languages| "en"))
               (GOOGLE::|geoTargeting| (GOOGLE::|countries| "AU")))
              (GOOGLE::|getAllAdWordsCampaignsReturn|
               (GOOGLE::|dailyBudget| "60000000")
               (GOOGLE::|id| "4462365")
               (GOOGLE::|name| "Campaign #3")
               (GOOGLE::|status| "Deleted")
               (GOOGLE::|startDate| "2005-04-02T23:48:13.000Z")
               (GOOGLE::|endDate| "2005-04-02T23:48:42.000Z")
               (GOOGLE::|optInSearchNetwork| "true")
               (GOOGLE::|optInContentNetwork| "true")
               (GOOGLE::|languageTargeting| (GOOGLE::|languages| "en"))
               (GOOGLE::|geoTargeting| (GOOGLE::|countries| "AU"))))))              
  (pprint (resolve-element "getAllAdWordsCampaignsResponse" 
                           lxml 
                           schema
                           *google-adwords-ns*)))

(let ((schema (get-xml-schema-definition 
               (wsdl-cache-get "https://adwords.google.com:443/api/adwords/v2/AdGroupService?wsdl")))
      (lxml '((GOOGLE::|getAllAdGroupsResponse| :|xmlns| "https://adwords.google.com/api/adwords/v2")
              (GOOGLE::|getAllAdGroupsReturn|
               (GOOGLE::|maxCpc| "8000000")
               (GOOGLE::|name| "Diversified Fund")
               (GOOGLE::|id| "200451315")
               (GOOGLE::|campaignId| "3871365")
               (GOOGLE::|status| "Deleted"))
              (GOOGLE::|getAllAdGroupsReturn|
               (GOOGLE::|maxCpc| "0")
               GOOGLE::|name|
               (GOOGLE::|id| "200451345")
               (GOOGLE::|campaignId| "3871365")
               (GOOGLE::|status| "Deleted"))
              (GOOGLE::|getAllAdGroupsReturn|
               (GOOGLE::|maxCpc| "150000")
               (GOOGLE::|name| "ETP")
               (GOOGLE::|id| "200451435")
               (GOOGLE::|campaignId| "3871365")
               (GOOGLE::|status| "Deleted"))
              (GOOGLE::|getAllAdGroupsReturn|
               (GOOGLE::|maxCpc| "5440000")
               (GOOGLE::|name| "Financial Plan")
               (GOOGLE::|id| "200451975")
               (GOOGLE::|campaignId| "3871365")
               (GOOGLE::|status| "Deleted"))
              (GOOGLE::|getAllAdGroupsReturn|
               (GOOGLE::|maxCpc| "65790000")
               (GOOGLE::|name| "Financial Planning Seminars")
               (GOOGLE::|id| "200452005")
               (GOOGLE::|campaignId| "3871365")
               (GOOGLE::|status| "Deleted"))
              (GOOGLE::|getAllAdGroupsReturn|
               (GOOGLE::|maxCpc| "90000")
               (GOOGLE::|name| "Investing Super")
               (GOOGLE::|id| "200452035")
               (GOOGLE::|campaignId| "3871365")
               (GOOGLE::|status| "Deleted"))
              (GOOGLE::|getAllAdGroupsReturn|
               (GOOGLE::|maxCpc| "43730000")
               (GOOGLE::|name| "1.Independent Financial Advisor (new)")
               (GOOGLE::|id| "202804125")
               (GOOGLE::|campaignId| "3871365")
               (GOOGLE::|status| "Enabled")))))
  (pprint (resolve-element "getAllAdGroupsResponse"
                           lxml
                           schema
                           *google-adwords-ns*)))

(let ((schema (get-xml-schema-definition 
               (wsdl-cache-get "https://adwords.google.com:443/api/adwords/v2/CampaignService?wsdl")))
      (lxml '((GOOGLE:|getAllAdWordsCampaignsResponse| :|xmlns| "https://adwords.google.com/api/adwords/v2")
              (GOOGLE:|getAllAdWordsCampaignsReturn|
               (GOOGLE:|dailyBudget| "1000000")
               (GOOGLE:|id| "5631435")
               (GOOGLE:|name| "Campaign #1")
               (GOOGLE:|status| "Active")
               (GOOGLE:|startDate| "2005-09-16T11:11:14.000Z")
               (GOOGLE:|endDate| "2011-01-01T07:59:59.000Z")
               (GOOGLE:|optInSearchNetwork| "true")
               (GOOGLE:|optInContentNetwork| "true")
               (GOOGLE:|languageTargeting| (GOOGLE:|languages| "en") (GOOGLE:|languages| "nl"))
               (GOOGLE:|geoTargeting| (GOOGLE:|countries| "BE") (GOOGLE:|countries| "NL"))))))
  (pprint (resolve-element "getAllAdWordsCampaignsResponse"
                           lxml
                           schema
                           *google-adwords-ns*)))

(let ((schema (get-xml-schema-definition 
               (wsdl-cache-get"https://adwords.google.com:443/api/adwords/v2/TrafficEstimatorService?wsdl")))
      (binding `("estimateKeywordList"
                 ("keywordRequests"
                  ,(mapcar #'(lambda (keyword)
                               (destructuring-bind (text type max-cpc)
                                   keyword
                                 `("text" ,text "type" ,type "maxCpc" ,max-cpc)))
                           '(("flowers" "Broad" 50000) ("tree" "Broad" 100000)))))))
  (pprint (bind-element "estimateKeywordList"
                        binding
                        schema
                        *google-adwords-ns*)))

(defun test-1 ()
  (let* ((schema (get-xml-schema-definition 
                  (wsdl-cache-get "https://adwords.google.com:443/api/adwords/v2/TrafficEstimatorService?wsdl")))
         (template (generate-xsd-template "estimateKeywordList" schema))
         (binding `("estimateKeywordList"
                    ("keywordRequests"
                     ,(mapcar #'(lambda (keyword)
                                  (destructuring-bind (text type max-cpc)
                                      keyword
                                    `("text" ,text "type" ,type "maxCpc" ,max-cpc)))
                              '(("flowers" "Broad" 50000) ("tree" "Broad" 100000)))))))
    (bind-xsd-template template
                       binding
                       schema
                       *google-adwords-ns*)))

(defun test-2 ()
  (let* ((schema (get-xml-schema-definition 
                  (wsdl-cache-get "https://adwords.google.com:443/api/adwords/v2/CampaignService?wsdl")))
         (template (generate-xsd-template "getAllAdWordsCampaignsResponse" schema))
         (lxml '((GOOGLE::|getAllAdWordsCampaignsResponse| :|xmlns| "https://adwords.google.com/api/adwords/v2")
                 (GOOGLE::|getAllAdWordsCampaignsReturn|
                  (GOOGLE::|dailyBudget| "1000000")
                  (GOOGLE::|id| "5631435")
                  (GOOGLE::|name| "Campaign #1")
                  (GOOGLE::|status| "Active")
                  (GOOGLE::|startDate| "2005-09-16T11:11:14.000Z")
                  (GOOGLE::|endDate| "2011-01-01T07:59:59.000Z")
                  (GOOGLE::|optInSearchNetwork| "true")
                  (GOOGLE::|optInContentNetwork| "true")
                  (GOOGLE::|languageTargeting| (GOOGLE::|languages| "en") (GOOGLE::|languages| "nl"))
                  (GOOGLE::|geoTargeting| (GOOGLE::|countries| "BE") (GOOGLE::|countries| "NL"))))))
    (resolve-xsd-template template
                          (list lxml)
                          *google-adwords-ns*)))

(defun test-3 ()
  (let* ((schema (get-xml-schema-definition 
                  (wsdl-cache-get "https://adwords.google.com:443/api/adwords/v2/ReportService?wsdl")))
         (template (generate-xsd-template "scheduleReportJob" schema))
         (binding `("scheduleReportJob"
                    ("job"
                     (xsi:|type| "UrlReportJob"
                      "name" "test123"
                      "startDate" ,(ut)
                      "endDate" ,(ut)
                      "clientAccounts" (100 200)
                      "adWordsType" "SearchOnly"
                      "campaigns" (101 201))))))
    (bind-xsd-template template
                       binding
                       schema
                       *google-adwords-ns*)))

;;;; eof
