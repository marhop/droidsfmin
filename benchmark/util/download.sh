#!/usr/bin/env bash

request='<?xml version="1.0" encoding="utf-8"?>
<soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
xmlns:xsd="http://www.w3.org/2001/XMLSchema"
xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
<soap:Body>
    <getSignatureFileV1 xmlns="http://pronom.nationalarchives.gov.uk"/>
</soap:Body>
</soap:Envelope>'

curl -s \
    -H 'Content-Type: text/xml;charset=UTF-8' \
    -H 'SOAPAction: "http://pronom.nationalarchives.gov.uk:getSignatureFileV1In"' \
    -d "$request" \
    http://www.nationalarchives.gov.uk/pronom/service.asmx \
    | xmlstarlet sel \
    -N 'n=http://www.nationalarchives.gov.uk/pronom/SignatureFile' \
    -t -c '//n:FFSignatureFile' \
    | xmlstarlet fo

