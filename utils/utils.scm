;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2017, 2019, 2020 Inria

(define-module (utils utils)
  #:use-module (guix)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system r)
  #:use-module (gnu packages)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages xml)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  ;; To remove when/if python2 packages sympy and mpi4py
  ;; are fixed in official repo
  #:use-module (guix build-system python)
  )

(define-public r-rlist
(package
 (name "r-rlist")
 (version "0.4.6.1")
 (source
  (origin
   (method url-fetch)
   (uri (cran-uri "rlist" version))
   (sha256
    (base32
     "08awy2p7rykc272wvvya4ddszbr7b7s7qv4wr3hs8ylr4jqlh0dv"))))
 (properties `((upstream-name . "rlist")))
 (build-system r-build-system)
 (propagated-inputs
  `(("r-data-table" ,r-data-table)
    ("r-jsonlite" ,r-jsonlite)
    ("r-xml" ,r-xml)
    ("r-yaml" ,r-yaml)))
 (home-page "https://renkun.me/rlist")
 (synopsis
  "A Toolbox for Non-Tabular Data Manipulation")
 (description
  "This package provides a set of functions for data manipulation with list objects, including mapping, filtering, grouping, sorting, updating, searching, and other useful functions.  Most functions are designed to be pipeline friendly so that data processing with lists can be chained.")
 (license expat)))
