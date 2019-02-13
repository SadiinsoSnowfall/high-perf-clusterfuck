;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2017, 2018 Inria

(define-module (inria storm-pm2)
  #:use-module (guix)
  #:use-module (guix build-system gnu)
  #:use-module (guix svn-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages linux)
  #:use-module (inria eztrace)
  #:use-module (inria simgrid)
  #:use-module (ice-9 match))

(define %pm2-home-page "http://pm2.gforge.inria.fr/")
(define %pm2-revision 27924)
(define %padicotm-revision 5179)
(define %pm2-svn "https://scm.gforge.inria.fr/anonscm/svn/pm2/trunk")
(define %padicotm-svn "https://scm.gforge.inria.fr/anonscm/svn/padico/PadicoTM/trunk")
(define %patch-path "inria/patches/autogen_building-tools.patch")

(define-public puk
  (package
   (name "puk")
   (version (number->string %padicotm-revision))
   (home-page (string-append %pm2-home-page "/PadicoTM"))
   (source (origin
	    (method svn-fetch)
	    (uri (svn-reference
		  (url (string-append %padicotm-svn "/PadicoTM/Puk"))
		  (revision %padicotm-revision)))
	    (sha256
	     (base32 "1cv20rqn210l59fq4h8i3plkinasfw3n433rsqr8gj509qfj24ag"))
	    (patches (search-patches %patch-path))))
   (build-system gnu-build-system)
   (arguments
    '(#:out-of-source? #t
      #:configure-flags '("--enable-optimize"
			  "--disable-debug"
			  "--disable-trace")
      #:phases (modify-phases %standard-phases
	         (add-after 'unpack 'fix-hardcoded-paths
		   (lambda _
		     (substitute* "building-tools/common_vars.mk.in"
		       (("/bin/sh")  (which "sh")))
		     #t))
		 (delete 'check))))
   (native-inputs
    `(("pkg-config" ,pkg-config)
      ("autoconf", autoconf)
      ("automake", automake)))
   (propagated-inputs
    `(("expat" ,expat)))
   (synopsis "dummy")
   (description "Dummy")
   (license license:lgpl2.0)))

(define-public pioman
  (package
   (name "pioman")
   (version (number->string %pm2-revision))
   (home-page (string-append %pm2-home-page "/pioman"))
   (source (origin
	    (method svn-fetch)
	    (uri (svn-reference
		  (url (string-append %pm2-svn "/pioman"))
		  (revision %pm2-revision)))
	    (sha256
	     (base32 "1jhdxsg262h62pb3k0y4srz6ag4nfzh15y05ifc9wrcnhqzcyqnm"))
	    (patches (search-patches %patch-path))))
   (build-system gnu-build-system)
   (arguments
    '(#:out-of-source? #t
      #:configure-flags '("--enable-optimize"
			  "--disable-debug"
			  "--with-pthread")
      #:phases (modify-phases %standard-phases
	         (add-after 'unpack 'fix-hardcoded-paths
		   (lambda _
		     (substitute* "building-tools/common_vars.mk.in"
		       (("/bin/sh")  (which "sh")))
		     #t))
		 (delete 'check))))
   (native-inputs
    `(("pkg-config" ,pkg-config)
      ("autoconf" ,autoconf)
      ("automake" ,automake)))
   (propagated-inputs
    `(("puk" ,puk)
      ("hwloc" ,hwloc "lib")))
   (synopsis "dummy")
   (description "Dummy")
   (license license:lgpl2.0)))

(define-public padicotm
  (package
   (name "padicotm")
   (version (number->string %padicotm-revision))
   (home-page (string-append %pm2-home-page "/PadicoTM"))
   (source (origin
	    (method svn-fetch)
	    (uri (svn-reference
		  (url (string-append %padicotm-svn "/PadicoTM/PadicoTM"))
		  (revision %padicotm-revision)))
	    (sha256
	     (base32 "0g1wggawjzf843l1m2bgw01c7ljb6p95haby77fl26434w25sl6a"))
	    (patches (search-patches %patch-path))))
   (build-system gnu-build-system)
   (arguments
    '(#:out-of-source? #t
      #:configure-flags '("--enable-optimize"
			  "--disable-debug"
			  "--with-pioman"
			  "--without-pukabi")
      #:phases (modify-phases %standard-phases
	         (add-after 'unpack 'fix-hardcoded-paths
		   (lambda _
		     (substitute* "building-tools/common_vars.mk.in"
		       (("/bin/sh")  (which "sh")))
		     #t))
		 (delete 'check))))
   (native-inputs
    `(("pkg-config" ,pkg-config)
      ("autoconf" ,autoconf)
      ("automake" ,automake)
      ("hwloc" ,hwloc "lib")
      ("rdma-core" ,rdma-core)
      ("psm" ,psm)
      ("psm2" ,psm2)))
   (propagated-inputs
    `(("puk" ,puk)
      ("pioman" ,pioman)))
   (synopsis "dummy")
   (description "Dummy")
   (license license:lgpl2.0)))

(define-public padicotm-mini
  (package
   (inherit padicotm)
   (name "padicotm-mini")
   (arguments
    (substitute-keyword-arguments (package-arguments padicotm)
      ((#:configure-flags flags '())
       `(cons "--without-pioman" (delete "--with-pioman" ,flags)))))
   (propagated-inputs
    `(("puk" ,puk)))))

(define-public nmad
  (package
   (name "nmad")
   (version (number->string %pm2-revision))
   (home-page (string-append %pm2-home-page "/NewMadeleine"))
   (source (origin
	    (method svn-fetch)
	    (uri (svn-reference
		  (url (string-append %pm2-svn "/nmad"))
		  (revision %pm2-revision)))
	    (sha256
	     (base32 "0kg4205y0k4ww1rzr97kkf1gfs4xwf1b1v317ypl0iv4y3rv604y"))
	    (patches (search-patches %patch-path))))
   (build-system gnu-build-system)
   (arguments
    '(#:out-of-source? #t
      #:configure-flags '("--enable-optimize"
			  "--disable-debug"
			  "--with-pioman"
			  "--without-pukabi"
			  "--enable-mpi"
			  "--disable-sampling")
      #:phases (modify-phases %standard-phases
		 (add-after 'unpack 'fix-hardcoded-paths
		   (lambda _
		     (substitute* "building-tools/common_vars.mk.in"
		       (("/bin/sh")  (which "sh")))
		     #t))
		 (delete 'check))))
   (native-inputs
    `(("pkg-config" ,pkg-config)
      ("autoconf" ,autoconf)
      ("automake" ,automake)
      ("gfortran" ,gfortran)
      ("hwloc" ,hwloc "lib")))
   (propagated-inputs
    `(("padicotm" ,padicotm)))
   (inputs
    `(("rdma-core" ,rdma-core)
      ("psm" ,psm)
      ("psm2" ,psm2)))
   (synopsis "dummy")
   (description "Dummy")
   (license license:lgpl2.0)))

(define-public nmad-mini
  (package
   (inherit nmad)
   (name "nmad-mini")
   (arguments
    (substitute-keyword-arguments (package-arguments nmad)
      ((#:configure-flags flags '())
       `(cons "--without-pioman" (delete "--with-pioman" ,flags)))))
   (propagated-inputs
    `(("padicotm" ,padicotm-mini)
      ,@(delete `("padicotm" ,padicotm) (package-inputs nmad))))))
