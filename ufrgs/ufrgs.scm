;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2017, 2019, 2020, 2021 Inria

(define-module (ufrgs ufrgs)
  #:use-module (guix)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system r)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages perl)
  #:use-module (inria hiepacs)
  #:use-module (inria mpi)
  #:use-module (inria storm)
  #:use-module (inria tadaam)
  #:use-module (inria eztrace)
  #:use-module (inria simgrid)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  ;; To remove when/if python2 packages sympy and mpi4py
  ;; are fixed in official repo
  #:use-module (guix build-system python)
  )

(define-public r-starvz
  (package
   (name "r-starvz")
   (version "0.4.0")
   (home-page "https://github.com/schnorr/starvz")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url home-page)
           (commit "ce80b8ec74b982a4d02f38fa1ceab6beee3335b6")
           (recursive? #f)))
     (file-name (string-append name "-" version "-checkout"))
     (sha256
      (base32
       "1w1wxk8043r2q46dqwz0kqi4av21dfdix4gzgsdhrka2dsyxd17i"))))
   (properties
    `((upstream-name . "starvz")))
   (build-system r-build-system)
   (propagated-inputs
    `(("awk" ,gawk)
      ("bash" ,bash)
      ("coreutils" ,coreutils)
      ("gcc-toolchain" ,gcc-toolchain)
      ("grep" ,grep)
      ("gzip" ,gzip)
      ("pageng" ,pageng)
      ("pmtool" ,pmtool)
      ("r-arrow" ,r-arrow)
      ("r-bh" ,r-bh)
      ("r-car" ,r-car)
      ("r-data-tree" ,r-data-tree)
      ("r-dplyr" ,r-dplyr)
      ("r-ggplot2" ,r-ggplot2)
      ("r-gtools" ,r-gtools)
      ("r-lpsolve" ,r-lpsolve)
      ("r-magrittr" ,r-magrittr)
      ("r-patchwork" ,r-patchwork)
      ("r-purrr" ,r-purrr)
      ("r-rcolorbrewer" ,r-rcolorbrewer)
      ("r-rcpp" ,r-rcpp)
      ("r-readr" ,r-readr)
      ("r-rlang" ,r-rlang)
      ("r-stringr" ,r-stringr)
      ("r-tibble" ,r-tibble)
      ("r-tidyr" ,r-tidyr)
      ("r-yaml" ,r-yaml)
      ("r-zoo" ,r-zoo)
      ("rectutils" ,recutils)
      ("sed" ,sed)
      ("starpu" ,starpu+fxt)
      ("which" ,which)))
   (synopsis
    "R-Based Visualization Techniques for Task-Based Applications")
   (description
    "Performance analysis workflow that combines the power of the R
language (and the tidyverse realm) and many auxiliary tools to provide a
consistent, flexible, extensible, fast, and versatile framework for the
performance analysis of task-based applications that run on top of the StarPU
runtime (with its MPI (Message Passing Interface) layer for multi-node support).
Its goal is to provide a fruitful prototypical environment to conduct
performance analysis hypothesis-checking for task-based applications that run on
heterogeneous (multi-GPU, multi-core) multi-node HPC (High-performance
computing) platforms.")
   (license license:gpl3)))

(define-public r-arrow
  (package
   (name "r-arrow")
   ;; The version of 'r-arrow' must match the version of the 'apache-arrow'
   ;; dependency which is currently '0.17.1'!
   (version "0.17.1")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "arrow" version))
     (sha256
      (base32
       "18rssakj0z81hh8x9qv7gbxdpbf2f2yix3sg3ffdi0ij3a7nipn6"))))
   (properties `((upstream-name . "arrow")))
   (build-system r-build-system)
   (arguments
    '(#:phases
      (modify-phases
       %standard-phases
       (add-before
        'install
        ;; Some files are changed during the shebang patching phase which makes
        ;; MD5 checksum validation fail for these files. Therefore, we need to
        ;; recompute the checksums and update the checksum file called 'MD5'.
        'update-checksums
        (lambda _
          (substitute*
           "MD5"
           (("b42138af7af04ffb1c62af0bbca3e73c")
            (begin
              (use-modules (ice-9 rdelim))
              (use-modules (ice-9 popen))
              (let*
                  ((port (open-input-pipe
                          "rhash --md5 -p %m cleanup"))
                   (hash (read-line port))
                   (close-pipe port))
                hash))))
          (substitute*
           "MD5"
           (("bb1348983f9da016353f648ee223c725")
            (begin
              (use-modules (ice-9 rdelim))
              (use-modules (ice-9 popen))
              (let*
                  ((port (open-input-pipe
                          "rhash --md5 -p %m configure"))
                   (hash (read-line port))
                   (close-pipe port))
                hash))))
          (substitute*
           "MD5"
           (("90383499dacaad811dbc26c8dcad74ae")
            (begin
              (use-modules (ice-9 rdelim))
              (use-modules (ice-9 popen))
              (let*
                  ((port (open-input-pipe
                          "rhash --md5 -p %m configure.win"))
                   (hash (read-line port))
                   (close-pipe port))
                hash))))
          (substitute*
           "MD5"
           (("c571fa1c92925077d0a2131d2aea0d8d")
            (begin
              (use-modules (ice-9 rdelim))
              (use-modules (ice-9 popen))
              (let*
                  ((port (open-input-pipe
                          "rhash --md5 -p %m inst/build_arrow_static.sh"))
                   (hash (read-line port))
                   (close-pipe port))
                hash)))) #t)))))
   (inputs
    `(("zlib" ,zlib)
      ("rhash", rhash)))
   (propagated-inputs
    `(("r-assertthat" ,r-assertthat)
      ("r-bit64" ,r-bit64)
      ("r-purrr" ,r-purrr)
      ("r-r6" ,r-r6)
      ("r-rcpp" ,r-rcpp)
      ("r-rlang" ,r-rlang)
      ("r-tidyselect" ,r-tidyselect)
      ("r-vctrs" ,r-vctrs)
      ;; Necessary for a compilation using the Arrow C++ libraries from Apache
      ("arrow:lib", apache-arrow "lib")
      ("arrow:include", apache-arrow "include")))
   (native-inputs
    `(("pkg-config" ,pkg-config)
      ("r-knitr" ,r-knitr)))
   (home-page "https://github.com/apache/arrow/")
   (synopsis "Integration to 'Apache' 'Arrow'")
   (description
    "'Apache' 'Arrow' <https://arrow.apache.org/> is a cross-language
development platform for in-memory data.  It specifies a standardized
language-independent columnar memory format for flat and hierarchical data,
organized for efficient analytic operations on modern hardware. This package
provides an interface to the 'Arrow C++' library.")
   (license license:asl2.0)))

(define-public r-data-tree
(package
  (name "r-data-tree")
  (version "1.0.0")
  (source
    (origin
      (method url-fetch)
      (uri (cran-uri "data.tree" version))
      (sha256
        (base32
          "0pizmx2312zsym4m42b97q2184bg3hibvbdrblcga05xln84qrs0"))))
  (properties
   `((upstream-name . "data.tree")))
  (build-system r-build-system)
  (propagated-inputs
   `(("r-r6" ,r-r6)
     ("r-stringi" ,r-stringi)))
  (native-inputs
   `(("r-knitr" ,r-knitr)))
  (home-page "http://github.com/gluc/data.tree")
  (synopsis
    "General Purpose Hierarchical Data Structure")
  (description
    "Create tree structures from hierarchical data, and traverse the tree in
various orders.  Aggregate, cumulate, print, plot, convert to and from
data.frame and more.  Useful for decision trees, machine learning, finance,
conversion from and to JSON, and many other applications.")
  (license license:gpl2+)))

(define-public pageng
  (package
   (name "pageng")
   (version "1.3.6")
   (home-page "https://github.com/schnorr/pajeng")
   (synopsis "PajeNG - Trace Visualization Tool")
   (description
    "PajeNG (Paje Next Generation) is a re-implementation (in C++) and direct
heir of the well-known Paje visualization tool for the analysis of execution
traces (in the Paje File Format) through trace visualization (space/time view).
The tool is released under the GNU General Public License 3. PajeNG comprises
the libpaje library, and a set of auxiliary tools to manage Paje trace files
(such as pj_dump and pj_validate). The space-time visualization tool called
pajeng is deprecated (removed from the sources) since modern tools do a better
job (see pj_gantt, for instance, or take a more general approach using R+ggplot2
to visualize the output of pj_dump). This effort was started as part of the
french INFRA-SONGS ANR project. Development has continued through a
collaboration between INF/UFRGS and INRIA.")
   (license license:gpl3+)
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url home-page)
           (commit "ce7bfb9b2c0e5bee13a2d55921abf289c3644ae9")
           (recursive? #f)))
     (file-name (string-append name "-" version "-checkout"))
     (sha256
      (base32
       "03vigx28spmn8smngkcw43mqw7b1cp8574f63fzb4g5sjd379am0"))))
   (build-system cmake-build-system)
   (arguments
    ;; To satisfy the 'runpath-validation' phase
    '(#:configure-flags  (list (string-append "-DCMAKE_EXE_LINKER_FLAGS="
                                              "-Wl,-rpath="
                                              (assoc-ref %outputs "out")
                                              "/lib"))
      #:phases
      (modify-phases
       %standard-phases
       ;; Test scripts require trace files to be at '../traces' during the
       ;; check phase. Given that 'make test' is executed from the 'build'
       ;; directory, we must copy the trace files from '../source/traces' to
       ;; '../traces'.
       (add-before 'check 'copy-trace-files-for-testing
                   (lambda _
                     (copy-recursively "../source/traces" "../traces") #t)))))
   (outputs
    '("debug" "out"))
   (inputs
    `(("asciidoc" ,asciidoc)
      ("boost" ,boost)
      ("r" ,r)
      ("recutils" ,recutils)))
   (native-inputs
    `(("gcc-toolchain" ,gcc-toolchain)
      ("bison" ,bison)
      ("flex" ,flex)
      ("perl" ,perl)))))
