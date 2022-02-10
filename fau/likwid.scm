;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2017, 2019, 2021, 2022 Inria

(define-module (fau likwid)
  #:use-module (guix)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages python)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages maths))

(define-public likwid
  (package
   (name "likwid")
   (version "5.2.1")
   (home-page "https://hpc.fau.de/research/tools/likwid/")
   (source
    (origin
     (method git-fetch)
     (uri
      (git-reference
       (url "https://github.com/RRZE-HPC/likwid")
       (commit (string-append "v" version))))
     (sha256
      (base32
       "0i2xlvlinjvfrgjpwzk5ggk8m4jg7blzlikdayl7vhz0hhl8hdfk"))))
   (build-system gnu-build-system)
   (arguments
    '(#:make-flags
      (list
       (string-append "PREFIX="
                      (assoc-ref %outputs "out"))
       (string-append "LUA_INCLUDE_DIR="
                      (assoc-ref %build-inputs "lua") "/include")
       (string-append "LUA_LIB_DIR="
                      (assoc-ref %build-inputs "lua") "/lib")
       (string-append "LUA_BIN="
                      (assoc-ref %build-inputs "lua") "/bin")
       (string-append "HWLOC_INCLUDE_DIR="
                      (assoc-ref %build-inputs "hwloc") "/include")
       (string-append "HWLOC_LIB_DIR="
                      (assoc-ref %build-inputs "hwloc") "/lib")
       "LUA_LIB_NAME=lua" "HWLOC_LIB_NAME=hwloc" "ACCESSMODE=perf_event")
      #:phases
      (modify-phases %standard-phases
                     (delete 'configure)
                     (delete 'check))))
   (inputs
    (list `(,hwloc "lib") lua python python-2.7 perl))
   (propagated-inputs
    (list gnuplot))
   (synopsis "Performance monitoring and benchmarking suite.")
   (description
    "Likwid is a simple to install and use toolsuite of command line
applications and a library for performance oriented programmers. It works for
Intel, AMD, ARMv8 and POWER9 processors on the Linux operating system. There is
additional support for Nvidia GPUs. There is support for ARMv7 and POWER8 but
there is currently no test machine in our hands to test them properly. Note that
in this recipe we compile the package with ACCESSMODE=perf_event because we lack
the root privileges on HPC platforms. One of the implications is that thermic
measures are disabled. See the Wiki on the home page of the package for more
details.")
   (license license:gpl2)))
