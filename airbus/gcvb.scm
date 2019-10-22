;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2017, 2019 Inria

(define-module (airbus gcvb)
  #:use-module (guix)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages graph)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1))
  
(define-public python-percy
  (package
    (name "python-percy")
    (version "2.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "percy" version))
        (sha256
          (base32
            "07821yabrqjyg0z45xlm4vz4hgm4gs7p7mqa3hi5ryh1qhnn2f32"))))
    (build-system python-build-system)
    (arguments
     ;; FIXME Broken tests or cyclic dependecies with other packages defined in
     ;; this file.
     '(#:tests? #f))
    (propagated-inputs
      `(("python-requests" ,python-requests)))
    (home-page
      "https://github.com/percy/python-percy-client")
    (synopsis
      "Python client library for visual regression testing with Percy
      (https://percy.io).")
    (description
      "Python client library for visual regression testing with Percy
      (https://percy.io).")
    (license license:expat)))

(define-public python-selenium
  (package
    (name "python-selenium")
    (version "3.141.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "selenium" version))
        (sha256
          (base32
            "039hf9knvl4s3hp21bzwsp1g5ri9gxsh504dp48lc6nr1av35byy"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-urllib3" ,python-urllib3)))
    (home-page
      "https://github.com/SeleniumHQ/selenium/")
    (synopsis "Python bindings for Selenium")
    (description "Python bindings for Selenium")
    (license license:asl2.0)))

(define-public python-fire
  (package
    (name "python-fire")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "fire" version))
        (sha256
          (base32
            "1jc4w1md352bdzfwv9klkwk2kisn5k4s58jnrv9179w1d7ygwrb8"))))
    (build-system python-build-system)
    (arguments
     ;; FIXME Broken tests or cyclic dependecies with other packages defined in
     ;; this file.
     '(#:tests? #f))
    (propagated-inputs
      `(("python-six" ,python-six)
        ("python-termcolor" ,python-termcolor)))
    (home-page
      "https://github.com/google/python-fire")
    (synopsis
      "A library for automatically generating command line interfaces.")
    (description
      "A library for automatically generating command line interfaces.")
    (license #f)))

(define-public python-dash-dangerously-set-inner-html
  (package
    (name "python-dash-dangerously-set-inner-html")
    (version "0.0.2")
    (source
      (origin
        (method url-fetch)
        ;; FIXME Fetching from pypi unavailable due to broken link! Temporarily
        ;; using hardcoded URL instead.
        (uri (string-append "https://files.pythonhosted.org/packages/56/b1/5102060b9b6836409db84265f4f934475c2707cce87e75f3f8a04493e0dc/dash_dangerously_set_inner_html-0.0.2.tar.gz"))
        (sha256
          (base32
            "0r7akwk9nxw9lpxyxg0b72zwlmd0msvi13rcwb9c87w5al3rkznp"))))
    (build-system python-build-system)
    (arguments
     ;; FIXME Broken tests or cyclic dependecies with other packages defined in
     ;; this file.
     '(#:tests? #f))
    (home-page "")
    (synopsis
      "A dash component for specifying raw HTML")
    (description
      "A dash component for specifying raw HTML")
    (license license:expat)))

(define-public python-dash-flow-example
  (package
    (name "python-dash-flow-example")
    (version "0.0.5")
    (source
      (origin
        (method url-fetch)
        ;; FIXME Fetching from pypi unavailable due to broken link! Temporarily
        ;; using hardcoded URL instead.
        (uri (string-append "https://files.pythonhosted.org/packages/7c/e7/a712e5ece98b64e841b582727a489de8fffa674bcc9e949c56e0789da3b6/dash_flow_example-0.0.5.tar.gz"))
        (sha256
          (base32
            "0w5l1w4q2i2k2xi6xv8q6j186jqvz1ffswnbvsdglxh306ld56my"))))
    (build-system python-build-system)
    (arguments
     ;; FIXME Broken tests or cyclic dependecies with other packages defined in
     ;; this file.
     '(#:tests? #f))
    (home-page "")
    (synopsis
      "Example of a Dash library that uses Flow Types")
    (description
      "Example of a Dash library that uses Flow Types")
    (license license:expat)))

(define-public python-dash-table
  (package
    (name "python-dash-table")
    (version "4.4.0")
    (source
      (origin
        (method url-fetch)
        ;; FIXME Fetching from pypi unavailable due to broken link! Temporarily
        ;; using hardcoded URL instead.
        (uri (string-append "https://files.pythonhosted.org/packages/37/43/132b6b19401da48cbc93c8330b263b63a711f0568ea04f155071f4fd220e/dash_table-4.4.0.tar.gz"))
        (sha256
          (base32
            "02pw4m8agy0hp8rjmjba9lg6x2qp9qmninxyzxgrgfw3xdsi9x28"))))
    (build-system python-build-system)
    (arguments
     ;; FIXME Broken tests or cyclic dependecies with other packages defined in
     ;; this file.
     '(#:tests? #f))
    (home-page "")
    (synopsis "Dash table")
    (description "Dash table")
    (license license:expat)))

(define-public python-dash-html-components
  (package
    (name "python-dash-html-components")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        ;; FIXME Fetching from pypi unavailable due to broken link! Temporarily
        ;; using hardcoded URL instead.
        (uri (string-append "https://files.pythonhosted.org/packages/0d/e8/e6f68c0a3c146d15bebe8d3570ebe535abdbba90b87e548bdf3363ecddbe/dash_html_components-1.0.1.tar.gz"))
        (sha256
          (base32
            "1r2g8lbani2c0jjlwf73azfi0gz5nva7s3pvh79bfpk32azq0zkx"))))
    (build-system python-build-system)
    (arguments
     ;; FIXME Broken tests or cyclic dependecies with other packages defined in
     ;; this file.
     '(#:tests? #f))
    (home-page
      "https://github.com/plotly/dash-html-components")
    (synopsis "Vanilla HTML components for Dash")
    (description "Vanilla HTML components for Dash")
    (license license:expat)))

(define-public python-dash-core-components
  (package
    (name "python-dash-core-components")
    (version "1.3.0")
    (source
      (origin
        (method url-fetch)
        ;; FIXME Fetching from pypi unavailable due to broken link! Temporarily
        ;; using hardcoded URL instead.
        (uri (string-append "https://files.pythonhosted.org/packages/48/92/5a482edc7221f108633d919f44751438e43789d1cb2ac9bcddb049e42981/dash_core_components-1.3.0.tar.gz"))
        (sha256
          (base32
            "0awfqfl5ispyg69avy6xa3qq9i3qjcbdffjhrggfcnpz3h93svir"))))
    (build-system python-build-system)
    (arguments
     ;; FIXME Broken tests or cyclic dependecies with other packages defined in
     ;; this file.
     '(#:tests? #f))
    (home-page "")
    (synopsis "Core component suite for Dash")
    (description "Core component suite for Dash")
    (license license:expat)))

(define-public python-dash-renderer
  (package
    (name "python-dash-renderer")
    (version "1.1.1")
    (source
      (origin
        (method url-fetch)
        ;; FIXME Fetching from pypi unavailable due to broken link! Temporarily
        ;; using hardcoded URL instead.
        (uri (string-append "https://files.pythonhosted.org/packages/b0/85/c69d8b07b871e7407965acfefcaeef3ffd211b6df91b77bf7edce78bdcc1/dash_renderer-1.1.1.tar.gz"))
        (sha256
          (base32
            "0dvy650f562nwgb7xz613psrp4jhrpl3zlkq3mbkylgd8hkr2r1s"))))
    (build-system python-build-system)
    (home-page "")
    (synopsis
      "Front-end component renderer for Dash")
    (description
      "Front-end component renderer for Dash")
    (license license:expat)))

(define-public python-flask-compress
  (package
    (name "python-flask-compress")
    (version "1.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "Flask-Compress" version))
        (sha256
          (base32
            "1cxdbdiyxkspg7vkchfmaqr7c6q79gwvakna3fjcc6nivps971j6"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-flask" ,python-flask)))
    (home-page
      "https://libwilliam.github.io/flask-compress/")
    (synopsis
      "Compress responses in your Flask app with gzip.")
    (description
      "Compress responses in your Flask app with gzip.")
    (license license:expat)))

(define-public python-dash
  (package
    (name "python-dash")
    (version "1.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "dash" version))
        (sha256
          (base32
            "0qqfpm3v7df0jr18bw87fg10yv3znzjpf1jqd7znnh13y9irv60f"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-dash-core-components"
         ,python-dash-core-components)
        ("python-dash-html-components"
         ,python-dash-html-components)
        ("python-dash-renderer" ,python-dash-renderer)
        ("python-dash-table" ,python-dash-table)
        ("python-flask" ,python-flask)
        ("python-flask-compress" ,python-flask-compress)
        ("python-future" ,python-future)
        ("python-plotly" ,python-plotly)))
    (native-inputs
      `(("python-astroid" ,python-astroid)
        ("python-beautifulsoup4" ,python-beautifulsoup4)
        ("python-coloredlogs" ,python-coloredlogs)
        ("python-dash-dangerously-set-inner-html"
         ,python-dash-dangerously-set-inner-html)
        ("python-dash-flow-example"
         ,python-dash-flow-example)
        ("python-fire" ,python-fire)
        ("python-flake8" ,python-flake8)
        ("python-lxml" ,python-lxml)
        ("python-mock" ,python-mock)
        ("python-percy" ,python-percy)
        ("python-pylint" ,python-pylint)
        ("python-pytest" ,python-pytest)
        ("python-pytest-mock" ,python-pytest-mock)
        ("python-pytest-sugar" ,python-pytest-sugar)
        ("python-pyyaml" ,python-pyyaml)
        ("python-requests" ,python-requests)
        ("python-selenium" ,python-selenium)
        ("python-virtualenv" ,python-virtualenv)
        ("python-waitress" ,python-waitress)))
    (home-page "https://plot.ly/dash")
    (synopsis
      "A Python framework for building reactive web-apps. Developed by Plotly.")
    (description
      "A Python framework for building reactive web-apps. Developed by Plotly.")
    (license license:expat)))

(define-public python-dash-bootstrap-components
  (package
    (name "python-dash-bootstrap-components")
    (version "0.7.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "dash-bootstrap-components" version))
        (sha256
          (base32
            "033air30spprrd2szm1fvmv5k8jm80wwh9369l3r603j8qkc2s9s"))))
    (build-system python-build-system)
    (arguments
     ;; FIXME Broken tests or cyclic dependecies with other packages defined in
     ;; this file.
     '(#:tests? #f))
    (propagated-inputs
      `(("python-dash" ,python-dash)))
    (home-page
      "https://dash-bootstrap-components.opensource.faculty.ai/")
    (synopsis
      "Bootstrap themed components for use in Plotly Dash")
    (description
      "Bootstrap themed components for use in Plotly Dash")
    (license #f)))

(define-public gcvb
  (package
    (name "gcvb")
    (version "0.0.1")
    (source (origin
        (method git-fetch)
        (uri (git-reference
          (url "https://github.com/jm-cc/gcvb")
          (commit "a62cd85c5e631524fcade39542e992f5cec577fd")))
        (sha256
          (base32
            "00qk7v9q9xl7qlgcgswb196fyb77n3rrg4nf9ia50ix4d0hycjzd"))))
    (build-system python-build-system)
    (propagated-inputs `(("PyYAML" ,python-pyyaml)))
    (native-inputs
     `(("python-dash-bootstrap-components" ,python-dash-bootstrap-components)))
    (home-page "https://github.com/jm-cc/gcvb")
    (synopsis
      "Python 3 module aiming at facilitating non-regression, validation and
      benchmarking of simulation codes")
    (description
      "gcvb (generate compute validate benchmark) is a Python 3 module aiming at
facilitating non-regression, validation and benchmarking of simulation codes.
gcvb is not a complete tool of continuous integration (CI). It is rather a
component of the testing part of a CI workflow. It can compare the different
metrics of your computation with references that can be a file, depends of the
'configuration' or are absolute.")
    (license license:expat)))
