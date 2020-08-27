;;; This module extends GNU Guix and is licensed under the same terms, those
;;; of the GNU GPL version 3 or (at your option) any later version.
;;;
;;; Copyright © 2020 Inria

(define-module (utils emacs-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages emacs-xyz))

(define-public emacs-org-tanglesync-1.1
  (let ((commit "af83a73ae542d5cb3c9d433cbf2ce1d4f4259117")
        (revision "1"))
    (package
     (name "emacs-org-tanglesync-1.1")
     (version (git-version "1.1" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mtekman/org-tanglesync.el")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11rfn0byy0k0321w7fjgpa785ik1nrk1j6d0y4j0j4a8gys5hjr5"))))
     (build-system emacs-build-system)
     (propagated-inputs
      `(("emacs-org" ,emacs-org)))
     (home-page "https://github.com/mtekman/org-tanglesync.el")
     (synopsis "Sync Org source blocks with tangled external files")
     (description "Tangled blocks provide a nice way of exporting code into
external files, acting as a fantastic agent to write literate dotfile configs.
However, such dotfiles tend to be changed externally, sometimes for the worse
and sometimes for the better.  In the latter case it would be nice to be able to
pull those external changes back into the original org src block it originated
from.")
     (license license:gpl3+))))
