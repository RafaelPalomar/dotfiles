(define-module (config packages cyrus-sasl-xoauth2)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages autotools)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build utils)
  #:use-module (guix search-paths)
  #:use-module (guix packages))

(define-public cyrus-sasl-xoauth2
  (package
   (name "cyrus-sasl-xoauth2")
   (version "v0.2")
   (source
    (origin
     (method url-fetch)
     (uri "https://github.com/moriyoshi/cyrus-sasl-xoauth2/archive/36aabca54fd65c8fa7a707cb4936751599967904.tar.gz")
     (sha256
      (base32 "1pdq27m2lchnyvd2p2jbbzl81mh2ar0xvjrhp0ikky17giyagmq7"))))
   (build-system gnu-build-system)
   (inputs
    `(("cyrus-sasl" ,cyrus-sasl)))
   (native-inputs
    (list autoconf
          automake
          libtool))
(arguments
 (list
  #:tests? #f
  #:configure-flags
   #~(list (string-append "--prefix=" #$output)
           (string-append "--libdir=" #$output "/lib"))
  #:phases
   #~(modify-phases %standard-phases
       (add-after 'unpack 'prepend-shebang-to-autogen-sh
         (lambda _
           (substitute* "autogen.sh"
             (("^" all)
              (string-append "#!/bin/sh\n" all)))))
       (add-before 'configure 'bootstrap
         (lambda _
           (invoke "./autogen.sh")))
       (replace 'install
         (lambda* (#:key outputs #:allow-other-keys)
           (let ((out (assoc-ref outputs "out")))
             (invoke "make" "install"
                     (string-append "pkglibdir=" out "/lib/sasl2"))))))))
   (native-search-paths
    (list (search-path-specification
            (variable "SASL_PATH")
            (files '("lib/sasl2")))))
   (synopsis "Cyrus SASL XOAUTH2 plugin")
   (description "This package provides the XOAUTH2 plugin for Cyrus SASL.")
   (home-page "https://github.com/moriyoshi/cyrus-sasl-xoauth2")
   (license #f)))
