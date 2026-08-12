(define-module (entelequia packages glab)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (nonguix build-system binary))

(define-public glab
  (package
    (name "glab")
    (version "1.113.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://gitlab.com/api/v4/projects/gitlab-org%2Fcli"
             "/packages/generic/glab/" version
             "/glab_" version "_linux_amd64.tar.gz"))
       (sha256
        (base32 "0rvlnh9ql8ijl9gklzlfhlgrjhpfxxb97pr7d85k33q1nagmhrf2"))))
    (build-system binary-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("bin/"        "/bin/")
          ("LICENSE"     "share/doc/glab/")
          ("README.md"   "share/doc/glab/")
          ("CHANGELOG.md" "share/doc/glab/"))
      ;; The upstream tarball has a flat layout (no top-level directory), so
      ;; the standard 'unpack phase chdir's into the only subdirectory it
      ;; finds at the build root: bin/.  Step back up so install-plan paths
      ;; resolve against the build root.
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install 'chdir-to-source-root
            (lambda _ (chdir ".."))))))
    (home-page "https://gitlab.com/gitlab-org/cli")
    (synopsis "GitLab's official command line tool")
    (description
     "@code{glab} is GitLab on the command line.  It brings merge requests,
issues, pipelines and other GitLab concepts to the terminal next to where
you are already working with @code{git} and your code.")
    (license license:expat)))
