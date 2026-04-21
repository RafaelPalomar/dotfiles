(define-module (entelequia packages aider)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-compression)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tree-sitter))


;;;
;;; Pinned tree-sitter 0.24 — aider 0.86.x requires exactly tree-sitter==0.24.0.
;;; Guix ships 0.25.x which removed Query.captures(); 0.24.0 still has it.
;;; 0.24.0 uses Rust/PyO3 so we install from the pre-built cp311 manylinux wheel.
;;;

(define python-tree-sitter-0.24
  (package
    (name "python-tree-sitter")
    (version "0.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://files.pythonhosted.org/packages/4c/aa/"
             "2fb4d81886df958e6ec7e370895f7106d46d0bbdcc531768326124dc8972/"
             "tree_sitter-" version
             "-cp311-cp311-manylinux_2_17_x86_64.manylinux2014_x86_64.whl"))
       (sha256
        (base32 "02b4xv8fq3iwa0ndj6xvhh61wx2xknxacpc7gwpvk21v02kh3sh1"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out  (assoc-ref %outputs "out"))
                (site (string-append out "/lib/python3.11/site-packages"))
                (src  (assoc-ref %build-inputs "source"))
                (unzip (string-append (assoc-ref %build-inputs "unzip") "/bin/unzip")))
           (mkdir-p site)
           (invoke unzip "-d" site src)))))
    (native-inputs (list unzip))
    (supported-systems '("x86_64-linux"))
    (home-page "https://github.com/tree-sitter/py-tree-sitter")
    (synopsis "Python bindings for tree-sitter (0.24.x)")
    (description
     "Python bindings for tree-sitter, pinned to 0.24.0 for compatibility
with aider-chat 0.86.x (tree-sitter 0.25.x removed Query.captures()).")
    (license license:expat)))


;;;
;;; Simple utility packages (pure Python)
;;;

(define-public python-backoff
  (package
    (name "python-backoff")
    (version "2.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "backoff" version))
       (sha256
        (base32 "1fjwz9x81wpfn22j96ck49l3nb2hn19qfgv44441h8qrpgsjky03"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-poetry-core))
    (arguments (list #:tests? #f))
    (home-page "https://github.com/litl/backoff")
    (synopsis "Function decoration for backoff and retry")
    (description
     "Python library providing function decorators which can be used to wrap a
function such that it will be retried until some condition is met.")
    (license license:expat)))

(define-public python-lox
  (package
    (name "python-lox")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "lox" version))
       (sha256
        (base32 "0k44cgrx3036sfmixygiibbadypfb2c04dnvlll2vq5jdm1gkx8z"))))
    (build-system python-build-system)
    (arguments (list #:tests? #f))
    (home-page "https://github.com/BrianPugh/lox")
    (synopsis "Threading primitives and decorators")
    (description
     "Lox provides easy-to-use concurrency utilities: thread-safe queues,
semaphores, worker pools, and lock decorators.")
    (license license:expat)))

(define-public python-mslex
  (package
    (name "python-mslex")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mslex" version))
       (sha256
        (base32 "0pd852pd3h4xsb8d5pvc02rp0gysmgjshdxgwbp11dix3myqh734"))))
    (build-system python-build-system)
    (arguments (list #:tests? #f))
    (home-page "https://github.com/smoofra/mslex")
    (synopsis "Lexer for Windows command line strings")
    (description
     "A lexer and splitter for Windows command line strings, analogous to
shlex for POSIX systems.")
    (license license:expat)))

(define-public python-oslex
  (package
    (name "python-oslex")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "oslex" version))
       (sha256
        (base32 "0bazd0vk2lzsk7cwgl2m669m6dqqhh4l0d6sn2yaiwjxqy1cvm0y"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-hatchling))
    (propagated-inputs (list python-mslex))
    (arguments (list #:tests? #f))
    (home-page "https://github.com/smoofra/oslex")
    (synopsis "OS-aware shell lexer wrapper")
    (description
     "Provides an OS-aware shlex equivalent: uses mslex on Windows and shlex
on POSIX systems.")
    (license license:expat)))

(define-public python-imgcat
  (package
    (name "python-imgcat")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "imgcat" version))
       (sha256
        (base32 "0p4ksyy6zy3xz75l71c52ni69m7bji6803ir26vgkwdkmml7nm5k"))))
    (build-system python-build-system)
    (arguments
     (list #:tests? #f
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'remove-setup-requires
                 (lambda _
                   (substitute* "setup.py"
                     (("setup_requires=.*") "")
                     (("'pytest-runner.*',?") "")))))))
    (home-page "https://github.com/wookayin/python-imgcat")
    (synopsis "Display images inline in the terminal")
    (description
     "A Python library and command-line tool to display images inline in
iTerm2, Kitty, and other terminals that support inline image protocols.")
    (license license:expat)))

(define-public python-json-logic
  (package
    (name "python-json-logic")
    (version "0.7.0a0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "json_logic" version))
       (sha256
        (base32 "037y286d3ms1imbrymrwp719z4v3lffg2i9psq07pv6g9fbhnn44"))))
    (build-system python-build-system)
    (propagated-inputs (list python-six))
    (arguments (list #:tests? #f))
    (home-page "https://github.com/nadir-akhtar/json-logic-python")
    (synopsis "Apply JsonLogic rules in Python")
    (description
     "A Python implementation of JsonLogic, a way to serialize conditional
logic as JSON.")
    (license license:expat)))

(define-public python-posthog
  (package
    (name "python-posthog")
    (version "7.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "posthog" version))
       (sha256
        (base32 "0kn1vfjd35shcwqisy6ygjn552gjwqc4rmw4nwbjyr3yp03v51ck"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-setuptools))
    (propagated-inputs
     (list python-backoff
           python-dateutil
           python-distro
           python-requests
           python-six
           python-typing-extensions))
    (arguments (list #:tests? #f))
    (home-page "https://github.com/PostHog/posthog-python")
    (synopsis "PostHog analytics Python client")
    (description
     "The official Python client library for PostHog product analytics.")
    (license license:expat)))

(define-public python-mixpanel
  (package
    (name "python-mixpanel")
    (version "5.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mixpanel" version))
       (sha256
        (base32 "0yimmlzw4amn14q946lgghjj3f0csbghdxr9xky1bqnk94xqyzda"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-setuptools))
    (propagated-inputs
     (list python-asgiref
           python-httpx
           python-json-logic
           python-pydantic
           python-requests))
    (arguments (list #:tests? #f))
    (home-page "https://github.com/mixpanel/mixpanel-python")
    (synopsis "Mixpanel analytics Python client")
    (description
     "The official Python library for Mixpanel data ingestion.")
    (license license:asl2.0)))


;;;
;;; Tree-sitter language packages (setuptools + C extensions)
;;;

;;; Tree-sitter grammar packages — installed from pre-built abi3 manylinux wheels.
;;; Building from source requires tree_sitter/parser.h which isn't readily
;;; available in the Guix tree-sitter packages.

(define-public python-tree-sitter-c-sharp
  (package
    (name "python-tree-sitter-c-sharp")
    (version "0.23.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://files.pythonhosted.org/packages/ca/72/"
             "fc6846795bcdae2f8aa94cc8b1d1af33d634e08be63e294ff0d6794b1efc/"
             "tree_sitter_c_sharp-" version
             "-cp39-abi3-manylinux_2_5_x86_64.manylinux1_x86_64"
             ".manylinux_2_17_x86_64.manylinux2014_x86_64.whl"))
       (sha256
        (base32 "1lhnfs3jz69im3j7b2xqgwy8k1455liiyclhvk312migdd34w0m8"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out  (assoc-ref %outputs "out"))
                (site (string-append out "/lib/python3.11/site-packages"))
                (src  (assoc-ref %build-inputs "source"))
                (unzip (string-append (assoc-ref %build-inputs "unzip") "/bin/unzip")))
           (mkdir-p site)
           (invoke unzip "-d" site src)))))
    (native-inputs (list unzip))
    (propagated-inputs (list python-tree-sitter-0.24))
    (supported-systems '("x86_64-linux"))
    (home-page "https://github.com/tree-sitter/tree-sitter-c-sharp")
    (synopsis "C# grammar for tree-sitter")
    (description "Tree-sitter C# language grammar bindings for Python.")
    (license license:expat)))

(define-public python-tree-sitter-yaml
  (package
    (name "python-tree-sitter-yaml")
    (version "0.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://files.pythonhosted.org/packages/72/92/"
             "c4b896c90d08deb8308fadbad2210fdcc4c66c44ab4292eac4e80acb4b61/"
             "tree_sitter_yaml-" version
             "-cp310-abi3-manylinux1_x86_64.manylinux_2_28_x86_64"
             ".manylinux_2_5_x86_64.whl"))
       (sha256
        (base32 "0hcsqb4n60sispgnzhldmnglh30d0agmc8z0m8vw1i5nk06cd9gi"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out  (assoc-ref %outputs "out"))
                (site (string-append out "/lib/python3.11/site-packages"))
                (src  (assoc-ref %build-inputs "source"))
                (unzip (string-append (assoc-ref %build-inputs "unzip") "/bin/unzip")))
           (mkdir-p site)
           (invoke unzip "-d" site src)))))
    (native-inputs (list unzip))
    (propagated-inputs (list python-tree-sitter-0.24))
    (supported-systems '("x86_64-linux"))
    (home-page "https://github.com/tree-sitter-grammars/tree-sitter-yaml")
    (synopsis "YAML grammar for tree-sitter")
    (description "Tree-sitter YAML language grammar bindings for Python.")
    (license license:expat)))

(define-public python-tree-sitter-embedded-template
  (package
    (name "python-tree-sitter-embedded-template")
    (version "0.25.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://files.pythonhosted.org/packages/9f/97/"
             "ea3d1ea4b320fe66e0468b9f6602966e544c9fe641882484f9105e50ee0c/"
             "tree_sitter_embedded_template-" version
             "-cp310-abi3-manylinux1_x86_64.manylinux_2_28_x86_64"
             ".manylinux_2_5_x86_64.whl"))
       (sha256
        (base32 "1hn1lhgn7ff38lc6avjzmhknm4qvdgzp3q4azsg3qjxrv0yqrj57"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out  (assoc-ref %outputs "out"))
                (site (string-append out "/lib/python3.11/site-packages"))
                (src  (assoc-ref %build-inputs "source"))
                (unzip (string-append (assoc-ref %build-inputs "unzip") "/bin/unzip")))
           (mkdir-p site)
           (invoke unzip "-d" site src)))))
    (native-inputs (list unzip))
    (propagated-inputs (list python-tree-sitter-0.24))
    (supported-systems '("x86_64-linux"))
    (home-page "https://github.com/tree-sitter/tree-sitter-embedded-template")
    (synopsis "Embedded template grammar for tree-sitter")
    (description
     "Tree-sitter embedded template (ERB/EJS) language grammar bindings for Python.")
    (license license:expat)))

(define-public python-tree-sitter-language-pack
  (package
    (name "python-tree-sitter-language-pack")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://files.pythonhosted.org/packages/da/a0/"
             "485128abc18bbb7d78a2dd0c6487315a71b609877778a9796968f43f36d9/"
             "tree_sitter_language_pack-" version
             "-cp39-abi3-manylinux2014_x86_64.whl"))
       (sha256
        (base32 "09qv65yhsz2xkrghqqvhxl6d0zq1ns10kwcsqv235rm2qbbsy8jr"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out  (assoc-ref %outputs "out"))
                (site (string-append out "/lib/python3.11/site-packages"))
                (src  (assoc-ref %build-inputs "source"))
                (unzip (string-append (assoc-ref %build-inputs "unzip") "/bin/unzip")))
           (mkdir-p site)
           (invoke unzip "-d" site src)))))
    (native-inputs (list unzip))
    (propagated-inputs
     (list python-tree-sitter-0.24
           python-tree-sitter-c-sharp
           python-tree-sitter-embedded-template
           python-tree-sitter-yaml))
    (supported-systems '("x86_64-linux"))
    (home-page "https://github.com/Goldziher/tree-sitter-language-pack")
    (synopsis "Bundled tree-sitter language grammars")
    (description
     "A collection of compiled tree-sitter language grammars for use with
the tree-sitter Python library.")
    (license license:expat)))

(define-public python-grep-ast
  (package
    (name "python-grep-ast")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "grep_ast" version))
       (sha256
        (base32 "1qrf9dfqdxlhrxnlm4plfj3iyrdf6k1adjfi709p5rlk8hm282k2"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-pathspec
           python-tree-sitter-language-pack))
    (arguments (list #:tests? #f))
    (home-page "https://github.com/paul-gauthier/grep-ast")
    (synopsis "Grep source files showing surrounding AST context")
    (description
     "A grep-like tool that shows matching lines with their surrounding
abstract syntax tree context, making it easier to understand code structure.")
    (license license:asl2.0)))


;;;
;;; Rust/maturin packages — installed from pre-built manylinux cp311 wheels
;;;

(define (install-wheel wheel-source python)
  "Builder thunk: unzip WHEEL-SOURCE into Python's site-packages."
  `(begin
     (use-modules (guix build utils))
     (let* ((out   (assoc-ref %outputs "out"))
            (site  (string-append out "/lib/python"
                                  ,(version-major+minor
                                    (package-version python))
                                  "/site-packages"))
            (src   (assoc-ref %build-inputs "source"))
            (unzip (string-append (assoc-ref %build-inputs "unzip") "/bin/unzip")))
       (mkdir-p site)
       (invoke unzip "-d" site src))))

(define-public python-jiter
  ;; Rust/PyO3 extension — installed from pre-built manylinux cp311 wheel.
  (package
    (name "python-jiter")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://files.pythonhosted.org/packages/ad/5e/"
             "0ddeb7096aca099114abe36c4921016e8d251e6f35f5890240b31f1f60ae/"
             "jiter-" version
             "-cp311-cp311-manylinux_2_17_x86_64.manylinux2014_x86_64.whl"))
       (sha256
        (base32 "0wx3yf0iwifh648jnk93x1lmj96mspd2dmax0pf50akccid1s02w"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out  (assoc-ref %outputs "out"))
                (site (string-append out "/lib/python3.11/site-packages"))
                (src  (assoc-ref %build-inputs "source"))
                (unzip (string-append (assoc-ref %build-inputs "unzip") "/bin/unzip")))
           (mkdir-p site)
           (invoke unzip "-d" site src)))))
    (native-inputs (list unzip))
    (supported-systems '("x86_64-linux"))
    (home-page "https://github.com/pydantic/jiter")
    (synopsis "Fast iterable JSON parser")
    (description
     "A fast Python JSON parser, implemented in Rust using PyO3.
Used by pydantic and the Anthropic SDK.")
    (license license:expat)))

(define-public python-fastuuid
  ;; Rust/PyO3 extension — installed from pre-built manylinux cp311 wheel.
  (package
    (name "python-fastuuid")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://files.pythonhosted.org/packages/fc/c2/"
             "f84c90167cc7765cb82b3ff7808057608b21c14a38531845d933a4637307/"
             "fastuuid-" version
             "-cp311-cp311-manylinux_2_17_x86_64.manylinux2014_x86_64.whl"))
       (sha256
        (base32 "1n4gmc3piiid8zcdfg80rjpvmqj4bvq2ff4gag93bd36bnqw9c5v"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out  (assoc-ref %outputs "out"))
                (site (string-append out "/lib/python3.11/site-packages"))
                (src  (assoc-ref %build-inputs "source"))
                (unzip (string-append (assoc-ref %build-inputs "unzip") "/bin/unzip")))
           (mkdir-p site)
           (invoke unzip "-d" site src)))))
    (native-inputs (list unzip))
    (supported-systems '("x86_64-linux"))
    (home-page "https://github.com/nickspring/fastuuid")
    (synopsis "Fast UUID generation via Rust bindings")
    (description
     "Provides faster UUID generation by wrapping Rust's UUID library
via PyO3 bindings.")
    (license license:expat)))


;;;
;;; LLM SDK packages
;;;

(define-public python-openai
  ;; Version 1.99.1 required by aider-chat 0.86.1; the Guix channel ships
  ;; 1.3.5 which lacks openai.lib._parsing (added in 1.20+).
  (package
    (name "python-openai")
    (version "1.99.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "openai" version))
       (sha256
        (base32 "03qjjdx8yw66hq8zrkcw2g3cm9m3ax175b5wjjxm33r9ii4qx79c"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-hatchling python-hatch-fancy-pypi-readme))
    (propagated-inputs
     (list python-anyio
           python-distro
           python-httpx
           python-jiter
           python-pydantic
           python-sniffio
           python-tqdm
           python-typing-extensions))
    (arguments (list #:tests? #f))
    (home-page "https://github.com/openai/openai-python")
    (synopsis "Python client library for the OpenAI API")
    (description
     "This package provides a Python client library for the OpenAI API.")
    (license license:expat)))

(define-public python-anthropic
  (package
    (name "python-anthropic")
    (version "0.84.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "anthropic" version))
       (sha256
        (base32 "0drwqh1vq4npqfw96ias1xmrj97skii17c3c6752vrpbb87gkxbj"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-hatchling python-hatch-fancy-pypi-readme))
    (propagated-inputs
     (list python-anyio
           python-distro
           python-docstring-parser
           python-httpx
           python-jiter
           python-pydantic
           python-sniffio
           python-typing-extensions))
    (arguments (list #:tests? #f))
    (home-page "https://github.com/anthropics/anthropic-sdk-python")
    (synopsis "Official Python library for the Anthropic API")
    (description
     "The official Python library for interacting with the Anthropic API,
providing access to Claude models.")
    (license license:expat)))

(define-public python-litellm
  (package
    (name "python-litellm")
    (version "1.75.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "litellm" version))
       (sha256
        (base32 "0qqx1ldfvb2pybjj5ch56g28z7cmf4gfgabcnfid970vkvkvyzzc"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-poetry-core))
    (propagated-inputs
     (list python-aiohttp
           python-click
           python-fastuuid
           python-httpx
           python-importlib-resources
           python-jinja2
           python-jsonschema
           python-openai
           python-pydantic
           python-dotenv
           python-tiktoken
           python-tokenizers))
    (arguments (list #:tests? #f))
    (home-page "https://github.com/BerriAI/litellm")
    (synopsis "Unified LLM API for 100+ providers")
    (description
     "LiteLLM provides a unified interface for calling LLM APIs from
OpenAI, Anthropic, Azure, Cohere, and 100+ other providers.")
    (license license:expat)))


;;;
;;; Aider itself
;;;

(define-public python-aider-chat
  (package
    (name "python-aider-chat")
    (version "0.86.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "aider_chat" version))
       (sha256
        (base32 "0ncih4cvm2by99p9a9nz7xdihw0il7350bgq7b0z02an5wr9v2pk"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-setuptools))
    (propagated-inputs
     (list python-aiohttp
           python-anthropic
           python-backoff
           python-beautifulsoup4
           python-certifi
           python-click
           python-configargparse
           python-diff-match-patch
           python-diskcache
           python-distro
           python-fastapi
           python-filelock
           python-gitpython
           python-grep-ast
           python-httpx
           python-huggingface-hub
           python-imgcat
           python-importlib-resources
           python-jinja2
           python-json5
           python-jsonschema
           python-litellm
           python-lox
           python-markdown-it-py
           python-networkx
           python-mixpanel
           python-openai
           python-oslex
           python-packaging
           python-pathspec
           python-pexpect
           python-pillow
           python-posthog
           python-prompt-toolkit
           python-psutil
           python-pydantic
           python-pydub
           python-pygments
           python-pypandoc
           python-pyperclip
           python-requests
           python-rich
           python-shtab
           python-six
           python-sounddevice
           python-soundfile
           python-tiktoken
           python-tokenizers
           python-tqdm
           python-tree-sitter-0.24
           python-tree-sitter-language-pack
           python-typer
           python-uvicorn))
    (arguments
     (list #:tests? #f
           #:phases
           #~(modify-phases %standard-phases
               (delete 'sanity-check))))
    (home-page "https://aider.chat")
    (synopsis "AI pair programming in your terminal")
    (description
     "Aider is an AI pair programming tool that runs in your terminal.
It works with your local git repository and uses LLMs (Claude, GPT-4, etc.)
to help you write and edit code.")
    (license license:asl2.0)))
