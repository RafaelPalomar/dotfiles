(define-module (entelequia home profiles documentation)
  #:use-module (gnu packages)
  #:use-module (entelequia packages entelequia-docs)
  #:export (documentation-home-packages))

(define documentation-home-packages
  "Documentation packages for the Entelequia system.

This includes the comprehensive Entelequia dotfiles documentation
in both Info manual and HTML formats, accessible via 'info entelequia'
or in Emacs with C-h i m entelequia RET."
  (list entelequia-docs))
