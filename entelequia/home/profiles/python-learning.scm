(define-module (entelequia home profiles python-learning)
  #:use-module (entelequia packages python-learning)
  #:export (python-learning-home-packages))

;;; Python learning home profile
;;;
;;; Tools for kids/beginners learning Python.  Opt-in: only attached
;;; to specific user homes (currently leandro on alucard and adrian
;;; on hopper).  Not part of any default profile.
;;;
;;; - thonny:        beginner IDE with step-by-step variable visualizer.
;;; - python-pgzero: zero-boilerplate 2D games framework (pgzrun).

(define (python-learning-home-packages)
  (list thonny python-pgzero))
