#lang racket/base

(require (only-in ffi/unsafe register-finalizer)
         ffi/vector
         opengl
         opengl/util
         racket/class
         racket/contract
         racket/gui/base
         racket/match
         racket/string
         threading
         toolbox/color
         toolbox/who
         "../contract.rkt"
         "../geometry.rkt")

(provide (maybe-contract-out
          [float-size exact-nonnegative-integer?]
          [uint-size exact-nonnegative-integer?]
          [transparent-color rgb?]

          [reverse-pixel-format exact-nonnegative-integer?]

          [make-gl-config (->* [] [#:hires-mode? any/c
                                   #:legacy? any/c
                                   #:multisample (integer-in 0 256)
                                   #:sync-swap? any/c]
                               (is-a?/c gl-config%))]
          [default-gl-config (is-a?/c gl-config%)]
          [current-gl-config (parameter/c (is-a?/c gl-config%))]

          [check-have-gl-context (-> symbol? (is-a?/c gl-context<%>))]

          [gl-gen-vertex-array (-> exact-nonnegative-integer?)]
          [gl-gen-buffer (-> exact-nonnegative-integer?)]
          [gl-gen-texture (-> exact-nonnegative-integer?)]
          [gl-gen-framebuffer (-> exact-nonnegative-integer?)]

          [tf->mat3 (-> transformation? f32vector?)]
          [glUniformMatrix3f/tf (-> exact-nonnegative-integer? transformation? void?)]
          [tf->glsl-mat3 (-> transformation? string?)]

          [program? predicate/c]
          [program-id (-> program? exact-nonnegative-integer?)]
          [link-program (->* [string? string?] [(listof symbol?)] program?)]
          [use-program! (-> program? (cons/c procedure? (cons/c symbol? list?)) ... void?)]
          [program-bind! (-> program? (cons/c procedure? (cons/c symbol? list?)) ... void?)]

          [gl-texture? predicate/c]
          [gl-texture-id (-> gl-texture? exact-nonnegative-integer?)]
          [make-gl-texture (->* [] [(or/c (is-a?/c bitmap%) #f)] gl-texture?)]
          [bind-gl-texture! (->* [gl-texture?] [exact-nonnegative-integer?] void?)]
          [gl-texture-load-bitmap! (-> gl-texture? (is-a?/c bitmap%) void?)]))

;; -----------------------------------------------------------------------------

(define float-size (gl-type-sizeof GL_FLOAT))
(define uint-size (gl-type-sizeof GL_UNSIGNED_INT))

(define transparent-color (rgb 0.0 0.0 0.0 0.0))

(define reverse-pixel-format
  (if (system-big-endian?)
      GL_UNSIGNED_INT_8_8_8_8_REV
      GL_UNSIGNED_INT_8_8_8_8))

(define (make-gl-config #:hires-mode? [hires-mode? #t]
                        #:legacy? [legacy? #f]
                        #:multisample [multisample-size 4]
                        #:sync-swap? [sync-swap? #f])
  (define config (new gl-config%))
  (send config set-hires-mode hires-mode?)
  (send config set-legacy? legacy?)
  (send config set-multisample-size multisample-size)
  (send config set-sync-swap sync-swap?)
  config)

(define default-gl-config (make-gl-config))

(define current-gl-config (make-parameter default-gl-config))

(define (check-have-gl-context who)
  (define ctx (get-current-gl-context))
  (unless ctx
    (raise-arguments-error who "no current gl-context<%>"))
  (unless (send ctx ok?)
    (raise-arguments-error who "current gl-context<%> is not ok"))
  ctx)

;; -----------------------------------------------------------------------------

(define (gl-gen-vertex-array)
  (u32vector-ref (glGenVertexArrays 1) 0))
(define (gl-gen-buffer)
  (u32vector-ref (glGenBuffers 1) 0))
(define (gl-gen-texture)
  (u32vector-ref (glGenTextures 1) 0))
(define (gl-gen-framebuffer)
  (u32vector-ref (glGenFramebuffers 1) 0))

;; -----------------------------------------------------------------------------

(define (tf->mat3 t)
  (match-define (transformation xx yx x0 xy yy y0) t)
  (f32vector (real->double-flonum xx)
             (real->double-flonum xy)
             0.0
             (real->double-flonum yx)
             (real->double-flonum yy)
             0.0
             (real->double-flonum x0)
             (real->double-flonum y0)
             1.0))

(define (glUniformMatrix3f/tf id tf)
  (glUniformMatrix3fv id 1 #f (tf->mat3 tf)))

(define (tf->glsl-mat3 t)
  (~> (f32vector->list (tf->mat3 t))
      (map number->string _)
      (string-join ",")
      (string-append "mat3(" _ ")")))

;; -----------------------------------------------------------------------------

(struct program (id uniforms) #:transparent)

(define/who (link-program vs-str fs-str [uniforms '()])
  (define ctx (check-have-gl-context who))

  (define vs (load-shader (open-input-string vs-str 'vertex-shader) GL_VERTEX_SHADER))
  (define fs (load-shader (open-input-string fs-str 'fragment-shader) GL_FRAGMENT_SHADER))
  (define id (create-program vs fs))
  (glDeleteShader vs)
  (glDeleteShader fs)

  (define prog
    (program
     id
     (for/hasheq ([uniform (in-list uniforms)])
       (values uniform (glGetUniformLocation id (symbol->string uniform))))))

  (register-finalizer
   prog
   (λ (prog)
     (when (send ctx ok?)
       (send ctx call-as-current (λ () (glDeleteProgram id))))))

  prog)

(define/who (use-program! prog . uniform-vals)
  (check-have-gl-context who)
  (glUseProgram (program-id prog))
  (apply program-bind! prog uniform-vals))

(define/who (program-bind! prog . uniform-vals)
  (check-have-gl-context who)
  (define uniforms (program-uniforms prog))
  (for ([uniform-val (in-list uniform-vals)])
    (match-define (list* proc name args) uniform-val)
    (apply proc (hash-ref uniforms name) args)))

;; -----------------------------------------------------------------------------

(struct gl-texture (id) #:transparent)

(define/who (make-gl-texture [bmp #f])
  (define ctx (check-have-gl-context who))
  (define id (gl-gen-texture))
  (define tex (gl-texture id))
  (register-finalizer
   tex
   (λ (tex)
     (when (send ctx ok?)
       (send ctx call-as-current
             (λ () (glDeleteTextures 1 (u32vector id)))))))

  (glBindTexture GL_TEXTURE_2D id)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
  (when bmp
    (gl-texture-load-bitmap! tex bmp))

  tex)

(define/who (bind-gl-texture! tex [target GL_TEXTURE_2D])
  (check-have-gl-context who)
  (glBindTexture target (gl-texture-id tex)))

(define/who (gl-texture-load-bitmap! tex bmp)
  (check-have-gl-context who)

  (define w (send bmp get-width))
  (define h (send bmp get-height))
  (define pixels (make-bytes (* w h 4)))
  (send bmp get-argb-pixels 0 0 w h pixels #f #t)

  (bind-gl-texture! tex)
  (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA8 w h 0 GL_BGRA reverse-pixel-format pixels))
