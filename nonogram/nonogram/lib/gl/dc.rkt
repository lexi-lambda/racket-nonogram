#lang at-exp racket/base

(require (only-in ffi/unsafe _float memcpy register-finalizer)
         ffi/vector
         opengl
         racket/class
         racket/contract
         racket/flonum
         racket/format
         racket/match
         toolbox/color
         toolbox/list
         toolbox/pict
         toolbox/who
         "../atlas.rkt"
         "../contract.rkt"
         "../geometry.rkt"
         "core.rkt")

(provide (maybe-contract-out
          [gl-dc? predicate/c]
          [make-gl-dc (->* [#:atlas atlas?
                            #:texture gl-texture?]
                           [#:usage exact-nonnegative-integer?]
                           gl-dc?)]
          [gl-dc-draw (-> gl-dc? void?)]
          [gl-dc-clear! (-> gl-dc? void?)]

          [current-gl-dc-transform (parameter/c transformation?)]
          [texture-mode/c flat-contract?]
          [gl-dc-add-rectangle! (->* [gl-dc? rect?]
                                     [#:color rgb?
                                      #:texture (or/c rect? pict? #f)
                                      #:texture-mode texture-mode/c]
                                     void?)]

          [tf:diag-hatch (->* [#:divisions exact-integer?
                               #:index real?]
                              [#:scale real?]
                              transformation?)]
          [link-gl-dc-program (-> program?)]
          [use-gl-dc-program! (->* [program?]
                                   [#:transform transformation?
                                    #:alpha (real-in 0 1)
                                    #:hatch-divisions exact-integer?
                                    #:hatch-transform transformation?]
                                   void?)]
          [gl-dc-program-bind! (->* [program?]
                                    [#:transform (or/c transformation? #f)
                                     #:alpha (or/c (real-in 0 1) #f)
                                     #:hatch-divisions (or/c exact-integer? #f)
                                     #:hatch-transform (or/c transformation? #f)]
                                    void?)]))

;; -----------------------------------------------------------------------------

(struct gl-dc
  (atlas
   texture
   vao
   byte-vbo
   float-vbo
   buffer-usage
   [dirty? #:mutable]
   [vertex-count #:mutable]
   [byte-data #:mutable]
   [float-data #:mutable])
  #:transparent)

(define float-vertex-attrib-sizes
  '(2   ;; position
    4   ;; color
    2)) ;; texture coord

(define float-data-stride (apply + float-vertex-attrib-sizes))
(define float-data-stride/bytes (* float-data-stride float-size))

(define/who (make-gl-dc #:atlas atl
                        #:texture tex
                        #:usage [buffer-usage GL_STREAM_DRAW])
  (define ctx (check-have-gl-context who))

  (define vao (gl-gen-vertex-array))
  (glBindVertexArray vao)

  (define byte-vbo (gl-gen-buffer))
  (glBindBuffer GL_ARRAY_BUFFER byte-vbo)
  (glVertexAttribIPointer 0 1 GL_UNSIGNED_BYTE 0 0)
  (glEnableVertexAttribArray 0)

  (define float-vbo (gl-gen-buffer))
  (glBindBuffer GL_ARRAY_BUFFER float-vbo)
  (for/fold ([j 0])
            ([size (in-list float-vertex-attrib-sizes)]
             [i (in-naturals 1)])
    (glVertexAttribPointer i size GL_FLOAT #f float-data-stride/bytes j)
    (glEnableVertexAttribArray i)
    (+ j (* size float-size)))

  (define dc
    (gl-dc atl
           tex
           vao
           byte-vbo
           float-vbo
           buffer-usage
           #t
           0
           (make-bytes 128)
           (make-f32vector 512)))

  (register-finalizer
   dc
   (λ (dc)
     (when (send ctx ok?)
       (send ctx call-as-current
             (λ ()
               (glDeleteVertexArrays 1 (u32vector vao))
               (glDeleteBuffers 2 (u32vector byte-vbo float-vbo)))))))

  dc)

(define (load-texture-data bmp)
  (define w (send bmp get-width))
  (define h (send bmp get-height))
  (define pixels (make-bytes (* w h 4)))
  (send bmp get-argb-pixels 0 0 w h pixels #f #t)
  (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA8 w h 0 GL_BGRA reverse-pixel-format pixels))

(define (gl-dc-clear! dc)
  (set-gl-dc-vertex-count! dc 0)
  (set-gl-dc-dirty?! dc #t))

(define/who (gl-dc-draw dc)
  (check-have-gl-context who)
  (bind-gl-texture! (gl-dc-texture dc))

  (glBindVertexArray (gl-dc-vao dc))
  (define vertex-count (gl-dc-vertex-count dc))
  (when (gl-dc-dirty? dc)
    (define buffer-usage (gl-dc-buffer-usage dc))
    (glBindBuffer GL_ARRAY_BUFFER (gl-dc-byte-vbo dc))
    (glBufferData GL_ARRAY_BUFFER
                  vertex-count
                  (gl-dc-byte-data dc)
                  buffer-usage)
    (glBindBuffer GL_ARRAY_BUFFER (gl-dc-float-vbo dc))
    (glBufferData GL_ARRAY_BUFFER
                  (* vertex-count float-data-stride/bytes)
                  (gl-dc-float-data dc)
                  buffer-usage)

    (set-gl-dc-dirty?! dc #f)
    #;(printf "vertices: ~v (~v bytes)\n" vertex-count (* vertex-count (+ float-data-stride/bytes 1))))

  (glDrawArrays GL_TRIANGLES 0 vertex-count))

(define current-gl-dc-transform (make-parameter tf:identity))

(define (gl-dc-add-vertex! dc p
                           #:color color
                           #:texture-mode texture-mode
                           #:texture-coord texture-p
                           #:transform tf)
  (define i (gl-dc-vertex-count dc))
  (define fi (* i float-data-stride))

  (define byte-data
    (let ()
      (define data (gl-dc-byte-data dc))
      (define len (bytes-length data))
      (cond
        [(= i len)
         (define data* (make-bytes (* len 2)))
         (bytes-copy! data* 0 data)
         (set-gl-dc-byte-data! dc data*)
         data*]
        [else data])))

  (define float-data
    (let ()
      (define data (gl-dc-float-data dc))
      (define len (f32vector-length data))
      (cond
        [(< (- len fi) float-data-stride)
         (define data* (make-f32vector (* len 2)))
         (memcpy (f32vector->cpointer data*)
                 (f32vector->cpointer data)
                 len
                 _float)
         (set-gl-dc-float-data! dc data*)
         data*]
        [else data])))

  (bytes-set! byte-data i (texture-mode->int texture-mode))

  (define p* (tf* tf p))
  (f32vector-set! float-data (+ fi 0) (real->double-flonum (point-x p*)))
  (f32vector-set! float-data (+ fi 1) (real->double-flonum (point-y p*)))
  (f32vector-set! float-data (+ fi 2) (real->double-flonum (rgb-red color)))
  (f32vector-set! float-data (+ fi 3) (real->double-flonum (rgb-green color)))
  (f32vector-set! float-data (+ fi 4) (real->double-flonum (rgb-blue color)))
  (f32vector-set! float-data (+ fi 5) (real->double-flonum (rgb-alpha color)))
  (f32vector-set! float-data (+ fi 6) (real->double-flonum (point-x texture-p)))
  (f32vector-set! float-data (+ fi 7) (real->double-flonum (point-y texture-p)))

  (set-gl-dc-dirty?! dc #t)
  (set-gl-dc-vertex-count! dc (add1 i)))

(define (gl-dc-add-rectangle! dc r
                              #:color [color transparent-color]
                              #:texture [texture-spec #f]
                              #:texture-mode [texture-mode (if texture-spec 'texture 'solid)]
                              #:transform [tf (current-gl-dc-transform)])
  (define texture-r
    (match texture-spec
      [(? rect?) texture-spec]
      [(? pict?) (atlas-child-rect (gl-dc-atlas dc) texture-spec)]
      [#f        zero-rect]))

  (define (add-vertex! rect-point)
    (gl-dc-add-vertex! dc (rect-point r)
                       #:color color
                       #:texture-mode texture-mode
                       #:texture-coord (rect-point texture-r)
                       #:transform tf))

  (add-vertex! rect-tl)
  (add-vertex! rect-bl)
  (add-vertex! rect-tr)
  (add-vertex! rect-bl)
  (add-vertex! rect-tr)
  (add-vertex! rect-br))

;; -----------------------------------------------------------------------------

(define texture-mode/c (or/c 'solid 'texture 'mask 'inverted-mask))
(define (texture-mode->int mode)
  (match mode
    ['solid         0]
    ['texture       1]
    ['mask          2]
    ['inverted-mask 3]))

(define vs:dc
  @~a{#version 330 core
      layout(location = 0) in uint flagsIn;
      layout(location = 1) in vec2 positionIn;
      layout(location = 2) in vec4 colorIn;
      layout(location = 3) in vec2 tex_positionIn;

      flat out uint flags;
      out vec4 color;
      out vec2 tex_position;
      out vec2 hatch_position;

      uniform mat3 transform;
      uniform mat3 hatch_tf;

      void main() {
        gl_Position.xyw = transform * vec3(positionIn, 1);
        gl_Position.z = 0;

        flags = flagsIn;
        color = colorIn;
        tex_position = tex_positionIn;
        hatch_position = (hatch_tf * vec3(positionIn, 1)).xy;
      }})

(define fs:dc
  @~a{#version 330 core
      out vec4 fragColor;

      flat in uint flags;
      in vec4 color;
      uniform float alpha;

      uniform sampler2D tex;
      in vec2 tex_position;

      // used only for rendering overlapping cursors
      uniform int hatch_divisions;
      in vec2 hatch_position;

      void main() {
        uint textureMode = flags;

        if (hatch_divisions >= 2) {
          int stripe = int(floor(fract(hatch_position.x) * hatch_divisions));
          if (stripe != 0) discard;
        }

        // color data currently uses unassociated alpha
        vec4 colorAssoc;
        colorAssoc.rgb = color.rgb * color.a;
        colorAssoc.a = color.a;

        switch (int(textureMode)) {
          case @texture-mode->int['solid]:
            fragColor = colorAssoc;
            break;
          case @texture-mode->int['texture]:
            fragColor = texture(tex, tex_position);
            break;
          case @texture-mode->int['mask]:
            fragColor = colorAssoc * texture(tex, tex_position).a;
            break;
          case @texture-mode->int['inverted-mask]:
            fragColor = colorAssoc * (1.0 - texture(tex, tex_position).a);
            break;
        }

        fragColor *= alpha;
      }})

(define (tf:hatch #:divisions divisions
                  #:index index
                  #:angle angle
                  #:scale [scale 1.0])
  (tf* (tf:translate (- (/ index (->fl divisions))) 0.0)
       (tf:scale scale)
       (tf:rotate (- angle))))

(define (tf:diag-hatch #:divisions divisions
                       #:index index
                       #:scale [scale 1.0])
  (tf:hatch #:divisions divisions
            #:index index
            #:angle (turns -1/8)
            #:scale (/ scale (sqrt 2))))

(define (link-gl-dc-program)
  (link-program vs:dc fs:dc '(transform alpha hatch_divisions hatch_index hatch_tf)))

(define (use-gl-dc-program! prog
                            #:transform [tf tf:identity]
                            #:alpha [alpha 1.0]
                            #:hatch-divisions [hatch-divisions 0]
                            #:hatch-transform [hatch-tf tf:identity])
  (use-program!
   prog
   (list glUniformMatrix3f/tf 'transform tf)
   (list glUniform1f 'alpha (real->double-flonum alpha))
   (list glUniform1i 'hatch_divisions hatch-divisions)
   (list glUniformMatrix3f/tf 'hatch_tf hatch-tf)))

(define (gl-dc-program-bind! prog
                             #:transform [tf #f]
                             #:alpha [alpha #f]
                             #:hatch-divisions [hatch-divisions #f]
                             #:hatch-transform [hatch-tf #f])
  (apply program-bind! prog
         (append
          (when/list tf
            (list glUniformMatrix3f/tf 'transform tf))
          (when/list alpha
            (list glUniform1f 'alpha (real->double-flonum alpha)))
          (when/list hatch-divisions
            (list glUniform1i 'hatch_divisions hatch-divisions))
          (when/list hatch-tf
            (list glUniformMatrix3f/tf 'hatch_tf hatch-tf)))))
