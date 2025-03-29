(in-package #:ten)

(defun demo-game-res ()
  (lets (
      shaders (hash
        :instancing-static (load-shader-to-gl
          (uiop:read-file-string "res/shaders/instancing_vertex.glsl")
          (uiop:read-file-string "res/shaders/instancing_fragment.glsl")
        )
        :instancing-skin (load-shader-to-gl
          (uiop:read-file-string "res/shaders/skin_instancing_vertex.glsl")
          (uiop:read-file-string "res/shaders/instancing_fragment.glsl")
        )
      )
      level-data (-> "res/castle/castle_desert.fbx" load-model-data)
      pose-keys (concatenate 'list
        (list :|entry|)
        ;; (list :|guard_0| :|guard_1| :|guard_2| :|guard_3|)
      )
      player-poses (last-> pose-keys
        (apply #'select-vals (-> level-data :pose))
        (map-by-key 'list 3)
        (mapcar (sfun x take x 3))
      )
      level-model (-> level-data load-model-to-gl load-gl-group)
      player-model (-> "res/chars/archer_anims.fbx" load-model-data load-model-to-gl load-gl-group)
      player-anims (-> player-model :anims vals into-vector)
      level-shapes (model-mesh-shapes level-data)
    )
    (make-assoc
      :level-model level-model
      :level-shapes level-shapes
      :player-model player-model
      :player-anims player-anims
      :player-poses player-poses
      :shaders shaders
    )
  )
)

(lets (
    player-height 2.2
    player-height/2 (/ player-height 2)
  )
  (defun demo-game-player (pos)
    (make-assoc
      :shape (char-shape 1/2 player-height (v+ pos (vector 0 player-height/2 0)))
    )
  )
  (defun player-pos (player)
    (-> player :shape :center (v- (vector 0 player-height/2 0)))
  )
)

(defun move-player (player mov)
  (update player
    (sfun s update s
      (sfun v with-vector-items (vx vy vz) v
        (with-vector-items (mx my mz) mov
          (vector mx my mz)
        )
      )
      :velocity
    )
    :shape
  )
)

(defun player-next (player time delta-time)
  (move-player player (-> (wasd-x0z) norm (v* (vvv 5))))
)

(defun non-player-next (player time delta-time)
  (move-player player #(0 0 0))
)

(defun demo-game-setup ()
  (lets (
      res (demo-game-res)
      player-poses (-> res :player-poses)
      players (mapcar #'demo-game-player player-poses)
    )
    (make-assoc
      :res res
      :state (make-assoc
        :campos (v+ (car player-poses) (vector 1 2 4))
        :camrot (vector 0 pi 0)
        :player (car players)
        :non-players (cdr players)
      )
      :next 'demo-game-next
      :display 'demo-game-display
    )
  )
)

(defun demo-game-next (dev res state)
  (with-maps-keys (
      ((time (dt :delta-time)) dev)
      ((campos camrot player non-players) state)
      ((level-shapes) res)
    )
    (lets (
        arrows (arrows-camrot)
        camrot (v+ (v* arrows (vvv* dt pi 1/2)) camrot)
        camdir (transform-vector (xyz->rotation camrot) #(0 0 1))
        mov (transform-vector (mat-rotation-y (aref camrot 1)) (v* (norm (wasd-xyz)) (vvv 10)))
        player (move-player player mov)
        players (cons player non-players)
        shapes (append (map-by-key 'list :shape players) level-shapes)
        shapes (shapes-fixed-cycle shapes dt)
        players (mapcar (sfun (p s) with-vals p :shape s) players shapes)
        player (car players)
        non-players (cdr players)
        player (player-next player time dt)
        non-players (mapcar (sfun p non-player-next p time dt) non-players)
      )
      (with-vals state
        :player player
        :non-players non-players
        :campos (v+ (player-pos player) #(0 2 0) (v* camdir (vvv -4)))
        :camrot camrot
      )
    )
  )
)

(defun debug-level-shapes (player level-shapes proj)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (-> proj mat-4x4->vec-16 gl:mult-matrix)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (with-map-keys (radius height center) (-> player :shape)
    (gl:with-pushed-matrix
      (applyv #'gl:translate center)
      (gl:scale (* radius 2) height (* radius 2))
      (gl:color 1 0 0 1)
      (glut:wire-cube 1)
    )
  )
  (gl:with-primitives :lines
    (with-maps-keys (((shape) player)
                     ((radius center) shape)
                     (((bounds shape)) #'shape-bounds))
      (loop for s in level-shapes do
        (when (bounds-intersectp (shape-bounds s) bounds)
          (loop for f in (-> s :triangles) do
            (gl:color 0 1 0 1)
            (with-items (a b c) (car f)
              (labels (
                  (line (a b)
                    (applyv #'gl:vertex a)
                    (applyv #'gl:vertex b)
                  )
                )
                (line a b)
                (line a c)
                (line b c)
              )
            )
          )
        )
      )
    )
  )
)

(defun demo-game-display (dev res state)
  (with-maps-keys (((level-model player-model player-anims shaders) res)
                   ((campos camrot player non-players) state)
                   (((w :width) (h :height) time) dev))
    (lets (
        players (cons player non-players)
        proj-mat (mul-mat-4x4
          (mat-perspective (/ w h) (/ pi 3) 1 1000)
          (mat-pos-rot-inversed campos camrot)
        )
        mat-stack (list (mat-identity 4))
        anim (map-key player-anims 0)
        instances (loop for pos in (mapcar #'player-pos players) collect
          (with-stack-push mat-stack (applyv 'mat-translation pos)
            (make-assoc
              :root (car mat-stack)
              :pose (animate anim time)
              :anim (make-assoc :index 0 :time time :length (-> anim :length))
            )
          )
        )
      )
      (gl:clear-color 1/2 1/2 1/2 1)
      (gl:clear :color-buffer-bit :depth-buffer-bit)
      (gl:viewport 0 0 w h)
      (display-gl-group-instanced
        player-model
        shaders
        instances
        :proj proj-mat)
      (display-gl-group level-model shaders :root (car mat-stack) :proj proj-mat)
      (debug-level-shapes player (-> res :level-shapes) proj-mat)
    )
  )
)

(defun start-demo-game ()
  (glut:display-window (make-instance 'window :setup-func 'demo-game-setup))
)
