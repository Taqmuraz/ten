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
      level-data (-> "res/arena/arena.fbx" load-model-data)
      player-poses (loop for i from 0 below 100 collect (list (floor i 10) 0 (mod i 10)))
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
      :look (list 0 0 1)
      :anim 0
      :shape (char-shape 1/2 player-height (l+ pos (list 0 player-height/2 0)))
    )
  )
  (defun player-pos (player)
    (-> player :shape :center (l- (list 0 player-height/2 0)))
  )
)

(defun move-player (player mov)
  (update player
    (sfun s update s
      (sfun v with-items (vx vy vz) v
        (with-items (mx my mz) mov
          (list mx my mz)
        )
      )
      :velocity
    )
    :shape
  )
)

(defun player-next (player cammat time delta-time)
  (lets (
      mov (last-> (wasd-x0z) norm (transform-vector cammat))
      no-move (-> mov len zerop)
      look (if no-move (-> player :look) mov)
      anim (if no-move 0 1)
      player (move-player player (l* mov (lll 3)))
    )
    (with-vals player
      :anim anim
      :look look
    )
  )
)

(defun non-player-next (player time delta-time)
  (move-player player (list 0 0 0))
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
        :campos (l+ (car player-poses) (list 1 2 4))
        :camrot (list 0 pi 0)
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
        camrot (l+ (l* arrows (lll* dt pi 1/2)) camrot)
        camdir (transform-vector (xyz->rotation camrot) (list 0 0 1))
        cammat (mat-rotation-y (elt camrot 1))
        players (cons player non-players)
        shapes (append (map-by-key 'list :shape players) level-shapes)
        shapes (shapes-fixed-cycle shapes dt)
        players (mapcar (sfun (p s) with-vals p :shape s) players shapes)
        player (car players)
        non-players (cdr players)
        player (player-next player cammat time dt)
        non-players (mapcar (sfun p non-player-next p time dt) non-players)
      )
      (with-vals state
        :player player
        :non-players non-players
        :campos (l+ (player-pos player) (list 0 2 0) (l* camdir (lll -4)))
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
        instances (loop for player in players collect
          (lets (
              anim-index (-> player :anim)
              anim (map-key player-anims anim-index)
              pos (player-pos player)
              look (-> player :look)
            )
            (with-stack-push mat-stack
              (mul-mat-4x4
                (applyv 'mat-translation pos)
                (-> look look->rotation xyz->rotation)
              )
              (make-assoc
                :root (car mat-stack)
                :pose (animate anim time)
                :anim (make-assoc :index anim-index :time time :length (-> anim :length))
              )
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
      ;; (debug-level-shapes player (-> res :level-shapes) proj-mat)
    )
  )
)

(defun start-demo-game ()
  (glut:display-window (make-instance 'window :setup-func 'demo-game-setup))
)
