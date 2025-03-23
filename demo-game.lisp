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
        (loop for i from 0 to 5 collect (keyword-of "guard_" (into-string i)))
        (loop for i from 0 to 5 collect (keyword-of "int_" (into-string i)))
        (loop for i from 0 to 7 collect (keyword-of "nav_" (into-string i)))
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

(defun demo-game-player (pos)
  (make-assoc
    :shape (sphere-shape 1 (v+ pos #(0 1 0)))
  )
)

(defun move-player (player mov)
  (update player
    (sfun s with-map-keys (radius center bounds) s
      (lets (
          center (v+ center mov)
          bounds (sphere-bounds radius center)
        )
        (with-vals s
          :center center
          :bounds bounds
        )
      )
    )
    :shape
  )
)

(defun player-pos (player)
  (-> player :shape :center (v- #(0 1/2 0)))
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
      (((dt :delta-time)) dev)
      ((campos camrot player non-players) state)
      ((level-shapes) res)
    )
    (lets (
        cammat (applyv 'mat-rotation camrot)
        mov (transform-vector cammat (v* (norm (wasd-xyz)) (vvv* dt 10)))
        player (move-player player mov)
        players (cons player non-players)
        shapes (append (map-by-key 'list :shape players) level-shapes)
        shapes (-> shapes shapes-tree (shapes-tree-sim dt))
        players (mapcar (sfun (p s) with-vals p :shape s) players shapes)
        player (car players)
        non-players (cdr players)
      )
      (with-vals state
        :player player
        :non-players non-players
        :campos (v+ (player-pos player) #(0 2 4))
      )
    )
  )
)

(defun debug-level-shapes (player level-shapes campos camrot proj)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (-> proj mat-4x4->vec-16 gl:mult-matrix)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:with-pushed-matrix
    (-> (mat-pos-rot-inversed campos camrot) mat-4x4->vec-16 gl:mult-matrix)
    (gl:with-pushed-matrix
      (applyv #'gl:translate (-> player :shape :center))
      (gl:rotate 90 1 0 0)
      (gl:color 1 0 0 1)
      (glut:wire-sphere (-> player :shape :radius) 16 16)
    )
    (gl:with-primitives :lines
      (gl:color 0 0 1 1)
      (with-maps-keys (((shape) player)
                       ((bounds) shape))
        (loop for s in level-shapes do
          (when (bounds-intersectp (-> s :bounds) bounds)
            (gl:color 0 1 0 1)
            (loop for f across (-> s :triangles) do
              (with-items (a b c) (car f)
                (applyv #'gl:vertex a)
                (applyv #'gl:vertex b)
                (applyv #'gl:vertex a)
                (applyv #'gl:vertex c)
                (applyv #'gl:vertex b)
                (applyv #'gl:vertex c)
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
        proj-mat (mat-perspective (/ w h) (/ pi 3) 1 1000)
        mat-stack (list (mat-pos-rot-inversed campos camrot))
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
      (debug-level-shapes player (-> res :level-shapes) campos camrot proj-mat)
    )
  )
)

(defun start-demo-game ()
  (glut:display-window (make-instance 'window :setup-func 'demo-game-setup))
)
