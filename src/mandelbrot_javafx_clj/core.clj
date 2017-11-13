(ns mandelbrot-javafx-clj.core
  (:import (javafx.scene.paint Color)
           (javafx.application Application)
           (javafx.stage Stage)
           (javafx.awt.Color)
           (javafx.scene Scene)
           (javafx.scene.layout.Pane)
           (javafx.scene.layout GridPane BorderPane HBox VBox)
           (javafx.geometry Pos Insets)
           (javafx.scene.text Text Font FontWeight)
           (javafx.scene.control Button ColorPicker Label)
           (javafx.event EventHandler Event)
           (javafx.embed.swing SwingFXUtils)
           (javafx.scene.image ImageView)
           (javafx.scene.canvas Canvas)
           )
  (:require [clojure.java.io :as io])
  (:gen-class
   :extends javafx.application.Application))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here is the function of calculating Mandelbrot sets / returns int RGB array ;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def width 600)
(def height 400)
(def depth 25)

(defn mandelbrotbean
  "mandelbrotbean \n
  x means x-position, y means y-position, \n
  opacity means javafx.scene.paint.Color instance \n
  Return: javafx.scene.paint.Color instance"
  [^clojure.lang.PersistentList pos
   ^javafx.scene.paint.Color color]
  (let [cx (first pos)
        cy (second pos)]
    (loop [x 0 y 0 times 1]
      (cond
        (== times depth) (.deriveColor color 0.0 1.0 0.0 0.0)
        (> (+ (* x x) (* y y)) 4) (.deriveColor color 0.0 1.0 (* 4 (/ 1 times)) 1.0)
        :else (recur (+ (* x x) (* -1.0 y y) cx)
                     (+ (* 2.0 x y) cy)
                     (inc times))))))

(defn write-image [int_list]
  (let [img (java.awt.image.BufferedImage. width height (java.awt.image.BufferedImage/TYPE_INT_ARGB))]
    (.setRGB img 0 0 width height  (int-array int_list) 0 width)
    img))

(defn mandelbrot
  "get mandelbrot list \n
  pos means {:x-min :x-max :y-min :y-max}, \n
  color means javafx.scene.paint.Color inst(def ex (atom nil))
  Return: Bufferedimage "
  [^clojure.lang.PersistentArrayMap pos
   ^javafx.scene.paint.Color color]
  (let [x-min (:x-min pos)
        x-max (:x-max pos)
        y-min (:y-min pos)
        y-max (:y-max pos)
        pos-data (for [k (range y-min y-max (/ (- y-max y-min) height))
                       i (range x-min x-max (/ (- x-max x-min ) width))] [i k])]
    (write-image (doall (for [pos-list pos-data]
                          (let [precolor (mandelbrotbean pos-list color)
                                recolor (java.awt.Color. (float (.getRed precolor))
                                                         (float (.getGreen precolor))
                                                         (float (.getBlue precolor))
                                                         (float (.getOpacity precolor)))]
                            (.getRGB recolor)))))))

(defn write-image-to-bufferedimage [^:java.awt.image.BufferedImage img]
  (SwingFXUtils/toFXImage img nil))

(defn mandelbrot-javafx [^clojure.lang.PersistentArrayMap pos
                         ^javafx.scene.paint.Color color]
  [pos color]
  (let [x-min (:x-min pos)
        x-max (:x-max pos)
        y-min (:y-min pos)
        y-max (:y-max pos)
        pos-data (for [k (range y-min y-max (/ (- y-max y-min) height))
                       i (range x-min x-max (/ (- x-max x-min ) width))] [i k])]
    (write-image-to-bufferedimage
     (write-image (doall (for [pos-list pos-data]
                           (let [precolor (mandelbrotbean pos-list color)
                                 recolor (java.awt.Color. (float (.getRed precolor))
                                                          (float (.getGreen precolor))
                                                          (float (.getBlue precolor))
                                                          (float (.getOpacity precolor)))]
                                                          (.getRGB recolor))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; You set below data, which is atomic data ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def initial-state {:pos {:x-mix -2 :x-max 1 :y-min -1 :y-max 1}
                    :color (Color/GREENYELLOW)
                    :root-stage? true
                    :redraw? false})

(def initial-data (mandelbrot-javafx {:x-min -2 :x-max 1 :y-min -1 :y-max 1} (Color/GREENYELLOW)))

;; initialize atomic data
(defonce data-data (ref initial-data))
(defonce data-state  (ref initial-state))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; You should add some button scene h-box and etc below ;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn set-text [^java.lang.String strings]
  (Text. strings))

(defn data-reset [^clojure.lang.PersistentArrayMap pos
                  ^javafx.scene.paint.Color color
                  ^java.lang.Boolean root-stage
                  ^clojure.lang.Ref new-data]
  (println "Call Function: data-reset")
  (dosync
   (ref-set
    data-state {:pos pos :color color
                :root-stage? root-stage :redraw? false})
   (ref-set
    data-data new-data)))

(defn set-image []
  (let [redraw (:redraw? @data-state)
        pos    (:pos @data-state)
        color (:color @data-state)
        root-stage (:root-stage? @data-state)]
    (if-not redraw
       @data-data
      (do
        (println "Call Function: mandelbrot")
        (data-reset pos color root-stage (mandelbrot-javafx pos color))
        (println "End Function: Imageview")
         @data-data))))


(def image (ImageView. ))
(defn set-writable-image []
  (let []
    (.setImage image (set-image))
    image))

(defn event-button-selected [^clojure.lang.Keyword keyword]
  (let [data-states @data-state
        pos (:pos data-states)
        color (:color data-states)
        root-stage? (:root-stage? data-state)
        redraw? (:redraw? data-state)
        x-min (:x-min pos)
        x-max (:x-max pos)
        x-quarter (/ (- x-max x-min) 4)
        y-min (:y-min pos)
        y-max (:y-max pos)
        y-quarter (/ (- y-max y-min) 4)]
    (println "call: " keyword)
    (cond
      (= keyword :Reset)
      (do
        (dosync
         (println "call: " keyword)
         (ref-set data-state
                  {:pos {:x-min -2 :x-max 1 :y-min -1 :y-max 1} :color (Color/GREENYELLOW)
                   :root-stage? false :redraw? true})
         (set-writable-image)))
      (= keyword :right)
      (do
        (println "call: " keyword)
        (dosync
         (ref-set data-state
                  {:pos {:x-min (+ x-min x-quarter)
                         :x-max (+ x-max x-quarter)
                         :y-min y-min :y-max y-max}
                   :color color
                   :root-stage? false :redraw? true})
         (set-writable-image)))
      (= keyword :left)
      (do
        (println "call: " keyword)
        (dosync
         (ref-set data-state
                  {:pos {:x-min (- x-min x-quarter)
                         :x-max (- x-max x-quarter)
                         :y-min y-min :y-max y-max}
                   :color color
                   :root-stage? false :redraw? true})
         (set-writable-image)))
      (= keyword :down)
      (do
        (println "call: " keyword)
        (dosync
         (ref-set data-state
                  {:pos {:x-min x-min :x-max x-max
                         :y-min (+ y-min y-quarter)
                         :y-max (+ y-max y-quarter)}
                   :color color
                   :root-stage? false :redraw? true})
         (set-writable-image)))
      (= keyword :up)
      (do
        (println "call: " keyword)
        (dosync
         (ref-set data-state
                  {:pos {:x-min x-min :x-max x-max
                         :y-min (- y-min y-quarter)
                         :y-max (- y-max y-quarter)}
                   :color color
                   :root-stage? false :redraw? true})
         (set-writable-image)))
      (= keyword :-)
      (do
        (println "call: " keyword)
        (dosync
         (ref-set data-state
                  {:pos {:x-min (- x-min x-quarter) :x-max (+ x-max x-quarter)
                         :y-min (- y-min y-quarter)
                         :y-max (+ y-max y-quarter)}
                   :color color
                   :root-stage? false :redraw? true})
         (set-writable-image)))
      (= keyword :+)
      (do
        (println "call: " keyword)
        (dosync
         (ref-set data-state
                  {:pos {:x-min (+ x-min x-quarter) :x-max (- x-max x-quarter)
                         :y-min (+ y-min y-quarter)
                         :y-max (- y-max y-quarter)}
                   :color color
                   :root-stage? false :redraw? true})
         (set-writable-image))))))

(defn set-button [^clojure.lang.Keyword keyword]
  (let [button (Button. (clojure.string/join (rest (str keyword))))]
    (doto button
      (.setOnAction (proxy [EventHandler] []
                      (handle [ActionEvent]
                        (event-button-selected keyword)))))
    button))

(defn event-color-pickup [^:ColorPicker this]
  (let [new-color (.getValue this)
        pos (:pos @data-state)
        root-stage? (:root-stage? @data-state)
        old-color (:color @data-state)]
    (when (not= new-color old-color)
      (dosync
       (println "Color Change!")
       (ref-set data-state
                {:pos pos :color new-color
                 :root-stage? root-stage? :redraw? true})
       (set-writable-image)))))

(defn set-color-picker []
  (let [color (ColorPicker. (javafx.scene.paint.Color/BLUE))]
    (doto color
      (.setOnAction (proxy [EventHandler] []
                      (handle [ActionEvent]
                        (event-color-pickup color)))))))

(defn set-border-pane []
  (let [top (set-button :up)
        bottom (set-button :down)
        right (set-button :right)
        left (set-button :left)
        border-pane (BorderPane. (set-button :Reset)
                                 top
                                 right
                                 bottom
                                 left)]
    (do
      (BorderPane/setAlignment top Pos/TOP_CENTER)
      (BorderPane/setAlignment right Pos/CENTER_RIGHT)
      (BorderPane/setAlignment bottom Pos/BOTTOM_CENTER)
      (BorderPane/setAlignment left Pos/CENTER_LEFT))
    border-pane))

(def gridpane (GridPane.))
(defn set-grid-pane []
  (let []
    (doto gridpane
      (.setAlignment Pos/CENTER)
      (.setHgap 10)
      (.setVgap 10)
      (.setPadding (Insets. 25 25 25 25))
      (.add (set-text "usage: " ) 0 0)
      (.add (set-text "put some button") 1 0)
      (.add (set-text "info: ") 0 1)
      (.add (set-text "take some seconds") 1 1)
      )))

(defn set-h-box []
  (let [hbox (doto (HBox. 5.0)
              (.setAlignment Pos/CENTER))
        _  (.add (.getChildren hbox) (set-grid-pane))
        _  (.add (.getChildren hbox) (set-color-picker))
        _  (.add (.getChildren hbox) (set-button :+))
        _  (.add (.getChildren hbox) (set-button :-))
        _  (.add (.getChildren hbox) (set-border-pane))]
    hbox))

(defn set-v-box []
  (let [vbox (doto (VBox. 10.0)
               (.setAlignment Pos/CENTER)
               ;;(-> .getChildren .add (set-text "development"))
              )
        _ (.add (.getChildren vbox) (doto (Text. "Fractals: Mandelbrot")
                                      (.setFont (Font/font "Verdana" 30))))
        _ (.add (.getChildren vbox) (set-h-box))
        _ (.add (.getChildren vbox) (set-writable-image))]
    vbox))

(defn set-scene []
  " arguments means children \n
  scene
    |- v-box :as set-v-box
         |- text (title) :as set-text
         |- h-box :as set-h-box
             |- grid-pane (view position) :as set-grid-pane
                    |- text (usage) 0 0 :as set-text-prop
                    |- text () 0 1 :as set-text-prop
                    |- text (info) 1 0 :as set-text-prop
                    |- text () 1 1 :as set-text-prop
             |- color-picker ! redraw :as set-color-picker :with event-color-pickup
             |- button (bigger) ! redraw :as set-button
             |- button (smaller) ! redraw :as set-button
             |- border-pane :as set-border-pane
                    |- top - button (up) ! redraw :as set-button
                    |- bottom - button (down) ! redraw :as set-button
                    |- right - button (right) ! redraw :as set-button
                    |- left - button (left) ! redraw :as set-button 
                    |- center - button (reset) ! redraw :as set-button
         |- canvas - writable-image (-> pixel-writer -> set-pixels) :as set-writable-image
         |- text (some-information) :as set-text 
  ref
    color - javafx.scene.Color.
    data - int-rgb-list (<- getRGB <- java.awt.color <-get-red/green/blue/opacity )
    root-stop? - force-exit
    redraw? - need of redrawing
  "
  (let []
    (doto (Scene. (set-v-box) (+ 250 width) (+ 200 height)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NECESSARY UTILITY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn force-exit
  " This is closed function \n"
  [root-stage?]
  (reify javafx.event.EventHandler
    (handle [this event]
      (when (not root-stage?)
        (do (println "Closing application")
            (javafx.application.Platform/exit))))))

(defn swap
  " This is change state of initialize \n"
  [root-stage?]
  (dosync (ref-set data-state {:root-stage? root-stage?
                               :pos {:x-min -2 :x-max 1 :y-min -1 :y-max 1}
                               :color (Color/GREENYELLOW)
                               :redraw? false})))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
