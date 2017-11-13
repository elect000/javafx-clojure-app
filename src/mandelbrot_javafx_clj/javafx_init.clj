(ns mandelbrot-javafx-clj.javafx-init
     (:require [mandelbrot-javafx-clj.core :as core])
     (:import (javafx.application Application)
              (javafx.stage Stage))
     (:gen-class
   :extends javafx.application.Application))

(defn -start
  " This is javafx-start funnction \n"
  [this ^Stage stage]
  (let []
    (doto stage
      (.setTitle "Fractals: Mandelbrot")
      (.setOnCloseRequest (core/force-exit {:root-stage? false}))
      (.setScene (core/set-scene))
      ;;(core/root-stage {:root-stage? false})
      .show)))

(defn -main
  " This is javafx-launch function \n"
  [& args]
  (core/swap false)
  (Application/launch mandelbrot_javafx_clj.javafx_init (into-array String [])))
