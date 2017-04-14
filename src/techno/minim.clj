;; (ns techno.minim)

;; (gen-class
;;  :name techno.minim.FileHandler
;;  :state state
;;  :init init
;;  :prefix "-"
;;  :main false
;;  :methods [[sketchPath [String] String]
;;            [createInput [String] InputStream]]
;;  (defn -init []
;;    [[] (atom {:base (str (.getCanonicalPath (clojure.java.io/file ".")) "\\musicradar-drum-samples\\Drum Kits")})])
;;  (defn -sketchPath [this file-name]
;;    (str @(.state this) file-name))
;;  (defn -createInput [this file-name]
;;    (FileInputStream. (str @(.state this) file-name))))
