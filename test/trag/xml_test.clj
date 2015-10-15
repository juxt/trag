;; Copyright © 2015, JUXT LTD.

(ns trag.xml-test
  (:require
   [juxt.iota :refer [given]]
   [clojure.test :refer :all]
   [clojure.xml :as xml]
   [clojure.java.io :as io]
   [trag.xml :refer [children tagp tag= attr= text attr-accessor]]))

(deftest xml-test []
  (let [doc (xml/parse (io/input-stream (io/resource "trag/doc.xml")))]

    (given doc
      identity :? some?
      :tag := :book
      [:content first :tag] := :frontmatter)

    (given (sequence children [doc])
      count := 3
      [(partial map :tag)] := [:frontmatter :chapter :chapter])

    (given (sequence (tag= :chapter) [doc])
      [(partial map :tag)] := [:chapter :chapter]
      [(partial map :tag) count] := 2)

    (given (sequence (comp (tag= :chapter)
                           (attr= :name "Conclusion"))
                     [doc])
      [(partial map :tag)] := [:chapter]
      [(partial map :tag) count] := 1
      [first :attrs :name] := "Conclusion")

    (is (= (sequence (comp (tag= :chapter)
                           (attr= :name "Introduction")
                           (tag= :para)
                           text)
                     [doc])
           ["Here is the intro" "Another paragraph"]))

    (is (=
         (sequence (comp
                    (tag= :chapter)
                    (map (juxt (attr-accessor :name)
                               #(sequence (comp (tag= :para) text) [%]))))
                   [doc])
         [["Introduction" ["Here is the intro" "Another paragraph"]]
          ["Conclusion" ["All done now"]]]))))
