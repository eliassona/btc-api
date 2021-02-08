(ns btc-api.int-test
  (:require [clojure.test :refer :all]
            [btc-api.core :refer :all]
            [btc-api.bx :refer :all])
  (:import  [btc_api.bx BxNode])
  )

(deftest lazy
  (let [n (BxNode.)]
    (is (= 5 (count (take 5 (block-headers n)))))
    )
  
  )