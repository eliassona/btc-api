(ns btc-api.core-test
  (:require [clojure.test :refer :all]
            [base58.core :as base58]
            [btc-api.core :refer :all]))



(deftest test-base58
  (is (= "2NEpo7TZRRrLZSi2U" (base58/encode "Hello World!" )))
  (is (= "Hello World!" (base58/decode "2NEpo7TZRRrLZSi2U" )))
  )
