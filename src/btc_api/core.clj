(ns btc-api.core
 ; (:require [base58.core :as base58])
  (:import [java.util.concurrent TimeUnit]
           [java.nio.charset StandardCharsets]
           [java.security SecureRandom MessageDigest]
           [org.apache.commons.codec.digest DigestUtils]
           [java.util Calendar]
           [org.bitcoinj.core Base58]))

(defmacro dbg [body]
  `(let [x# ~body]
     (println "dbg:" '~body "=" x#)
     x#))

(def btc-start-reward  50)
(def halving 210000)  
(def btc-max-circulation 21e6)
(def reward-period-in-minutes 10)
(def rewards-per-day (* 24 (/ 60 reward-period-in-minutes)))
(def rewards-per-year (* 365 rewards-per-day))

(defn blocks-after-halving [h] (mod h halving))
(defn blocks-until-halving [h] (- halving (blocks-after-halving h)))

(defn blocks [height]
  (let [b (map (fn [_] halving) (range (int (/ height halving))))
        bh (blocks-after-halving height)]
    (if (> bh 0)
      (concat b [bh])
      b)))


(defn rewards 
  ([] (rewards btc-start-reward))
  ([r] (lazy-seq (cons r (rewards (/ r 2))))))

(defn block-and-rewards [height]
  (let [b (blocks height)]
    (map (fn [b r] [b r]) b (take (count b) (rewards)))))


(defn block-and-reward-pair 
  [height]
  (let [bh (reverse (block-and-rewards height))]
    (mapcat (fn [[h r]] (map (fn [_] r) (range h))) bh)))

(defn reward-of [height] (-> height  block-and-rewards last second))

(defn circulation-btc [height]
  (long (reduce (fn [acc [b r]] (+ acc (* b r))) 0 (block-and-rewards height))))


(defn stock->flow-of [height]
  (let [now (circulation-btc height)
        inflation (- now (circulation-btc (max (- height rewards-per-year) 0)))
        ]
    (/ now inflation)))


(defn sf-marketcap-of [sf]
  (* (Math/exp 14.6) (Math/pow sf 3.3)))

(defn sf-price-btc-of [height]
  (/ (-> height stock->flow-of sf-marketcap-of) (circulation-btc height)))

(defn height->time [height current-height]
  (+ (* (- height current-height) (.toMillis (TimeUnit/MINUTES) reward-period-in-minutes)) 
    (System/currentTimeMillis)))


(defn time->height [time current-height]
  (let [ct (System/currentTimeMillis)
        diff (dbg (long (- ct time)))
        ]
    (- current-height (/ diff (.toMillis (TimeUnit/MINUTES) reward-period-in-minutes)))))

(defn time-millis->date [time]
  (let [c (Calendar/getInstance)]
    (.setTimeInMillis c time)
    {:year (.get c Calendar/YEAR)
     :month (inc (.get c Calendar/MONTH))
     :day (.get c Calendar/DAY_OF_MONTH)
     :hour (.get c Calendar/HOUR),
     :minute (.get c Calendar/MINUTE)}))
  


(defn date->time-millis [date]
  (let [c (Calendar/getInstance)]
    (.set c Calendar/YEAR (:year date))
    (.set c Calendar/MONTH (dec (:month date)))
    (.set c Calendar/DAY_OF_MONTH (:day date))
    (.set c Calendar/HOUR (:hour date))
    (.set c Calendar/MINUTE (:minute date))
    (.set c Calendar/SECOND 0)
    (.set c Calendar/MILLISECOND 0)
    (.set c Calendar/ZONE_OFFSET (* 1000 60 60))
    (.set c Calendar/DST_OFFSET (* 1000 60 60))
    (.getTimeInMillis c)
  ))



(defprotocol Node
  (current-height [node])
  (block-hash-of [node height])
  (block-header-of [node height-or-hash])
  (block-of [node height-or-hash])
  (tx-of [node tx-hash])
  )

(defprotocol Crypto
  (sha160-of [this base16-value])
  (sha256-of [this base16-value])
  (sha512-of [this base16-value])
  (ripemd160-of [this base16-value])
  (base16-decode-of [this base16-value])
  (base16-encode-of [this data])
  (base58-decode-of [this base58-value])
  (base58-encode-of [this data])
  (base64-decode-of [this base64-value])
  (base64-encode-of [this data])
  
  (script-decode-of [this base-16-script]) 
  (script-encode-of [this script]) 
  (script-to-address-of [this script])
  (tx-decode-of [this base-16-tx])
  (tx-encode-of [this tx-hash index payment-address sats])
  (input-sign-of [this ec-private-key contract base-16-tx])
  (input-set-of [this endorsement-script base-16-tx])    
  (input-validate-of [this ec-public-key contract endorsement base-16-tx])
  
    )


(defprotocol Exchange 
  (tickers [site])
  (ticker [site symbol])
  )


(defn block-headers 
  ([node] (block-headers node 0 (.current-height node)))
  ([node i n] (if (< i n)
                (lazy-seq (cons (.block-header-of node i) (block-headers node (inc i) n)))
                '())))
#_(defn blocks 
   ([node] (blocks node 0 (.current-height node)))
   ([node i n] (if (< i n)
                 (lazy-seq (cons (.block-of node i) (blocks node (inc i) n)))
                 '())))


(defn transactions-of [height-or-hash-or-block node] 
  (map #(.tx-of node %) 
       (:tx 
         (if (map? height-or-hash-or-block)
           height-or-hash-or-block    
           (.block-of node height-or-hash-or-block)))))



(defn transactions 
  ([node] (transactions node 0 (.current-height node)))
  ([node i n] (if (< i n)
                (lazy-seq (concat (transactions node (inc i) n) (transactions-of i node)))
                '())))



(defn marketcap 
  ([height exchange-site ticker-symbol]
    (marketcap height ((.ticker exchange-site ticker-symbol) "last")))
  ([height rate]
  (* (circulation-btc height) rate)))


(defn hex-str->number [hex-str]
  (BigInteger. hex-str, 16))

(defn number->hex-str [v]
  (.toString v 16))

  

(defn gen-private-key []
  (BigInteger. 256 (SecureRandom/getInstanceStrong)))
  
(defn sha-256 [bytes]
  (let [md (MessageDigest/getInstance "SHA-256")]
    (.digest md bytes)))
    
(defn hexify [ba]
  (apply str
    (map #(format "%02x" (bit-and % 255)) ba)))

(defn checksum [extended]
  (->> extended DigestUtils/sha256Hex DigestUtils/sha256Hex (take 8)))

#_(defn private-key->wif [pk]
   (let [extended (format "80%s" pk )]
     (base58/encode (format "%s%s" extended (checksum extended)))))


(def ix->base58-map (into {} (map-indexed (fn [ix v] [ix v]) "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz")))
(def base58->ix-map (into {} (map (fn [e] [(val e) (key e)]) ix->base58-map)))
;(defn base58->hex [s]



  
  
  
  

(def sk-sick-salary (- 131454	39433))

(def hb-sick-salary (reduce + [12663
9569
6191
16884
17447
1689
27015
563]))



(def sk-salary (- 503487	163850))


(def hb-salary 
  (reduce + [10221
  619
  5531
  5694
  27447
  39290
  41647
  41192
  41192
  39254
  44051
  40993]))
  
  
(def diff-sk->hb-salary (- sk-salary hb-salary))  


(def brutto (reduce + [63283.20 
                       69462.80
                       58662.40
                       62800
                       62800
                       63804.80
                       58587.60
                       36501.88
                       6400.77
                       6194.20
                       619.41
                       12215.72
                       ]))
(def tax (reduce + [21938
                    25412
                    19408
                    21608
                    21608
                    22158
                    19298
                    9055
                    707
                    663
                    1995]))
