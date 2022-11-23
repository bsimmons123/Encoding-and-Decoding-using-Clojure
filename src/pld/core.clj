(ns pld.core
  (:require [clojure.string :as str]))

(use '[clojure.string :only [index-of]]) ; import statement

; very similar to global variables
(def file-to-hash "Example.txt") ; this file should hold the values you are trying to hash
(def file-to-hash-from "hash.txt") ; this file should hold the values to hash from
(def file-to-write-hash-to "hashedExample.txt") ; this file will hold the output

; returns the value of the char in the hash
(defn find-char-in-hash [charToFind hashValues]
  (let [s (str charToFind)]
    (if (false? (clojure.string/blank? s)) ; if false then char is an actual value
      (index-of hashValues charToFind) -1)) ; if not found return -1
    )

; hashes the number
(defn hashCharNumber [character]
  (let [hashFile (slurp file-to-hash-from)] ; file to hash from
    (mod (+ (find-char-in-hash character hashFile) 13) 26)))

; hashes the character
(defn hashChar [hashedCharNumber]
  (let [hashValues (slurp file-to-hash-from)]
    (if (false? (= hashedCharNumber 12)) ; value 12 is a \n
      (get hashValues hashedCharNumber) \newline)))

; writes output to new file
(defn hash-file [hashedPhrase]
  (with-open [w (clojure.java.io/writer file-to-write-hash-to :append false)]
    (.write w (str hashedPhrase))))

; reads the file and returns all the values after being hashed
(defn readFile []
  (let [strval (slurp file-to-hash)]
    (loop [i 0 result []]
      (if (<= i (- (count strval) 1))
        (recur (+ i 1) (conj result (hashChar (hashCharNumber (get strval i)))))result))))

; hashes the file and writes out the new file
(hash-file (apply str (readFile)))

(println (apply str (readFile)))
