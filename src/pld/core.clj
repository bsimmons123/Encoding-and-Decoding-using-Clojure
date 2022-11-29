(ns pld.core
  (:require [clojure.string :as str]))

(use '[clojure.string :only [index-of lower-case]]) ; import statement

; very similar to global variables
(def file-to-hash "Example.txt") ; this file should hold the values you are trying to hash
(def file-to-hash-from "hash.txt") ; this file should hold the values to hash from
(def file-to-write-hash-to "hashedExample.txt") ; this file will hold the output

; returns the value of the char in the hash
(defn find-char-in-hash [charToFind hashValues]
  (let [s (str charToFind)]
    (if (or (and (> (int charToFind) 96) (< (int charToFind) 123)) (or (= (int charToFind) 32)(= (int charToFind) 10)))
      (if (= charToFind \newline) -2
        (if (false? (clojure.string/blank? s)) ; if false then char is an actual value
          (index-of hashValues charToFind) -1)) ; if not found return -1
      charToFind))) ; if not return special character

; hashes the number
(defn hash-char-number [character]
  (let [hashFile (slurp file-to-hash-from)]
    (let [charNum (find-char-in-hash character hashFile)]
      (if (false? (char? charNum))
        (if (> charNum 0)
          (mod (+ charNum 13) 26)charNum) ; return hashed num
        charNum))))  ; else return the original value

; hashes the character
(defn hash-char [hashedCharNumber]
  (let [hashValues (slurp file-to-hash-from)]
    (if (false? (char? hashedCharNumber))
      (if (false? (= hashedCharNumber -2)) ; if not newline
        (if (false? (= hashedCharNumber -1)) ; value 12 is space
          (get hashValues hashedCharNumber) " ") \newline)hashedCharNumber)))

(defn clear-hashed-file [file] (with-open [w (clojure.java.io/writer file :append false)] ; will overwrite values already in file
                             (.write w (str ""))))

; writes output to new file
(defn hash-file [hashedPhrase]
  (with-open [w (clojure.java.io/writer file-to-write-hash-to :append true)] ; will append to values already in file
    (.write w (str hashedPhrase))))

; reads file line by line
(defn read-file-line-hash []
  (clear-hashed-file file-to-write-hash-to) ; empties out all contents of file FOR NEW HASH
  (with-open [r (clojure.java.io/reader file-to-hash)]
    (doseq [line (line-seq r)]
      (let [newLineToHash (str (clojure.string/replace line #"\newline" " ") \newline)]
        (loop [i 0 result []]
          (if (<= i (- (count newLineToHash) 1))
            (recur (+ i 1) (conj result (hash-char (hash-char-number (get newLineToHash i)))))
            (hash-file (apply str result)))))))
  (slurp file-to-write-hash-to)) ; returns new hashed file

; hashes each line and calls all functions
(println (read-file-line-hash))