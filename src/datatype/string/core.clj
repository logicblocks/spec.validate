(ns datatype.string.core
  (:require
   [clojure.string :as string]

   [datatype.support :as dts]))

(declare
  blank?
  not-blank?
  ascii-digits?
  lowercase-ascii-alphabetics?
  uppercase-ascii-alphabetics?
  ascii-alphabetics?
  lowercase-ascii-alphanumerics?
  uppercase-ascii-alphanumerics?
  ascii-alphanumerics?)

(defn blank? [value]
  (dts/exception->false
    (and
      (not (nil? value))
      (string/blank? value))))

(defn not-blank? [value]
  (dts/exception->false (not (string/blank? value))))

(defn ascii-digits? [value]
  (dts/exception->false
    (dts/re-satisfies? #"^\d+$" value)))

(defn lowercase-ascii-alphabetics? [value]
  (dts/exception->false
    (dts/re-satisfies? #"^[a-z]+$" value)))

(defn uppercase-ascii-alphabetics? [value]
  (dts/exception->false
    (dts/re-satisfies? #"^[A-Z]+$" value)))

(defn ascii-alphabetics? [value]
  (dts/exception->false
    (dts/re-satisfies? #"^[a-zA-Z]+$" value)))

(defn lowercase-ascii-alphanumerics? [value]
  (dts/exception->false
    (dts/re-satisfies? #"^[a-z0-9]+$" value)))

(defn uppercase-ascii-alphanumerics? [value]
  (dts/exception->false
    (dts/re-satisfies? #"^[A-Z0-9]+$" value)))

(defn ascii-alphanumerics? [value]
  (dts/exception->false
    (dts/re-satisfies? #"^[a-zA-Z0-9]+$" value)))
