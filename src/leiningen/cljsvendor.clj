(ns leiningen.cljsvendor
  (:require [clj-time.coerce :as coerce]
            [clj-time.core :as time]
            [clojure.java.shell :as shell]
            [clojure.string :as string]
            [leiningen.core.eval :as core.eval]
            [leiningen.core.main :as main]
            [leiningen.core.project :as project]
            [me.raynes.fs :as fs]))

(defn warn-and-exit
  ([msg]
   (warn-and-exit 1 msg))
  ([code msg]
   (main/warn msg)
   (main/exit code msg)))

(defn merge-config [env]
  (let [args (project/read)
        global (get-in args [:cljsvendor :global])
        local (get-in args [:cljsvendor (keyword env)])]
    (if (nil? local)
      (if (nil? global)
        (warn-and-exit "No global configuration is set, and no environment was specified.")
        (if-let [local (get-in args [:cljsvendor (:default-env global)])]
          (merge global local)
          global))
      (merge global local))))

(defn execute-cmd
  [cmd f]
  (let [result (apply shell/sh (string/split cmd #"\s+"))]
    (if (= (:exit result) 0)
      (f (:out result))
      (warn-and-exit (:exit result) (:err result)))))

(defn execute-streamed-cmd
  [cmd]
  (let [result (apply core.eval/sh (string/split cmd #"\s+"))]
    (when (not= result 0)
      (main/exit 1 "An error occurred."))))

(defn format-sha [raw]
  (-> raw
    (string/trim)
    (string/replace "\"" "")))

(defn git-sha []
  (execute-cmd "git log -1 --format=\"%H\"" format-sha))

(defn mkdir
  ([dir]
   (let [split-dir (string/split dir #"/")]
     (mkdir (first split-dir) (rest split-dir))))
  ([base-dir sub-dirs]
   (when (not (fs/exists? base-dir))
     (fs/mkdir base-dir))
   (when (not (empty? sub-dirs))
     (mkdir (str base-dir "/" (first sub-dirs)) (rest sub-dirs)))))

(defn copy-sources [dir args]
  (if-let [sources (:sources args)]
    (doseq [source sources] (if (fs/directory? source)
                              (fs/copy-dir source dir)
                              (fs/copy source (str dir "/" source))))
    (warn-and-exit "No sources given.")))

(defn hook [args k]
  (when-let [cmd (get args k)]
    (execute-streamed-cmd cmd)))

(defn compile-source [args]
  (hook args :before-compile)
  (if-let [cmd (:compile-cmd args)]
    (execute-streamed-cmd cmd)
    (warn-and-exit "No compile-cmd given."))
  (hook args :after-compile))

(defn cljsvendor
  ""
  ([project]
   (cljsvendor project nil))
  ([project env]
   (let [args (merge-config env)
         build-dir (if (:build-dir args)
                     (:build-dir args)
                     (warn-and-exit "No build-dir given."))
         timestamp (str (coerce/to-long (time/now)))
         dir (if (:using-git? args)
               (str build-dir "/" (git-sha) "/" timestamp)
               (str build-dir "/" timestamp))
         linked-dir (if (:linked-dir args)
                      (:linked-dir args)
                      (str build-dir "/current"))]
     (mkdir dir)
     (mkdir linked-dir)
     (copy-sources dir args)
     (fs/with-cwd (str fs/*cwd* "/" dir)
                  (compile-source args))
     (fs/delete linked-dir)
     (fs/sym-link linked-dir dir))))
