(defproject reapper "0.0.0"
  :description "Thin React Wrapper for ClojureScript"
  :url "https://github.com/milankinen/reapper"
  :license {:name "MIT"
            :url  "https://opensource.org/licenses/MIT"}
  :dependencies []
  :plugins [[lein-cloverage "1.0.13"]
            [lein-shell "0.5.0"]
            [lein-ancient "0.6.15"]
            [lein-changelog "0.3.2"]
            [lein-eftest "0.5.9"]]
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.10.1"]
                                  [eftest "0.5.9"]
                                  [clj-kondo "2020.12.12"]]}}
  :deploy-repositories [["releases" :clojars]]
  :aliases {"update-readme-version" ["shell" "sed" "-i" "s/\\\\[reapper \"[0-9.]*\"\\\\]/[reapper \"${:version}\"]/" "README.md"]
            "t"                     ["eftest"]
            "lint"                  ["trampoline" "run" "-m" "clj-kondo.main" "--lint" "src" "test"]}
  :release-tasks [["shell" "git" "diff" "--exit-code"]
                  ["change" "version" "leiningen.release/bump-version"]
                  ["change" "version" "leiningen.release/bump-version" "release"]
                  ["changelog" "release"]
                  ["update-readme-version"]
                  ["vcs" "commit"]
                  ["vcs" "tag"]
                  ["deploy"]
                  ["vcs" "push"]]
  :eftest {:multithread? false})
