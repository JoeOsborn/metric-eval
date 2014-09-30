Data
===========

See `Experiments/glz-eval/data/`. The `experiment#` folders contain the data used by the designer to rate traces; the `experiment#.{csv,xls}` files contain the actual ratings. The FinalStates directory contains end-of-game states for (at a minimum) the traces considered in the designer's ratings.

Code and Execution
===========

The analysis code lives in `Experiments/glz-eval/glz-eval/`. The `java-src` folder is used for the angular distance calculations, and the `src` folder contains Clojure code that loads Prom Week traces (in Gamalyzer format) and outputs CSVs with summaries of the analysis.

To execute the analysis, it is currently necessary to have a working installation of [Leiningen](http://leiningen.org) and then run (from the `Experiments/glz-eval/glz-eval/` directory:

    bash$ lein compile
    ;; Some output as dependencies are fetched and work is performed
    bash$ lein repl
    ;; Some output as a REPL is launched
    user=> (require 'pw)
    ;; Lots of diagnostic and progress output as the analysis is run (get a coffee!)
    nil
    user=> (pw/write-results)
    nil
    ;; results.csv placed in current directory
    user=>

This repository also includes JARs of several older development snapshots of [Gamalyzer](https://github.com/JoeOsborn/gamalyzer).
