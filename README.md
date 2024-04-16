# Index by Stock

## Workshop demo

Four special *Workshop* demos have been prepared that are useful both as
SOFIA-TAF tutorials and for testing the VRE setup before workshops. The features
of a Workshop demo is that it:

* Is very light (<1 MB) and contains no results.
* Includes a `script.R` that runs `taf.boot()` and `sourceAll()`.
* Has a description label 'VRE demo' which makes them easy to find when
  searching on GitHub.

The `script.R` runs the full analysis, which takes around 10-30 mins. At the end
of the analysis, a `report` folder contains plots showing the results.

The four Workshop demos are:

1. [Effort Shared](https://github.com/sofia-taf/WorkshopEffortShared)
2. [Effort by Stock](https://github.com/sofia-taf/WorkshopEffortByStock)
3. [Index by Stock](https://github.com/sofia-taf/WorkshopIndexByStock)
4. [Priors by Stock](https://github.com/sofia-taf/WorkshopPriorsByStock)

## VRE tests

Note that a variety of warnings and errors may be shown, but for the purposes of
VRE tests, a run is successful if it produces a `report` folder containing at
least one PDF file.

## Further information

See [sofia-taf/doc](https://github.com/sofia-taf/doc#readme) and the [SOFIA-TAF
tutorial](https://github.com/sofia-taf/doc/blob/main/sofia_taf_tutorial.md).

This demo is a special Workshop version of
[2022Area31DemoIndexByStock](https://github.com/sofia-taf/2022Area31DemoIndexByStock).
