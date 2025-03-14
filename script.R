# This script runs the full analysis (takes around 10-30 min)
# When finished running, plots will be inside the 'report' folder

library(TAF)

taf.boot()
source.all()
cat("\n** Run completed\n")

# Note that a variety of warnings and errors may be shown, but for the purposes
# of VRE tests, a run is successful if it produces a `report` folder containing
# at least one PDF file. See the README.md for further information
