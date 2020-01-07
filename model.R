## Run analysis, write model results

## Before:
## After:

library(icesTAF)

mkdir("model")

sourceTAF("model-fit-lwr.R")
sourceTAF("model-simulate-trends.R")
sourceTAF("model-run-tests.R")
sourceTAF("model-centre-of-gravity.R")
