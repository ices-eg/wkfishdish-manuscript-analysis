# -------------------------------------
#
# Fit density surface for each species and survey
#
# -------------------------------------

library(icesTAF)
library(parallel)
source("utilities_header.R")
source("utilities.R")

mkdir("model")

species <- unique(getControlTable()$Species)

mcluster <- makeCluster(detectCores())

# read in spatial datasets
spat <- load("data/spatial_model_data.rData")
# Get HH and cpue data:
con <- dbConnect(SQLite(), dbname = "bootstrap/data/datras-db/datras.sqlite")
hh <- dbReadTable(con, "hh")
cpue <- dbReadTable(con, "cpue")
dbDisconnect(con)

# join hh onto cpue data
cpue <-
  cpue %>%
  as_tibble() %>%
  left_join(hh, by = "haulID")

# create data.frame for model fits
cpue <- nest_legacy(cpue, -species, -Survey, -Quarter, -Year, .key = Data)


clusterExport(mcluster, c(spat, "cpue"))

tmp <- clusterEvalQ(mcluster, source("utilities_header.R"))
tmp <- clusterEvalQ(mcluster, source("utilities.R"))

out <-
parLapply(mcluster,
  species,
  function(selected.species) {
  # selected.species <- "Megrim"

  sim_fname <- paste0("model/", selected.species, "_sims.rData")
  if (!file.exists(sim_fname) || FALSE) {

    # for each row fit a surface
    data <-
      cpue %>%
      filter(species == selected.species, Year > 1995)

    data %<>% mutate(Model = map(Data, fit_surface))

    # simulate
    data %<>% mutate(Sims = map(Model, sim_cpue))

    save(data, file = sim_fname)
  }
})

stopCluster(mcluster)
