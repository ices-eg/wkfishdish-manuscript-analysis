# -------------------------------------
#
# Do some plots of data and model fits
#
# -------------------------------------

library(icesTAF)

mkdir("report")

# load packages etc.
source("utilities_header.R")
source("utilities.R")

# read in spatial datasets
load("data/spatial_model_data.rData")

# read design table and look at species
fulltab <- getControlTable()
species <- unique(fulltab$Species)

# Get HH and cpue data:
con <- dbConnect(SQLite(), dbname = "bootstrap/data/datras-db/datras.sqlite")
hh <- dbReadTable(con, "hh")
data <- dbReadTable(con, "cpue")
dbDisconnect(con)

# join hh onto cpue data
data %<>% as_data_frame() %>%
  left_join(hh, by = "haulID")

# subset by year
data %<>% filter(Year > 1995)

# create data.frame for plots
data <- nest_legacy(data, -species, -Survey, -Quarter, -Year, .key = Data)
data <- nest_legacy(data, -species, -Survey, -Quarter, .key = Data)

# get extent of each survey
statrec_extent <- function(df) {
  sr_extent <-
    df %>%
    unnest_legacy(Data) %>%
    select(StatRec) %>%
    unlist() %>%
    unique()
  statrec[statrec$StatRec %in% sr_extent,]
}

data %<>% mutate(statrec = map(Data, statrec_extent))

pdf(paste0("report/survey_coverage.pdf"), onefile = TRUE, paper = "a4")
# set par - starts a new page
par(mfrow = c(2,2), mar = c(0.2,0.2,1.5,0.2))
data %>% filter(species == "Megrim") %>% mutate(tmp = pmap(list(statrec, Survey, Quarter), front_plot))
dev.off()


for (i in seq_along(species)) {
  cat("Working on species:", species[i], " (", i ,"/", length(species), ")\n"); flush.console()
  sp <- species[i]
  sdata <- data %>% filter(species == sp)

  pdf(paste0("report/", species[i], "_survey_data.pdf"), onefile = TRUE, paper = "a4")
    for (j in 1:nrow(sdata)) {
      plot_one_survey_data(sdata[j,])
    }
  dev.off()
}
