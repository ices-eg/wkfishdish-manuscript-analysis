
library(icesTAF)
mkdir("report")

# plots for supplimentary material

source("utilities_header.R")
source("utilities.R")
source("utilities_supplementary_material.R")

load("data/spatial_model_data.rData")

species_all <- c("Herring", "Haddock", "Cod", "Plaice", "Hake", "Sole", "Horse Mackerel")
pdat_all <- do.call(rbind, lapply(species_all, get_pdat))

library(gridExtra)

pher <- get_plots("Herring")
phad <- get_plots("Haddock")
pcod <- get_plots("Cod")
pple <- get_plots("Plaice")
phke <- get_plots("Hake")
psol <- get_plots("Sole")
phma <- get_plots("Horse Mackerel")

mypng("biomass_trend_1")
grid.arrange(
  pher[[1]], pher[[2]], pher[[3]],
  phad[[1]], phad[[2]], phad[[3]],
  ncol = 3,
  heights  = heights(c(2, 5)))
dev.off()

mypng("biomass_trend_2")
grid.arrange(
  pcod[[1]], pcod[[2]], pcod[[3]],
  pple[[1]], pple[[2]], pple[[3]],
  psol[[1]], psol[[2]], psol[[3]],
  phma[[1]], phma[[2]], phma[[3]],
  ncol = 3,
  heights  = heights(c(1, 3, 1, 2)))
dev.off()

mypng("biomass_trend_3")
grid.arrange(
  phke[[1]], phke[[2]], phke[[3]],
  ncol = 3)
dev.off()
