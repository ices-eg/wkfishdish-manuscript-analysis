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

#plot
png("report/ices_areas.png",
    width = 7, height = 7, units = "in", pointsize = 12,
    bg = "white", res = 600,
    type = "cairo-png")
  # plot regions with names
  plot(area, col = gplots::rich.colors(nrow(area), alpha = 0.5), border = grey(0.4))
  text(sp::coordinates(area),
       labels = area$SubAreaDiv,
       cex = 0.45, font = 2)
dev.off()


#plot
png("report/spatial_smoother_structure.png",
    width = 7, height = 7, units = "in", pointsize = 8,
    bg = "white", res = 600,
    type = "cairo-png")
  # plot regions with names
  plot(area, col = gplots::rich.colors(nrow(area), alpha = 0.5), border = grey(0.4))

  plot(statrec, col = grey(0.5, alpha = 0.5), add = TRUE)
  xy <- coordinates(statrec)
  nbs <- cbind(rep(1:length(adj), sapply(adj, length)), unlist(adj))
  nbs <- unique(t(apply(nbs, 1, sort)))
  segments(xy[nbs[,1],1], xy[nbs[,1],2],
           xy[nbs[,2],1], xy[nbs[,2],2],
           col = "blue")
  points(xy, pch = 16, col = "red", cex = 0.4)
dev.off()



#plot
pdf("report/data_covarage.pdf", paper = "a4", onefile = TRUE)

par(mfrow=c(2,2), mar = c(0,0,1,0))
for (yr in sort(unique(dat$Year))) {
  plot(statrec, main = yr, border = grey(0.5, alpha = 0.5))
  points(dat[dat$Year == yr,], cex=0.5)
}

dev.off()
