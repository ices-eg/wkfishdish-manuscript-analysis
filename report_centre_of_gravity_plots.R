# -------------------------------------
#
# Fit density surface for each species and survey
#
# -------------------------------------

library(icesTAF)
mkdir("report")

source("utilities_header.R")
source("utilities.R")

# read in spatial datasets
load("data/spatial_model_data.rData")

species <- unique(getControlTable()$Species)

#selected.species <- "Pollack"
out_cg <-
  lapply(species,
         function(selected.species)
         {
           load(paste0("model/", selected.species, "_centre_gravity.rData"))

           data_cg <-
             data %>%
               unnest_legacy(data) %>%
               unnest_legacy(cg) %>%
               select(-Model, -cg_ci, -cg_sim, -Data) %>%
               nest_legacy(Year, x, y)

           mycor <- function(x, y) {
             if (length(unique(round(x, 4))) < 3) NA else cor(x, y, method = "spearman")
           }

           corx <- function(df) mycor(df$x, df$Year)
           cory <- function(df) mycor(df$y, df$Year)
           data_cg %>%
             mutate(corx = map_dbl(data, corx),
                    cory = map_dbl(data, cory)) %>%
             select(-data)
         })

out_pval <-
  lapply(species,
       function(selected.species)
       {
         load(paste0("model/", selected.species, "_centre_gravity.rData"))

         data_sim <-
           data %>%
           unnest_legacy(data) %>%
           unnest_legacy(cg_sim)
         data_sim$sim_id <- 1:1000
         data_sim %<>% group_by(species, Survey, Quarter, sim_id) %>% nest()

         mycor <- function(x, y) {
           if (length(unique(round(x, 4))) < 3) NA else cor(x, y, method = "spearman")
         }

         corxsim <- function(df) mycor(df$x.sim, df$Year)
         corysim <- function(df) mycor(df$y.sim, df$Year)
         data_sim %<>%
           mutate(corx = map_dbl(data, corxsim),
                  cory = map_dbl(data, corysim)) %>%
           select(-data) %>%
           nest_legacy(sim_id, corx, cory)

         mypvalue <- function(x) {
           if (all(is.na(x))) {
             NA
           } else {
             p2 <- sum(x > 0, na.rm = TRUE) / (1 + sum(!is.na(x)))
             2 * min(p2, 1-p2)
           }
         }

         xpvalue <- function(df) mypvalue(df$corx)
         ypvalue <- function(df) mypvalue(df$cory)
         data_sim %>%
           mutate(px = map_dbl(data, xpvalue),
                  py = map_dbl(data, ypvalue)) %>%
           select(-data)
       })

out_cg <- do.call(rbind, out_cg)
out_pval <- do.call(rbind, out_pval)

out <- out_cg
out[c("px", "py")] <- out_pval[c("px", "py")]

table(out$species, out$Survey)

arrow <- function(length = 1, theta = 0, col = grey(0.5), border = grey(0), offset = c(0,0)) {
  hy <- 0.4
  hx1 <- 0.1
  hx2 <- 0.1
  R <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), 2, 2)
  x <- c(-hx1, -hx1, -hx1-hx2, 0, hx1+hx2,  hx1, hx1, -hx1)
  y <- c(  -1, 1-hy,     1-hy, 1,    1-hy, 1-hy,  -1,   -1) * length
  xy <- cbind(x, y) %*% R
  polygon(xy[,1] + offset[1], xy[,2] + offset[2], col = col, border = border)
}

arrow2 <- function(x, y, col = grey(0.5), border = grey(0), offset = c(0,0)) {
  length <- sqrt(x^2 + y^2)
  theta = atan2(y, x)
  arrow(length, theta, col, border, offset)
}

offset <- function(row, col) {
  c(col*2-2, -2*row+2)
}

#  plot(0, 0, ann = FALSE, axes = FALSE, xlim = c(-1,1), ylim = c(-1,1), type = "n", asp = 1)
#  arrow2(-.5, .1)


#  plot(0, 0, ann = FALSE, axes = TRUE, type = "n",
#       xlim = c(-3, 3), ylim = c(-1, 3), asp = 1)
#  abline(v = c(-3, -1, 1, 3), h = c(-3, -1, 1, 3))
#  arrow2(1, 1, offset = c(-2, 0))
#  arrow2(1, .1, offset = c(0, 0))
#  arrow2(-.5, .1, offset = c(2, 0))


# get north to south survey locations
con <- dbConnect(SQLite(), dbname = "bootstrap/data/datras-db/datras.sqlite")
hh <- dbReadTable(con, "hh")
dbDisconnect(con)

ys <- tapply(hh$ShootLat, hh$Survey, mean, na.rm = TRUE)
xs <- tapply(hh$ShootLon, hh$Survey, mean, na.rm = TRUE)

if (FALSE) {
  png("paper_figures/survey_map.png", width = 5, height = 5, units = "in", res = 400)
  plot(area, border = grey(0.5, alpha = 0.5))
  text(xs, ys, names(xs), cex = 0.5, font = 2)
  dev.off()
}

survey.ord <- names(sort(ys, decreasing = TRUE))

# get species ranges
species_rnge <- read.csv("bootstrap/data/species_ranges.csv")
species.ord <- species_rnge$lat


# subset out non flat fish from beam trawls
out <- out[!(out$Survey %in% c("BTS", "BTS-VIIa") &
             out$species %in% c("Norway Pout", "Herring", "Haddock", "Cod", "Saithe",
                                "Pollack", "Whiting", "White anglerfish", "Blue Whiting",
                                "Mackerel", "Sprat", "Spurdog", "Hake",
                                "Black-bellied anglerfish", "Horse Mackerel", "Anchovy")),]

# table
tab <- table(factor(out$Survey, levels = survey.ord),
             factor(out$species, levels = species.ord))
dim(tab)

# adjust p values
ps <- p.adjust(c(out$px, out$py), method = "BH")
out$pxadj <- ps[1:nrow(out)]
out$pyadj <- ps[nrow(out) + 1:nrow(out)]

out$row <- unname(sapply(out$Survey, function(x) which(x == rownames(tab))))
out$col <- unname(sapply(out$species, function(x) which(x == colnames(tab))))
colour <- grey(c(0, .2, .4, .6))
colour <- RColorBrewer::brewer.pal(4, "Set1")

png("report/centre_of_gravity_change.png", width = 10, height = 7, units = "in", res = 400)

xs <- ncol(tab); ys <- nrow(tab)
par(mar = c(0, 5.5, 2, 0))
plot(0, 0, ann = FALSE, axes = FALSE, type = "n",
     xlim = c(-1, 2*xs-1), ylim = c(-2*ys+1, 1), asp = 1)
text(seq(0, 2*xs-2, by = 2), 1.5, labels = colnames(tab), xpd = NA,
     srt = -45, adj = 1, cex = 0.7, font = 2)
text(-1.5, seq(0, -2*ys+2, by = -2), labels = rownames(tab)[1:ys], xpd = NA,
     adj = 1, cex = 0.7, font = 2)

out1 <- subset(out, row <= ys)

# plot grey background
polygon(c(-1, 2*xs-1)[c(1,2,2,1)], c(1, -2*ys+1)[c(1, 1, 2, 2)], col = grey(0.8), border=NA)

isGrey <- matrix(TRUE, ys, xs)

for (i in 1:nrow(out1)) {
  xy <- offset(out1$row[i], out1$col[i])
  # only run this if the sqaure has not been plotted before...
  if (isGrey[out1$row[i], out1$col[i]]) {
    polygon( xy[1] + c(-1,1,1,-1), xy[2] + c(-1, -1, 1, 1), border = NA, col = "white")
    isGrey[out1$row[i], out1$col[i]] <- FALSE
  }
  if (is.na(out1$corx[i]) | is.na(out1$cory[i])) {
    #points(xy[1], xy[2], pch = 16, cex = 0.3)
  } else {
    if (out1$pxadj[i] < 0.05 || out1$pyadj[i] < 0.05) {
      #polygon( xy[1] + c(-1,1,1,-1), xy[2] + c(-1, -1, 1, 1), border = NA, col = "lightblue")
      arrow2(out1$corx[i], out1$cory[i], col = colour[out1$Quarter[i]],
             border = grey(0),
             offset = offset(out1$row[i], out1$col[i]))

    } else {
      arrow2(out1$corx[i], out1$cory[i], col = "transparent",
             border = colour[out1$Quarter[i]],
             offset = offset(out1$row[i], out1$col[i]))

    }
  }
}

# add grid lines
segments(-1, seq(-2*ys+1, 1, by = 2), 2*xs-1, col = grey(0.6))
segments(seq(-1, 2*xs-1, by = 2), 1, y1 = -2*ys+1, col = grey(0.6))

# add legend
legendAt = 1:3/4 * 2*xs
text(paste("Quarter", c(1,3,4)), x = legendAt, y = -2*ys, adj = 1)
segments(legendAt + 0.5, -2*ys, legendAt + 1.5, lwd = 4, col = colour[c(1,3,4)])

dev.off()

