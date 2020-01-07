# -------------------------------------
#
# Fit density surface for each species and survey
#
# -------------------------------------

library(icesTAF)
library(parallel)

mkdir("model")

source("utilities_header.R")
source("utilities.R")

species <- unique(getControlTable()$Species)

mcluster <- makeCluster(detectCores())

spat <- load("data/spatial_model_data.rData")

# join on divisions
fulldata <- getControlTable() %>%
  as_data_frame() %>%
  rename(species = Species, Survey = Survey.name) %>%
  select(species, Division, Survey, Quarter)

clusterExport(mcluster, c(spat, "fulldata"))

tmp <- clusterEvalQ(mcluster, source("utilities_header.R"))
tmp <- clusterEvalQ(mcluster, source("utilities.R"))


#selected.species <- "Plaice"
out <-
parLapply(mcluster,
  species,
  function(selected.species) {
  # select a species
  cat(selected.species, "\n")

  sim_fname <- paste0("model/", selected.species, "_sims.rData")
  trends_fname <- paste0("model/", selected.species, "_trends.rData")

  if ((file.exists(sim_fname) && !file.exists(trends_fname)) || FALSE) {

    load(sim_fname)

    data %<>% select(-Data) %>%
      left_join(fulldata) %>%
      filter(!is.na(Division))

    # drop unused sims
    data %<>%
      mutate(Sim = map2(Sims, Division, function(x, div) x[,div])) %>%
      select(-Sims, -Model)

    # only consider comparisons between adjacent regions
    stocks <- tibble::tibble(Division  = area$SubAreaDiv[nbs[,1]],
                             Division2 = area$SubAreaDiv[nbs[,2]]) %>%
      left_join(data, by = "Division") %>%
      select(species, Year, Quarter, Division, Division2, Survey, Sim) %>%
      rename(Sim1 = Sim, Division1 = Division, Survey1 = Survey) %>%
      filter(!is.na(Survey1))

    stocks %<>% rename(Division = Division2) %>%
      left_join(data, by = c("species", "Year", "Quarter", "Division")) %>%
      select(species, Year, Quarter, Division1, Division, Survey1, Survey, Sim1, Sim) %>%
      rename(Sim2 = Sim, Division2 = Division, Survey2 = Survey) %>%
      filter(!is.na(Survey2))

    stocks <- stocks[!sapply(stocks$Sim1, is.null) & !sapply(stocks$Sim2, is.null),]

    # for each row calculate the log ratio and rearange by comparison
    stocks %<>%
      mutate(lograt = map2(Sim1, Sim2, function(x, y) tibble::tibble(id = 1:length(x), val = log(x) - log(y), x = x, y = y))) %>%
      select(-Sim1, -Sim2) %>%
      unnest_legacy(lograt) %>%
      nest_legacy(Year, id, val, x, y)

    # now fit something to each?
    test <- function(x) {
      test_by <- function(x) {
        y <- x$val[order(x$Year)]
        out <- try(Kendall::MannKendall(y), silent = TRUE)
        #out <- try(coef(lm(val ~ Year, data = x))[2])
        if (inherits(out, "try-error")) out <- list(sl=NA, tau = NA)
        tibble::tibble(kendall_p = out$sl,
                       slope = out$tau,
                       start_year = min(x$Year),
                       end_year = max(x$Year))
      }

      x %>% group_by(id) %>%
        do(test_by(.)) %>%
        ungroup() %>%
        summarise(p = (1+sum(kendall_p>0.05))/(length(kendall_p)+1),
                  median_p = median(kendall_p),
                  slope = median(slope),
                  start_year = min(start_year),
                  end_year = min(end_year))
    }

    res <- stocks %>% mutate(test = map(data, test)) %>%
      select(-data) %>%
      unnest_legacy(test) %>%
      filter(!is.na(p))

    # correct for false discovery - do this by species?
    res$p_adj <- p.adjust(res$p, method = "BH")

    res %>% filter(p_adj < 0.05) %>% select(-p)

    save(res, stocks, file = trends_fname)
  }
})

stopCluster(mcluster)
