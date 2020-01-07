
get_pdat <- function(species) {
  icesTAF::msg("Doing ", species)
  load(paste0('model/', species, '_trends.rData'))
  res$p_adj <- p.adjust(res$median_p, method = 'BH')
  res %<>% filter(p_adj < 0.05) %>% select(-p)

  # plot log ratios
  tmp <- unnest(stocks, data)
  tmp <-
    left_join(
      res, tmp,
      by = c("species", "Quarter", "Division1", "Division2", "Survey1", "Survey2"))
  tmp$comparison <- factor(paste0(tmp$Division1, ' ~ ', tmp$Division2, ' (', round(tmp$slope,2), ', p=', round(tmp$p_adj, 3), ')'))
  pdat <-
    do.call(
      rbind,
      lapply(
        seq_len(nrow(res)),
        function(i) {
          # set up dataframe
          pdat <- tmp[tmp$id %in% 1:100 &
                      tmp$comparison == levels(tmp$comparison)[i],]

          what <- c('ratio', 'from', 'to')
          comparison_name <-
            c(paste0(pdat$Division1[1], " / ", pdat$Division2[1]),
              paste0(pdat$Division1[1], ",", pdat$Division2[1]),
              paste0(pdat$Division1[1], ",", pdat$Division2[1]))

          data.frame(
            Year = rep(pdat$Year, 3),
            what = factor(rep(what, each = nrow(pdat)), levels = c("from", "to", "ratio")),
            val = c(pdat$val, log(pdat$x), log(pdat$y)),
            comparison_name = rep(comparison_name, each = nrow(pdat)),
            from = pdat$Division1[1],
            to = pdat$Division2[1],
            pvalue_adj = pdat$p_adj[1],
            id = rep(pdat$id, 3),
            species = species
          )
        }
      )
    )
}


label_div1 <- function (labels, multi_line = TRUE, which = 1)
{
  labels <- lapply(labels, as.character)
  lapply(labels, function(values) {
    values <- as.character(values)
    if (any(grepl(",", values))) {
      values <- lapply(strsplit(values, ","), "[[", which)
    }
    values
  })
}

label_div2 <- function(...) label_div1(..., which = 2)


library(ggplot2)

mytheme <- theme_minimal

get_plots <- function(sp) {

  pdat <- pdat_all %>% filter(species %in% sp)

  p1 <-
    ggplot(
      data = pdat %>% filter(what == "from"),
      aes(x = Year, y = val, group = id)
    ) +
    geom_point(colour = grey(0.5), alpha = 0.5) +
    facet_grid(
      cols = vars(what),
      rows = vars(comparison_name),
      switch = "y",
      scales = "free",
      labeller = "label_div1") +
    ylab("log abundance") +
    xlab("") +
    geom_line(stat = "smooth", method = "lm", se = FALSE, colour = grey(0.5), alpha = 0.1) +
    mytheme() +
    theme(axis.text.y=element_blank()) +
    ggtitle(pdat$species[1])

  p2 <-
    ggplot(
      data = pdat %>% filter(what == "to"),
      aes(x = Year, y = val, group = id)
    ) +
    geom_point(colour = grey(0.5), alpha = 0.5) +
    facet_grid(
      cols = vars(what),
      rows = vars(comparison_name),
      switch = "y",
      scales = "free",
      labeller = "label_div2") +
    ylab("") +
    xlab("Year") +
    geom_line(stat = "smooth", method = "lm", se = FALSE, colour = grey(0.5), alpha = 0.1) +
    mytheme() +
    theme(axis.text.y=element_blank()) +
    ggtitle(" ")


  # p value labels
  plabels <-
    pdat %>%
    filter(what == "ratio") %>%
    group_by(comparison_name) %>%
    mutate(
      Year = min(Year),
      val = max(val),
      id = 1) %>%
    unique()


  p3 <-
    ggplot(
      data = pdat %>% filter(what == "ratio"),
      aes(x = Year, y = val, group = factor(id))
    ) +
    geom_point(colour = grey(0.5), alpha = 0.5) +
    #facet_wrap( ~ species + comparison_name, ncol = 1, scales = "free_y") +
    facet_grid(
      cols = vars(what),
      rows = vars(comparison_name),
      switch = "y",
      scales = "free") +
    ylab("log ratio of abundance") +
    xlab("") +
    geom_line(stat = "smooth", method = "lm", se = FALSE, colour = grey(0.5), alpha = 0.1) +
    mytheme() +
    theme(axis.text.y=element_blank()) +
    ggtitle(" ") +
    geom_label(
      aes(label = paste0("p = ", round(pvalue_adj, 3))),
      data = plabels, size = 3, hjust = "inward"
      )

  list(p1, p2, p3)
}

mypng <- function(fname) {
  scale <- 1.2
  png(paste0(file.path("report", fname), ".png"),
      width = 297 / (sqrt(2) * 1.2) * scale,
      height = 297 * scale,
      units = "mm",
      res = 400)
}

heights <- function(vec, frac = 0.1) {
  (vec+frac)/(sum(vec)+length(vec)*frac)
}
