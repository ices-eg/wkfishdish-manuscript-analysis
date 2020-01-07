
tab <- read.table("../Overview_of_surveys_to_be_used.txt", sep = ",", header = TRUE, stringsAsFactors = FALSE)

# expand quarter and start year
qtr <- strsplit(tab$Quarter, ",")
syr <- strsplit(tab$Start.year, ",")
orig.row <- rep(1:nrow(tab), sapply(qtr, length))

tab <- cbind(tab[orig.row,c("Species", "Division", "Survey.name", "Gear")],
             data.frame(Quarter = as.integer(unlist(qtr)),
                        Start.year = as.integer(unlist(syr))) )

# expand division
div <- strsplit(tab $ Division, ",")
orig.row <- rep(1:nrow(tab), sapply(div, length))

tab <- cbind(tab[orig.row,c("Species", "Survey.name", "Gear", "Quarter", "Start.year")],
             data.frame(Division = unlist(div)) )

# expand species
sp <- strsplit(tab $ Species, ",")
sp <- lapply(sp, function(x) trimws(gsub("\n|(and)", "", x)))
orig.row <- rep(1:nrow(tab), sapply(sp, length))

tab <- cbind(data.frame(Species = unlist(sp)),
             tab[orig.row,c("Division", "Survey.name", "Gear", "Quarter", "Start.year")])

tab$Survey.name <- trimws(tab$Survey.name)
tab$Division <- trimws(tab$Division)

row.names(tab) <- NULL

control_table <- tab

icesTAF::write.taf(control_table, quote = TRUE)
