

download.ICESshape <- function(what) {
  download.file(paste0("http://gis.ices.dk/shapefiles/", what, ".zip"),
                paste0(what, ".zip"))

  unzip(paste0(what, ".zip"), exdir = ".")
}

if (!file.exists("ICES_Areas_20160601_dense.shp")) {
  download.ICESshape("ICES_areas")
}

if (!file.exists("ICES_StatRec_mapto_ICES_Areas.shp")) {
  download.ICESshape("ICES_StatRec_mapto_ICES_Areas")
}


# -------------------------------------
#
# Subset spatial data
#
# -------------------------------------

# load packages etc.
library(icesTAF)
source("../../../utilities_header.R")
source("../../../utilities.R")

# read design table and look at species
fulltab <- read.taf("../control-table/control_table.csv")
areas <- unique(fulltab$Division)

# read in spatial datasets
area <- readOGR(".", "ICES_Areas_20160601_dense")
# hard code projection in case GDAL is not installed
proj4string(area) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
statrec <- readOGR(".", "ICES_StatRec_mapto_ICES_Areas")
# hard code projection in case GDAL is not installed
#proj4string(statrec) <- CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +no_defs")
statrec <- spTransform(statrec, crs(area)) # transform to wgs84

# add Division columns - keep baltic seperate

statrec$SubAreaDiv <- sapply(strsplit(statrec$Max_Area, "[.]"), function(x) paste(x[1:2], collapse = "."))
area$SubAreaDiv <- paste(area$SubArea, area$Division, sep = ".")

# subset to analysis extent
area <- area[area$SubAreaDiv %in% unique(trimws(fulltab$Division)),]
statrec <- statrec[which(statrec$SubAreaDiv %in% fulltab$Division),]

# join area to form larger areas
tmpsp <- gUnaryUnion(area, id = area$SubAreaDiv)
tmpdat <- data.frame(SubAreaDiv = sapply(tmpsp@polygons, function(x) x@ID))
rownames(tmpdat) <- tmpdat$SubAreaDiv
area <- SpatialPolygonsDataFrame(tmpsp, tmpdat)

# save modified shapefiles
writeOGR(statrec, ".", "ICES_StatRec_WKFISHDISH", driver="ESRI Shapefile", overwrite_layer=TRUE)
writeOGR(area, ".", "ICES_Areas_WKFISHDISH", driver="ESRI Shapefile", overwrite_layer=TRUE)

# save as R objects
save(statrec, area, file = "spatial_data.rData")

# done -------------------------------------
