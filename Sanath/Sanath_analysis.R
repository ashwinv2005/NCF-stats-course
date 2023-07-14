## dealing with spatial data, Sanath

require(lubridate)
library(tidyverse)
library(sf)
library(extrafont)

## read eBird data

# select only necessary columns
preimp = c("CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT",
           "LOCALITY.ID","LOCALITY.TYPE","REVIEWED","APPROVED","STATE","COUNTY",
           "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
           "PROTOCOL.TYPE","DURATION.MINUTES","EFFORT.DISTANCE.KM","EXOTIC.CODE",
           "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER","SAMPLING.EVENT.IDENTIFIER")


nms = read.delim("Sanath/eBird data.txt", nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                 na.strings = c(""," ",NA))
nms = names(nms)
nms[!(nms %in% preimp)] = "NULL"
nms[nms %in% preimp] = NA

# read data from certain columns only
data = read.delim(rawpath, colClasses = nms, sep = "\t", header = T, quote = "", 
                  stringsAsFactors = F, na.strings = c(""," ",NA))

imp = c("CATEGORY","COMMON.NAME","OBSERVATION.COUNT",
        "LOCALITY.ID", "REVIEWED","APPROVED","EXOTIC.CODE",
        "LOCALITY.TYPE","STATE","COUNTY",
        "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED",
        "OBSERVER.ID","PROTOCOL.TYPE",
        "DURATION.MINUTES","EFFORT.DISTANCE.KM",
        "ALL.SPECIES.REPORTED","group.id","SAMPLING.EVENT.IDENTIFIER")


## read provided data

data = read.csv("Sanath/filtered_KL.csv")

data = data %>%
  # create a column "group.id" which can help remove duplicate checklists
  mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), 
                           SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER)) %>%
  dplyr::select(all_of(imp))


coastal_polygons = st_read("Sanath/Sanath _ Ebird_Polygon/Sanath _ Ebird_Polygon.shp")
coastal_polygons = coastal_polygons %>% dplyr::select(fid,Name,geometry)
crs_string = "+proj=longlat +datum=WGS84 +no_defs"
coastal_polygons = coastal_polygons %>% st_transform(crs_string)

check = data %>%
  distinct(LONGITUDE, LATITUDE) %>% 
  # joining map vars
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), remove = F) %>%
  #st_set_crs(st_crs(siang_grids)) %>%
  st_set_crs(crs_string) %>%
  # grid cells
  st_join(coastal_polygons) %>%
  st_drop_geometry()

data = data %>% left_join(check)


  