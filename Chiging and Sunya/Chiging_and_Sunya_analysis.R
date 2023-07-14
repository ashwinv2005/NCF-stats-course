## dealing with spatial data, Chiging

library(tidyverse)
library(sf)
library(extrafont)

## make sure that columns are named the same across files! (grid and ID)

## make sure that files are stored as separate CSVs, not XLSXs

siang_grids = st_read("Chiging and Sunya/Upper_east siang grids_2023/Upper_east siang grids_2023.shp")
siang_grids = siang_grids %>% dplyr::select(id,geometry)
siang_grids = st_make_valid(siang_grids)
plot(siang_grids)

## reading in aok and detection data

pang.det = read.csv("Chiging and Sunya/detection.csv")
aok = read.csv("Chiging and Sunya/aok.csv")

## how to bring data from a polygon into a dataframe with lat-long values

# first lets rename the grid 'id' column from pang.det to say, 'id.old'
pang.det = pang.det %>% rename(id.old = id)
crs_string = "+proj=longlat +datum=WGS84 +no_defs"
siang_grids = siang_grids %>% st_transform(crs_string)


check = pang.det %>%
  distinct(longitude, latitude) %>% 
  # joining map vars
  st_as_sf(coords = c("longitude", "latitude"), remove = F) %>%
  #st_set_crs(st_crs(siang_grids)) %>%
  st_set_crs(crs_string) %>%
  # grid cells
  st_join(siang_grids %>% dplyr::select(id)) %>%
  st_drop_geometry()

pang.det = pang.det %>% left_join(check)



## get information from the dataframe into the plot

dat = pang.det %>% group_by(id) %>% reframe(pr = sum(presence.absence)) %>%
  mutate(pr=replace(pr, pr >= 1, 1)) %>% dplyr::select(id,pr)

siang_grids.1 = left_join(siang_grids,dat,by = c('id' = "id")) 
#siang_grids.1 = siang_grids.1 %>% 
#  mutate(pr=replace(pr, is.na(pr), 0))
siang_grids.1$pr = as.factor(siang_grids.1$pr)
vals = c("#daeafa","#99CCFF","#6699CC","#336699","#003399")


ggplot() +
  geom_sf(data = siang_grids.1, aes(fill=pr), colour = "black")+  
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin=unit(c(0,0,0,0), "cm"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank())+
  scale_fill_manual(breaks = c(0,1),labels = c(0,1), values = vals[c(1,5)],
                    name = "occupancy") +
  theme(legend.title = element_text(size = 12), legend.text = element_text(size = 12),
        legend.position = "bottom")  +
  guides(fill = guide_legend(nrow = 1)) +
  coord_sf()





## expand 

expanded.det = pang.det %>%
  separate_rows(sighting.month, sep = ",")














## occupancy

require(tidyverse)
require(reshape2)
require(data.table)
require(unmarked)

setDT(selexp)

det = dcast(selexp, gridg ~ group.id, value.var = "OBSERVATION.COUNT")
cov.month = dcast(selexp, gridg ~ group.id, value.var = "month")
cov.nosp = dcast(selexp, gridg ~ group.id, value.var = "no.sp")

det = setDF(det)
cov.month = setDF(cov.month)
cov.nosp = setDF(cov.nosp)

umf = unmarkedFrameOccu(y=det[,-1], siteCovs = data.frame(nb8g = detn$nb8), 
                        obsCovs = list(cov1 = cov.nosp[,-1], 
                                       cov2 = cov.month[,-1]))


occu_det = occu(~log(cov1)*cov2 ~nb8g, data=umf, starts = c(0,0,0,0,0,0,0,0,0,0), 
     engine = "C")

f1 = predict(occ_det, newdata = newdat1, type = "det")
f2 = predict(occ_det, newdata = newdat2, type = "state")