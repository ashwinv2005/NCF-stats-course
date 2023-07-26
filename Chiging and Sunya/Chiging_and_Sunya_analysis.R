## dealing with spatial data, Chiging

library(tidyverse)
library(sf)
library(extrafont)

## make sure that columns are named the same across files! (grid and ID)

## make sure that files are stored as separate CSVs, not XLSXs

siang_grids = st_read("Chiging and Sunya/Upper_east siang grids_2023/Upper_east siang grids_2023.shp")
siang_grids = siang_grids %>% dplyr::select(id,geometry)
siang_grids = st_make_valid(siang_grids)
#plot(siang_grids)

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




## set up the data for occupancy

# step 1 - create aok transit, expand various rows, prepare for left join with det

aok.expanded = aok %>%
  mutate(visit.reason = "transit") %>%
  separate_rows(other.visit.grids, sep = ";") %>%
  separate_rows(visit.month, sep = ";") %>%
  separate_rows(visit.reason, sep = ",") %>%
  mutate(id = as.numeric(other.visit.grids)) %>%
  bind_rows(aok %>% separate_rows(visit.month, sep = ";") %>% separate_rows(visit.reason, sep = ",")) %>% 
  select(-date,-village,-latitude,-longitude,-distance,-other.visit.grids) %>%
  filter(!is.na(id)) %>% rename(month = visit.month) %>%
  mutate(month = as.numeric(month)) %>% 
  mutate(season = case_when(month %in% c(12,1,2) ~ "Win",
                           month %in% c(3,4,5) ~ "Sum",
                           month %in% c(6,7,8) ~ "Mon",
                           month %in% c(9,10,11) ~ "Aut")) %>% 
  mutate(season = as.factor(season)) %>%
  mutate(visit.reason = case_when(visit.reason %in% c("trapping","takin hunting","squirrel hunting",
                                                      "ritual hunting","junglefowl hunting",
                                                      "bat hunting","hunting","Pangolin searching") ~ "hunting",
                                  visit.reason %in% c("vegetable farming","teak farming","tea plantation",
                                                      "sugarcane farming","rubber farming","rice farming",
                                                      "pepper farming","palm oil plantation","orange farming",
                                                      "lemon farming","jhum farming","ginger farming",
                                                      "cardamom farming","bamboo garden") ~ "farming",
                                  visit.reason %in% c("tuber collection","timber collection","silkworm rearing",
                                                      "pig fodder collection","phrynium pubinerve collection",
                                                      "mushroom collection","livistona jenkinsiana collection",
                                                      "frog collection","fishing","firewood collection",
                                                      "cane collection","bamboo collection","ant larvae collection",
                                                      "paris polyphylla collection") ~ "collection",
                                  visit.reason %in% c("cow searching","cow grazing","mithun searching") ~ "cattle",
                                  visit.reason %in% c("transit") ~ "transit",
                                  visit.reason %in% c("resident") ~ "resident",
                                  TRUE ~ "miscellaneous")) %>% 
  mutate(visit.reason = as.factor(visit.reason)) %>%
  group_by(unique.ind,id,season,visit.reason) %>% slice(1) %>%
  # remove instances of multiple visits to the same grids by the same person 
  ungroup()

# step 2 - expand detection data

det.expanded = pang.det %>%
  separate_rows(sighting.month, sep = ";") %>%
  select(unique.ind,id,sighting.month,signs,presence.absence) %>%
  filter(!is.na(id)) %>% rename(month = sighting.month) %>%
  mutate(month = as.numeric(month)) %>% 
  mutate(season = case_when(month %in% c(12,1,2) ~ "Win",
                            month %in% c(3,4,5) ~ "Sum",
                            month %in% c(6,7,8) ~ "Mon",
                            month %in% c(9,10,11) ~ "Aut")) %>% 
  mutate(season = as.factor(season)) %>%
  group_by(unique.ind,id,season) %>% arrange(desc(presence.absence)) %>% 
  slice(1) %>%
  # remove instances of multiple visits to the same grids by the same person
  ungroup()             

# step 3 - divide det.expanded into two - 1) with month info 2) without month info

det.expanded.1 = det.expanded %>%
  filter(!is.na(season)) %>% select(-month)
det.expanded.2 = det.expanded %>%
  filter(is.na(season)) %>% select(-month)

# step 4 - combine aok and det data, separately where there is month info and where there isn't

aok.det.1 = aok.expanded %>% select(-month) %>%
  left_join(det.expanded.1)

aok.det.2 = aok.expanded %>% select(-month) %>%
  left_join(det.expanded.2)

aok.det = aok.det.1 %>%
  bind_rows(aok.det.2) %>%
  distinct() %>%
  mutate(presence.absence = case_when(is.na(presence.absence)~0,TRUE~presence.absence)) %>%
  mutate(id = as.character(id)) %>%
  group_by(id) %>% mutate(no.visit = 1:n()) %>% ungroup()


# step 5 - bring in elevation

ele = read.csv("Chiging and Sunya/mean_elevation_up_east.csv")
ele = ele %>% filter(!is.na(mean)) %>%
  rename(elevation = mean) %>%
  mutate(elevation = round(elevation/2500,3), id = as.character(id))



# step 6 - now that we have information about presence/absence, it's time to create the occupancy objects

library(reshape2)
library(data.table)
library(unmarked)

setDT(aok.det)

det = dcast(aok.det, id ~ no.visit, value.var = "presence.absence")
cov.season = dcast(aok.det, id ~ no.visit, value.var = "season")
cov.days.per.visit = dcast(aok.det, id ~ no.visit, value.var = "days.per.visit")
cov.visit.reason = dcast(aok.det, id ~ no.visit, value.var = "visit.reason")


det = setDF(det)
cov.season = setDF(cov.season)
cov.days.per.visit = setDF(cov.days.per.visit)
cov.visit.reason = setDF(cov.visit.reason)


# step 7 - align site covariates like elevation

det.ele = det %>% left_join(ele) %>% select(id,elevation)




# step 8 - run the occupancy analysis

umf = unmarkedFrameOccu(y=det[,-1], siteCovs = data.frame(ele = det.ele$elevation), 
                        obsCovs = list(cov1 = cov.season[,-1], 
                                       cov2 = cov.days.per.visit[,-1],
                                       cov3 = cov.visit.reason[,-1]))


occu_det = occu(~ cov1 + cov3 ~ ele, data=umf, starts = rep(0,12), 
                engine = "C")
summary(occu_det)



f1 = predict(occu_det, type = 'state')













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


