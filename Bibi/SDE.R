library(lubridate)
library(tidyverse)


boot_function <- function(c)
{
  data = c
  mn = numeric(1000)
  for (i in 1:1000)
  {
    temp = sample(data, replace = T)
    mn[i] = mean (temp, na.rm= T)
  }
  lci = quantile(mn, 0.025, na.rm= T)
  mean = median(mn,na.rm= T)
  rci = quantile(mn,0.975, na.rm= T)
  results = c(lci, mean, rci)
  return(results)
}  

visit = read.csv("Bibi/tree_watch_visitation_rate.csv") %>%
  mutate(species_group = tolower(species_group))
focal = read.csv("Bibi/focal_scan.csv") %>%
  mutate(species_group = tolower(species_group))


# first get an idea of how many fruits are eaten/handled per unit time by each frugivore species
# on each plant species

focal = focal %>%
  mutate(arr_time = ifelse(nchar(arr_time) == 4, paste0("0",arr_time,sep=""), arr_time)) %>%
  mutate(dep_time = ifelse(nchar(dep_time) == 4, paste0("0",dep_time,sep=""), dep_time)) %>%
  mutate(
    numeric_arr = as.numeric(substring(arr_time, 1, 2)) * 60 +
      as.numeric(substring(arr_time, 4, 5)),
    numeric_dep = as.numeric(substring(dep_time, 1, 2)) * 60 +
      as.numeric(substring(dep_time, 4, 5))
  )

frugivore_intake = focal %>%
  mutate(handled_per_min = seed_handled/(numeric_dep-numeric_arr),
          dispersed_per_min = number_of.seed_dispersed/(numeric_dep-numeric_arr)) %>%
  group_by(species_group,ficus_nonficus) %>%
  reframe(handled_per_min_cil = boot_function(handled_per_min)[1],
          handled_per_min_mean = boot_function(handled_per_min)[2],
          handled_per_min_cir = boot_function(handled_per_min)[3],
          dispersed_per_min_cil = boot_function(dispersed_per_min)[1],
          dispersed_per_min_mean = boot_function(dispersed_per_min)[2],
          dispersed_per_min_cir = boot_function(dispersed_per_min)[3])

# next step is to find out how much time each frugivore is spending on each tree species per watch

visit = visit %>%
  mutate(arr_time = ifelse(nchar(arr_time) == 4, paste0("0",arr_time,sep=""), arr_time)) %>%
  mutate(dep_time = ifelse(nchar(dep_time) == 4, paste0("0",dep_time,sep=""), dep_time)) %>%
  mutate(
    numeric_arr = as.numeric(substring(arr_time, 1, 2)) * 60 +
      as.numeric(substring(arr_time, 4, 5)),
    numeric_dep = as.numeric(substring(dep_time, 1, 2)) * 60 +
      as.numeric(substring(dep_time, 4, 5))
  )

frugivore_visit = visit %>%
  mutate(time_per_watch = numeric_dep-numeric_arr) %>%
  group_by(date,tree_code,ficus_nonficus,species_group) %>%
  reframe(time_per_watch = sum(time_per_watch)) %>%
  filter(!is.na(time_per_watch)) %>%
  group_by(species_group,ficus_nonficus) %>%
  reframe(time_per_watch = mean(time_per_watch))


frugivore_visit_intake = frugivore_visit %>%
  left_join(frugivore_intake) %>%
  filter(!is.na(handled_per_min_mean)) %>%
  mutate(handled_cil = handled_per_min_cil*time_per_watch,
         handled_mean = handled_per_min_mean*time_per_watch,
         handled_cir = handled_per_min_cir*time_per_watch,
         dispersed_cil = dispersed_per_min_cil*time_per_watch,
         dispersed_mean = dispersed_per_min_mean*time_per_watch,
         dispersed_cir = dispersed_per_min_cir*time_per_watch) %>%
  filter(!species_group %in% c("oriole"))

ficus_final = frugivore_visit_intake %>% filter(ficus_nonficus == "ficus")
nonficus_final = frugivore_visit_intake %>% filter(ficus_nonficus == "non_ficus")

# scale to 1

ficus_final$scaled_handled_cil = ficus_final$handled_per_min_cil/max(ficus_final$handled_per_min_cir)
ficus_final$scaled_handled_mean = ficus_final$handled_per_min_mean/max(ficus_final$handled_per_min_cir)
ficus_final$scaled_handled_cir = ficus_final$handled_per_min_cir/max(ficus_final$handled_per_min_cir)

ficus_final$scaled_dispersed_cil = ficus_final$dispersed_per_min_cil/max(ficus_final$dispersed_per_min_cir)
ficus_final$scaled_dispersed_mean = ficus_final$dispersed_per_min_mean/max(ficus_final$dispersed_per_min_cir)
ficus_final$scaled_dispersed_cir = ficus_final$dispersed_per_min_cir/max(ficus_final$dispersed_per_min_cir)


nonficus_final$scaled_handled_cil = nonficus_final$handled_per_min_cil/max(nonficus_final$handled_per_min_cir)
nonficus_final$scaled_handled_mean = nonficus_final$handled_per_min_mean/max(nonficus_final$handled_per_min_cir)
nonficus_final$scaled_handled_cir = nonficus_final$handled_per_min_cir/max(nonficus_final$handled_per_min_cir)

nonficus_final$scaled_dispersed_cil = nonficus_final$dispersed_per_min_cil/max(nonficus_final$dispersed_per_min_cir)
nonficus_final$scaled_dispersed_mean = nonficus_final$dispersed_per_min_mean/max(nonficus_final$dispersed_per_min_cir)
nonficus_final$scaled_dispersed_cir = nonficus_final$dispersed_per_min_cir/max(nonficus_final$dispersed_per_min_cir)


df <- expand.grid(A = seq(0, 1, 0.001), B = seq(0, 1, .001))
df1 = df %>%
  mutate(AB = round(A*B,3)) %>% 
  filter(AB > 0, AB %in% c(0.01,0.04,0.09,0.16,0.25,0.36,0.49,0.64,0.81,0.95))


ggplot(data = ficus_final, aes(x = scaled_handled_mean, y = scaled_dispersed_mean)) +
  geom_point() +
  geom_text(aes(label = species_group)) +
  geom_errorbar(aes(ymin = scaled_dispersed_cil, ymax = scaled_dispersed_cir)) +
  geom_errorbar(aes(xmin = scaled_handled_cil, xmax = scaled_handled_cir)) +
  geom_line(data = df1, aes(x = A, y = B, group = AB))


ggplot(data = nonficus_final, aes(x = scaled_handled_mean, y = scaled_dispersed_mean)) +
  geom_point() +
  geom_text(aes(label = species_group)) +
  geom_errorbar(aes(ymin = scaled_dispersed_cil, ymax = scaled_dispersed_cir)) +
  geom_errorbar(aes(xmin = scaled_handled_cil, xmax = scaled_handled_cir)) +
  geom_line(data = df1, aes(x = A, y = B, group = AB))

