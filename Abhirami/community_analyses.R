library(tidyverse)
library(vegan)

adult = read.csv("Abhirami/AdultTree_Final.csv") %>%
  dplyr::select(PlotID,Accepted_name,Height,Effective_dia)
sapling = read.csv("Abhirami/SaplingDATA.csv") %>%
  dplyr::select(PlotID,Accepted_name,Height,dbh1) %>%
  rename(Effective_dia = dbh1)
seedling = read.csv("Abhirami/SeedlingDATA.csv")
plot_details = read.csv("Abhirami/Plot_details.csv") %>%
  dplyr::select(PlotID,Category,Mean_LD,Mean_canopy)
species_details = read.csv("Abhirami/Species_details.csv") %>%
  dplyr::select(Accepted_name,Family,SuccessionalStatus,DispersalMode,Habit) %>%
  distinct()


# simple differences in alpha, beta and gamma diversity

# seedlings

test = seedling %>% group_by(PlotID,Accepted_name) %>% reframe(n = n()) %>%
  filter(n>1) %>% distinct(PlotID,Accepted_name)
rep.seedling = seedling %>% semi_join(test) %>%
  arrange(PlotID,Accepted_name)
seedling_wide = seedling %>%
  group_by(PlotID,Accepted_name) %>% slice(1) %>%
  pivot_wider(names_from = Accepted_name, values_from = Abundance, values_fill = 0) %>%
  column_to_rownames("PlotID")


seedling_div = seedling_wide %>% 
  mutate(shannon = diversity(seedling_wide, "shannon")) %>%
  rownames_to_column("PlotID") %>%
  dplyr::select(PlotID,shannon)


## Plot diversity per category (mean and SE)
## Calculate diversity of family, successional status and dispersal mode






# beta diversity

seedling_wide_AC = seedling %>%
  group_by(PlotID,Accepted_name) %>% slice(1) %>%
  pivot_wider(names_from = Accepted_name, values_from = Abundance, values_fill = 0) %>%
  left_join(plot_details %>% dplyr::select(PlotID,Category)) %>%
  filter(Category == "AC") %>% dplyr::select(-Category) %>%
  column_to_rownames("PlotID")

seedling_wide_EC = seedling %>%
  group_by(PlotID,Accepted_name) %>% slice(1) %>%
  pivot_wider(names_from = Accepted_name, values_from = Abundance, values_fill = 0) %>%
  left_join(plot_details %>% dplyr::select(PlotID,Category)) %>%
  filter(Category == "EC") %>% dplyr::select(-Category) %>%
  column_to_rownames("PlotID")

seedling_wide_RF = seedling %>%
  group_by(PlotID,Accepted_name) %>% slice(1) %>%
  pivot_wider(names_from = Accepted_name, values_from = Abundance, values_fill = 0) %>%
  left_join(plot_details %>% dplyr::select(PlotID,Category)) %>%
  filter(Category == "RF") %>% dplyr::select(-Category) %>%
  column_to_rownames("PlotID")

beta_AC = betadiver(seedling_wide_AC,'w')
beta_AC




# pairwise Bray-Curtis

beta_AC = vegdist(decostand(seedling_wide_AC,"hell"), "bray") #decostand standardizes abundances to 1
beta_EC = vegdist(decostand(seedling_wide_EC,"hell"), "bray")
beta_RF = vegdist(decostand(seedling_wide_RF,"hell"), "bray")

AC = data.frame(Category = "AC", beta = c(beta_AC))

# create a dataframe with all three and calculate and plot means and SEs
# stop here






# can use betadisper with groups, worth trying

beta = betadiver(seedling_wide,'w')
groups = seedling %>%
  group_by(PlotID,Accepted_name) %>% slice(1) %>%
  pivot_wider(names_from = Accepted_name, values_from = Abundance, values_fill = 0) %>%
  left_join(plot_details %>% dplyr::select(PlotID,Category)) 

disp = betadisper(beta, groups$Category, type = c("median","centroid"), bias.adjust = FALSE)





# ordination analyses 

seedling_ord = metaMDS(seedling_wide,distance = "bray", k = 2, trymax = 50)
