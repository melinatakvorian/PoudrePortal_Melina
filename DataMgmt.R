#load in packages
source("setup.R")

res_chem <- read.csv("prepped_reservoir_chemistry.csv")

# Creating tidyResChem -------------------

# need to remove NA rows on the uniqueSite list
resChemNA <- res_chem %>% 
  drop_na(Lat)

spatialResChem <- st_as_sf(resChemNA, 
                 coords = c("Long", "Lat"),
                 crs = 4326)

#### make the dates in the correct format, ####
# first establish them as characters,
spatialResChem$newdate <- 
  strptime(as.character(spatialResChem$Date), "%m/%d/%Y")
# then establish it as a date, in the correct format
spatialResChem$txtdate <- format(spatialResChem$newdate, "%Y-%m-%d")
# make thes date an actual Date class
spatialResChem$dates <- as.Date(spatialResChem$newdate) 
# now remove the unnecessary columns
tidyResChem <- subset(spatialResChem, select = -c(Date, txtdate, newdate)) 

latLong <- select(resChemNA, Site, Lat, Long)

tidyResChem <- left_join(tidyResChem, latLong, by = "Site")

save(tidyResChem, file = "data/tidyResChem.RDS")




# Creating sites_table -----------------------
## Caitlin way for sites_table -----------
data_avail_chem <- resChemNA %>% group_by(Site) %>% 
  summarise(across(c("Turbidity","TSS", "ChlA", "DOC", "DTN", "pH", "ANC","SC", 
                     "Na",  "NH4", "K", "Mg",  "Ca",  "F", "Cl", "NO3", "PO4","SO4"),
                   ~paste(unique(.), collapse = ""))) %>% 
  mutate(data_available = paste("Turbidity","TSS", "ChlA", "DOC", "DTN", "pH", "ANC","SC", 
                                "Na",  "NH4", "K", "Mg",  "Ca",  "F", "Cl", "NO3", "PO4","SO4")) %>% 
  dplyr::select(Site, data_available)


sites_table <- 
  resChemNA %>%
  group_by(Site) %>%
  mutate(start_date = min(Date),
         end_date = max(Date)) %>%
  ungroup() %>%
  distinct(Site, .keep_all = TRUE) %>% 
  dplyr::select(Site, Long, Lat, start_date, end_date) %>%
  left_join(data_avail_chem, by = "Site")


save(sites_table, file = "data/sites_table.RDS")

