#load in packages
source("setup.R")

tmap_mode("view")
save(tidyResChem, file = "tidyResChem.RData")

res_chem <- read.csv("prepped_reservoir_chemistry.csv")

#### Map ####
##### Data -> spatial points ####

# need to remove NA rows on the uniqueSite list
resChemNA <- res_chem %>% 
  drop_na(Lat)

spatialResChem <- st_as_sf(resChemNA, 
                 coords = c("Long", "Lat"),
                 crs = 4326)

qtm(spatialResChem)

#### make the dates in the correct format, ####
# first establish them as characters,
spatialResChem$newdate <- 
  strptime(as.character(spatialResChem$Date), "%m/%d/%Y")

# then establish it as a date, in the correct format
spatialResChem$txtdate <- format(spatialResChem$newdate, "%Y-%m-%d")

# now remove the unnecessary columns
tidyResChem <- subset(spatialResChem, select = -c(Date, txtdate))





