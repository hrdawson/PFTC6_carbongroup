library(dataDownloader)
# library(tidyverse)
source("code/metaturf.R")


# meta turfID -------------------------------------------------------------

# get_file(node = "pk4bg",
#          file = "Three-D_metaturfID.csv",
#          path = "clean_data",
#          remote_path = "Site")
# 
# metadata <- read_csv("clean_data/Three-D_metaturfID.csv") %>% 
#   select(warming, turfID)


# CFlux data -------------------------------------------------------------

get_file(node = "fcbw4",
         file = "PFTC6_24h-cflux_vikesland_2022.csv",
         path = "clean_data",
         remote_path = "c_flux_data")

cflux_vikesland <- read_csv("clean_data/PFTC6_24h-cflux_vikesland_2022.csv", col_types = "ffdddTtd")

get_file(node = "fcbw4",
         file = "PFTC6_24h-cflux_hogsete_2022.csv",
         path = "clean_data",
         remote_path = "c_flux_data")

cflux_hogsete <- read_csv("clean_data/PFTC6_24h-cflux_hogsete_2022.csv", col_types = "ffdddTtd")

get_file(node = "fcbw4",
         file = "PFTC6_24h-cflux_liahovden_2022.csv",
         path = "clean_data",
         remote_path = "c_flux_data")

cflux_liahovden <- read_csv("clean_data/PFTC6_24h-cflux_liahovden_2022.csv", col_types = "ffdddTtd")

get_file(node = "fcbw4",
         file = "PFTC6_24h-cflux_joasete_2022.csv",
         path = "clean_data",
         remote_path = "c_flux_data")

cflux_joasete <- read_csv("clean_data/PFTC6_24h-cflux_joasete_2022.csv", col_types = "ffdddTtd")

#all fluxes together
cflux <- bind_rows(cflux_vikesland, cflux_hogsete, cflux_liahovden, cflux_joasete) %>% 
  right_join(metaturf)