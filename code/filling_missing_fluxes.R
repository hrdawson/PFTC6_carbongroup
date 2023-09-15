my_packages <- c("dataDownloader",
                 "tidyverse",
                 "lubridate",
                 "ggpubr",
                 "viridis",
                 "patchwork"
)

lapply(my_packages, library, character.only = TRUE) 

get_file(node = "fcbw4",
         file = "PFTC6_24h_cflux_allsites_2022.csv",
         path = "clean_data",
         remote_path = "c_flux_data")

get_file(node = "fcbw4",
         file = "PFTC6_microclimate_allsites_2022.csv", #this version of the file has big holes, trying to figure out with Hilary what happened
         path = "clean_data",
         remote_path = "microclimate")

fluxes <- read_csv("clean_data/PFTC6_24h_cflux_allsites_2022.csv")

microclimate <- read_csv("clean_data/PFTC6_microclimate_allsites_2022.csv") %>% 
  mutate(
    site = str_to_lower(site),
    site = as_factor(site)
  )

# Strategy 1: Drop ----
# drop that missing flux value(s) at that time for all the sites 
# so that all sites have the same number of flux measures

# How many times are fluxes missing?
fluxes.drop = fluxes |>
  mutate(flux.hour = paste0("H", hour(datetime))) |>
  filter(!flag %in% c("start_error", "discard", "weird_flux")) |>
  group_by(type, site, flux.hour) |>
  summarize(n = length(flux_corrected)) |>
  pivot_wider(names_from = flux.hour, values_from = n)

# Every hour has multiple fluxes missing so that's not very feasible

# Strategy 2: Fill ----
 #fill in the missing data with a gap filling method 
# (maybe interpolate based on diurnal curve, or take an average between two measures 
# that bookend the missing value)
missing.lia = fluxes |>
  filter(site == "liahovden") |>
  select(site, turfID, type) |>
  distinct() |>
  mutate(datetime = ymd_hms("2022-07-27 00:00:01"),
         flux.hour = hour(datetime))
  

fluxes.fill = fluxes |>
  mutate(flux.hour = hour(datetime)) |>
  # Add in missing Lia hour
  bind_rows(missing.lia) |>
  # Group so we don't accidentally match the wrong turf/type
  group_by(turfID, type) |>
  arrange(flux.hour) |>
  # Hacking tidyverse to take means of rows above and below
  mutate(downup = flux_corrected,
         updown = flux_corrected) |>
  fill(downup, .direction = "downup") |>
  fill(updown, .direction = "updown") |>
  # Average all three (original and filled) to get means
  group_by(site, turfID, type, datetime, flux.hour) |>
  rowwise() |>
  summarize(flux_filled = mean(downup, updown)) |>
  mutate( #making things easier to make graphs later
    time = hms::as_hms(datetime),
    site = factor(site, levels = c("vikesland", "hogsete", "joasete", "liahovden"))
  )

# Visualize on the diurnal to check for egregious mistakes ----
diurnal_fluxes <- fluxes.fill %>% 
  filter(
    type %in% c("ER", "GPP", "NEE")
  ) %>%
  ggplot(aes(time, flux_filled, color=site)) +
  geom_point(size=0.05) +
  geom_smooth(method = "loess", span = 0.3) +
  facet_grid(type~., scales = "free") +
  scale_color_viridis(discrete=T) +
  theme_bw() +
  ## For some reason this theme is throwing an error...
  # theme(legend.position="none",
  #       text=element_text(size=font_size)) +
  labs(
    y="Fluxes",
    x= "Time"
  )
diurnal_fluxes

# Example boxplot for culmulative fluxes ----
fluxes_cumul <- fluxes.fill %>% 
  filter(
    type %in% c("ER", "GPP", "NEE")
  ) %>% 
  group_by(site, type, turfID) %>% 
  summarise(
    cumul_flux = sum(flux_filled) # we sum each fluxes for each turfID
  ) %>% 
  ggplot(aes(y = cumul_flux, x = site, fill = site, color = site)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  geom_jitter() +
  scale_fill_viridis(discrete=T, labels = c( #this is the plot providing the legend to the patchwork
    hogsete = "Hogsete",
    joasete = "Joasete",
    liahovden = "Liahovden",
    vikesland = "Vikesland"
  )) +
  scale_color_viridis(discrete=T, labels = c(
    hogsete = "Hogsete",
    joasete = "Joasete",
    liahovden = "Liahovden",
    vikesland = "Vikesland"
  )) +
  scale_y_continuous(position = "right") +
  labs(
    y="Cumulative fluxes",
    fill = "Site",
    color = "Site"
  ) +
  facet_wrap(~type, scales = "free", ncol = 1) +
  theme_bw() +
  theme(
    # strip.text.x = element_blank(),
    axis.title.x = element_blank(),
    # axis.text.x=element_blank(),
    # axis.ticks.x=element_blank(),
    # text=element_text(size=font_size)
    legend.position = "none"
  )

fluxes_cumul
ggsave("visualizations/2023.09.14_fluxesfilled.png", width = 8, height = 6, units = "in")
