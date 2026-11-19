# ---- header ----
#
# author: sheila saia
# email: sheila@rladies.org
# date: 2026-11-15
#
# script name: 
#
#
# script description:
#
#
# ---- notes: -----
#
#
# ---- load libraries -----
library(tidyverse)
library(here)
library(fs)
library(sf)
library(nhdplusTools)
library(tmap)
library(tmaptools)
library(tidycensus)
library(usethis)
library(dataRetrieval)
library(RColorBrewer)

# see if census api key is set
my_env <- Sys.getenv()
my_env[grep("CENSUS", names(my_env), ignore.case = TRUE)]
# none listed at first

# edit this once for your current r installation
usethis::edit_r_environ()
# add something like CENSUS_API_KEY = "YOUR KEY FROM USCB HERE"
# you can request an api key at https://www.census.gov/data/developers/guidance/api-user-guide.API_Key.html - click on the "request a key" link
# you will need to restart r for it to work

# check again after setting
Sys.getenv("CENSUS_API_KEY")
# prints out key!, check.

# set key for tidycensus package
tidycensus::census_api_key(key = Sys.getenv("CENSUS_API_KEY"),
                           install = TRUE,
                           overwrite = TRUE)

# see if usgs api key is set
my_env[grep("USGS", names(my_env), ignore.case = TRUE)]
# none listed at first

# edit this once for your current r installation
usethis::edit_r_environ()
# add something like API_USGS_PAT = "YOUR KEY FROM USCB HERE"
# you can request an api key at https://api.waterdata.usgs.gov/signup/
# see setup notes here https://water.code-pages.usgs.gov/dataRetrieval/articles/read_waterdata_functions.html
# you will need to restart r for it to work

# check again after setting
Sys.getenv("API_USGS_PAT")
# prints out key!, check.

# define projections
wgs84_proj <- sf::st_crs(4326) # wgs 1984 (lat/lon, unprojcted)
conus_aeac_proj <- sf::st_crs(5070) # albers equal area conic for conus (meters, projected)


# ----- get county bounds -----
# get counties
nc_counties_sf <- get_acs(geography = "county",
                          state = "NC",
                          variables = "B01003_001",  # total population (any variable is ok to use)
                          year = 2023,
                          geometry = TRUE)

# get sampson county and drop acs variables (don't need those)
sampson_bounds_sf <- nc_counties_sf |>
  filter(NAME == "Sampson County, North Carolina") |>
  select(GEOID, NAME, geometry) |>
  st_transform(crs = conus_aeac_proj)

# export
sf::st_write(obj = sampson_bounds_sf,
             dsn = here::here("data", "sampson_county_bounds.gpkg"),
             layer = "county_bounds",
             delete_dsn = TRUE)

# check crs
st_crs(sampson_bounds_sf)
# 5070, check.

# ---- use county bounds to get ws bounds ----
# get huc-10 bounds
sampson_huc10_bounds_sf <- nhdplusTools::get_huc(AOI = sampson_bounds_sf,
                                              type = "huc10") |>
  sf::st_transform(crs = conus_aeac_proj) |>
  mutate(loaddate = as.character(loaddate)) |>
  select(huc10, name, loaddate, areaacres:states, geometry)
# overlaps with 11 huc10 ws's

# check crs
st_crs(sampson_huc10_bounds_sf)
# 5070, check!

# plot to check
tm_shape(sampson_huc10_bounds_sf) +
  tm_polygons(fill = "white", col = "blue") +
  tm_shape(sampson_bounds_sf) +
  tm_polygons(fill = "grey90", col = "black", lwd = 2, fill_alpha = 0)
# looks good!

# get little and great coharie river huc10's
gcr_huc10_bounds_sf <- sampson_huc10_bounds_sf |>
  filter(huc10 %in% c("0303000603", "0303000604", "0303000608"))

# get huc-12 bounds (just to check)
sampson_huc12_bounds_sf <- nhdplusTools::get_huc(AOI = sampson_bounds_sf,
                                                 type = "huc12") |>
  sf::st_transform(crs = conus_aeac_proj) |>
  mutate(loaddate = as.character(loaddate)) |>
  select(huc12, name, loaddate, areaacres:states, geometry)
# overlaps with 47 huc12 ws's

# check crs
st_crs(sampson_huc12_bounds_sf)
# 5070. check!

# plot to check
tm_shape(sampson_huc12_bounds_sf) +
  tm_polygons(fill = "white", col = "blue") +
  tm_shape(sampson_bounds_sf) +
  tm_polygons(fill = "grey90", col = "black", lwd = 2, fill_alpha = 0) +
  tm_shape(gcr_huc10_bounds_sf) +
  tm_polygons(fill = "grey90", col = "darkgreen", lwd = 2.5, fill_alpha = 0)
# looks good!

# export
sf::st_write(obj = sampson_huc10_bounds_sf,
             dsn = here::here("data", "sampson_huc10_bounds.gpkg"),
             layer = "ws_bounds",
             delete_dsn = TRUE)
sf::st_write(obj = gcr_huc10_bounds_sf,
             dsn = here::here("data", "gcr_huc10_bounds.gpkg"),
             layer = "ws_bounds",
             delete_dsn = TRUE)
sf::st_write(obj = sampson_huc12_bounds_sf,
             dsn = here::here("data", "sampson_huc12_bounds.gpkg"),
             layer = "ws_bounds",
             delete_dsn = TRUE)


# ---- pull all usgs gages in the gcr bounds ----
# look at codes, etc.
# param_codes <- dataRetrieval::read_waterdata_metadata("parameter-codes")
# site_types <- dataRetrieval::read_waterdata_metadata("site-types")
# statistic_codes <- read_waterdata_metadata("statistic-codes")

# get gcr sites
nc_gage_initial_sf <- dataRetrieval::read_waterdata_monitoring_location(state_name = "North Carolina",
                                                                        agency_name = "U.S. Geological Survey",
                                                                        site_type_code = "ST")
# check
length(unique(nc_gage_initial_sf$monitoring_location_id)) # 2194

# trim down site numbers df
gcr_gage_sel_sf <- nc_gage_initial_sf |>
  st_transform(crs = conus_aeac_proj) |>
  select(monitoring_location_id, monitoring_location_name, state_code, district_code,
         country_code, county_name, site_type, site_type_code) |>
  st_intersection(gcr_huc10_bounds_sf)
# 14 usgs gages

# drop geometry
gcr_gage_sel_df <- gcr_gage_sel_sf |>
  st_drop_geometry()

# make mostly empty sf object to append to
gcr_gage_meta_sf <- NULL

# define vars
itt = 1 # counter
chunk_size = 1 # chunk size

# loop
if (length(unique(gcr_gage_sel_sf$monitoring_location_id)) > chunk_size) {
  # define number of chunks
  num_chunks = ceiling(length(gcr_gage_sel_sf$monitoring_location_id)/chunk_size)
  
  # define start and end site positions
  start_site_pos = itt
  end_site_pos = chunk_size
  
  for(c in 1:num_chunks) {
    # query
    temp_request <- dataRetrieval::read_waterdata_ts_meta(monitoring_location_id = gcr_gage_sel_sf$monitoring_location_id[start_site_pos:end_site_pos],
                                                          parameter_code = "00060", # discharge
                                                          computation_period_identifier = "Daily", # daily
                                                          statistic_id = "00003") # mean
    
    # if there's data
    if (dim(temp_request)[1] > 0) {
      # pair down to select rows
      temp_request_sel <- temp_request |>
        select(monitoring_location_id, begin, end, unit_of_measure, parameter_code,
               parameter_name, parameter_description, statistic_id, computation_period_identifier,
               computation_identifier, geometry)
      
      # append
      gcr_gage_meta_sf <- bind_rows(gcr_gage_meta_sf, temp_request_sel)
      
      # advance position
      start_site_pos = end_site_pos + 1
      end_site_pos = chunk_size * (c+1)
      
      # print
      print(paste0("gages ", start_site_pos, " to ", end_site_pos, " requested."))
      print(paste0("chunk #", c, " of ", num_chunks, " chunks"))
      
    } else {
      # advance position
      start_site_pos = end_site_pos + 1
      end_site_pos = chunk_size * (c+1)
      
      # print
      print(paste0("gages ", start_site_pos, " to ", end_site_pos, " requested."))
      print(paste0("chunk #", c, " of ", num_chunks, " chunks was empty. skipping to next."))
    }
  }
} else {
  # query
  gcr_gage_meta_sf <- dataRetrieval::read_waterdata_ts_meta(monitoring_location_id = gcr_gage_sel_sf$monitoring_location_id,
                                                           parameter_code = "00060", # discharge
                                                           computation_period_identifier = "Daily", # daily
                                                           statistic_id = "00003") |> # mean
    select(monitoring_location_id, begin, end, unit_of_measure, parameter_code,
           parameter_name, parameter_description, statistic_id, computation_period_identifier,
           computation_identifier, geometry)
}
# this will take a little bit of time to run (~1 second per chunk)

# check
length(unique(gcr_gage_meta_sf$monitoring_location_id)) # 3
# only one is still running
st_crs(gcr_gage_meta_sf)

# current year
current_year <- 2026

# clip down to current ones (and also keep little coharie river)
gcr_gage_meta_tokeep_sf <- gcr_gage_meta_sf |>
  st_transform(crs = conus_aeac_proj) |>
  mutate(monitoring_location_id = as.character(monitoring_location_id),
         begin_date = as.Date(begin),
         end_date = as.Date(end),
         unit_of_measure = as.character(unit_of_measure),
         parameter_code = as.character(parameter_code),
         parameter_name = as.character(parameter_name),
         statistic_id = as.character(statistic_id),
         computation_period_identifier = as.character(computation_period_identifier),
         computation_identifier = as.character(computation_identifier),
         end_year = year(end)) |>
  filter(end_year == current_year | end_year == 1992) |> # keep the gage on little coharie river too
  distinct() |>
  left_join(gcr_gage_sel_df, by = "monitoring_location_id") |>
  select(monitoring_location_id:computation_identifier, end_year:site_type_code, geometry)

# export
sf::st_write(obj = gcr_gage_meta_tokeep_sf,
             dsn = here::here("data", "gcr_gage_meta_tokeep.gpkg"),
             layer = "gages", 
             delete_layer = TRUE)

# pull data for little coharie river
lcr_gage_id <- gcr_gage_meta_tokeep_sf$monitoring_location_id[1]
lcr_start_date <- "1951-01-01"
lcr_end_date <- "1991-12-31"
lcr_discharge_data <- read_waterdata_daily(monitoring_location_id = lcr_gage_id,
                                     parameter_code = c("00060"), # discharge
                                     statistic_id = "00003", # mean
                                     time = c(lcr_start_date, lcr_end_date)) |>
  st_drop_geometry()

# pull data for black river
br_gage_id <- gcr_gage_meta_tokeep_sf$monitoring_location_id[2]
br_start_date <- "1952-01-01"
br_end_date <- "2025-12-31"
br_discharge_data <- read_waterdata_daily(monitoring_location_id = br_gage_id,
                                           parameter_code = c("00060"), # discharge
                                           statistic_id = "00003", # mean
                                           time = c(br_start_date, br_end_date)) |>
  st_drop_geometry()

# export
write_csv(x = lcr_discharge_data, file = here::here("data", "lcr_discharge.csv"))
write_csv(x = br_discharge_data, file = here::here("data", "br_discharge.csv"))

# pull out year and doy
lcr_years_data <- lcr_discharge_data |>
  mutate(year = year(time),
         doy = yday(time))
br_years_data <- br_discharge_data |>
  mutate(year = year(time),
         doy = yday(time))


# ---- lcr data exploration ----
# daily discharge vs time
ggplot(data = lcr_discharge_data) +
  geom_line(mapping = aes(x = time, y = value)) +
  labs(x = "Time", y = "Dischrage (cfs)", title = "Little Coharie Creek Gage (USGS-02106000)") +
  theme_classic()
ggsave(filename = here::here("data", "lcr_q_vs_time.png"), width = 10, height = 8, dpi = 200)

# daily maximum annual series
lcr_ams_data <- lcr_discharge_data |>
  mutate(year = year(time)) |>
  group_by(year) |>
  summarize(max_discharge_cfs = max(value, na.rm = TRUE))
ggplot(data = lcr_ams_data) +
  geom_line(mapping = aes(x = year, y = max_discharge_cfs)) +
  labs(x = "Year", y = "Annual Maximum Daily Discharge (cfs)", title = "Little Coharie Creek Gage (USGS-02106000)") +
  theme_classic()
ggsave(filename = here::here("data", "lcr_max_q_vs_time.png"), width = 10, height = 8, dpi = 200)

# daily discharge vs time with years overlayed
ggplot(data = lcr_years_data) +
  geom_line(mapping = aes(x = doy, y = value, color = year)) +
  labs(x = "Day of Year", y = "Discharge (cfs)",
       title = "Little Coharie Creek Gage (USGS-02106000)",
       color = "Year") +
  scale_color_distiller(palette = "Blues", direction = 1) +
  theme_classic()
ggsave(filename = here::here("data", "lcr_q_vs_time_overlay.png"), width = 10, height = 8, dpi = 200)


# ---- lcr polar plot ----
# version 1
ggplot(data = lcr_years_data) +
  geom_point(mapping = aes(x = doy, 
                           y = value, 
                           color = year, 
                           group = year),
             size = 0.5, alpha = 0.5) +
  scale_color_distiller(palette = "Blues", direction = 1) +
  coord_polar(start = 0) +
  labs(x = NULL, y = "Discharge", color = "Year") +
  theme_classic()
ggsave(filename = here::here("data", "lcr_q_vs_time_polar_v1.png"), width = 10, height = 8, dpi = 200)

# version 2
month_breaks <- yday(ymd(paste0("2023-", 1:12, "-01")))
ggplot(data = lcr_years_data) +
  geom_point(mapping = aes(x = doy, 
                          y = value, 
                          color = year, 
                          group = year),
              size = 0.5, alpha = 0.5) +
  scale_x_continuous(limits = c(1, 366),
                     breaks = month_breaks,
                     labels = month.abb) +
  scale_color_distiller(palette = "Blues", direction = 1) +
  coord_polar(start = 0) +
  labs(x = NULL, y = "Discharge", color = "Year") +
  theme_classic()
ggsave(filename = here::here("data", "lcr_q_vs_time_polar_v2.png"), width = 10, height = 8, dpi = 200)

# version 3
ring_breaks <- c(10, 100, 1000, 10000)
label_angle <- 200   # day of year where the labels sit (pick a gap in the lines)
ggplot(data = lcr_years_data) +
  # points for each
  geom_point(mapping = aes(x = doy, 
                           y = value, 
                           color = year, 
                           group = year),
             size = 1, alpha = 0.5) +
  # ring labels on a spoke
  annotate("text", x = label_angle, y = ring_breaks,
           label = scales::label_comma()(ring_breaks),
           size = 3, vjust = -0.3, color = "gray30") +
  # arrow along the spoke
  annotate("segment", x = label_angle, xend = label_angle,
           y = min(ring_breaks), yend = max(ring_breaks),
           arrow = arrow(length = unit(0.15, "cm")), color = "gray30") +
  scale_x_continuous(limits = c(1, 366),
                     breaks = month_breaks,
                     labels = month.abb) +
  scale_y_log10(breaks = ring_breaks, minor_breaks = NULL) +
  scale_color_distiller(palette = "Blues", direction = 1) +
  coord_polar() +
  labs(x = NULL, y = NULL, color = "Year",
       title = "Little Coharie Creek Gage (USGS-02106000)" #,
       # subtitle = expression("Distance from the center is log discharge in cfs")
       ) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
ggsave(filename = here::here("data", "lcr_q_vs_time_polar_v3.png"), width = 10, height = 8, dpi = 200)

# version 4
ring_breaks <- c(10, 100, 1000, 10000)
label_angle <- 50   # day of year where the labels sit (pick a gap in the lines)
ggplot(data = lcr_years_data) +
  # lines for each year
  geom_line(mapping = aes(x = doy, 
                          y = value, 
                          color = year, 
                          group = year),
            linewidth = 0.5) +
  # ring labels on a spoke
  annotate("text", x = label_angle, y = ring_breaks,
           label = scales::label_comma()(ring_breaks),
           size = 3, vjust = -0.3, color = "black",
           fontface = "bold") +
  # arrow along the spoke
  annotate("segment", x = label_angle, xend = label_angle,
           y = min(ring_breaks), yend = max(ring_breaks),
           arrow = arrow(length = unit(0.15, "cm")), color = "black") +
  scale_x_continuous(limits = c(1, 366),
                     breaks = month_breaks,
                     labels = month.abb) +
  scale_y_log10(breaks = ring_breaks, minor_breaks = NULL) +
  scale_color_distiller(palette = "Blues", direction = 1) +
  coord_polar() +
  labs(x = NULL, y = NULL, color = "Year",
       title = "Little Coharie Creek Gage (USGS-02106000)" #,
       # subtitle = expression("Distance from the center is log discharge in cfs")
       ) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
ggsave(filename = here::here("data", "lcr_q_vs_time_polar_v4.png"), width = 10, height = 8, dpi = 200)

# version 5
ring_breaks <- c(0.1, 10, 100, 1000, 10000)
label_angle <- 50   # day of year where the labels sit (pick a gap in the lines)
daily_mean_line <- lcr_years_data |>
  ungroup() |>
  group_by(doy) |>
  summarize(mean_daily_discharge_cfs = mean(value, na.rm = TRUE), .groups = "drop")
ggplot(data = lcr_years_data) +
  # lines for each year
  geom_line(mapping = aes(x = doy, 
                          y = value, 
                          color = year, 
                          group = year),
            linewidth = 0.5) +
  # line for mean daily flow
  geom_line(data = daily_mean_line,
            mapping = aes(x = doy, 
                          y = mean_daily_discharge_cfs),
            color = "black",
            linewidth = 1) +
  # ring labels on a spoke
  annotate("text", x = label_angle, y = ring_breaks,
           label = scales::label_comma()(ring_breaks),
           size = 3, vjust = -0.3, color = "black",
           fontface = "bold") +
  # arrow along the spoke
  annotate("segment", x = label_angle, xend = label_angle,
           y = min(ring_breaks), yend = max(ring_breaks),
           arrow = arrow(length = unit(0.15, "cm")), color = "black") +
  scale_x_continuous(limits = c(1, 366),
                     breaks = month_breaks,
                     labels = month.abb) +
  scale_y_log10(breaks = ring_breaks, minor_breaks = NULL) +
  scale_color_distiller(palette = "Blues", direction = 1) +
  coord_polar() +
  labs(x = NULL, y = NULL, color = "Year",
       title = "Little Coharie Creek Gage (USGS-02106000)" #,
       # subtitle = expression("Distance from the center is log discharge in cfs"),
       # caption = "Black line is the mean daily flow across all years"
       ) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
ggsave(filename = here::here("data", "lcr_q_vs_time_polar_v5.png"), width = 10, height = 8, dpi = 200)

# version 6
ring_breaks <- c(0.1, 10, 100, 1000, 10000)
label_angle <- 50   # day of year where the labels sit (pick a gap in the lines)
daily_mean_line <- lcr_years_data |>
  ungroup() |>
  group_by(doy) |>
  summarize(mean_daily_discharge_cfs = mean(value, na.rm = TRUE), .groups = "drop")
daily_summary <- lcr_years_data |>
  group_by(doy) |>
  summarize(q_min  = min(value, na.rm = TRUE),
            q_max  = max(value, na.rm = TRUE),
            q_mean = mean(value, na.rm = TRUE),
            .groups = "drop")
ggplot() +
  # min-max envelope
  geom_ribbon(data = daily_summary,
    aes(x = doy, ymin = q_min, ymax = q_max),
    inherit.aes = FALSE,
    fill = "gray90", alpha = 0.7) +
  # lines for each year
  geom_line(data = lcr_years_data,
            mapping = aes(x = doy, 
                          y = value, 
                          group = year),
            linewidth = 0.25,
            color = "grey80") +
  # line for mean daily flow
  geom_line(data = daily_mean_line,
            mapping = aes(x = doy, 
                          y = mean_daily_discharge_cfs),
            color = "black",
            linewidth = 1) +
  # arrow along the spoke
  annotate("segment", x = label_angle, xend = label_angle,
           y = min(ring_breaks), yend = max(ring_breaks),
           arrow = arrow(length = unit(0.15, "cm")), color = "black") +
  # ring labels on a spoke
  annotate("label", x = label_angle, y = ring_breaks,
           label = scales::label_comma()(ring_breaks),
           size = 3, fontface = "bold", color = "black",
           fill = "white", alpha = 0.8, linewidth = 0) +
  scale_x_continuous(limits = c(1, 366), breaks = month_breaks, labels = month.abb) +
  scale_y_log10(breaks = ring_breaks, minor_breaks = NULL) +
  coord_polar() +
  labs(x = NULL, y = NULL,
       title = "Little Coharie Creek Gage (USGS-02106000)") +
  theme_minimal() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
ggsave(filename = here::here("data", "lcr_q_vs_time_polar_v6.png"), width = 10, height = 8, dpi = 200)

# version 7
ring_breaks <- c(0.1, 10, 100, 1000, 10000)
label_angle <- 50   # day of year where the labels sit (pick a gap in the lines)
daily_mean_line <- lcr_years_data |>
  ungroup() |>
  group_by(doy) |>
  summarize(mean_daily_discharge_cfs = mean(value, na.rm = TRUE), .groups = "drop")
daily_summary <- lcr_years_data |>
  group_by(doy) |>
  summarize(q_min  = min(value, na.rm = TRUE),
            q_max  = max(value, na.rm = TRUE),
            q_mean = mean(value, na.rm = TRUE),
            .groups = "drop")
decade_lines <- lcr_years_data |>
  ungroup() |>
  filter(year >= 1960, year <= 1989) |>
  mutate(decade = case_when(year <= 1969 ~ "1960-1969",
                            year <= 1979 ~ "1970-1979",
                            year <= 1989 ~ "1980-1989")) |>
  group_by(decade, doy) |>
  summarize(mean_daily_discharge_cfs = mean(value, na.rm = TRUE),
            .groups = "drop")
ggplot() +
  # min-max envelope
  geom_ribbon(data = daily_summary,
              aes(x = doy, ymin = q_min, ymax = q_max),
              inherit.aes = FALSE,
              fill = "gray90", alpha = 0.7) +
  # lines for each year
  geom_line(data = lcr_years_data,
            mapping = aes(x = doy, 
                          y = value, 
                          group = year),
            linewidth = 0.25,
            color = "grey80") +
  # decadal mean daily flow
  geom_line(data = decade_lines,
            mapping = aes(x = doy,
                          y = mean_daily_discharge_cfs,
                          color = decade,
                          group = decade),
            linewidth = 0.5) +
  # line for mean daily flow (all years)
  geom_line(data = daily_mean_line,
            mapping = aes(x = doy,
                          y = mean_daily_discharge_cfs),
            color = "black",
            linewidth = 1) +
  # arrow along the spoke
  annotate("segment", x = label_angle, xend = label_angle,
           y = min(ring_breaks), yend = max(ring_breaks),
           arrow = arrow(length = unit(0.15, "cm")), color = "black") +
  # ring labels on a spoke
  annotate("label", x = label_angle, y = ring_breaks,
           label = scales::label_comma()(ring_breaks),
           size = 3, fontface = "bold", color = "black",
           fill = "white", alpha = 0.8, linewidth = 0) +
  scale_x_continuous(limits = c(1, 366), 
                     breaks = month_breaks, 
                     labels = month.abb) +
  scale_y_log10(breaks = ring_breaks, minor_breaks = NULL) +
  scale_color_manual(values = c("1960-1969" = "#bdd7e7",
                                "1970-1979" = "#6baed6",
                                "1980-1989" = "#2171b5")) +
  coord_polar() +
  labs(x = NULL, y = NULL, color = "Mean Daily Discharge (cfs)",
       title = "Little Coharie Creek Gage (USGS-02106000)") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "bottom")
ggsave(filename = here::here("data", "lcr_q_vs_time_polar_v7.png"), width = 10, height = 8, dpi = 200)


# ---- br polar plot ---- 
br_ring_breaks <- c(10, 100, 1000, 10000, 100000)
br_label_angle <- 50   # day of year where the labels sit (pick a gap in the lines)
br_daily_mean_line <- br_years_data |>
  ungroup() |>
  group_by(doy) |>
  summarize(mean_daily_discharge_cfs = mean(value, na.rm = TRUE), .groups = "drop")
br_daily_summary <- br_years_data |>
  group_by(doy) |>
  summarize(q_min  = min(value, na.rm = TRUE),
            q_max  = max(value, na.rm = TRUE),
            q_mean = mean(value, na.rm = TRUE),
            .groups = "drop")
br_decade_lines <- br_years_data |>
  ungroup() |>
  filter(year >= 1960, year <= 2019) |>
  mutate(decade = case_when(year <= 1969 ~ "1960-1969",
                            year <= 1979 ~ "1970-1979",
                            year <= 1989 ~ "1980-1989",
                            year <= 1999 ~ "1990-1999",
                            year <= 2009 ~ "2000-2009",
                            year <= 2019 ~ "2010-2019")) |>
  group_by(decade, doy) |>
  summarize(mean_daily_discharge_cfs = mean(value, na.rm = TRUE),
            .groups = "drop")
# pick 6 colors in blues
# decadal_colors <- brewer.pal(n = 6, name = "Blues")
br_decadal_colors <- setNames(colorRampPalette(brewer.pal(9, "Blues")[4:9])(6),
                              c("1960-1969", "1970-1979", "1980-1989", "1990-1999", "2000-2009", "2010-2019"))
br_month_breaks <- yday(ymd(paste0("2026-", 1:12, "-01")))
ggplot() +
  # min-max envelope
  geom_ribbon(data = br_daily_summary,
              aes(x = doy, ymin = q_min, ymax = q_max),
              inherit.aes = FALSE,
              fill = "gray90", alpha = 0.7) +
  # lines for each year
  geom_line(data = br_years_data,
            mapping = aes(x = doy, 
                          y = value, 
                          group = year),
            linewidth = 0.25,
            color = "grey80") +
  # decadal mean daily flow
  geom_line(data = br_decade_lines,
            mapping = aes(x = doy,
                          y = mean_daily_discharge_cfs,
                          color = decade,
                          group = decade),
            linewidth = 0.5) +
  # line for mean daily flow (all years)
  geom_line(data = br_daily_mean_line,
            mapping = aes(x = doy,
                          y = mean_daily_discharge_cfs),
            color = "black",
            linewidth = 1) +
  # arrow along the spoke
  annotate("segment", x = label_angle, xend = label_angle,
           y = min(ring_breaks), yend = max(ring_breaks),
           arrow = arrow(length = unit(0.15, "cm")), color = "black") +
  # ring labels on a spoke
  annotate("label", x = label_angle, y = ring_breaks,
           label = scales::label_comma()(ring_breaks),
           size = 3, fontface = "bold", color = "black",
           fill = "white", alpha = 0.8, linewidth = 0) +
  scale_x_continuous(limits = c(1, 366), 
                     breaks = br_month_breaks, 
                     labels = month.abb) +
  scale_y_log10(breaks = br_ring_breaks, minor_breaks = NULL) +
  scale_color_manual(values = br_decadal_colors) +
  coord_polar() +
  labs(x = NULL, y = NULL, color = "Mean Daily Discharge (cfs)",
       title = "Black River Gage (USGS-02106500)") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "bottom")
ggsave(filename = here::here("data", "br_q_vs_time_polar.png"), width = 10, height = 8, dpi = 200)


# ---- ribbon for drawing ----
# lcr
ggplot() +
  # min-max envelope
  geom_ribbon(data = daily_summary,
              aes(x = doy, ymin = q_min, ymax = q_max),
              inherit.aes = FALSE,
              fill = "gray80", alpha = 0.7) +
  # lines for each year
  geom_line(data = lcr_years_data,
            mapping = aes(x = doy, 
                          y = value, 
                          group = year),
            linewidth = 0.25,
            color = "grey60") +
  scale_x_continuous(limits = c(1, 366), 
                     breaks = month_breaks, 
                     labels = month.abb) +
  scale_y_log10(breaks = ring_breaks, minor_breaks = NULL) +
  coord_polar() +
  labs(x = NULL, y = NULL,
       title = "Little Coharie Creek Gage (USGS-02106000)") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "bottom")
ggsave(filename = here::here("data", "lcr_q_vs_time_polar_ribbon.png"), width = 10, height = 8, dpi = 200)

# br
ggplot() +
  # min-max envelope
  geom_ribbon(data = br_daily_summary,
              aes(x = doy, ymin = q_min, ymax = q_max),
              inherit.aes = FALSE,
              fill = "gray80", alpha = 0.7) +
  # lines for each year
  geom_line(data = br_years_data,
            mapping = aes(x = doy, 
                          y = value, 
                          group = year),
            linewidth = 0.25,
            color = "grey60") +
  scale_x_continuous(limits = c(1, 366), 
                     breaks = br_month_breaks, 
                     labels = month.abb) +
  scale_y_log10(breaks = br_ring_breaks, minor_breaks = NULL) +
  coord_polar() +
  labs(x = NULL, y = NULL,
       title = "Black River Gage (USGS-02106500)") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "bottom")
ggsave(filename = here::here("data", "br_q_vs_time_polar_ribbon.png"), width = 10, height = 8, dpi = 200)
