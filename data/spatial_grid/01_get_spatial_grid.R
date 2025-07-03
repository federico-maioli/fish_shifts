# libraries ---------------------------------------------------------------
library(tidyverse)
library(here)
library(sdmTMB)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(terra)

# load function and data -------------------------------------------------

source(here('R_functions/exclude_too_far.R'))
data <- readRDS('data/trawl_surveys/fishglob_clean.rds')

# prepare survey spatial grid ----------------------------------------------

unique_locations <- data |> distinct(region_short, depth, latitude, longitude)
unique_locations <- data |> distinct(region_short, depth, latitude, longitude, year)
split_data <- split(unique_locations, unique_locations$region_short)

# grid settings
grids <- list()
cell_width <- 4  # grid resolution (4 km)

round_down_even <- function(x, base = 1000) base * floor(x / base)

# load coast and depth data
coastline <- rnaturalearth::ne_countries(scale = 10, returnclass = "sf") |> 
  st_transform(crs = 4326) |> 
  st_make_valid()

depth_raster <- terra::rast(here('data/environmental/depth/GEBCO_2023_sub_ice_topo.nc'))

# process regions ----------------------------------------------------------

grids = list()

for (this_region in names(split_data)) {
  
  sub <- split_data[[this_region]]
  
  sub <- sdmTMB::add_utm_columns(sub, ll_names = c("longitude", "latitude"), units = 'm')
  crs <- sdmTMB::get_crs(sub, c("longitude", "latitude"))
  sub <- sub |> distinct(X, Y, .keep_all = TRUE)
  
  # create concave hull
  sf_use_s2(FALSE)
  sf_points <- st_as_sf(sub, coords = c("X", "Y"), crs = crs)
  boundary <- st_buffer(st_concave_hull(st_union(sf_points), ratio = 0.05), dist = 10000)
  
  # generate regular grid
  pred_grid <- expand.grid(
    X = seq(round_down_even(min(sub$X)), max(sub$X), cell_width * 1000),
    Y = seq(round_down_even(min(sub$Y)), max(sub$Y), cell_width * 1000)
  )
  
  # keep only points inside boundary
  sf_object <- st_as_sf(pred_grid, coords = c("X", "Y"), crs = crs)
  inside_boundary <- st_intersects(sf_object, boundary, sparse = FALSE)
  pred_grid <- pred_grid[apply(inside_boundary, 1, any), ]
  
  # transform to lat/lon and remove land points
  sf_use_s2(TRUE)
  sf_wgs84 <- st_transform(st_as_sf(pred_grid, coords = c("X", "Y"), crs = crs), crs = 4326)
  on_land <- st_intersects(sf_wgs84, coastline, sparse = FALSE)
  pred_grid$longitude <- st_coordinates(sf_wgs84)[, 1]
  pred_grid$latitude  <- st_coordinates(sf_wgs84)[, 2]
  pred_grid <- pred_grid[!apply(on_land, 1, any), ]
  
  # apply depth filter
  pred_grid$depth <- terra::extract(depth_raster$elevation, 
                                    pred_grid |> select(longitude, latitude), method = 'bilinear')$elevation
  pred_grid <- pred_grid |> filter(depth <= 0)
  pred_grid$depth <- abs(pred_grid$depth)
  
  depth_percentiles <- quantile(sub$depth, probs = c(0.01, 0.99), na.rm = TRUE)
  pred_grid <- pred_grid |>
    filter(depth >= depth_percentiles[1], depth <= depth_percentiles[2])
  
  # exclude points too far from observations
  dist_limit <- if (this_region == 'BS') 40000 else 30000
  too_far <- exclude_too_far(pred_grid$X, pred_grid$Y, sub$X, sub$Y, dist = dist_limit)
  pred_grid <- pred_grid[!too_far, ]
  
  # store grid (convert X/Y to km)
  grids[[this_region]] <- pred_grid |> mutate(X = X / 1000, Y = Y / 1000)
}

# Save processed grids
saveRDS(grids, here('data/spatial_grid/grid.rds'))




# ### Plot and save survey grids ----------------------------------------------
# 
# for (this_region in names(grids)) {
#   this_region <- "GOA"
#   df <- grids[[this_region]]  # Get survey grid data
#   
#   # Get the CRS from the data
#   crs_df <- sdmTMB::get_crs(df, c("longitude", "latitude"))
#   
#   # Load and transform coastline
#   coastline <- rnaturalearth::ne_countries(scale = 10, returnclass = "sf")
#   coastline_transformed <- st_transform(coastline, crs = crs_df)
#   coastline_transformed <- st_make_valid(coastline_transformed)
#   # Convert X and Y to meters for plotting
#   df <- df |> mutate(X_meters = X * 1000, Y_meters = Y * 1000)
#   
#   # Create and save plot
#   plot <- ggplot(data = coastline_transformed) +
#     geom_sf() +
#     geom_raster(data = df, aes(x = X_meters, y = Y_meters, fill = depth), inherit.aes = FALSE) +
#     xlim(min(df$X_meters), max(df$X_meters)) +
#     ylim(min(df$Y_meters), max(df$Y_meters)) +
#     ggtitle(paste0(this_region)) +
#     scale_fill_viridis_c()
#   
#   plot
#   
#   ggsave(plot = plot, filename = paste0(here('output/region_domain/'), this_region, '.png'))
# }

