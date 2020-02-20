# notes ----
# General Template for mapping ggplot
# author: Tyler Jackson
# contact: tyler.jackson@alaska.gov
# last updated: 2020/2/19

# load ----

## shapfile prep code, view R script for details
source("./code/shapfile_prep.R", echo = F)

## high resolution map of alaska and canada (plots very slow when layer in turned on)
## raster library required, but if library is loaded dplyr::select is masked
canam <- raster::getData("GADM", country = c("USA", "Canada"), level = 1, path = "./data/raster")

## low resolution map of russia, alaska, canada (quick!)
ak <- maps::map("world", region=c('russia', 'usa:alaska', 'canada'), fill=T, plot=F) %>%
  broom::tidy()



# data ----

# scallop survey haul data for example
survey_data <- read_csv("./data/survey/HaulTableData_Cruise1901.csv")



# example (statewide scallop survey 2019) ----

## load shapefile of interest
grid <- f_shp_prep("./data/shapefiles/statewide_scallop_survey_grid_2019", 
                   "scalGrid2019_all_albers")  
## check polygon data frame 'long' and 'lat' to examine projection
as_tibble(grid)
## if necessary re-project shapfile to a different coordinate system
## example: scallop survey grid was projected in albers, but base map was projected in NAD 1983
grid <- f_albers_to_nad83(grid) 

## if there is biological data by station (for surveys), join with grid
survey_data %>%
  ### manipulate scallop survey station name to align with convention of shapfile data
  mutate(station_number = gsub("[[:alpha:]]", "", station),
         station_letter = substring(gsub("\\d", "", station), 2, 3),
         station = paste(station_letter, station_number, sep = " ")) %>%
  ### select data of interest
  select(station, avg_depth) %>%
  ### use a right join to the grid (there are multiple rows per cell that need 'fill' data)
  right_join(grid, by = "station") %>%
  ## need to sort by order so polygon is drawn correctly
  arrange(order) -> fillable_grid

## map plot
ggplot()+
  geom_polygon(data = canam, aes(x = long, y = lat, group = group, fill = avg_depth), 
               color = "black", fill = "grey40")+
  geom_polygon(data = fillable_grid, aes(x = long, y = lat, group = group, fill = avg_depth), 
               color = "black")+
  scale_fill_gradientn(colours = terrain.colors(n = 10))+
  coord_map(projection = "albers", lat0 = 55, lat = 62, 
            xlim = c(-139.5, -138.3), ylim = c(58.75, 59.5))+
  labs(x = "Longitude", y = "Latitude", fill = "Depth (fa)")+
  theme_bw() -> map

## save figure
ggsave("./figures/example_map.png", plot = map, width = 5, height = 3, units = "in")
  




