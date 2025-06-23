#Maputo TVET Schools Accessibility

rJavaEnv::java_quick_install(version = 21)
rJavaEnv::java_check_version_rjava()
options(java.parameters = "-Xmx12G")

#remove and reinstall updated r5r (only necessary if old installation exists)
#utils::remove.packages('r5r')
#devtools::install_github('Chrisjb/basemapR')

library(tidyverse)
library(gtfstools)
library(r5r)
library(sf)
library(ggplot2)
library(dplyr)
library(rJava)
library(r5r)
library(patchwork)
library(stringr)
library(elevatr)
library(raster)
library(stringr)
library(spdep)
#library(rgdal)
library(magrittr)
library(spatialreg)
library(rgeoda)
library(stplanr)
library(ggmap)
library(basemapR)
library(scales)
library(stats)
library(viridis)
library(ggrepel)
library(biscale)
library(stringr)
library(gtfsio)

#.jinit()
#runtime <- rJava::.jcall("java/lang/Runtime", "Ljava/lang/Runtime;", "getRuntime")
#max_memory_gb <- rJava::.jcall(runtime, "J", "maxMemory") / 1024^3
#cat("Max Java heap memory:", round(max_memory_gb, 2), "GB\n")

select <- dplyr::select
path <- getwd()

#### Load grid and population data ####################################################################
# Load required data

#Spatial data
grid <- st_read(paste0(path,"/Shapefiles/worldpop2020_constrained_mma.shp"))
bbox <- expand_bbox(st_bbox(grid), X = 0, Y = 150000)
box <- st_bbox(grid)

bairros <- st_read(paste0(path,"/Poverty/first_version/Bairro level/maputo_bairro.shp")) %>%
  mutate(Distrito = ifelse(Distrito == "ManhiÃ§a","Manhiça",Distrito)) %>% 
  mutate(Municipio = ifelse(Provincia == "Maputo Cidade","Cidade de Maputo",Distrito))

#municipios <- st_read(paste0(path,"/Shapefiles/INE/moz_admbnda_adm3_ine_20190607.shp")) #all municipalities
municipios <- bairros %>% group_by(Municipio) %>% summarise()
municipios %>%
  ggplot() +
  geom_sf()

#Importing TVETs
tvets <- read_csv2(paste0(path,"/ANEP/Updated_acredited_TVETs.csv")) %>%
  mutate(type3 = ifelse(STEM == "Yes","STEM","non-STEM")) %>%
  mutate(HDD = ifelse(is.na(HDD),"No",HDD)) %>%
  mutate(type4 = ifelse(HDD == "Yes","HDD","-")) %>%
  mutate(type5 = ifelse(MozSkills == "Yes","MozSkills","-"))
tvets_shp <- tvets %>% st_as_sf(coords= c("Longitude","Latitude")) %>% st_set_crs(4326)
tvets_shp %>%
  ggplot() +
  geom_sf()

#### Poverty data ####################################################################
# Intersect with poverty data
poverty <- st_read(paste0(path,"/Poverty/second_version/maputo_grid_combined.shp"))
#intersect <- grid %>% st_intersection(poverty)

# Weighted average for overlapping grids (intersection perfomed in QGIS)
intersect <- st_read(paste0(path,"/Shapefiles/intersect_poverty_grid2.shp")) %>%
  mutate(area = as.numeric(st_area(.))) %>%
  st_drop_geometry() %>%
  group_by(fid) %>%
  summarise(predicted_ = sum(predicted_*area)/sum(area))

# Nearest point for isolated pop cells (distance to the nearest hub perfomed in QGIS)
no_intersect <- st_read(paste0(path,"/Shapefiles/distance_nearest_poverty_grid2.shp")) %>%
  st_drop_geometry() %>%
  filter(!fid %in% intersect$fid) %>%
  rename(ua = "HubName") %>%
  select(fid,ua) %>%
  left_join(poverty %>% select(ua,predicted_) %>% st_drop_geometry(),by="ua") %>%
  select(-ua)

poverty <- bind_rows(intersect,no_intersect) %>%
  rename(consumption = "predicted_")

centroids <- grid %>% st_centroid() %>%
  select(fid) %>%
  st_join(municipios, join = st_intersects) %>%
  st_drop_geometry()

grid <- grid %>%
  left_join(poverty,by="fid") %>%
  left_join(centroids,by="fid") %>%
  arrange(consumption) %>%
  mutate(poverty = pop_count/sum(pop_count)) %>%
  mutate(poverty = cumsum(poverty)) %>%
  mutate(poverty_group = 1 + as.numeric(floor(poverty*10))) %>%
  mutate(poverty_group = ifelse(poverty == 1,10,poverty_group))

rm(centroids,no_intersect,poverty,intersect)

#### Merge GTFS #####################################################################

gtfs <- list.files("GTFS",pattern = ".zip",full.names = T)
gtfs
gtfs <- gtfs[!gtfs %in% c("GTFS/Maputo Chapas.zip","GTFS/MyLove.zip")] 
gtfs
gtfs <- gtfs %>% map(read_gtfs)

gtfs_merged <- merge_gtfs(gtfs,prefix=T)
gtfs_merged[["fare_rules"]] <- NULL
gtfs_merged[["fare_attributes"]] <- NULL

write_gtfs(gtfs_merged,"r5r/GTFS.zip")

gtfs <- c(list.files("GTFS",pattern = ".zip",full.names = T),list.files("BRT",pattern = ".zip",full.names = T))
gtfs <- gtfs[!gtfs %in% c("GTFS/Maputo Chapas.zip","GTFS/MyLove.zip")] 
gtfs
gtfs <- gtfs %>% map(read_gtfs)

gtfs_merged <- merge_gtfs(gtfs,prefix=T)
gtfs_merged[["fare_rules"]] <- NULL
gtfs_merged[["fare_attributes"]] <- NULL

write_gtfs(gtfs_merged,"r5r_brt/GTFS_brt.zip")

#### Create the GTFS for the BRT #####################################################################

gtfs <- read_gtfs(paste0(path,"/GTFS/FTC.zip"))
gtfs[["agency"]] <- gtfs[["agency"]] %>% mutate(agency_id = "BRT_Maputo",
                                                agency_name = "BRT")

stops <-  st_read("BRT/trunk_stations/trunk_stations.shp") %>%
  mutate(corridor = "MISTRA") %>%
  bind_rows(st_read("BRT/trunk_stations/juventud_albazine.shp")  %>%
              mutate(corridor = "ALBRA")) %>%
  bind_rows(st_read("BRT/trunk_stations/missaoroque_zimpeto_stations.shp") %>%
              mutate(corridor = "ZIMTRA") %>%
              mutate(Stop. = gsub("N1-","N-",Stop.))) %>%
  st_transform(4326) %>%
  mutate(stop_lon = sf::st_coordinates(.)[,1],
         stop_lat = sf::st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>%
  mutate(Stop. = gsub("[()]","",Stop.)) %>%
  mutate(Stop. = str_squish(Stop.)) %>%
  group_by(Stop.) %>%
  mutate(stop_id = ifelse(Stop. %in% c("TRA","MIS","ZIM","ALB"),Stop.,paste0(gsub(" ","_", Stop.,fixed = T)))) %>%
  ungroup() %>%
  mutate(stop_code = NA,
         stop_name = paste0(Location),
         stop_desc = Direction,
         zone_id = NA,
         stop_url = NA,
         location_type = 0,
         parent_station = NA,
         stop_timezone = NA,
         wheelchair_boarding = NA) %>%
  select(colnames(gtfs[["stops"]]))
gtfs[["stops"]] <- stops
rm(list=ls(pattern="stops"))

routes <- tibble(
  route_id = c("ZIMTRA","ALBTRA","MISTRA"),
  agency_id = rep("BRT_Maputo",3),
  route_short_name = c("ZIMTRA","ALBTRA","MISTRA"),
  route_long_name = c("Zimpeto-Trabalhadores","Albazine-Trabalhadores","MissaoRoque-Trabalhadores"),
  route_desc = NA,
  route_type = rep(3,3),
  route_url = NA,
  route_color = c("D1BF78","5A9638","E2B3D9"),
  route_text_color = c("000000","000000","000000"))
gtfs[["routes"]] <- routes
rm(list=ls(pattern="routes"))

trips <- tibble(
  trip_id = c("T1_SB","T1_NB","TX1_SB","TX1_NB","T2_SB","T2_NB","TX2_SB","TX2_NB","T3_SB","T3_NB","TX3_SB","TX3_NB"),
  route_id = rep(c("ZIMTRA","ALBTRA","MISTRA"),each=4),
  service_id = "service_0002",
  trip_headsign = c(rep(c("Zimpeto","Trabalhadores"),2),rep(c("Albazine","Trabalhadores"),2),rep(c("Missão Roque","Trabalhadores"),2)),
  trip_short_name = trip_id,
  direction_id = rep(c(0,1),6),
  block_id = "",
  shape_id = paste0(route_id,rep(c("_SB","_NB"),6)),
  wheelchair_accessible = "")
gtfs[["trips"]] <- trips
rm(list=ls(pattern="trips"))

gtfs[["fare_attributes"]] <- NULL
gtfs[["fare_rules"]] <- NULL

shapes_sb <- st_read("BRT/trunk_shape/shape_pts_MISTRA.shp") %>%
  mutate(shape_id = "MISTRA_SB") %>%
  group_by(id) %>%
  mutate(vertex_ind = ifelse(id %in% c(3,4),rev(vertex_ind),vertex_ind)) %>%
  bind_rows(st_read("BRT/trunk_shape/shape_pts_ZIMTRA.shp") %>%
              mutate(shape_id = "ZIMTRA_SB") %>%
              group_by(id) %>%
              mutate(vertex_ind = ifelse(id %in% c(3,4,5),rev(vertex_ind),vertex_ind))) %>%
  bind_rows(st_read("BRT/trunk_shape/shape_pts_ALBRA.shp") %>%
              mutate(shape_id = "ALBTRA_SB")  %>%
              group_by(id) %>%
              mutate(vertex_ind = ifelse(id %in% c(3,4),rev(vertex_ind),vertex_ind))) %>%
  select(-distance) %>%
  arrange(shape_id,id,vertex_ind) %>%
  mutate(p = 1:nrow(.)) %>%
  mutate(lp = lag(p))
dist <- st_distance(shapes_sb) %>%
  as_tibble() 
colnames(dist) <- 1:nrow(dist)
dist <- dist %>%
  mutate(p = 1:nrow(.)) %>%
  gather(key="lp",value="dist",-p) %>%
  mutate_all(as.numeric)
shapes_sb <- shapes_sb %>%
  left_join(dist,by=c("p","lp")) %>%
  mutate(dist = ifelse(shape_id == lag(shape_id),dist,0)) %>%
  mutate(dist = ifelse(is.na(dist),0,dist)) %>%
  mutate(shape_pt_lon = sf::st_coordinates(.)[,1],
         shape_pt_lat = sf::st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>%
  group_by(shape_id) %>%
  mutate(shape_pt_sequence = 1:n()) %>%
  mutate(shape_dist_traveled = cumsum(dist)) %>% 
  ungroup() %>%
  select(colnames(gtfs[["shapes"]]))
shapes_nb <- shapes_sb %>%
  arrange(shape_id,desc(shape_pt_sequence)) %>%
  group_by(shape_id) %>%
  mutate(shape_pt_sequence = 1:n()) %>%
  mutate(dist = ifelse(shape_pt_sequence == 1,0,-(shape_dist_traveled - lag(shape_dist_traveled)))) %>%
  mutate(shape_dist_traveled = cumsum(dist)) %>%
  select(-dist) %>%
  mutate(shape_id = gsub("SB","NB",shape_id))
gtfs[["shapes"]] <- shapes_sb %>% bind_rows(shapes_nb)
rm(list=ls(pattern="shapes"))

stop_times <- st_read("BRT/trunk_stations/MISTRA_stations.shp") %>%
  mutate(corridor = "MISTRA") %>%
  bind_rows(st_read("BRT/trunk_stations/ZIMTRA_stations.shp") %>%
              mutate(corridor = "ZIMTRA") %>%
              mutate(Stop. = gsub("N1-","N-",Stop.))) %>%
  bind_rows(st_read("BRT/trunk_stations/ALBRA_stations.shp") %>%
              mutate(corridor = "ALBRA")) %>%
  st_drop_geometry() %>%
  mutate(Stop. = gsub("[()]","",Stop.)) %>%
  mutate(Stop. = str_squish(Stop.)) %>%
  mutate(stop_id = ifelse(Stop. %in% c("TRA","MIS","ZIM","ALB"),Stop.,paste0(gsub(" ","_", Stop.,fixed = T)))) %>%
  select(stop_id,corridor,order) %>%
  mutate(direction = case_when(grepl("NB",order) == T ~ "NB",
                               grepl("SB",order) == T ~ "SB",
                               TRUE ~ "Terminal")) %>%
  mutate(stop_sequence = parse_number(order)) %>%
  select(-order)
stop_times_zimtra <- stop_times %>% filter(corridor == "ZIMTRA") %>%
  select(-corridor) %>%
  filter(direction != "Terminal") %>%
  arrange(direction,stop_sequence) %>%
  bind_rows(tibble(stop_id = c("ZIM","MIS","TRA","TRA","MIS","ZIM"),
                   direction = c("NB","NB","NB","SB","SB","SB"),
                   stop_sequence = c(1,11,30,1,20,30))) %>%
  arrange(direction,stop_sequence)
aux <- stop_times_zimtra %>%
  group_by(direction) %>%
  filter(stop_sequence == min(stop_sequence) | stop_sequence == max(stop_sequence) | stop_id %in% c("ZIM","MIS","TRA")) %>%
  ungroup() %>%
  mutate(trip_id = rep(c("TX1_NB","TX1_SB"),each= 3))
stop_times_zimtra <- stop_times_zimtra %>% 
  arrange(stop_sequence) %>%
  mutate(trip_id = rep(c("T1_NB","T1_SB"),nrow(stop_times_zimtra)/2)) %>%
  arrange(trip_id) %>%
  bind_rows(aux)
stop_times_albra <- stop_times %>% filter(corridor == "ALBRA") %>%
  select(-corridor) %>%
  filter(direction != "Terminal") %>%
  arrange(direction,stop_sequence) %>%
  bind_rows(tibble(stop_id = c("ALB","TRA","TRA","ALB"),
                   direction = c("NB","NB","SB","SB"),
                   stop_sequence = c(1,25,1,25))) %>%
  arrange(direction,stop_sequence)
aux <- stop_times_albra %>%
  group_by(direction) %>%
  filter(stop_sequence == min(stop_sequence) | stop_sequence == max(stop_sequence)) %>%
  ungroup() %>%
  mutate(trip_id = rep(c("TX2_NB","TX2_SB"),each= 2))
stop_times_albra <- stop_times_albra %>% 
  arrange(stop_sequence) %>%
  mutate(trip_id = rep(c("T2_NB","T2_SB"),nrow(stop_times_albra)/2)) %>%
  arrange(trip_id) %>%
  bind_rows(aux)
stop_times_mistra <- stop_times %>% filter(corridor == "MISTRA") %>%
  select(-corridor) %>%
  filter(direction != "Terminal") %>%
  arrange(direction,stop_sequence) %>%
  bind_rows(tibble(stop_id = c("MIS","TRA","TRA","MIS"),
                   direction = c("NB","NB","SB","SB"),
                   stop_sequence = c(1,20,1,20))) %>%
  arrange(direction,stop_sequence)
aux <- stop_times_mistra %>%
  group_by(direction) %>%
  filter(stop_sequence == min(stop_sequence) | stop_sequence == max(stop_sequence)) %>%
  ungroup() %>%
  mutate(trip_id = rep(c("TX3_NB","TX3_SB"),each= 2))
stop_times_mistra <- stop_times_mistra %>% 
  arrange(stop_sequence) %>%
  mutate(trip_id = rep(c("T3_NB","T3_SB"),nrow(stop_times_mistra)/2)) %>%
  arrange(trip_id) %>%
  bind_rows(aux)
stop_times <- stop_times_zimtra %>%
  bind_rows(stop_times_albra) %>%
  bind_rows(stop_times_mistra) %>%
  group_by(trip_id) %>%
  mutate(departure_time = case_when(grepl(c("T1|T2|T3"),trip_id) ~ as.POSIXct("05:00:00", format="%H:%M:%S"),
                                  grepl(c("TX1|TX2|TX3"),trip_id) ~ as.POSIXct("06:00:00", format="%H:%M:%S"),
                                  TRUE ~ NA)) %>%
  mutate(travel_time = case_when(grepl("T1",trip_id) ~ 60,
                                 grepl("TX1",trip_id) ~ 50,
                                 grepl("T2",trip_id) ~ 60,
                                 grepl("TX2",trip_id) ~ 55,
                                 grepl("T3",trip_id) ~ 55,
                                 grepl("TX3",trip_id) ~ 45)) %>%
  group_by(trip_id) %>%
  mutate(departure_time = departure_time + (stop_sequence-1)*travel_time*60/(max(stop_sequence)-1)) %>%
  mutate(departure_time = format(departure_time, "%H:%M:%S")) %>%
  mutate(arrival_time = departure_time) %>%
  select(trip_id,arrival_time,departure_time,stop_id,stop_sequence)
gtfs[["stop_times"]] <- stop_times
rm(list=ls(pattern="stop_times"))

frequencies_zimtra <- tibble(trip_id = c("T1_SB","T1_NB","TX1_SB","TX1_SB","TX1_NB","TX1_NB"),
                      start_time = c("05:00:00","05:00:00","06:00:00","16:00:00","06:00:00","16:00:00"),
                      end_time = c("22:30:00","22:30:00","09:00:00","19:00:00","09:00:00","19:00:00"),
                      headway_secs = 60*c(15,15,5,5,5,5),
                      exact_times = 0)
frequencies_albra <- tibble(trip_id = c("T2_SB","T2_NB","TX2_SB","TX2_SB","TX2_NB","TX2_NB"),
                            start_time = c("05:00:00","05:00:00","06:00:00","16:00:00","06:00:00","16:00:00"),
                            end_time = c("22:30:00","22:30:00","09:00:00","19:00:00","09:00:00","19:00:00"),
                            headway_secs = 60*c(15,15,6,6,6,6),
                            exact_times = 0)
frequencies_mistra <- tibble(trip_id = c(rep("T3_SB",5),rep("T3_NB",5),"TX3_SB","TX3_SB","TX3_NB","TX3_NB"),
                            start_time = c("05:00:00","06:00:00","09:00:00","16:00:00","19:00:00","05:00:00","06:00:00","09:00:00","16:00:00","19:00:00","06:00:00","16:00:00","06:00:00","16:00:00"),
                            end_time = c("06:00:00","09:00:00","16:00:00","19:00:00","20:00:00","06:00:00","09:00:00","16:00:00","19:00:00","20:00:00","09:00:00","19:00:00","09:00:00","19:00:00"),
                            headway_secs = 60*c(20,10,20,10,20,20,10,20,10,20,10,10,10,10),
                            exact_times = 0)
frequencies <- frequencies_zimtra %>%
  bind_rows(frequencies_albra) %>%
  bind_rows(frequencies_mistra) %>%
  mutate_at(c("start_time","end_time"),~(p=as.POSIXct(., format="%H:%M:%S"))) %>%
  mutate_at(c("start_time","end_time"),~(p=format(., "%H:%M:%S")))
gtfs[["frequencies"]] <- frequencies
rm(list=ls(pattern="frequencies"))

feed_info <- tibble(feed_publisher_name = "Taina A. Bittencourt",
         feed_publisher_url = "https://github.com/tainabittencourt",
         feed_lang = "pt",
         feed_start_date = as.POSIXct("2017-09-20", "%Y-%m-%d"),
         feed_end_date = as.POSIXct("2020-12-31", "%Y-%m-%d"),
         feed_version = 1)
gtfs[["feed_info"]] <- NULL

output_path <- tempfile("validation_result")
validator_path <- download_validator(tempdir())
validate_gtfs(gtfs, output_path, validator_path)

write_gtfs(gtfs,"BRT/BRT_gtfs.zip")

#### Aggregate grid for the BRT analysis #####################################################################

grid_100 <- st_transform(st_read(paste0(path,"/Shapefiles/worldpop2020_constrained_mma.shp")), 32736) 

# Get centroids and their coordinates
centroids <- st_centroid(grid_100)
coords <- st_coordinates(centroids)

# Create group variables by rounding coordinates to 200m grid
grid_100 <- grid_100 %>%
  mutate(x_grp = floor(coords[, "X"] / 200) * 200,
         y_grp = floor(coords[, "Y"] / 200) * 200,
         group_id = paste0("cell_", x_grp, "_", y_grp))

# Aggregate by group_id and union geometries
grid_200 <- grid_100 %>%
  group_by(group_id) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

# Add a numeric ID if needed
grid_200 <- grid_200 %>%
  mutate(id = row_number()) %>%
  select(id, group_id, geometry)

# Save to file (optional)
st_write(grid_200, "Shapefiles/aggregated_200m_grid.shp")

#### BRT Travel time matrices ##############################################################

sf_use_s2(FALSE)

grid_200 <- st_read("Shapefiles/aggregated_200m_grid.shp") %>%
  st_buffer(0) %>%
  st_make_valid() %>%
  st_transform(4326)

#Importing TVETs
tvets <- read_csv2(paste0(path,"/ANEP/Updated_acredited_TVETs.csv")) %>%
  mutate(type3 = ifelse(STEM == "Yes","STEM","non-STEM")) %>%
  mutate(HDD = ifelse(is.na(HDD),"No",HDD)) %>%
  mutate(type4 = ifelse(HDD == "Yes","HDD","-")) %>%
  mutate(type5 = ifelse(MozSkills == "Yes","MozSkills","-"))

#R5R setup
r5r_cache(list_files = TRUE, delete_file = "all")
#r5r_core <- setup_r5(data_path = paste0(path,"/r5r"), verbose = F)
r5r_core <- setup_r5(data_path = paste0(path,"/r5r_brt"), verbose = F)

#Defining origins based on grid centroids
origins <- grid_200 %>%
  st_centroid()
origins <- do.call(rbind, st_geometry(origins)) %>% 
  as_tibble() %>% setNames(c("lon","lat")) %>%
  mutate(id = origins$id) %>%
  select(id,lon,lat)

destinations <- tvets %>%
  select(N,Latitude,Longitude) %>%
  rename(id = "N",
         lon = "Longitude",
         lat = "Latitude")%>%
  select(id,lon,lat)
  
# Expanded travel time matrices with time window
departure_datetime <- as.POSIXct("08-01-2019 07:00:00", format = "%d-%m-%Y %H:%M:%S",tz="Africa/Maputo")
travel_time_matrix(r5r_core = r5r_core,
                   origins = origins,
                   destinations = destinations,
                   mode = c("WALK","TRANSIT"),
                   departure_datetime = departure_datetime,
                   time_window = 30,
                   max_walk_time = 60,
                   max_trip_duration = 2*60,
                   progress = TRUE) %>%
  #saveRDS(paste0(path,"/ttm_transit_tvets_7_brt_200.rds")) #%>%
  saveRDS(paste0(path,"/ttm_transit_tvets_7_base_200.rds"))

# Expanded travel time matrices with time window
departure_datetime <- as.POSIXct("08-01-2019 15:00:00", format = "%d-%m-%Y %H:%M:%S",tz="Africa/Maputo")
travel_time_matrix(r5r_core = r5r_core,
                   origins = origins,
                   destinations = destinations,
                   mode = c("WALK","TRANSIT"),
                   departure_datetime = departure_datetime,
                   time_window = 30,
                   max_walk_time = 60,
                   max_trip_duration = 2*60,
                   progress = TRUE) %>%
  saveRDS(paste0(path,"/ttm_transit_tvets_15_brt_200.rds")) #%>%
  #saveRDS(paste0(path,"/ttm_transit_tvets_15_base_200.rds"))

# Expanded travel time matrices with time window
departure_datetime <- as.POSIXct("08-01-2019 21:00:00", format = "%d-%m-%Y %H:%M:%S",tz="Africa/Maputo")
travel_time_matrix(r5r_core = r5r_core,
                   origins = origins,
                   destinations = destinations,
                   mode = c("WALK","TRANSIT"),
                   departure_datetime = departure_datetime,
                   time_window = 30,
                   max_walk_time = 60,
                   max_trip_duration = 2*60,
                   progress = TRUE) %>%
  saveRDS(paste0(path,"/ttm_transit_tvets_21_brt_200.rds")) #%>%
  #(paste0(path,"/ttm_transit_tvets_21_base_200.rds"))


#### BRT accessibility calculations #####################################################################

grid_200 <- st_read("Shapefiles/aggregated_200m_grid.shp") %>%
  st_transform(4326) %>%
  st_join(grid %>% st_centroid(),join=st_intersects) %>%
  st_drop_geometry() %>%
  filter(!is.na(pop_count)) %>%
  group_by(id) %>%
  summarise(consumption = weighted.mean(consumption,pop_count,na.rm=T),
            pop_count = sum(pop_count)) %>%
  arrange(consumption) %>%
  mutate(poverty = pop_count/sum(pop_count)) %>%
  mutate(poverty = cumsum(poverty)) %>%
  mutate(poverty_group = 1 + as.numeric(floor(poverty*10))) %>%
  mutate(poverty_group = ifelse(poverty == 1,10,poverty_group))

grid_200 <-  st_read("Shapefiles/aggregated_200m_grid.shp") %>%
  left_join(grid_200,by="id")

#Open travel time matrices
ttm <- readRDS(paste0(path,"/ttm_transit_tvets_7_base_200.rds")) %>%
  rename(base_7 = "travel_time_p50") %>%
  left_join(readRDS(paste0(path,"/ttm_transit_tvets_15_base_200.rds")) %>%
              rename(base_15 = "travel_time_p50"),by = c("from_id","to_id")) %>%
  left_join(readRDS(paste0(path,"/ttm_transit_tvets_21_base_200.rds")) %>%
              rename(base_21 = "travel_time_p50"),by = c("from_id","to_id")) %>%
  
  left_join(readRDS(paste0(path,"/ttm_transit_tvets_7_brt_200.rds")) %>%
              rename(brt_7 = "travel_time_p50"),by = c("from_id","to_id")) %>%
  left_join(readRDS(paste0(path,"/ttm_transit_tvets_15_brt_200.rds")) %>%
              rename(brt_15 = "travel_time_p50"),by = c("from_id","to_id")) %>%
  left_join(readRDS(paste0(path,"/ttm_transit_tvets_21_brt_200.rds")) %>%
              rename(brt_21 = "travel_time_p50"),by = c("from_id","to_id")) %>%
  
  mutate_at(c("from_id","to_id"),as.numeric)

base <- ttm %>%
  left_join(tvets %>% select(N,type1,type2,type3,type4,type5),by=c("to_id"="N"))

#Calculating accessibility to public and private TVETs
access <- NULL
for(t in c("Public","Private","IEP","CFP","STEM","non-STEM","HDD","MozSkills","all")){
  print(t)
  
  if(t %in% c("Public","Private")){
    aux <- base %>%
      filter(type1 == t)
  }
  if(t %in% c("IEP","CFP")){
    aux <- base %>%
      filter(type2 == t)
  }
  if(t %in% c("STEM","non-STEM")){
    aux <- base %>%
      filter(type3 == t)
  }
  if(t %in% c("HDD","-")){
    aux <- base %>%
      filter(type4 == t)
  }
  if(t %in% c("MozSkills","-")){
    aux <- base %>%
      filter(type5 == t)
  }
  if(t == "all"){
    aux <- base
  }
  
  aux <-  aux %>%
    mutate_all(.,~(p=ifelse(is.na(.),500,.))) %>%
    
    mutate(cum30_base_7 = ifelse(base_7 <= 30,1,0),
           cum60_base_7 = ifelse(base_7 <= 60,1,0),
           cum30_brt_7 = ifelse(brt_7 <= 30,1,0),
           cum60_brt_7 = ifelse(brt_7 <= 60,1,0),
           
           cum30_base_15 = ifelse(base_15 <= 30,1,0),
           cum60_base_15 = ifelse(base_15 <= 60,1,0),
           cum30_brt_15 = ifelse(brt_15 <= 30,1,0),
           cum60_brt_15 = ifelse(brt_15 <= 60,1,0),
           
           cum30_base_21 = ifelse(base_21 <= 30,1,0),
           cum60_base_21 = ifelse(base_21 <= 60,1,0),
           cum30_brt_21 = ifelse(brt_21 <= 30,1,0),
           cum60_brt_21 = ifelse(brt_21 <= 60,1,0)) %>%
    
    group_by(from_id) %>%
    
    summarise(near_base_7 = min(base_7,na.rm=T),
              near_brt_7 = min(brt_7,na.rm=T),
              
              near_base_15 = min(base_15,na.rm=T),
              near_brt_15 = min(brt_15,na.rm=T),
              
              near_base_21 = min(base_21,na.rm=T),
              near_brt_21 = min(brt_21,na.rm=T),
              
              
              cum30_base_7 = sum(cum30_base_7,na.rm=T),
              cum60_base_7 = sum(cum60_base_7,na.rm=T),
              cum30_brt_7 = sum(cum30_brt_7,na.rm=T),
              cum60_brt_7 = sum(cum60_brt_7,na.rm=T),
              
              cum30_base_15 = sum(cum30_base_15,na.rm=T),
              cum60_base_15 = sum(cum60_base_15,na.rm=T),
              cum30_brt_15 = sum(cum30_brt_15,na.rm=T),
              cum60_brt_15 = sum(cum60_brt_15,na.rm=T),
              
              cum30_base_21 = sum(cum30_base_21,na.rm=T),
              cum60_base_21 = sum(cum60_base_21,na.rm=T),
              cum30_brt_21 = sum(cum30_brt_21,na.rm=T),
              cum60_brt_21 = sum(cum60_brt_21,na.rm=T)) %>%
    
    mutate(type = t)
  
  access <- bind_rows(access,aux)
}

access[access == 500] <- NA  

rm(destinations,origins,centroids,cor,gtfs,gtfs_trip,intersect,no_intersect,points,poverty,shape,stop_times,stops,street,ttm,r5r_core)
gc()

#### Ex-ante accessibility analysis #####################################################################

access %>%
  ungroup() %>%
  left_join(grid_200 %>% st_drop_geometry(),by=c("from_id"="id")) %>%
  filter(!is.na(pop_count)) %>%
  group_by(type) %>%
  summarise_at(c("near_base_7","near_brt_7",
                 "near_base_15","near_brt_15",
                 "near_base_21","near_brt_21",
                 "cum30_base_7","cum30_brt_7",
                 "cum30_base_15","cum30_brt_15",
                 "cum30_base_21","cum30_brt_21",
                 "cum60_base_7","cum60_brt_7",
                 "cum60_base_15","cum60_brt_15",
                 "cum60_base_21","cum60_brt_21"),~(p=stats::weighted.mean(.,pop_count,na.rm=T))) %>%
  gather(key="method",value="access",-c(type)) %>%
  mutate(time = sub(".*_(.*)$", "\\1", method),
         mode = sub("^[^_]*_(.*)$", "\\1", method),
         method = sub("_.*$", "", method)) %>%
  mutate(mode = sub("_.*$", "", mode)) %>%
  arrange(type,time) %>%
  print(n = 200)

access <- access %>%
  mutate(diff_cum60_7 = ifelse(cum60_base_7 == cum60_brt_7,1,cum60_brt_7/cum60_base_7),
         diff_cum60_15 = ifelse(cum60_base_15 == cum60_brt_7,1,cum60_brt_15/cum60_base_15),
         diff_cum60_21 = ifelse(cum60_base_21 == cum60_brt_7,1,cum60_brt_21/cum60_base_21),
         diff_near_7 = ifelse(near_base_7 == near_brt_7,1,near_brt_7/near_base_7),
         diff_near_15 = ifelse(near_base_15 == near_brt_15,1,near_brt_15/near_base_15),
         diff_near_21 = ifelse(near_base_21 == near_brt_21,1,near_brt_21/near_base_21))

access <- grid_200 %>%
  st_transform(4326) %>%
  left_join(access,by=c("id"="from_id"))

access %>%
  filter(type == "all") %>%
  ggplot() +
  geom_point(aes(x=cum60_base_21,cum60_brt_21,color=poverty_group),alpha = 0.4) +
  scale_color_distiller(palette = 'Spectral',direction = 1,breaks = seq(1,10,1),name= "Poverty decile") +
  geom_abline() +
  theme_minimal() +
  xlab("Before") +
  ylab("After") +
  xlim(0,70) + ylim(0,70)  +
  theme(legend.position="bottom",
        legend.key.width = unit(1, 'cm'))
ggsave(paste0(path,"/Outputs/graph_cum60_transit_21_brt_200.png"),width = 6,height = 6,dpi=500)  


#### Accessibility maps ######################################################################
# Maps

transit_map <- st_read("BRT/trunk_shape/shape_MISTRA.shp") %>%
  bind_rows(st_read("BRT/trunk_shape/shape_ALBRA.shp")) %>%
  bind_rows(st_read("BRT/trunk_shape/shape_ZIMTRA.shp")) %>%
  summarise()

access <- access %>%
  filter(!is.na(type))

aux <- access$diff_near_7
aux <- aux[is.finite(aux)] 
aux <- aux[aux != 1]
aux <- aux[!is.na(aux)] 
quantile(aux,na.rm=T,probs=seq(0.1,1, by=0.1))

i <- "all"
label <- paste0("Access to ",i," TVETs by transit\nbefore and after the BRT")

map <- access %>% filter(type == i)

breaks <-  c(0,0.95,0.975,1.0,1.025,1.05,10) 
#breaks <-  c(0,0.8,0.9,1.0,1.1,1.2,100) 

map <- map %>% mutate(var = cut(diff_near_21,breaks,labels = breaks[2:7]))

# Get TVETs
if(i == "all"){
  tvets_map <- tvets_shp
} else{
  tvets_map <- tvets_shp %>%
    filter(type1 == i | type2 == i | type3 == i | type4 == i | type5 == i)
}

# Map
m <- ggplot() +
  base_map(bbox, increase_zoom = 7, basemap = 'positron',nolabels=T) +
  geom_sf(data=map, aes(fill=var),color="transparent") +
  geom_sf(data = bairros,aes(),fill="transparent",linetype="dashed",linewidth=0.1) +
  geom_sf(data=municipios,fill="transparent",color="black",linewidth=0.4) +
  geom_sf(data=transit_map, aes(),color="grey",linewidth=0.2) +
  #geom_sf(data=tvets_map,aes(geometry=geometry),size=0.4,color="#00bfc4")+
  scale_fill_manual(values = (c("#264653","#287271","#2a9d8f","#e9c46a","#f4a261","#ee8959","#e76f51")),
                    breaks=breaks,
                    na.value = "transparent",
                    name=label,
                    drop = FALSE) +
  #coord_sf(datum = NA,
  #         xlim = c(box['xmin'], box['xmax']),
  #        ylim = c(box['ymin'], box['ymax'])) +
  coord_sf(datum = NA,
           xlim = c(32.25,32.85),
           ylim = c(-26.1,-25.35)) +
  theme_minimal() +
  theme(legend.position="bottom",
        legend.key.width = unit(1, 'cm'))
ggsave(plot = m,paste0(path,"/Outputs/diff_brt_near_21_200_",i,".png"),width = 5,height = 7,dpi=600)

#### Export data ##################################################################

# Aggregated accessibility data by bairro
aux <- access %>%
  filter(type == "all") %>%
  mutate(across(-geometry, ~ ifelse(is.nan(.), NA, .)))

aux %>% st_write("accessibility_grid_diff_transit.shp",delete_layer = T)

aux2 <- aux %>%
  st_join(bairros,join=st_intersects) %>%
  st_drop_geometry() %>%
  group_by(area_id,Provincia,Distrito,Posto,Localidade,Bairro,LocalBairr) %>%
  summarise(across(
    .cols = c("near_base_7","near_brt_7","near_base_15","near_brt_15","near_base_21","near_brt_21",
              "cum30_base_7","cum30_brt_7","cum30_base_15","cum30_brt_15","cum30_base_21","cum30_brt_21",
              "cum60_base_7","cum60_brt_7","cum60_base_15","cum60_brt_15","cum60_base_21","cum60_brt_21",
              "consumption"),
    .fns = ~ {
      tmp <- na.omit(data.frame(x = ., w = pop_count))
      if (nrow(tmp) == 0) NA_real_ else weighted.mean(tmp$x, tmp$w)}),.groups = "drop")

aux2 <- aux %>%
  st_join(bairros,join=st_intersects) %>%
  st_drop_geometry() %>%
  group_by(area_id,Provincia,Distrito,Posto,Localidade,Bairro,LocalBairr) %>%
  summarise(pop_count=sum(pop_count,na.rm=T)) %>%
  left_join(aux2,by=c("area_id","Provincia","Distrito","Posto","Localidade","Bairro","LocalBairr"))


aux2 <- aux2 %>%
  mutate(diff_cum60_7 = ifelse(cum60_base_7 == cum60_brt_7,1,cum60_brt_7/cum60_base_7),
         diff_cum60_15 = ifelse(cum60_base_15 == cum60_brt_15,1,cum60_brt_15/cum60_base_15),
         diff_cum60_21 = ifelse(cum60_base_21 == cum60_brt_21,1,cum60_brt_21/cum60_base_21),
         diff_near_7 = ifelse(near_base_7 == near_brt_7,1,near_brt_7/near_base_7),
         diff_near_15 = ifelse(near_base_15 == near_brt_15,1,near_brt_15/near_base_15),
         diff_near_21 = ifelse(near_base_21 == near_brt_21,1,near_brt_21/near_base_21))

aux2 <- aux2 %>%
  ungroup() %>%
  arrange(consumption) %>%
  mutate(poverty = pop_count/sum(pop_count)) %>%
  mutate(poverty = cumsum(poverty)) %>%
  mutate(poverty_group = 1 + as.numeric(floor(poverty*10))) %>%
  mutate(poverty_group = ifelse(poverty == 1,10,poverty_group)) %>%
  mutate(poverty_group = case_when(poverty_group <= 4 ~ "High - bottom 40%",
                                   poverty_group >= 9 ~ "Low - top 20%",
                                   TRUE ~ "Middle - 40-80%")) %>%
  arrange(diff_cum60_21) %>%
  mutate(accessibility_gains = pop_count/sum(pop_count)) %>%
  mutate(accessibility_gains = cumsum(accessibility_gains)) %>%
  mutate(accessibility_gains_group = 1 + as.numeric(floor(accessibility_gains*10))) %>%
  mutate(accessibility_gains_group = ifelse(accessibility_gains == 1,10,accessibility_gains_group)) %>%
  mutate(accessibility_gains_group = case_when(accessibility_gains_group <= 4 ~ "Low - bottom 40%",
                                         accessibility_gains_group >= 9 ~ "High - top 20%",
                                         TRUE ~ "Moderate - 40-80%")) %>%
  arrange(desc(near_base_7)) %>%
  mutate(accessibility = pop_count/sum(pop_count)) %>%
  mutate(accessibility = cumsum(accessibility)) %>%
  mutate(accessibility_group = 1 + as.numeric(floor(accessibility*10))) %>%
  mutate(accessibility_group = ifelse(accessibility == 1,10,accessibility_group)) %>%
  mutate(accessibility_group = case_when(accessibility_group <= 4 ~ "Low - bottom 40%",
                                         accessibility_group >= 9 ~ "High - top 20%",
                                         TRUE ~ "Middle - 40-80%")) %>%
  mutate(group = case_when(poverty_group == "High - bottom 40%" & accessibility_group == "Low - bottom 40%" ~ "High-poverty | Low-accessibility",
                           poverty_group == "High - bottom 40%" & accessibility_group == "Middle - 40-80%" ~ "High-poverty | Mid-accessibility",
                           poverty_group == "High - bottom 40%" & accessibility_group == "High - top 20%" ~ "High-poverty | High-accessibility",
                           poverty_group == "Middle - 40-80%" & accessibility_group == "Low - bottom 40%" ~ "Mid-poverty | Low-accessibility",
                           poverty_group == "Middle - 40-80%" & accessibility_group == "Middle - 40-80%" ~ "Mid-poverty | Mid-accessibility",
                           poverty_group == "Middle - 40-80%" & accessibility_group == "High - top 20%" ~ "Mid-poverty | High-accessibility",
                           poverty_group == "Low - top 20%" & accessibility_group == "Low - bottom 40%" ~ "Low-poverty | Low-accessibility",
                           poverty_group == "Low - top 20%" & accessibility_group == "Middle - 40-80%" ~ "Low-poverty | Mid-accessibility",
                           poverty_group == "Low - top 20%" & accessibility_group == "High - top 20%" ~ "Low-poverty | High-accessibility")) %>%
  mutate(group = case_when(poverty_group == "High - bottom 40%" & accessibility_gains_group == "Low - bottom 40%" ~ "High-poverty | Low accessibility gains",
                           poverty_group == "High - bottom 40%" & accessibility_gains_group == "Moderate - 40-80%" ~ "High-poverty | Moderate accessibility gains",
                           poverty_group == "High - bottom 40%" & accessibility_gains_group == "High - top 20%" ~ "High-poverty | High accessibility gains",
                           poverty_group == "Middle - 40-80%" & accessibility_gains_group == "Low - bottom 40%" ~ "Mid-poverty | Low accessibility gains",
                           poverty_group == "Middle - 40-80%" & accessibility_gains_group == "Moderate - 40-80%" ~ "Mid-poverty | Moderate accessibility gains",
                           poverty_group == "Middle - 40-80%" & accessibility_gains_group == "High - top 20%" ~ "Mid-poverty | High accessibility gains",
                           poverty_group == "Low - top 20%" & accessibility_gains_group == "Low - bottom 40%" ~ "Low-poverty | Low accessibility gains",
                           poverty_group == "Low - top 20%" & accessibility_gains_group == "Moderate - 40-80%" ~ "Low-poverty | Moderate accessibility gains",
                           poverty_group == "Low - top 20%" & accessibility_gains_group == "High - top 20%" ~ "Low-poverty | High accessibility gains")) %>%
  mutate(poverty_group = ifelse(is.na(consumption),NA,poverty_group),
       accessibility_group = ifelse(is.na(near_base_7),NA,accessibility_group),
       accessibility_gains_group = ifelse(is.na(diff_cum60_21),NA,accessibility_gains_group)) %>%
  mutate(group = ifelse(is.na(poverty_group) | is.na(accessibility_gains_group) | is.na(accessibility_group),NA,group))


aux <- bairros %>%
  select(area_id,Provincia,Distrito,Posto,Localidade,Bairro,LocalBairr) %>%
  left_join(aux2,by=c("area_id","Provincia","Distrito","Posto","Localidade","Bairro","LocalBairr")) %>%
  filter(!is.na(group)) %>%
  mutate(group = factor(group,levels = c("Low-poverty | Low accessibility gains","Mid-poverty | Low accessibility gains","High-poverty | Low accessibility gains",
                                         "Low-poverty | Moderate accessibility gains","Mid-poverty | Moderate accessibility gains","High-poverty | Moderate accessibility gains",
                                         "Low-poverty | High accessibility gains","Mid-poverty | High accessibility gains","High-poverty | High accessibility gains")))


m <- ggplot() +
  base_map(bbox, increase_zoom = 7, basemap = 'positron',nolabels=T) +
  geom_sf(data = bairros,aes(),fill="transparent",linetype="dashed",linewidth=0.1) +
  geom_sf(data=municipios,fill="transparent",color="black",linewidth=0.4) +
  geom_sf(data=transit_map, aes(),color="grey",linewidth=0.2) +
  geom_sf(data=aux, aes(fill=group),alpha = 0.9) +
  scale_fill_manual(na.value = "transparent",
                    values = c("#cabed0","#bc7c8f","#ae3a4e",
                               "#89a1c8","#806a8a","#77324c",
                               "#4885c1","#435786","#3f2949"), 
                    name=NULL,
                    drop = FALSE,
                    guide = "legend") +
  coord_sf(datum = NA,
           xlim = c(box['xmin'], box['xmax']),
           ylim = c(box['ymin'], box['ymax'])) +
  theme_minimal() +
  theme(legend.position="bottom",
        legend.key.width = unit(1, 'cm')) +
  xlab("") + ylab("") +
  guides(fill = guide_legend(nrow = 3)) 
ggsave(plot = m,paste0(path,"/Outputs/bairros_groups_poverty_access_gains.png"),width = 10,height = 14,dpi=600)

aux %>% st_drop_geometry() %>% write_csv("Outputs/poverty_accessibility_brt.csv")


#### People near transit #########################################################

gtfs <- read_gtfs("BRT/BRT_gtfs.zip")
stations <- gtfs[["stops"]] %>%
  st_as_sf(coords = c("stop_lon","stop_lat")) %>%
  st_set_crs(4326) %>%
  st_transform(32736) %>%
  st_buffer(500)%>%
  st_transform(4326) %>%
  summarise() %>%
  mutate(brt = 1)
pnt <- grid %>% st_centroid() %>%
  st_join(stations,join = st_intersects) %>%
  filter(brt == 1) %>%
  st_drop_geometry() %>%
  group_by(poverty_group) %>%
  #group_by(stop_id,stop_name,poverty_group) %>%
  summarise(pnt = sum(pop_count,na.rm=T))
pnt <-  pnt %>% left_join(grid %>% st_drop_geometry() %>%
                            group_by(poverty_group) %>%
                            summarise(pop_count = sum(pop_count,na.rm=T)),by="poverty_group") %>%
  mutate(pct_group = 100*pnt/pop_count) %>%
  ungroup() %>%
  #group_by(stop_id) %>%
  mutate(pct_brt = 100*pnt/sum(pnt))
pnt
pnt %>% write_csv("Outputs/pnt_brt_stations_poverty.csv")

box_t <- st_bbox(stations %>% st_transform(32736) %>% st_buffer(1500) %>% st_transform(4326))

cover <- bairros %>% st_difference(stations)

# Map of 500-meter from the new stations by poverty group
ggplot() +
  base_map(box_t, increase_zoom = 7, basemap = 'positron',nolabels=T) +
  geom_sf(data = grid, aes(fill=factor(as.character(poverty_group),levels = c("1","2","3","4","5","6","7","8","9","10"))),color="transparent",alpha = 0.6,show.legend = T) +
  geom_sf(data = bairros,aes(),fill="transparent",linetype="dashed",linewidth=0.3) +
  geom_sf(data=municipios,fill="transparent",color="black",linewidth=0.4) +
  geom_sf(data=cover, aes(),color="darkgrey",alpha = 0.7) +
  scale_fill_manual(values = c("#9b2226","#ae2012","#bb3e03","#ca6702","#ee9b00","#e9d8a6","#94d2bd","#0a9396","#005f73","#001219"),
                    name="Poverty group",
                    breaks = c("1","2","3","4","5","6","7","8","9","10"),
                    drop = F) +
  guides(show.legend = TRUE) +
  coord_sf(datum = NA,
           xlim = c(box_t['xmin'], box_t['xmax']),
           ylim = c(box_t['ymin'], box_t['ymax'])) +
  theme_minimal() +
  theme(legend.key.width = unit(1.5, 'cm'))
ggsave(paste0(path,"/Outputs/poverty_pnt_stations.png"),width = 6,height = 7,dpi=600)

# Population counts within 500 meters from the new stations
ggplot() +
  base_map(box_t, increase_zoom = 7, basemap = 'positron',nolabels=T) +
  geom_sf(data = grid, aes(fill=pop_count),,color="transparent",alpha = 0.6,show.legend = T) +
  geom_sf(data = bairros,aes(),fill="transparent",linetype="dashed",linewidth=0.3) +
  geom_sf(data=municipios,fill="transparent",color="black",linewidth=0.4) +
  geom_sf(data=cover, aes(),color="darkgrey",alpha = 0.7) +
  scale_fill_gradient(low = "#E4F1E7",
                      high = "#283618",
                      name="Population",
                      n.breaks = 10) +
  guides(show.legend = TRUE) +
  coord_sf(datum = NA,
           xlim = c(box_t['xmin'], box_t['xmax']),
           ylim = c(box_t['ymin'], box_t['ymax'])) +
  theme_minimal() +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 20))
ggsave(paste0(path,"/Outputs/pop_count_pnt_stations.png"),width = 6,height = 7,dpi=600)

