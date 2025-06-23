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

#### GTFS fixes ####################################################################
# Fix GTFS of Informal transport to create stops at every 100 meters

generate_points_along_line <- function(line, distance) {
  line_length <- as.numeric(st_length(line)) 
  num_points <- ceiling(line_length / distance)  
  fractions <- seq(0, 1, length.out = num_points + 1)  
  points <- st_line_sample(line, sample = fractions)  
  return(st_cast(st_sfc(points, crs = st_crs(line)), "POINT"))  
}

list.files(paste0(path,"/GTFS/"),pattern = ".zip",full.names = T)
gtfs <- read_gtfs(paste0(path,"/GTFS/Maputo Chapas.zip"))
gtfs[["routes"]] %>%
  group_by(agency_id) %>%
  summarise(n_routes = n())

stop_times <- NULL
stops <- NULL
for(i in gtfs[["trips"]]$trip_id){
  print(i)
  
  gtfs_trip <- filter_by_trip_id(gtfs,i)
  
  shape <- convert_shapes_to_sf(gtfs_trip,sort_sequence = T)
  
  if(nrow(shape) > 1) {
    print(paste0("More than one shape for trip ",i))
    break
  }
  
  shape <- shape %>% st_transform(32736)

  shape <- st_sf(geometry = shape$geometry, crs = 32736)
  
  points <- generate_points_along_line(shape,100)
  points <- st_sf(geometry = points, crs = 32736)
  points <- points %>%
    mutate(stop_id = paste0(i,"-",row_number()),
           stop_sequence = row_number())
  
  ggplot() + 
    geom_sf(data = shape, color = "red") +
    geom_sf(data = points, aes(color = stop_sequence))
  
  duration <- as.POSIXct(max(gtfs_trip[["stop_times"]]$arrival_time), format="%H:%M:%S") - as.POSIXct(min(gtfs_trip[["stop_times"]]$arrival_time), format="%H:%M:%S")
  duration <- duration/nrow(points)
  duration
  
  aux <- points %>%
    mutate(trip_id = i) %>%
    mutate(arrival_time = case_when(stop_sequence == 1 ~ min(gtfs_trip[["stop_times"]]$arrival_time),
                                    stop_sequence == nrow(points) ~ max(gtfs_trip[["stop_times"]]$arrival_time),
                                    TRUE ~ strftime(as.POSIXct(min(gtfs_trip[["stop_times"]]$arrival_time), format="%H:%M:%S") + duration*stop_sequence, format="%H:%M:%S"))) %>%
    mutate(departure_time = arrival_time,
           stop_headsign = NA,
           pickup_type = NA,
           drop_off_type = NA,
           shape_dist_traveled = ifelse(stop_sequence == 1,0,(stop_sequence-1)*100)) %>%
    st_transform(4326) %>%
    mutate(stop_lon = st_coordinates(.)[,1],
           stop_lat = st_coordinates(.)[,2]) %>%
    st_drop_geometry() 
  
  aux_stops <- aux %>% select(stop_id,stop_lon,stop_lat) %>%
    mutate(stop_code = NA,
           stop_name = "-",
           stop_desc = NA,
           zone_id = NA,
           stop_url = NA,
           location_type = 0,
           parent_station = NA,
           stop_timezone = NA,
           wheelchair_boarding = NA) %>%
    select(colnames(gtfs_trip[["stops"]]))
  stops <- stops %>% bind_rows(aux_stops)
  
  aux_stop_times <- aux %>%
    select(colnames(gtfs_trip[["stop_times"]]))
  stop_times <- stop_times %>% bind_rows(aux_stop_times)
}

gtfs[["stop_times"]] <- stop_times
gtfs[["stops"]] <- stops
gtfs[["routes"]] <- gtfs[["routes"]] %>%
  mutate(route_type = 3)

write_gtfs(gtfs,paste0(path,"/GTFS/Maputo Chapas_fixed.zip"))

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

#### Statistics ####################################################################
# Population Statistics
grid %>%
  st_drop_geometry() %>%
  mutate(bottom40 = ifelse(poverty <= 0.4,pop_count,0)) %>%
  group_by(Municipio) %>%
  summarise(pop = sum(pop_count),
            bottom40 = sum(bottom40),
            consumption = stats::weighted.mean(consumption,pop_count,na.rm=T),
            urban_area = n()*(100*100/10^6)) %>%
  arrange(desc(pop)) %>%
  left_join(municipios %>% mutate(area = as.numeric(st_area(.))/10^6) %>%
              select(Municipio,area) %>% st_drop_geometry(),by="Municipio") %>%
  mutate(density = pop/area,
         urban_density = pop/urban_area,
         pct_bottom = bottom40/pop)

# TVETs Statistics
tvets %>%
  mutate(type2 = ifelse(type2=="IEP","Long-term","Short-term")) %>%
  group_by(type1,type2,type3,type4,type5) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  ggplot(aes(x = type2,y= n,group=type2)) +
  geom_col(aes(fill=type1,color=interaction(type4,type5)),size=1) +
  geom_text(aes(label = n),position = position_stack(vjust = .5)) +
  scale_fill_brewer(palette=2) +
  scale_color_manual(values = c("transparent","#F28F3B","#1D3461","#B33951")) +
  facet_wrap(~type3) +
  theme_minimal() +
  xlab("") + ylab("Number of TVETs by type") + 
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())
ggsave(paste0(path,"/Outputs/tvets.png"),width = 7,height = 5,dpi=500)

#### Travel time matrices #####################################################################
# Estimate travel time matrices (transit, formal transit and walking)

#Elevation data
#boundaries <- grid %>% st_transform(4326) %>% st_make_valid() %>% summarise()
#elevation <- get_elev_raster(boundaries, z = 10)
#writeRaster(elevation,paste0(path,"/r5r/elevation_maputo.tif"),overwrite=TRUE)

#R5R setup
r5r_cache(list_files = TRUE, delete_file = "all")
#r5r_core <- setup_r5(data_path = paste0(path,"/r5r"), verbose = F)
#r5r_core <- setup_r5(data_path = paste0(path,"/r5r_formal"), verbose = F)
#r5r_core <- setup_r5(data_path = paste0(path,"/r5r_walk"), verbose = F, overwrite = T)
r5r_core <- setup_r5(data_path = paste0(path,"/r5r_brt"), verbose = F)

#Street and Transit network visualization
street <- street_network_to_sf(r5r_core)
transit <- transit_network_to_sf(r5r_core)

#map check
ggplot() +
  base_map(bbox, increase_zoom = 7, basemap = 'google-terrain') +
  geom_sf(data=municipios,fill="transparent",color="black",linewidth=0.4) +
  geom_sf(data=street$edges, aes(), color="grey80", linewidth=0.05)+
  geom_sf(data=transit$routes, aes(color=agency_name), linewidth=0.2) +
  #geom_sf(data=tvets_shp,aes(shape = type3),size=1) +
  scale_shape_manual(values = c(0,16),name="TVETs") +
  scale_color_manual(name = "Transit systems",values = c("red","#53B400","#00C094","#00B6EB","#F8766D","#C49A00","#FB61D7","#A58AFF")) +
  coord_sf(datum = NA,
           xlim = c(box['xmin'], box['xmax']),
           ylim = c(box['ymin'], box['ymax'])) +
  theme_minimal()
#ggsave(paste0(path,"/Outputs/transit_systems.png"),width = 9,height = 12,dpi=500)
ggsave(paste0(path,"/Outputs/transit_systems_brt.png"),width = 9,height = 12,dpi=500)

#Defining origins based on grid centroids
origins <- grid %>%
  st_centroid()
origins <- do.call(rbind, st_geometry(origins)) %>% 
  as_tibble() %>% setNames(c("lon","lat")) %>%
  mutate(id = origins$fid) %>%
  select(id,lon,lat)

destinations <- tvets %>%
  select(N,Latitude,Longitude) %>%
  rename(id = "N",
         lon = "Longitude",
         lat = "Latitude")%>%
  select(id,lon,lat)

gc()

#Travel time matrix
departure_datetime <- as.POSIXct("08-01-2019 07:00:00", format = "%d-%m-%Y %H:%M:%S",tz="Africa/Maputo")
ttm <- travel_time_matrix(r5r_core = r5r_core,
                          origins = origins,
                          destinations = destinations,
                          mode = "WALK",
                          departure_datetime = departure_datetime,
                          max_walk_time = 2*60,
                          max_trip_duration = 5*60)
ttm %>% saveRDS(paste0(path,"/ttm_walk_tvets.rds"))

#Travel time matrix
departure_datetime <- as.POSIXct("08-01-2019 07:00:00", format = "%d-%m-%Y %H:%M:%S",tz="Africa/Maputo")
ttm <- travel_time_matrix(r5r_core = r5r_core,
                          origins = origins,
                          destinations = destinations,
                          mode = c("WALK","TRANSIT"),
                          departure_datetime = departure_datetime,
                          max_walk_time = 2*60,
                          max_trip_duration = 5*60)
#ttm %>% saveRDS(paste0(path,"/ttm_transit_tvets_7.rds"))
#ttm %>% saveRDS(paste0(path,"/ttm_formal_transit_tvets_7.rds"))
ttm %>% saveRDS(paste0(path,"/ttm_transit_tvets_7_brt.rds"))

#Travel time matrix
departure_datetime <- as.POSIXct("08-01-2019 15:00:00", format = "%d-%m-%Y %H:%M:%S",tz="Africa/Maputo")
ttm <- travel_time_matrix(r5r_core = r5r_core,
                          origins = origins,
                          destinations = destinations,
                          mode = c("WALK","TRANSIT"),
                          departure_datetime = departure_datetime,
                          max_walk_time = 2*60,
                          max_trip_duration = 5*60)
#ttm %>% saveRDS(paste0(path,"/ttm_transit_tvets_15.rds"))
#ttm %>% saveRDS(paste0(path,"/ttm_formal_transit_tvets_15.rds"))
ttm %>% saveRDS(paste0(path,"/ttm_transit_tvets_15_brt.rds"))

#Travel time matrix
departure_datetime <- as.POSIXct("08-01-2019 21:00:00", format = "%d-%m-%Y %H:%M:%S",tz="Africa/Maputo")
ttm <- travel_time_matrix(r5r_core = r5r_core,
                          origins = origins,
                          destinations = destinations,
                          mode = c("WALK","TRANSIT"),
                          departure_datetime = departure_datetime,
                          max_walk_time = 2*60,
                          max_trip_duration = 5*60)
#ttm %>% saveRDS(paste0(path,"/ttm_transit_tvets_21.rds"))
#ttm %>% saveRDS(paste0(path,"/ttm_formal_transit_tvets_21.rds"))
ttm %>% saveRDS(paste0(path,"/ttm_transit_tvets_21_brt.rds"))

#### Accessibility calculations #####################################################################
# Calculate accessibility

#Open travel time matrices
ttm <- readRDS(paste0(path,"/ttm_transit_tvets_7.rds")) %>%
  rename(transit_7 = "travel_time_p50") %>%
  left_join(readRDS(paste0(path,"/ttm_transit_tvets_15.rds")) %>%
              rename(transit_15 = "travel_time_p50"),by = c("from_id","to_id")) %>%
  left_join(readRDS(paste0(path,"/ttm_transit_tvets_21.rds")) %>%
              rename(transit_21 = "travel_time_p50"),by = c("from_id","to_id")) %>%
  
  left_join(readRDS(paste0(path,"/ttm_formal_transit_tvets_7.rds")) %>%
              rename(formaltransit_7 = "travel_time_p50"),by = c("from_id","to_id")) %>%
  left_join(readRDS(paste0(path,"/ttm_formal_transit_tvets_15.rds")) %>%
              rename(formaltransit_15 = "travel_time_p50"),by = c("from_id","to_id")) %>%
  left_join(readRDS(paste0(path,"/ttm_formal_transit_tvets_21.rds")) %>%
              rename(formaltransit_21 = "travel_time_p50"),by = c("from_id","to_id")) %>%
  
  left_join(readRDS(paste0(path,"/ttm_walk_tvets.rds")) %>%
              rename(walking = "travel_time_p50"),by = c("from_id","to_id")) %>%
  
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
    
  mutate(cum30_transit_7 = ifelse(transit_7 <= 30,1,0),
         cum60_transit_7 = ifelse(transit_7 <= 60,1,0),
         cum30_formaltransit_7 = ifelse(formaltransit_7 <= 30,1,0),
         cum60_formaltransit_7 = ifelse(formaltransit_7 <= 60,1,0),
         
         cum30_transit_15 = ifelse(transit_15 <= 30,1,0),
         cum60_transit_15 = ifelse(transit_15 <= 60,1,0),
         cum30_formaltransit_15 = ifelse(formaltransit_15 <= 30,1,0),
         cum60_formaltransit_15 = ifelse(formaltransit_15 <= 60,1,0),
         
         cum30_transit_21 = ifelse(transit_21 <= 30,1,0),
         cum60_transit_21 = ifelse(transit_21 <= 60,1,0),
         cum30_formaltransit_21 = ifelse(formaltransit_21 <= 30,1,0),
         cum60_formaltransit_21 = ifelse(formaltransit_21 <= 60,1,0),
         
         cum30_walking = ifelse(walking <= 30,1,0),
         cum60_walking = ifelse(walking <= 60,1,0)) %>%
    
  group_by(from_id) %>%
    
  summarise(near_transit_7 = min(transit_7,na.rm=T),
            near_formaltransit_7 = min(formaltransit_7,na.rm=T),
            
            near_transit_15 = min(transit_15,na.rm=T),
            near_formaltransit_15 = min(formaltransit_15,na.rm=T),
            
            near_transit_21 = min(transit_21,na.rm=T),
            near_formaltransit_21 = min(formaltransit_21,na.rm=T),
            
            near_walking_0 = min(walking,na.rm=T),
            
            cum30_transit_7 = sum(cum30_transit_7,na.rm=T),
            cum60_transit_7 = sum(cum60_transit_7,na.rm=T),
            cum30_formaltransit_7 = sum(cum30_formaltransit_7,na.rm=T),
            cum60_formaltransit_7 = sum(cum60_formaltransit_7,na.rm=T),
            
            cum30_transit_15 = sum(cum30_transit_15,na.rm=T),
            cum60_transit_15 = sum(cum60_transit_15,na.rm=T),
            cum30_formaltransit_15 = sum(cum30_formaltransit_15,na.rm=T),
            cum60_formaltransit_15 = sum(cum60_formaltransit_15,na.rm=T),
            
            cum30_transit_21 = sum(cum30_transit_21,na.rm=T),
            cum60_transit_21 = sum(cum60_transit_21,na.rm=T),
            cum30_formaltransit_21 = sum(cum30_formaltransit_21,na.rm=T),
            cum60_formaltransit_21 = sum(cum60_formaltransit_21,na.rm=T),
            
            cum30_walking_0 = sum(cum60_walking,na.rm=T),
            cum60_walking_0 = sum(cum60_walking,na.rm=T)) %>%
    
    mutate(type = t)
  
  access <- bind_rows(access,aux)
}

access[access == 500] <- NA  

rm(destinations,origins,centroids,cor,gtfs,gtfs_trip,intersect,no_intersect,points,poverty,shape,stop_times,stops,street,ttm,r5r_core)
gc()

#### Accessibility differences ######################################################################
# Accessibility differences
access %>%
  ungroup() %>%
  left_join(grid,by=c("from_id"="fid")) %>%
  filter(!is.na(pop_count)) %>%
  group_by(type) %>%
  summarise_at(c("near_transit_7","near_formaltransit_7",
                 "near_transit_15","near_formaltransit_15",
                 "near_transit_21","near_formaltransit_21",
                 "cum30_transit_7","cum30_formaltransit_7",
                 "cum30_transit_15","cum30_formaltransit_15",
                 "cum30_transit_21","cum30_formaltransit_21",
                 "cum60_transit_7","cum60_formaltransit_7",
                 "cum60_transit_15","cum60_formaltransit_15",
                 "cum60_transit_21","cum60_formaltransit_21",
                 "near_walking_0","cum30_walking_0","cum60_walking_0"),~(p=stats::weighted.mean(.,pop_count,na.rm=T))) %>%
  gather(key="method",value="access",-c(type)) %>%
  mutate(time = sub(".*_(.*)$", "\\1", method),
         mode = sub("^[^_]*_(.*)$", "\\1", method),
         method = sub("_.*$", "", method)) %>%
  mutate(mode = sub("_.*$", "", mode)) %>%
  arrange(type,mode)

# Accessibility differences by time
aux <- access %>%
  mutate(dif15 = near_transit_15/near_transit_7,
         dif21 = near_transit_21/near_transit_7) %>%
  dplyr::select(from_id,near_transit_7,dif15,dif21)

access %>%
  filter(type == "all") %>%
  ggplot() +
  geom_point(aes(x=near_transit_7,near_transit_15),alpha = 0.4,color = "darkgreen") +
  geom_abline() +
  theme_minimal() +
  xlab("Time to the nearest TVET by transit at 7am") +
  ylab("Time to the nearest TVET by transit at 3pm") +
  xlim(0,250) + ylim(0,250)
ggsave(paste0(path,"/Outputs/diff_near_transit_15.png"),width = 6,height = 6,dpi=500)  

access %>%
  filter(type == "all") %>%
  ggplot() +
  geom_point(aes(x=near_transit_7,near_transit_21),alpha = 0.4,color = "darkgreen") +
  geom_abline() +
  theme_minimal() +
  xlab("Time to the nearest TVET by transit at 7am") +
  ylab("Time to the nearest TVET by transit at 9pm")+
  xlim(0,250) + ylim(0,250)
ggsave(paste0(path,"/Outputs/diff_near_transit_21.png"),width = 6,height = 6,dpi=500)  
  
#### Accessibility maps ######################################################################
# Maps

#r5r_cache(list_files = TRUE, delete_file = "all")
#r5r_core <- setup_r5(data_path = paste0(path,"/r5r"), verbose = F)
#transit_map <- transit_network_to_sf(r5r_core)
#transit_map$routes %>% st_write(paste0(path,"/GTFS/transit_routes.shp"))
transit_map <- st_read(paste0(path,"/GTFS/transit_routes.shp"))
#r5r_cache(list_files = TRUE, delete_file = "all")
#r5r_core <- setup_r5(data_path = paste0(path,"/r5r_formal"), verbose = F)
#transit_map <- transit_network_to_sf(r5r_core)
#transit_map$routes %>% st_write(paste0(path,"/GTFS/formal_transit_routes.shp"))

rm(r5r_cache,r5r_core,transit_map)
gc()

aux_maps <- access %>%
  gather(key="method",value = "access",-c("type","from_id"))

t <- aux_maps %>% select(type,method)
t <- t[!duplicated(t),]
t <- t %>% filter(grepl("cum30",method))
t <- t %>% filter(grepl("all",type))

for(i in 1:nrow(t)){
  print(i)
  # filter database
  ty <- t[i,] %>% select(type) %>% unlist()
  me <- t[i,] %>% select(method) %>% unlist()
  mo <- sub("^[^_]*_([^_]*)_.*$", "\\1", me)
  mo <- ifelse(mo == "formaltransit","formal transit",mo)
  tm <- sub(".*_(.*)$", "\\1", me)
  label <- ifelse(mo == "walking",
                  paste0("Access to ",ty," TVETs","\n","by ",mo),
                  paste0("Access to ",ty," TVETs","\n","by ",mo," at ",tm,"h"))
  
  map <- aux_maps %>% filter(type == ty,
                        method == me)
  map <- grid %>%
    left_join(map,by=c("fid"="from_id"))
  
  if(grepl("near",me)){
   breaks <- c(0,30,60,90,120,150,180)
  } else{
   breaks <-  c(0,5,10,15,20,25,85) 
  }
  
  map <- map %>% mutate(var = cut(access,breaks,labels = breaks[2:7]))
  
  dir <- ifelse(grepl("near",me) == T,-1,1)
  
  # Get transit routes
  if(mo == "transit"){
    transit_map <- st_read(paste0(path,"/GTFS/transit_routes.shp"))
  }
  if(mo == "formal transit"){
    transit_map <- st_read(paste0(path,"/GTFS/formal_transit_routes.shp"))
  }
  if(mo == "walking"){
    transit_map <- data.frame(lon = 32.588711,lat = -25.953724)
    transit_map <- st_as_sf(transit_map, coords = c("lon", "lat"), crs = 4326)
  }
  
  # Get TVETs
  if(ty == "all"){
    tvets_map <- tvets_shp
  } else{
    tvets_map <- tvets_shp %>%
      filter(type1 == ty | type2 == ty | type3 == ty)
  }

  # Map
m <- ggplot() +
  base_map(bbox, increase_zoom = 7, basemap = 'positron',nolabels=T) +
  geom_sf(data=map, aes(fill=var),color="transparent") +
  geom_sf(data = bairros,aes(),fill="transparent",linetype="dashed",linewidth=0.1) +
  geom_sf(data=municipios,fill="transparent",color="black",linewidth=0.4) +
  geom_sf(data=transit_map, aes(),color="grey",linewidth=0.2) +
  geom_sf(data=tvets_map,aes(geometry=geometry),size=0.5,color="#00bfc4")+
  scale_fill_viridis_d(direction = dir,
                       breaks=breaks,
                       option="magma",
                       na.value = "black",
                       name=label,
                       drop = FALSE) +
  coord_sf(datum = NA,
           xlim = c(box['xmin'], box['xmax']),
           ylim = c(box['ymin'], box['ymax'])) +
  theme_minimal() +
  theme(legend.position="bottom",
        legend.key.width = unit(1, 'cm'))
ggsave(plot = m,paste0(path,"/Outputs/access_",ty,"_",me,".png"),width = 5,height = 7,dpi=600)
}

#### Poverty analysis  ######################################################################
# Summary of accessibility by poverty level

# Poverty map
ggplot() +
  base_map(bbox, increase_zoom = 7, basemap = 'positron',nolabels=T) +
  geom_sf(data = grid %>% filter(poverty_group > 4),color="transparent",fill="lightgrey") +
  geom_sf(data = grid %>% filter(poverty_group <= 4), aes(fill=pop_count),color="transparent") +
  geom_sf(data = bairros,aes(),fill="transparent",linetype="dashed",linewidth=0.1) +
  geom_sf(data=municipios,fill="transparent",color="black",linewidth=0.4) +
  scale_fill_viridis_c(direction = 1,option="viridis",
                       name="Population") +
                       #name="Household consumption") +
  coord_sf(datum = NA,
           xlim = c(box['xmin'], box['xmax']),
           ylim = c(box['ymin'], box['ymax'])) +
  theme_minimal() +
  theme(legend.position="bottom",
       legend.key.width = unit(1.5, 'cm'))
ggsave(paste0(path,"/Outputs/poverty40_all2.png"),width = 6,height = 7,dpi=500)

aux <- grid %>%
  st_drop_geometry() %>%
  left_join(access,by=c("fid"="from_id")) %>%
  mutate_all(~ifelse(is.nan(.), NA,.)) %>%
  filter(!is.na(type)) %>%
  group_by(type,poverty_group) %>%
  summarise_at(c("near_transit_7","near_formaltransit_7","near_transit_15",
                 "near_formaltransit_15","near_transit_21","near_formaltransit_21","near_walking_0",
                "cum30_transit_7","cum60_transit_7","cum30_formaltransit_7","cum60_formaltransit_7",
                "cum30_transit_15","cum60_transit_15","cum30_formaltransit_15","cum60_formaltransit_15",
                "cum30_transit_21","cum60_transit_21","cum30_formaltransit_21","cum60_formaltransit_21",
                "cum30_walking_0","cum60_walking_0"),
               ~(p = weighted.mean(.,pop_count,na.rm=T))) %>%
  gather(key = "method",value = "access",-c(type,poverty_group)) %>%
  mutate(mode = sub("^[^_]*_([^_]*)_.*$", "\\1", method)) %>%
  mutate(time = sub(".*_(.*)$", "\\1", method)) %>%
  mutate(type = case_when(type == "Private" ~ "Private TVETs",
                          type == "Public" ~ "Public TVETs",
                          type == "all" ~ "All TVETs",
                          type == "CFP" ~ "Short-term TVETs",
                          type == "IEP" ~ "Long-term TVETs",
                          type == "STEM" ~ "STEM TVETs",
                          type == "non-STEM" ~ "non-STEM TVETs",
                          type == "HDD" ~ "HDD TVETs",
                          type == "MozSkills" ~ "MozSkills TVETs")) %>%
  mutate(type = factor(type,levels = c("Public TVETs","Private TVETs","Short-term TVETs","Long-term TVETs","All TVETs","STEM TVETs","non-STEM TVETs","HDD TVETs","MozSkills TVETs"))) %>%
  mutate(mode = case_when(mode == "transit" ~ "Transit",
                          mode == "formaltransit" ~ "Only formal transit",
                          mode == "walking" ~ "Walking"))


# Plot 
aux %>%
  filter(time %in% c(7,0)) %>%
  filter(type != "All TVETs") %>%
  filter(grepl("near",method)) %>%
  ggplot(aes(x = poverty_group, y = access, color = mode, group = mode)) +
  geom_line(size = 1) +
  geom_text(aes(label = round(access)), vjust = -1, show.legend = FALSE,size=3) +
  facet_wrap(~type,ncol = 2) +
  scale_color_discrete(name = "Transport mode") +
  scale_x_continuous(breaks = c(1:10)) +
  ylim(15,150) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.key.width = unit(1.5, 'cm')) +
  xlab('Poverty group') + ylab("Weighted average time to the nearest TVET")
ggsave(paste0(path,"/Outputs/graph_average_time2.png"),width = 8,height = 10,dpi=500)

#% of people without access within 60 min
aux <- grid %>%
  st_drop_geometry() %>%
  left_join(access,by=c("fid"="from_id")) %>%
  filter(!is.na(type)) %>%
  mutate_all(~ifelse(is.nan(.), NA,.)) %>%
  select("type","poverty_group","pop_count","cum60_transit_7","cum60_formaltransit_7","cum60_walking_0") %>%
  mutate_at(c("cum60_transit_7","cum60_formaltransit_7","cum60_walking_0"),~(p = ifelse(.>=1,0,pop_count))) %>%
  group_by(type,poverty_group) %>%
  summarise_at(c("cum60_transit_7","cum60_formaltransit_7","cum60_walking_0","pop_count"),~(p = sum(.))) %>%
  mutate_at(c("cum60_transit_7","cum60_formaltransit_7","cum60_walking_0"),~(p = ./pop_count)) %>%
  mutate(type = case_when(type == "Private" ~ "Private TVETs",
                          type == "Public" ~ "Public TVETs",
                          type == "all" ~ "All TVETs",
                          type == "CFP" ~ "Short-term TVETs",
                          type == "IEP" ~ "Long-term TVETs",
                          type == "STEM" ~ "STEM TVETs",
                          type == "non-STEM" ~ "non-STEM TVETs",
                          type == "HDD" ~ "HDD TVETs",
                          type == "MozSkills" ~ "MozSkills TVETs")) %>%
  gather(key = "method",value="pct_group",-c(type,poverty_group,pop_count)) %>%
  mutate(type = factor(type,levels = c("Public TVETs","Private TVETs","Short-term TVETs","Long-term TVETs","All TVETs","STEM TVETs","non-STEM TVETs","HDD TVETs","MozSkills TVETs"))) %>%
  mutate(mode = sub("^[^_]*_([^_]*)_.*$", "\\1", method)) %>%
  mutate(mode = case_when(mode == "transit" ~ "Transit",
                          mode == "formaltransit" ~ "Only formal transit",
                          mode == "walking" ~ "Walking"))

# Plot
aux %>%
  filter(type != "All TVETs") %>%
  ggplot(aes(x = poverty_group, y = pct_group, fill = mode, group = mode)) +
  geom_col(size = 1,position = "dodge") +
  geom_text(aes(label = round(100*pct_group),color=mode), vjust = -1, show.legend = FALSE,size=3,
            position = position_dodge(width = .9)) +
  facet_wrap(~type,ncol = 2) +
  scale_fill_discrete(name = "Transport mode") +
  scale_x_continuous(breaks = c(1:10)) +
  ylim(0, 1.1) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.key.width = unit(1.5, 'cm')) +
  xlab('Poverty group') + ylab("% of people without access to any TVET within 60 min.")
ggsave(paste0(path,"/Outputs/graph_pct_without_access2.png"),width = 8,height = 10,dpi=500)


# Poverty maps (selected neighborhoods)
neighborhood <- grid %>%
  st_join(bairros %>% select(Distrito,Posto,Localidade,Bairro,LocalBairr),join=st_intersects)

n <- tibble(Distrito = c("Nlhamankulu","KaMaxakeni","Nlhamankulu","KaMaxakeni","KaMavota","Cidade da Matola","Cidade da Matola","Cidade da Matola","Manhiça"),
            Posto = c("Nao Aplicavel","Nao Aplicavel","Nao Aplicavel","Nao Aplicavel","Nao Aplicavel","Infulene","Infulene","Infulene","Manhica-Sede"),
            Bairro = c("Xipamanine","Maxaquene D","Chamanculo D","Polana Canico B","Ferroviario","Zona Verde","Ndlavela","Intaca","Nao Aplicavel"),
            LocalBairr = c(10,5,6,7,3,15,7,8,2))

for(i in 1:row_number(n)){
  print(i)
  
  neigh <- neighborhood %>%
    filter(Distrito == unlist(n[i,1]),
           Posto == unlist(n[i,2]),
           Bairro == unlist(n[i,3]),
           LocalBairr == unlist(n[i,4]))
  box_n <- st_bbox(neigh %>% st_transform(2736) %>% st_buffer(500) %>% st_transform(4326))
  
  a <- bairros %>% filter(Distrito == unlist(n[i,1]),
                         Posto == unlist(n[i,2]),
                         Bairro == unlist(n[i,3]),
                         LocalBairr == unlist(n[i,4]))
    
ggplot() +
  base_map(box_n, increase_zoom = 6, basemap = 'mapnik',nolabels=F) +
  #geom_sf(data = grid,color="transparent",fill="lightgrey",alpha = 0.3) +
  geom_sf(data = grid, aes(fill=factor(as.character(poverty_group),levels = c("1","2","3","4","5","6","7","8","9","10"))),color="transparent",alpha = 0.6,show.legend = T) +
  geom_sf(data = bairros,aes(),fill="transparent",linetype="dashed",linewidth=0.3) +
  geom_sf(data=a,fill="transparent",color="black",linewidth=0.5) +
  geom_sf(data=municipios,fill="transparent",color="black",linewidth=0.4) +
  scale_fill_manual(values = c("#9b2226","#ae2012","#bb3e03","#ca6702","#ee9b00","#e9d8a6","#94d2bd","#0a9396","#005f73","#001219"),
                    name="Poverty group",
                    breaks = c("1","2","3","4","5","6","7","8","9","10"),
                    drop = F) +
  guides(show.legend = TRUE) +
  coord_sf(datum = NA,
           xlim = c(box_n['xmin'], box_n['xmax']),
           ylim = c(box_n['ymin'], box_n['ymax'])) +
  theme_minimal() +
theme(legend.position="bottom",
     legend.key.width = unit(1.5, 'cm'))
ggsave(paste0(path,"/Outputs/poverty_neighborhoods_",n[i,3],"_mapnik2.png"),width = 6,height = 7,dpi=600)

}

#### Export data #####################################################################

# Aggregated accessibility data by bairro
aux <- access %>%
  select(from_id,type,near_transit_7) %>%
  spread(key = "type",value = "near_transit_7")

aux2 <- grid %>% left_join(aux,by=c("fid"="from_id"))
aux2 %>% st_write("accessibility_grid_near_transit_7.shp",delete_layer = T)


aux2 <- grid %>%
  left_join(aux,by=c("fid"="from_id")) %>%
  st_join(bairros,join=st_intersects) %>%
  st_drop_geometry() %>%
  group_by(area_id,Provincia,Distrito,Posto,Localidade,Bairro,LocalBairr) %>%
  summarise_at(c("all","CFP","HDD","IEP","non-STEM","MozSkills","Private","Public","STEM","consumption"),~(p = weighted.mean(.,pop_count,na.rm=T)))
aux <- aux2 %>%
  left_join(grid %>%
              left_join(aux,by=c("fid"="from_id")) %>%
              st_join(bairros,join=st_intersects) %>%
              st_drop_geometry() %>%
              group_by(area_id,Provincia,Distrito,Posto,Localidade,Bairro,LocalBairr) %>%
              summarise(pop_count=sum(pop_count,na.rm=T)),by=c("area_id","Provincia","Distrito","Posto","Localidade","Bairro","LocalBairr"))
aux <- aux %>%
  filter(!is.na(all)) %>%
  ungroup() %>%
  arrange(consumption) %>%
  mutate(poverty = pop_count/sum(pop_count)) %>%
  mutate(poverty = cumsum(poverty)) %>%
  mutate(poverty_group = 1 + as.numeric(floor(poverty*10))) %>%
  mutate(poverty_group = ifelse(poverty == 1,10,poverty_group)) %>%
  mutate(poverty_group = case_when(poverty_group <= 4 ~ "High - bottom 40%",
                                   poverty_group >= 9 ~ "Low - top 20%",
                                   TRUE ~ "Middle - 40-80%")) %>%
  arrange(desc(all)) %>%
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
                           poverty_group == "Low - top 20%" & accessibility_group == "High - top 20%" ~ "Low-poverty | High-accessibility"))
aux <- bairros %>%
    select(area_id,Provincia,Distrito,Posto,Localidade,Bairro,LocalBairr) %>%
  left_join(aux,by=c("area_id","Provincia","Distrito","Posto","Localidade","Bairro","LocalBairr")) %>%
  filter(!is.na(group)) %>%
  mutate(group = factor(group,levels = c("Low-poverty | Low-accessibility","Mid-poverty | Low-accessibility","High-poverty | Low-accessibility",
                                  "Low-poverty | Mid-accessibility","Mid-poverty | Mid-accessibility","High-poverty | Mid-accessibility",
                                  "Low-poverty | High-accessibility","Mid-poverty | High-accessibility","High-poverty | High-accessibility")))
  
  
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
ggsave(plot = m,paste0(path,"/Outputs/bairros_groups2.png"),width = 10,height = 14,dpi=600)

aux %>% st_drop_geometry() %>% write_csv("Outputs/poverty_accessibility_bairro2.csv")

#### LISA Maps #####################################################################

bairros_centroids <- bairros %>%
  st_centroid()
bairros_centroids <- do.call(rbind, st_geometry(bairros_centroids)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))
bairros_centroids$Bairro <- bairros$Bairro 

data <- grid %>%
  left_join(access %>%
  filter(type == "all"),by=c("fid"="from_id")) %>%
  mutate(poor40 = ifelse(poverty_group <= 4,pop_count,0)) %>%
  select(fid,poor40,near_transit_7) %>%
  na.omit()

queen_w <- queen_weights(data)
summary(queen_w)

lisa <- local_bimoran(queen_w,data[c('poor40','near_transit_7')])
lms <- lisa_values(gda_lisa = lisa)
pvals <- lisa_pvalues(lisa)
cats <- lisa_clusters(lisa, cutoff = 0.05)
lbls <- lisa_labels(lisa)

lisa <- data %>%
  mutate(cats = cats) %>%
  mutate(labels = case_when(cats == 0 ~ "Not significant",
                            cats == 1 ~ "High-High",
                            cats == 2 ~ "Low-Low",
                            cats == 3 ~ "Low-High",
                            cats == 4 ~ "High-Low",
                            cats == 5 ~ "Undefined",
                            cats == 6 ~ "Isolated")) %>%
  filter(!labels %in% c("Isolated","Not significant"))

ggplot() +
  base_map(bbox, increase_zoom = 7, basemap = 'positron',nolabels=T) +
  geom_sf(data=lisa, aes(fill=labels), color="NA",alpha=0.5) +
  geom_sf(data = bairros,aes(),fill="transparent",linetype="dashed") +
  geom_sf(data=st_read(paste0(path,"/GTFS/transit_routes.shp")), aes(),color="grey",linewidth = 0.2) +
  geom_sf(data=municipios,fill="transparent",color="black",linewidth=0.4) +
  #geom_text(data=bairros_centroids,aes(x=lon,y=lat,label=Bairro),size=1) +
  scale_fill_manual(values = c("red", "pink", "lightblue", "darkblue")) + 
  guides(fill = guide_legend(title="LISA clusters")) +
 coord_sf(datum = NA,
           xlim = c(box['xmin'], box['xmax']),
           ylim = c(box['ymin'], box['ymax'])) +
  theme_minimal() +
  theme(legend.position="bottom",
        legend.key.width = unit(0.8, 'cm')) +
  #labs(title="Bottom 40% <> Time to the nearest TVET\nby transit") +
  xlab("") + ylab("")
ggsave(paste0(path,"/Outputs/lisa_poor40_all_transit2.png"),width = 5,height = 7,dpi=500)

#### Accessibility by TVET #####################################################################

#Open travel time matrices
ttm <- readRDS(paste0(path,"/ttm_transit_tvets_7.rds")) %>%
  rename(transit_7 = "travel_time_p50") %>%
  left_join(readRDS(paste0(path,"/ttm_transit_tvets_15.rds")) %>%
              rename(transit_15 = "travel_time_p50"),by = c("from_id","to_id")) %>%
  left_join(readRDS(paste0(path,"/ttm_transit_tvets_21.rds")) %>%
              rename(transit_21 = "travel_time_p50"),by = c("from_id","to_id")) %>%
  
  left_join(readRDS(paste0(path,"/ttm_formal_transit_tvets_7.rds")) %>%
              rename(formaltransit_7 = "travel_time_p50"),by = c("from_id","to_id")) %>%
  left_join(readRDS(paste0(path,"/ttm_formal_transit_tvets_15.rds")) %>%
              rename(formaltransit_15 = "travel_time_p50"),by = c("from_id","to_id")) %>%
  left_join(readRDS(paste0(path,"/ttm_formal_transit_tvets_21.rds")) %>%
              rename(formaltransit_21 = "travel_time_p50"),by = c("from_id","to_id")) %>%
  
  left_join(readRDS(paste0(path,"/ttm_walk_tvets.rds")) %>%
              rename(walking = "travel_time_p50"),by = c("from_id","to_id")) %>%
  
  mutate_at(c("from_id","to_id"),as.numeric)


#Calculating accessibility to public and private TVETs
aux <-  ttm %>%
  left_join(grid %>% st_drop_geometry(),by=c("from_id"="fid")) %>%
    mutate(cum30_transit_7 = ifelse(transit_7 <= 30,pop_count,0),
           cum60_transit_7 = ifelse(transit_7 <= 60,pop_count,0),
           cum30_formaltransit_7 = ifelse(formaltransit_7 <= 30,pop_count,0),
           cum60_formaltransit_7 = ifelse(formaltransit_7 <= 60,pop_count,0),
           
           cum30_transit_15 = ifelse(transit_15 <= 30,pop_count,0),
           cum60_transit_15 = ifelse(transit_15 <= 60,pop_count,0),
           cum30_formaltransit_15 = ifelse(formaltransit_15 <= 30,pop_count,0),
           cum60_formaltransit_15 = ifelse(formaltransit_15 <= 60,pop_count,0),
           
           cum30_transit_21 = ifelse(transit_21 <= 30,pop_count,0),
           cum60_transit_21 = ifelse(transit_21 <= 60,pop_count,0),
           cum30_formaltransit_21 = ifelse(formaltransit_21 <= 30,pop_count,0),
           cum60_formaltransit_21 = ifelse(formaltransit_21 <= 60,pop_count,0),
           
           cum30_walking = ifelse(walking <= 30,pop_count,0),
           cum60_walking = ifelse(walking <= 60,pop_count,0)) %>%
    
    group_by(to_id) %>%
    
    summarise(cum30_transit_7 = sum(cum30_transit_7,na.rm=T),
              cum60_transit_7 = sum(cum60_transit_7,na.rm=T),
              cum30_formaltransit_7 = sum(cum30_formaltransit_7,na.rm=T),
              cum60_formaltransit_7 = sum(cum60_formaltransit_7,na.rm=T),
              
              cum30_transit_15 = sum(cum30_transit_15,na.rm=T),
              cum60_transit_15 = sum(cum60_transit_15,na.rm=T),
              cum30_formaltransit_15 = sum(cum30_formaltransit_15,na.rm=T),
              cum60_formaltransit_15 = sum(cum60_formaltransit_15,na.rm=T),
              
              cum30_transit_21 = sum(cum30_transit_21,na.rm=T),
              cum60_transit_21 = sum(cum60_transit_21,na.rm=T),
              cum30_formaltransit_21 = sum(cum30_formaltransit_21,na.rm=T),
              cum60_formaltransit_21 = sum(cum60_formaltransit_21,na.rm=T),
              
              cum30_walking_0 = sum(cum60_walking,na.rm=T),
              cum60_walking_0 = sum(cum60_walking,na.rm=T))

aux <- tvets_shp %>%
  st_drop_geometry() %>%
  select("N","Province","Accredited Private Institutes","type1","type2","type3","type4","type5") %>%
  left_join(aux,by=c("N"="to_id")) %>%
  gather(key="method",value = "access",
         -c("N","Province","Accredited Private Institutes","type1","type2","type3","type4","type5"))
aux <- tvets_shp %>% select(N) %>%
  left_join(aux,by="N") %>%
  rename(Name = "Accredited Private Institutes")

aux <- aux %>%
  st_join(bairros %>% select(Provincia,Distrito,Posto,Localidade,Bairro,LocalBairr),join=st_intersects) %>%
  mutate(Bairro = ifelse(is.na(Bairro),Localidade,Bairro)) %>%
  select(-Localidade)

tv <- t %>%
  filter(grepl("cum60_transit_7",method)) %>%
  filter(type %in% c("MozSkills","HDD"))

for(i in 1:nrow(tv)){
  print(i)
  # filter database
  ty <- tv[i,] %>% select(type) %>% unlist()
  me <- tv[i,] %>% select(method) %>% unlist()
  mo <- sub("^[^_]*_([^_]*)_.*$", "\\1", me)
  mo <- ifelse(mo == "formaltransit","formal transit",mo)
  tm <- sub(".*_(.*)$", "\\1", me)
  tt <- case_when(ty == "CFP" ~ "short-term",
                  ty == "IEP" ~ "long-term",
                  TRUE ~ ty)
  label <- ifelse(mo == "walking",paste0("People within 60 min to","\n",tt," TVETs by ",mo),
                  paste0("People within 60 min to","\n",tt," TVETs by ",mo," at ",tm,"h"))
  
  
  map <- aux %>% filter(method == me)
  
  # Get TVETs
  if(ty == "all"){
    map <- aux %>%
      filter(method == me)
  } else{
    map <- aux  %>%
      filter(method == me) %>%
      filter(type1 == ty | type2 == ty | type3 == ty | type4 == ty | type5 == ty)
  }
  
  breaks <- c(0,100,200,300,400,500,2000)
  
  map <- map %>%
    mutate(access = ifelse(is.na(access),0,access)) %>%
    mutate(var = factor(cut(access/1000,breaks,labels = breaks[2:7]),levels = breaks[2:7])) 
  
  # Get transit routes
  if(mo == "transit"){
    transit_map <- st_read(paste0(path,"/GTFS/transit_routes.shp"))
  }
  if(mo == "formal transit"){
    transit_map <- st_read(paste0(path,"/GTFS/formal_transit_routes.shp"))
  }
  if(mo == "walking"){
    transit_map <- data.frame(lon = 32.588711,lat = -25.953724)
    transit_map <- st_as_sf(transit_map, coords = c("lon", "lat"), crs = 4326)
  }
  
  # Map
  m <- ggplot() +
    base_map(bbox, increase_zoom = 7, basemap = 'positron',nolabels=T) +
    geom_sf(data = bairros,aes(),fill="transparent",linetype="dashed",linewidth=0.1) +
    geom_sf(data=municipios,fill="transparent",color="black",linewidth=0.4) +
    geom_sf(data=transit_map, aes(),color="grey",linewidth=0.2) +
    geom_sf(data=map, aes(color=var,size=var),alpha = 0.7) +
    geom_text_repel(aes(x = st_coordinates(map)[,1],y = st_coordinates(map)[,2],label = map$Name),color="black",size = 1) +
    scale_color_manual(breaks=breaks[2:7],
                       values = plasma(6),
                       na.value = "black",
                       name=label,
                       drop = FALSE,
                       guide = "legend") +
    scale_size_manual(values = seq(0.5,3,0.5),
                      breaks=breaks[2:7],
                      drop = FALSE,
                      name=label,
                      guide = "legend") +
    guides(color=guide_legend(), size = guide_legend()) +
    coord_sf(datum = NA,
             xlim = c(box['xmin'], box['xmax']),
             ylim = c(box['ymin'], box['ymax'])) +
    theme_minimal() +
    theme(legend.position="bottom",
          legend.key.width = unit(1, 'cm')) +
    xlab("") + ylab("")
ggsave(plot = m,paste0(path,"/Outputs/tvets_access_",ty,"_",me,".png"),width = 5,height = 7,dpi=600)
}

aux2 <- aux %>% st_drop_geometry() %>% filter(method %in% c("cum60_transit_7","cum60_formaltransit_7","cum60_walking_0")) %>%
  spread(key="method",value = "access") %>%
  filter(!is.na(cum60_transit_7)) %>%
  arrange(desc(cum60_transit_7)) %>%
  mutate(accessibility = 1/nrow(.)) %>%
  mutate(accessibility = cumsum(accessibility)) %>%
  mutate(accessibility_group = 1 + as.numeric(floor(accessibility*10))) %>%
  mutate(accessibility_group = ifelse(accessibility == 1,10,accessibility_group)) %>%
  mutate(accessibility_group = case_when(accessibility_group <= 4 ~ "Low - bottom 40%",
                                         accessibility_group >= 9 ~ "High - top 20%",
                                         TRUE ~ "Middle - 40-80%"))



aux2 %>% write_csv("Outputs/accessibility_TVETs.csv")
