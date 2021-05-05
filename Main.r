# LA = Local Authority. These are sub regions in the UK
# MSOA = Middle Layer Super Output Area. These are sub regions inside LAs

# Set up ----
# Load packages
library(dplyr)
library(readr) # Loading the data
library(tidyr)
library(sf) # For the maps
library(ggplot2)
library(viridis)
library(gganimate) # For the animated map

# Network packages  
library(igraph) # build network
library(spdep) # builds network
library(tidygraph)
library(ggraph) # for plotting networks
library(transformr)
library(tweenr)

# Load mso map data. Downloaded from, and then extract all files into data/maps:
# https://opendata.arcgis.com/datasets/
mosa_map_object <- sf::st_read("data/maps/Middle_Layer_Super_Output_Areas_(December_2011)_Boundaries_Full_Extent_(BFE)_EW_V3.shp")

# Load msoa_covidcases_object case data
msoa_covidcases_object <- readr::read_csv("data/msoa_cases_raw_as_backup.csv", 
                                          col_types = cols()) %>% 
  # Drop columns that are all NA
  select(where(~!all(is.na(.)))) %>% 
  # Pivot longer
  pivot_longer(dplyr::starts_with("wk_"), names_to = "week", values_to = "cases", names_prefix = "wk_") %>% 
  # Turn week to numeric
  mutate(week = as.integer(week)) %>% 
  # Turn NAs into 0s
  mutate(across(c(latest_7_days, cases), .fns = ~ifelse(is.na(.), 0, .)))

msoa_covidcases_object <- replace(msoa_covidcases_object, msoa_covidcases_object<0,0)

#
la <- msoa_covidcases_object %>% 
  group_by(lad19_nm, week) %>% 
  summarise(cases = sum(cases))

la %>% 
  arrange(desc(cases)) %>% 
  head(100) %>% 
  View

#
la %>% 
  filter(lad19_nm == "Leicester") %>% 
  ggplot(aes(x = week, y = cases)) +
  geom_line(size = 1) +
  theme_minimal() +
  ggtitle("Time series gaph of cases for Leicester")

ggsave(file = "outputs/Leicester cases.png")


# 
msoa_covidcases_object %>% 
  filter(lad19_nm == "Leicester") %>% 
  group_by(msoa11_hclnm) %>% 
  summarise(max_cases = max(cases)) %>% 
  arrange(desc(max_cases)) %>% 
  slice(1:10) %>% 
  inner_join(msoa_covidcases_object) %>% 
  mutate(msoa11_hclnm = factor(msoa11_hclnm) %>% reorder(desc(max_cases))) %>% 
  ggplot(aes(x = week, y = cases, group = msoa11_hclnm, color = msoa11_hclnm)) +
  geom_line(size = 1) +
  scale_color_viridis(discrete = TRUE) +
  theme_minimal() +
  ggtitle("Time series gaph of cases for MSOAs within Leicester") +
  guides(color=guide_legend(title="MSOA"))

ggsave("outputs/Leciester MSOA cases.png")


# Maps -----
# 
msoa_data_wk25 <- msoa_covidcases_object %>% 
  filter(lad19_nm == "Leicester", 
         week ==25) 

# We see that covid cases are clustered in several MSOAs
mosa_map_object %>% 
  inner_join(msoa_data_wk25 %>% distinct(msoa11_cd, cases), by = c("MSOA11CD" = "msoa11_cd")) %>% 
  ggplot()+
  geom_sf(aes(geometry = geometry, fill = cases)) +
  scale_fill_viridis_c() + 
  theme_void() +
  ggtitle(label = "MSOA map for Leicester",
          subtitle = "The colour shows Covid cases during week 25")
ggsave(file = "outputs/maps/leicester_week_25.png")

# 
p <- mosa_map_object %>% 
  inner_join(msoa_covidcases_object %>% filter(lad19_nm == "Leicester"),
             by = c("MSOA11CD" = "msoa11_cd")) %>% 
  ggplot(aes(group = week)) +
  geom_sf(aes(fill = cases)) +
  scale_fill_viridis_c() +
  transition_time(week) +
  labs(title = paste0("New covid cases for MSOAs within Leicester"),
       subtitle = "Week = {frame_time}") +
  theme_void() 


num_weeks <- n_distinct(msoa_covidcases_object$week)

# animate(p, nframes = num_weeks, fps = 1, end_pause = 4)
# 
# # Save  
# anim_save(file = "animated_map_leicester.gif")

#  
##(It takes very long for  all of England, so we will start with just Leicester)
leicester_sf <- mosa_map_object %>% 
  inner_join(msoa_covidcases_object %>% filter(lad19_nm == "Leicester", week == 27) , 
             by = c("MSOA11CD" = "msoa11_cd"))

# Using the poly2nb to get the neighbourhood of each area
leicester_msoa_neighbours <- spdep::poly2nb(leicester_sf)

# Using nb2mat to turn this into an adjacency matrix
adj_mat <- spdep::nb2mat(leicester_msoa_neighbours, style = "B")
rownames(adj_mat) <- leicester_sf$msoa11_hclnm
colnames(adj_mat) <- leicester_sf$msoa11_hclnm


# Using as_tbl_graph to turn this into a tidygraph
leicester_network <- igraph::graph_from_adjacency_matrix(adj_mat, mode = "undirected") %>% 
  tidygraph::as_tbl_graph()


leicester_network %>% 
  activate("nodes") %>% 
  as_tibble()

# We can also plot this using ggraph, but this doesn't match up to the actual geographic locations
ggraph(leicester_network)  +
  geom_edge_link(colour = 'black', width = 2) + 
  geom_node_point(size = 5, colour = 'steelblue') +
  theme_void() +
  ggtitle("Network plot of MSOAs in Leicester", 
          subtitle = "Each node is an MSOA, and an edge joins two nodes if they are neighbours")

ggsave(file = "outputs/maps/Leicester MSOA network simple.png")

# Converting LONG LAT coordinates into Uk grid 
# (This is only necessary if you want to overlay the network on the map)
coords <- leicester_sf %>% 
  as.data.frame() %>% 
  select( LONG_, LAT) %>% 
  sp::SpatialPoints(proj4string=CRS("+init=epsg:4326")) %>% # LAT LONG code
  sp::spTransform(CRS("+init=epsg:27700")) %>%  # UK grid code
  as.data.frame() %>% 
  bind_cols(msoa11_hclnm = leicester_sf$msoa11_hclnm) # to allow us to bind on 

ggsave(file = "outputs/maps/Leicester MSOA network overlaying map.png")

