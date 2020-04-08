### Map of All Tsunamis

library(leaflet)

# cntl + shift + c for comment chunks
# tsunami <- read.csv("Tsunami.csv")
# TMap <- leaflet(tsunami) %>% 
#   leaflet(data) %>% 
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addCircleMarkers(
#     ~LONGITUDE, ~LATITUDE, label = data$EQ_MAGNITUDE, #the placement
#     clusterOptions = markerClusterOptions(), #the cluster 
#     radius = (6), color = "blue", stroke = FALSE, fillOpacity = 0.5)

map <- function(){
  leaflet(data) %>% 
    #basegroups:
    addTiles(group = "OSM(default)") %>%
    addProviderTiles(providers$CartoDB.Positron, group = "Grayscale") %>%
    addProviderTiles(providers$Stamen.TonerLite, group = "B&W") %>%
    
    # Overlay groups
    addCircles( ~LONGITUDE, ~LATITUDE, radius = data$EQ_MAGNITUDE, fillOpacity = ".8", stroke = F, label = data$EQ_MAGNITUDE, group = "Individual") %>%
    addCircleMarkers(
      ~LONGITUDE, ~LATITUDE, label = data$YEAR, #the placement
      clusterOptions = markerClusterOptions(), #the cluster 
      radius = (6), color = "blue", stroke = FALSE, fillOpacity = 0.5, group = "Grouped") %>% #marker style
    
    # Layers control
    addLayersControl(
      baseGroups = c("OSM (default)", "Grayscale", "B&W"),
      overlayGroups = c("Individual", "Grouped"),
      options = layersControlOptions(collapsed = FALSE)
    )
  
}
