

map_gen <- function(df,metric,USA,...) {
  # Function to generate map plot for given metric in df 
  # This is laid on top of USA map
  # Inputs:
  # df      : dataframe with metrics, lat, lon, WORKSITE columns
  # metric  : metric for data comparison 
  # USA     : dataframe for US maps with lat, long columns. map_data(map = "usa") from ggplot2
  # Output  : ggplot object
  
  
  # Creating Map Dataframe
  df %>%
    mutate(certified =ifelse(CASE_STATUS == "CERTIFIED",1,0)) %>%
    group_by(WORKSITE,lat,lon) %>%
    summarise(TotalApps = n(),CertiApps = sum(certified), Wage = median(PREVAILING_WAGE)) -> map_df
   
  # # Lat-Long Limits
  # df %>%
  #   summarise(lat_min = min(lat,na.rm=TRUE),
  #             lat_max = max(lat,na.rm=TRUE),
  #             long_min = min(lon,na.rm=TRUE),
  #             long_max = max(lon,na.rm=TRUE)) -> geo_coord

  # Finding top Locations for metric
  top_locations <- unlist(find_top(df,"WORKSITE",metric, ...))
  
  # First layer    : USA Map
  # Second layer   : geom_point() with point alpha and size varying with metric
  # Third layer    : points mapping to top locations using ggrepel package
  g <- ggplot(USA, aes(x=long, y=lat)) + 
    geom_polygon() + xlab("Longitude (deg)") + ylab("Latitude(deg)") + 
    geom_point(data=map_df, aes_string(x="lon", y="lat", label = "WORKSITE", alpha = metric, size = metric), color="yellow") + 
    geom_label_repel(data=map_df %>% filter(WORKSITE %in% top_locations),aes_string(x="lon", y="lat",label = "WORKSITE"),
                     fontface = 'bold', color = 'black',
                     box.padding = unit(0.35, "lines"),
                     point.padding = unit(1.0, "lines"),
                     segment.color = 'grey50',
                     force = 3) +
    # Zoom into the specific location input
    #coord_map(ylim = c(max(geo_coord$lat_min - 5,23), min(geo_coord$lat_max - 5,50)),xlim=c(max(geo_coord$long_min - 5,-130),min(geo_coord$long_max + 5,-65))) +
    # Using the whole USA map
    coord_map(ylim = c(23,50),xlim=c(-130,-65)) +
    get_theme()
  
  return(g)
}

