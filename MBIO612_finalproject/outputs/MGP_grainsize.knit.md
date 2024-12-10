---
title: "MBIO 612 Final Project - Grain Size Distribution - Wet vs. Dry Seasons"
author: "Kyle Bosworth"
date: "2024-12-10"
output:
  pdf_document:
    toc: true
  html_document:
    toc: true
    toc_float: true
always_allow_html: true
---




### Load Libraries

``` r
library(here)
library(tidyverse)
library(leaflet)
library(tidyr)
```

### Read in Data

``` r
#Had to manually load grainsize data. For some reason I could not get R to read in my csv file.


grainsize <- read.csv(here("MBIO612_finalproject","data", "grainsize.csv"))

head(grainsize)
```

```
##      Date SiteName Alt.site.name         Zone      Lat         Long TotalWeight
## 1 10/1/22    MS_B9          P_01 Mullet South 21.43225 _157.8066923      36.708
## 2 10/1/22   MS_B10          P_02 Mullet South 21.43336 _157.8062108      36.539
## 3 10/1/22   MS_B11          P_03 Mullet South 21.43438   _157.80534      59.917
## 4 10/1/22   ME_B13          P_04  Mullet East 21.43591   _157.80556      44.272
## 5 10/1/22   ME_B14          P_05  Mullet East 21.43704   _157.80606      58.144
## 6 10/1/22     <NA>          P_06         <NA> 21.43913   _157.80856      36.614
##   Total. Gravel CoarseSand MediumSand FineSand SiltClay TempC   DO.   DO    SpC
## 1    100 12.271     11.226     13.972   18.741   43.791  25.3  28.3 1.95 47.985
## 2    100 32.646     21.761      9.925   24.244   11.424  26.2  34.9 2.37 48.185
## 3    100 29.637     22.837      9.878   18.066   19.582  27.4  59.5 3.89 52.181
## 4    100 39.237     40.258      9.724    6.388    4.392  27.6  80.8 5.25 52.832
## 5    100 19.361     29.697      9.900   18.589   22.454  27.8 100.9 6.52 53.004
## 6    100 13.160     15.923      5.271   15.953   49.693  27.4  67.1  4.4 50.854
##   Salinity   pH Turbidity
## 1    31.26 7.83      0.71
## 2    31.38 7.92      0.75
## 3    34.28 7.99      0.99
## 4    34.76 8.20      0.88
## 5    34.88 8.25      0.80
## 6    33.30 7.90      2.44
```

``` r
#Had a lot of trouble with my naming schemes in my data frame, here i just wanted to double check my names
str(grainsize$SiteName)  # Should show character vector with names as MW_B5 not MW-B5
```

```
##  chr [1:79] "MS_B9" "MS_B10" "MS_B11" "ME_B13" "ME_B14" NA "MN_B1" "MW_B5" ...
```

``` r
#Checked a few example site names too
head(unique(grainsize$SiteName))
```

```
## [1] "MS_B9"  "MS_B10" "MS_B11" "ME_B13" "ME_B14" NA
```

### The Goal
I just wanted to focus on 2 sampling dates that occured in the dry and wet season, 9/14/23 and 12/20/23, my goald is to create map that will display grain size % and WQ data in an interactive form. Iʻd like to me able to make a story map of sorts and each site will have a pop up window that displays info for that day. 


### Data Prep

``` r
#I want to first clean and prepare my data to remove NAs, and focus on spcific dates

grainsize_filter <- grainsize %>%
  mutate(
    Long = as.numeric(gsub("_", "-", as.character(Long))),
    Lat = as.numeric(as.character(Lat)) #this converts my long/lat values to numeric to makes sure that they are read in coordinate format. I replaced my "hypnons "-" with underscores and didnʻt want to go back and edi out each cell.
  ) %>%
  filter(!is.na(Long), !is.na(Lat)) %>%
  filter(Date %in% c("9/14/23", "12/20/23")) %>% # Remove rows where coordinates are NA and selecting for dates. 
  mutate(
    Gravel_pct = (Gravel/TotalWeight) * 100,
    CoarseSand_pct = (CoarseSand/TotalWeight) * 100,
    MediumSand_pct = (MediumSand/TotalWeight) * 100,
    FineSand_pct = (FineSand/TotalWeight) * 100,
    SiltClay_pct = (SiltClay/TotalWeight) * 100
  )
#here i calculated my grain size % based on total weight so that in my map i can view this as a percentage.


#I then thought that it might be easier to view all this dat if I made 2 different map. So i created 2 distinct seasonal data sets.

dry_season_data <- grainsize_filter %>% 
  filter(Date == "9/14/23")

wet_season_data <- grainsize_filter %>% 
  filter(Date == "12/20/23")
```


### Creating a pop-up

``` r
#I would like to depict the sampling points on an interactive map that has a window that pops up when you click on a specific site. 

#I did some researching and found that I could make a pop-up using "popup_content" and structure the text using using html tags!
#the basics are as followed 


#"<strong>" wraps the lables 
#"<br/>" is a break and is used to create the next line
#round was used to round my percents, coordinates and data off to a degree.



#Line 1: "<strong>Environmental Parameters:</strong><br/>",
#Line 2: "<strong>Environmental Parameters:</strong><br/>",


grainsize_filter <- grainsize_filter %>%
  mutate(
    popup_content = paste(
      "<strong>Site:</strong>", SiteName, "<br/>",
      "<strong>Zone:</strong>", Zone, "<br/>",
      "<strong>Coordinates:</strong><br/>",
      "Lat: ", round(as.numeric(Lat), 6), "<br/>",
      "Long: ", round(as.numeric(Long), 6), "<br/>",
      "<strong>Grain Size Distribution:</strong><br/>",
      "Gravel: ", round(as.numeric(Gravel_pct), 1), "%<br/>",
      "Coarse Sand: ", round(as.numeric(CoarseSand_pct), 1), "%<br/>",
      "Medium Sand: ", round(as.numeric(MediumSand_pct), 1), "%<br/>",
      "Fine Sand: ", round(as.numeric(FineSand_pct), 1), "%<br/>",
      "Silt/Clay: ", round(as.numeric(SiltClay_pct), 1), "%<br/>",
      "<strong>Environmental Parameters:</strong><br/>",
      "Temperature: ", round(as.numeric(TempC), 2), "°C<br/>",
      "Salinity: ", round(as.numeric(Salinity), 2), "<br/>",
      "DO.: ", round(as.numeric(`DO.`), 2)
    )
  )
```

### Adding in the map + Heʻeia Fishpond Coordinates

``` r
heeia_lat <- 21.4351  # This is the coordinates i used for my map layer
heeia_lng <- -157.8060

heeiamap <- function(data, season_name) {
  heeiamap_data <- data %>%
    filter(!is.na(Long), !is.na(Lat),
           Long < 0,  #makes sure that my longitude is negative (for Hawaiʻi), chk for me as my long had underscores
           Long > -158, # Set bounds
           Lat > 21,    
           Lat < 22)    
  
  #I wanted to define manual colors for mullet zones
  zone_colors <- c(
    "Mullet East" = "gold",
    "Mullet West" = "darkgreen",
    "Mullet South" = "royalblue",
    "Mullet North" = "red"
  )
  
  #i created a color palette function with the colors
  pal <- colorFactor(palette = zone_colors, domain = heeiamap_data$Zone)
  
  #Using the leaflet package i made my maps! creates a map using my heeiamap_data
  leaflet(heeiamap_data) %>%
    addTiles() %>%
    setView(lng = -157.8060, lat = 21.4351, zoom = 16) %>% #same as bounds and cenyers it on my heʻeia coordinates
    
    #here i added in circle markers to represent my sample sites on the maps
    addCircleMarkers(
    lng = ~Long,      #longitude from data
    lat = ~Lat,       #latitude from data
    popup = ~popup_content,  #brings up popup only when clicked
    color = ~pal(Zone),     #uses my color pal for zones
    radius = 8,            #changes the size of circle markers  on map
    fillOpacity = 0.7,     #70% opacity for circle markers
    stroke = TRUE,         #added an outer border
    weight = 1            #and a thickness to boarder
) %>%
    addLegend(
      position = "bottomright",
      pal = pal,  # Use the same custom palette
      values = ~Zone,
      title = paste(season_name, "Mullet Sampling Sites"),
      opacity = 0.7
    )
}

# Create seasonal datasets
dry_season_data <- grainsize_filter %>% 
  filter(Date == "9/14/23")
wet_season_data <- grainsize_filter %>% 
  filter(Date == "12/20/23")

# Generate and display maps
dry_map <- heeiamap(dry_season_data, "Dry Season")
wet_map <- heeiamap(wet_season_data, "Wet Season")

# Display maps
dry_map
```

```{=html}
<div class="leaflet html-widget html-fill-item" id="htmlwidget-7334b5c2af9b72e9040a" style="width:504px;height:360px;"></div>
<script type="application/json" data-for="htmlwidget-7334b5c2af9b72e9040a">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addTiles","args":["https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"https://openstreetmap.org/copyright/\">OpenStreetMap<\/a>,  <a href=\"https://opendatacommons.org/licenses/odbl/\">ODbL<\/a>"}]},{"method":"addCircleMarkers","args":[[21.43822,21.43785747,21.43725,21.43728,21.4364,21.4346,21.43526813,21.43555556,21.4322495,21.4333585,21.43438,21.43331934,21.43591,21.43704,21.43609408,21.43689497],[-157.81061,-157.8089375,-157.810889,-157.80956,-157.81007,-157.80894,-157.8080256,-157.8097222,-157.8066923,-157.8062108,-157.80534,-157.807793,-157.80556,-157.80606,-157.8066134,-157.8073568],8,null,null,{"interactive":true,"className":"","stroke":true,"color":["#006400","#006400","#006400","#006400","#FF0000","#FF0000","#FF0000","#FF0000","#4169E1","#4169E1","#4169E1","#4169E1","#FFD700","#FFD700","#FFD700","#FFD700"],"weight":1,"opacity":0.5,"fill":true,"fillColor":["#006400","#006400","#006400","#006400","#FF0000","#FF0000","#FF0000","#FF0000","#4169E1","#4169E1","#4169E1","#4169E1","#FFD700","#FFD700","#FFD700","#FFD700"],"fillOpacity":0.7},null,null,["<strong>Site:<\/strong> MN_B1 <br/> <strong>Zone:<\/strong> Mullet North <br/> <strong>Coordinates:<\/strong><br/> Lat:  21.43822 <br/> Long:  -157.81061 <br/> <strong>Grain Size Distribution:<\/strong><br/> Gravel:  93.9 %<br/> Coarse Sand:  58.8 %<br/> Medium Sand:  24.5 %<br/> Fine Sand:  91.8 %<br/> Silt/Clay:  184.9 %<br/> <strong>Environmental Parameters:<\/strong><br/> Temperature:  28.1 °C<br/> Salinity:  7.82 <br/> DO.:  110.2","<strong>Site:<\/strong> MN_B2 <br/> <strong>Zone:<\/strong> Mullet North <br/> <strong>Coordinates:<\/strong><br/> Lat:  21.437857 <br/> Long:  -157.808938 <br/> <strong>Grain Size Distribution:<\/strong><br/> Gravel:  17 %<br/> Coarse Sand:  24.5 %<br/> Medium Sand:  8.9 %<br/> Fine Sand:  153.7 %<br/> Silt/Clay:  265 %<br/> <strong>Environmental Parameters:<\/strong><br/> Temperature:  27.8 °C<br/> Salinity:  8.2 <br/> DO.:  149.2","<strong>Site:<\/strong> MN_B3 <br/> <strong>Zone:<\/strong> Mullet North <br/> <strong>Coordinates:<\/strong><br/> Lat:  21.43725 <br/> Long:  -157.810889 <br/> <strong>Grain Size Distribution:<\/strong><br/> Gravel:  28.5 %<br/> Coarse Sand:  39.8 %<br/> Medium Sand:  16.6 %<br/> Fine Sand:  280.6 %<br/> Silt/Clay:  406.4 %<br/> <strong>Environmental Parameters:<\/strong><br/> Temperature:  28.3 °C<br/> Salinity:  8.02 <br/> DO.:  111.2","<strong>Site:<\/strong> MN_B4 <br/> <strong>Zone:<\/strong> Mullet North <br/> <strong>Coordinates:<\/strong><br/> Lat:  21.43728 <br/> Long:  -157.80956 <br/> <strong>Grain Size Distribution:<\/strong><br/> Gravel:  94.2 %<br/> Coarse Sand:  20.5 %<br/> Medium Sand:  11.8 %<br/> Fine Sand:  72 %<br/> Silt/Clay:  497.5 %<br/> <strong>Environmental Parameters:<\/strong><br/> Temperature:  28.2 °C<br/> Salinity:  8.15 <br/> DO.:  135.2","<strong>Site:<\/strong> MW_B5 <br/> <strong>Zone:<\/strong> Mullet West <br/> <strong>Coordinates:<\/strong><br/> Lat:  21.4364 <br/> Long:  -157.81007 <br/> <strong>Grain Size Distribution:<\/strong><br/> Gravel:  81.1 %<br/> Coarse Sand:  32.8 %<br/> Medium Sand:  23.8 %<br/> Fine Sand:  134.8 %<br/> Silt/Clay:  301.4 %<br/> <strong>Environmental Parameters:<\/strong><br/> Temperature:  28.2 °C<br/> Salinity:  7.93 <br/> DO.:  103.5","<strong>Site:<\/strong> MW_B6 <br/> <strong>Zone:<\/strong> Mullet West <br/> <strong>Coordinates:<\/strong><br/> Lat:  21.4346 <br/> Long:  -157.80894 <br/> <strong>Grain Size Distribution:<\/strong><br/> Gravel:  19.1 %<br/> Coarse Sand:  51.4 %<br/> Medium Sand:  25.4 %<br/> Fine Sand:  117 %<br/> Silt/Clay:  75.4 %<br/> <strong>Environmental Parameters:<\/strong><br/> Temperature:  29 °C<br/> Salinity:  8.22 <br/> DO.:  135","<strong>Site:<\/strong> MW_B7 <br/> <strong>Zone:<\/strong> Mullet West <br/> <strong>Coordinates:<\/strong><br/> Lat:  21.435268 <br/> Long:  -157.808026 <br/> <strong>Grain Size Distribution:<\/strong><br/> Gravel:  88.8 %<br/> Coarse Sand:  47 %<br/> Medium Sand:  16.9 %<br/> Fine Sand:  42.1 %<br/> Silt/Clay:  26.4 %<br/> <strong>Environmental Parameters:<\/strong><br/> Temperature:  28.8 °C<br/> Salinity:  8.29 <br/> DO.:  148.8","<strong>Site:<\/strong> MW_B8 <br/> <strong>Zone:<\/strong> Mullet West <br/> <strong>Coordinates:<\/strong><br/> Lat:  21.435556 <br/> Long:  -157.809722 <br/> <strong>Grain Size Distribution:<\/strong><br/> Gravel:  145.7 %<br/> Coarse Sand:  47.8 %<br/> Medium Sand:  17.9 %<br/> Fine Sand:  65.2 %<br/> Silt/Clay:  76.2 %<br/> <strong>Environmental Parameters:<\/strong><br/> Temperature:  28.5 °C<br/> Salinity:  7.97 <br/> DO.:  113.2","<strong>Site:<\/strong> MS_B9 <br/> <strong>Zone:<\/strong> Mullet South <br/> <strong>Coordinates:<\/strong><br/> Lat:  21.43225 <br/> Long:  -157.806692 <br/> <strong>Grain Size Distribution:<\/strong><br/> Gravel:  92.6 %<br/> Coarse Sand:  63.2 %<br/> Medium Sand:  9.1 %<br/> Fine Sand:  22.2 %<br/> Silt/Clay:  26 %<br/> <strong>Environmental Parameters:<\/strong><br/> Temperature:  29.2 °C<br/> Salinity:  8.23 <br/> DO.:  169","<strong>Site:<\/strong> MS_B10 <br/> <strong>Zone:<\/strong> Mullet South <br/> <strong>Coordinates:<\/strong><br/> Lat:  21.433359 <br/> Long:  -157.806211 <br/> <strong>Grain Size Distribution:<\/strong><br/> Gravel:  50.3 %<br/> Coarse Sand:  52.7 %<br/> Medium Sand:  29.8 %<br/> Fine Sand:  63.8 %<br/> Silt/Clay:  50.2 %<br/> <strong>Environmental Parameters:<\/strong><br/> Temperature:  29 °C<br/> Salinity:  8.25 <br/> DO.:  126","<strong>Site:<\/strong> MS_B11 <br/> <strong>Zone:<\/strong> Mullet South <br/> <strong>Coordinates:<\/strong><br/> Lat:  21.43438 <br/> Long:  -157.80534 <br/> <strong>Grain Size Distribution:<\/strong><br/> Gravel:  44.3 %<br/> Coarse Sand:  50.9 %<br/> Medium Sand:  24.4 %<br/> Fine Sand:  65.7 %<br/> Silt/Clay:  42.1 %<br/> <strong>Environmental Parameters:<\/strong><br/> Temperature:  29.1 °C<br/> Salinity:  8.26 <br/> DO.:  156.8","<strong>Site:<\/strong> MS_B12 <br/> <strong>Zone:<\/strong> Mullet South <br/> <strong>Coordinates:<\/strong><br/> Lat:  21.433319 <br/> Long:  -157.807793 <br/> <strong>Grain Size Distribution:<\/strong><br/> Gravel:  10.1 %<br/> Coarse Sand:  61.7 %<br/> Medium Sand:  20.7 %<br/> Fine Sand:  92.7 %<br/> Silt/Clay:  40.9 %<br/> <strong>Environmental Parameters:<\/strong><br/> Temperature:  28.7 °C<br/> Salinity:  8.13 <br/> DO.:  120","<strong>Site:<\/strong> ME_B13 <br/> <strong>Zone:<\/strong> Mullet East <br/> <strong>Coordinates:<\/strong><br/> Lat:  21.43591 <br/> Long:  -157.80556 <br/> <strong>Grain Size Distribution:<\/strong><br/> Gravel:  98.7 %<br/> Coarse Sand:  111.6 %<br/> Medium Sand:  12 %<br/> Fine Sand:  10.3 %<br/> Silt/Clay:  0.7 %<br/> <strong>Environmental Parameters:<\/strong><br/> Temperature:  27.1 °C<br/> Salinity:  7.8 <br/> DO.:  111.2","<strong>Site:<\/strong> ME_B14 <br/> <strong>Zone:<\/strong> Mullet East <br/> <strong>Coordinates:<\/strong><br/> Lat:  21.43704 <br/> Long:  -157.80606 <br/> <strong>Grain Size Distribution:<\/strong><br/> Gravel:  82.3 %<br/> Coarse Sand:  56.2 %<br/> Medium Sand:  18.8 %<br/> Fine Sand:  50.7 %<br/> Silt/Clay:  18.1 %<br/> <strong>Environmental Parameters:<\/strong><br/> Temperature:  27.2 °C<br/> Salinity:  7.97 <br/> DO.:  114.4","<strong>Site:<\/strong> ME_B15 <br/> <strong>Zone:<\/strong> Mullet East <br/> <strong>Coordinates:<\/strong><br/> Lat:  21.436094 <br/> Long:  -157.806613 <br/> <strong>Grain Size Distribution:<\/strong><br/> Gravel:  16.4 %<br/> Coarse Sand:  78.3 %<br/> Medium Sand:  30.5 %<br/> Fine Sand:  62.2 %<br/> Silt/Clay:  7.8 %<br/> <strong>Environmental Parameters:<\/strong><br/> Temperature:  28.1 °C<br/> Salinity:  8.15 <br/> DO.:  140.5","<strong>Site:<\/strong> ME_B16 <br/> <strong>Zone:<\/strong> Mullet East <br/> <strong>Coordinates:<\/strong><br/> Lat:  21.436895 <br/> Long:  -157.807357 <br/> <strong>Grain Size Distribution:<\/strong><br/> Gravel:  107.2 %<br/> Coarse Sand:  41.3 %<br/> Medium Sand:  10.7 %<br/> Fine Sand:  23.6 %<br/> Silt/Clay:  11 %<br/> <strong>Environmental Parameters:<\/strong><br/> Temperature:  28.2 °C<br/> Salinity:  8.28 <br/> DO.:  163.8"],null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]},{"method":"addLegend","args":[{"colors":["#FFD700","#006400","#4169E1","#FF0000"],"labels":["Mullet East","Mullet North","Mullet South","Mullet West"],"na_color":null,"na_label":"NA","opacity":0.7,"position":"bottomright","type":"factor","title":"Dry Season Mullet Sampling Sites","extra":null,"layerId":null,"className":"info legend","group":null}]}],"setView":[[21.4351,-157.806],16,[]],"limits":{"lat":[21.4322495,21.43822],"lng":[-157.810889,-157.80534]}},"evals":[],"jsHooks":[]}</script>
```

``` r
wet_map
```

```{=html}
<div class="leaflet html-widget html-fill-item" id="htmlwidget-bd2aef274121124b9c75" style="width:504px;height:360px;"></div>
<script type="application/json" data-for="htmlwidget-bd2aef274121124b9c75">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addTiles","args":["https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"https://openstreetmap.org/copyright/\">OpenStreetMap<\/a>,  <a href=\"https://opendatacommons.org/licenses/odbl/\">ODbL<\/a>"}]},{"method":"addCircleMarkers","args":[[21.43822,21.43785747,21.43725,21.43728,21.4364,21.4346,21.43526813,21.43555556,21.4322495,21.4333585,21.43438,21.43331934,21.43591,21.43704,21.43609408,21.43689497],[-157.81061,-157.8089375,-157.810889,-157.80956,-157.81007,-157.80894,-157.8080256,-157.8097222,-157.8066923,-157.8062108,-157.80534,-157.807793,-157.80556,-157.80606,-157.8066134,-157.8073568],8,null,null,{"interactive":true,"className":"","stroke":true,"color":["#006400","#006400","#006400","#006400","#FF0000","#FF0000","#FF0000","#FF0000","#4169E1","#4169E1","#4169E1","#4169E1","#FFD700","#FFD700","#FFD700","#FFD700"],"weight":1,"opacity":0.5,"fill":true,"fillColor":["#006400","#006400","#006400","#006400","#FF0000","#FF0000","#FF0000","#FF0000","#4169E1","#4169E1","#4169E1","#4169E1","#FFD700","#FFD700","#FFD700","#FFD700"],"fillOpacity":0.7},null,null,["<strong>Site:<\/strong> MN_B1 <br/> <strong>Zone:<\/strong> Mullet North <br/> <strong>Coordinates:<\/strong><br/> Lat:  21.43822 <br/> Long:  -157.81061 <br/> <strong>Grain Size Distribution:<\/strong><br/> Gravel:  83.6 %<br/> Coarse Sand:  31.9 %<br/> Medium Sand:  8.1 %<br/> Fine Sand:  60.3 %<br/> Silt/Clay:  257.7 %<br/> <strong>Environmental Parameters:<\/strong><br/> Temperature:  21.5 °C<br/> Salinity:  7.84 <br/> DO.:  88.1","<strong>Site:<\/strong> MN_B2 <br/> <strong>Zone:<\/strong> Mullet North <br/> <strong>Coordinates:<\/strong><br/> Lat:  21.437857 <br/> Long:  -157.808938 <br/> <strong>Grain Size Distribution:<\/strong><br/> Gravel:  64.2 %<br/> Coarse Sand:  46.8 %<br/> Medium Sand:  22.9 %<br/> Fine Sand:  55.8 %<br/> Silt/Clay:  184.1 %<br/> <strong>Environmental Parameters:<\/strong><br/> Temperature:  21.6 °C<br/> Salinity:  7.97 <br/> DO.:  94","<strong>Site:<\/strong> MN_B3 <br/> <strong>Zone:<\/strong> Mullet North <br/> <strong>Coordinates:<\/strong><br/> Lat:  21.43725 <br/> Long:  -157.810889 <br/> <strong>Grain Size Distribution:<\/strong><br/> Gravel:  20 %<br/> Coarse Sand:  3.4 %<br/> Medium Sand:  3.3 %<br/> Fine Sand:  22.1 %<br/> Silt/Clay:  421.8 %<br/> <strong>Environmental Parameters:<\/strong><br/> Temperature:  21.3 °C<br/> Salinity:  7.77 <br/> DO.:  89.7","<strong>Site:<\/strong> MN_B4 <br/> <strong>Zone:<\/strong> Mullet North <br/> <strong>Coordinates:<\/strong><br/> Lat:  21.43728 <br/> Long:  -157.80956 <br/> <strong>Grain Size Distribution:<\/strong><br/> Gravel:  0.3 %<br/> Coarse Sand:  3.8 %<br/> Medium Sand:  0.9 %<br/> Fine Sand:  45.9 %<br/> Silt/Clay:  325 %<br/> <strong>Environmental Parameters:<\/strong><br/> Temperature:  21.5 °C<br/> Salinity:  7.89 <br/> DO.:  89.6","<strong>Site:<\/strong> MW_B5 <br/> <strong>Zone:<\/strong> Mullet West <br/> <strong>Coordinates:<\/strong><br/> Lat:  21.4364 <br/> Long:  -157.81007 <br/> <strong>Grain Size Distribution:<\/strong><br/> Gravel:  4.5 %<br/> Coarse Sand:  10.2 %<br/> Medium Sand:  6.9 %<br/> Fine Sand:  72.1 %<br/> Silt/Clay:  423.6 %<br/> <strong>Environmental Parameters:<\/strong><br/> Temperature:  21.4 °C<br/> Salinity:  7.76 <br/> DO.:  91.2","<strong>Site:<\/strong> MW_B6 <br/> <strong>Zone:<\/strong> Mullet West <br/> <strong>Coordinates:<\/strong><br/> Lat:  21.4346 <br/> Long:  -157.80894 <br/> <strong>Grain Size Distribution:<\/strong><br/> Gravel:  31.8 %<br/> Coarse Sand:  25.9 %<br/> Medium Sand:  24.5 %<br/> Fine Sand:  150.4 %<br/> Silt/Clay:  119.6 %<br/> <strong>Environmental Parameters:<\/strong><br/> Temperature:  21.3 °C<br/> Salinity:  7.27 <br/> DO.:  80.8","<strong>Site:<\/strong> MW_B7 <br/> <strong>Zone:<\/strong> Mullet West <br/> <strong>Coordinates:<\/strong><br/> Lat:  21.435268 <br/> Long:  -157.808026 <br/> <strong>Grain Size Distribution:<\/strong><br/> Gravel:  200.4 %<br/> Coarse Sand:  67.6 %<br/> Medium Sand:  21.7 %<br/> Fine Sand:  44.6 %<br/> Silt/Clay:  24.3 %<br/> <strong>Environmental Parameters:<\/strong><br/> Temperature:  21.4 °C<br/> Salinity:  7.99 <br/> DO.:  96.2","<strong>Site:<\/strong> MW_B8 <br/> <strong>Zone:<\/strong> Mullet West <br/> <strong>Coordinates:<\/strong><br/> Lat:  21.435556 <br/> Long:  -157.809722 <br/> <strong>Grain Size Distribution:<\/strong><br/> Gravel:  78.7 %<br/> Coarse Sand:  73.2 %<br/> Medium Sand:  28.8 %<br/> Fine Sand:  102.9 %<br/> Silt/Clay:  68.8 %<br/> <strong>Environmental Parameters:<\/strong><br/> Temperature:  21.4 °C<br/> Salinity:  7.74 <br/> DO.:  86.5","<strong>Site:<\/strong> MS_B9 <br/> <strong>Zone:<\/strong> Mullet South <br/> <strong>Coordinates:<\/strong><br/> Lat:  21.43225 <br/> Long:  -157.806692 <br/> <strong>Grain Size Distribution:<\/strong><br/> Gravel:  70.3 %<br/> Coarse Sand:  77.3 %<br/> Medium Sand:  12.1 %<br/> Fine Sand:  31.9 %<br/> Silt/Clay:  63.8 %<br/> <strong>Environmental Parameters:<\/strong><br/> Temperature:  21.4 °C<br/> Salinity:  7.44 <br/> DO.:  69.8","<strong>Site:<\/strong> MS_B10 <br/> <strong>Zone:<\/strong> Mullet South <br/> <strong>Coordinates:<\/strong><br/> Lat:  21.433359 <br/> Long:  -157.806211 <br/> <strong>Grain Size Distribution:<\/strong><br/> Gravel:  74.7 %<br/> Coarse Sand:  57.7 %<br/> Medium Sand:  26.3 %<br/> Fine Sand:  50.3 %<br/> Silt/Clay:  16.8 %<br/> <strong>Environmental Parameters:<\/strong><br/> Temperature:  21.5 °C<br/> Salinity:  7.79 <br/> DO.:  79.4","<strong>Site:<\/strong> MS_B11 <br/> <strong>Zone:<\/strong> Mullet South <br/> <strong>Coordinates:<\/strong><br/> Lat:  21.43438 <br/> Long:  -157.80534 <br/> <strong>Grain Size Distribution:<\/strong><br/> Gravel:  43.4 %<br/> Coarse Sand:  59.8 %<br/> Medium Sand:  22.7 %<br/> Fine Sand:  62.6 %<br/> Silt/Clay:  37.8 %<br/> <strong>Environmental Parameters:<\/strong><br/> Temperature:  21.8 °C<br/> Salinity:  7.86 <br/> DO.:  74.2","<strong>Site:<\/strong> MS_B12 <br/> <strong>Zone:<\/strong> Mullet South <br/> <strong>Coordinates:<\/strong><br/> Lat:  21.433319 <br/> Long:  -157.807793 <br/> <strong>Grain Size Distribution:<\/strong><br/> Gravel:  19.5 %<br/> Coarse Sand:  52.1 %<br/> Medium Sand:  26.6 %<br/> Fine Sand:  123.1 %<br/> Silt/Clay:  78 %<br/> <strong>Environmental Parameters:<\/strong><br/> Temperature:  21.3 °C<br/> Salinity:  7.8 <br/> DO.:  81.7","<strong>Site:<\/strong> ME_B13 <br/> <strong>Zone:<\/strong> Mullet East <br/> <strong>Coordinates:<\/strong><br/> Lat:  21.43591 <br/> Long:  -157.80556 <br/> <strong>Grain Size Distribution:<\/strong><br/> Gravel:  277.7 %<br/> Coarse Sand:  164.7 %<br/> Medium Sand:  17.6 %<br/> Fine Sand:  48.6 %<br/> Silt/Clay:  7 %<br/> <strong>Environmental Parameters:<\/strong><br/> Temperature:  22.1 °C<br/> Salinity:  8 <br/> DO.:  87.3","<strong>Site:<\/strong> ME_B14 <br/> <strong>Zone:<\/strong> Mullet East <br/> <strong>Coordinates:<\/strong><br/> Lat:  21.43704 <br/> Long:  -157.80606 <br/> <strong>Grain Size Distribution:<\/strong><br/> Gravel:  249 %<br/> Coarse Sand:  25.2 %<br/> Medium Sand:  14.5 %<br/> Fine Sand:  34.6 %<br/> Silt/Clay:  32.3 %<br/> <strong>Environmental Parameters:<\/strong><br/> Temperature:  22 °C<br/> Salinity:  8.03 <br/> DO.:  90.9","<strong>Site:<\/strong> ME_B15 <br/> <strong>Zone:<\/strong> Mullet East <br/> <strong>Coordinates:<\/strong><br/> Lat:  21.436094 <br/> Long:  -157.806613 <br/> <strong>Grain Size Distribution:<\/strong><br/> Gravel:  28.4 %<br/> Coarse Sand:  7.2 %<br/> Medium Sand:  1.9 %<br/> Fine Sand:  56.3 %<br/> Silt/Clay:  1.3 %<br/> <strong>Environmental Parameters:<\/strong><br/> Temperature:  21.6 °C<br/> Salinity:  7.8 <br/> DO.:  82.7","<strong>Site:<\/strong> ME_B16 <br/> <strong>Zone:<\/strong> Mullet East <br/> <strong>Coordinates:<\/strong><br/> Lat:  21.436895 <br/> Long:  -157.807357 <br/> <strong>Grain Size Distribution:<\/strong><br/> Gravel:  123.2 %<br/> Coarse Sand:  42.2 %<br/> Medium Sand:  15.5 %<br/> Fine Sand:  29.2 %<br/> Silt/Clay:  13.9 %<br/> <strong>Environmental Parameters:<\/strong><br/> Temperature:  21.6 °C<br/> Salinity:  7.9 <br/> DO.:  88.2"],null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]},{"method":"addLegend","args":[{"colors":["#FFD700","#006400","#4169E1","#FF0000"],"labels":["Mullet East","Mullet North","Mullet South","Mullet West"],"na_color":null,"na_label":"NA","opacity":0.7,"position":"bottomright","type":"factor","title":"Wet Season Mullet Sampling Sites","extra":null,"layerId":null,"className":"info legend","group":null}]}],"setView":[[21.4351,-157.806],16,[]],"limits":{"lat":[21.4322495,21.43822],"lng":[-157.810889,-157.80534]}},"evals":[],"jsHooks":[]}</script>
```

