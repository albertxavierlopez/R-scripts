# https://www.r-graph-gallery.com/180-change-background-in-leaflet-map/

###################################################################
############ Barcelona Latitude = 41.3597, Longitude = 2.2566
############ Other map "Esri.WorldImagery"

# Load libraries
library(shiny)
library(leaflet)

plastic<-read.table(".../barcelona_plastic.csv", header = TRUE, sep = ",")
data<-plastic[ which(plastic$year==0 & plastic$month==0), ]
data2<-plastic[ which(plastic$year==0 & plastic$month==2), ]
data4<-plastic[ which(plastic$year==0 & plastic$month==4), ]
data6<-plastic[ which(plastic$year==0 & plastic$month==6), ]
data8<-plastic[ which(plastic$year==0 & plastic$month==8), ]
data10<-plastic[ which(plastic$year==0 & plastic$month==10), ]
data12<-plastic[ which(plastic$year==1 & plastic$month==0), ]
data24<-plastic[ which(plastic$year==2 & plastic$month==0), ]
data36<-plastic[ which(plastic$year==3 & plastic$month==0), ]

# Make data with several positions
data_plastic=data.frame(LONG=data$lng, LAT=data$lat, PLACE=paste("Plastic"), PROB=data$probability)
data_plastic2=data.frame(LONG=data2$lng, LAT=data2$lat, PLACE=paste("Plastic2"),PROB=data2$probability )
data_plastic4=data.frame(LONG=data4$lng, LAT=data4$lat, PLACE=paste("Plastic4"),PROB=data4$probability )
data_plastic6=data.frame(LONG=data6$lng, LAT=data6$lat, PLACE=paste("Plastic6"),PROB=data6$probability )
data_plastic8=data.frame(LONG=data8$lng, LAT=data8$lat, PLACE=paste("Plastic8"),PROB=data8$probability )
data_plastic10=data.frame(LONG=data10$lng, LAT=data10$lat, PLACE=paste("Plastic10"),PROB=data10$probability )
data_plastic12=data.frame(LONG=data12$lng, LAT=data12$lat, PLACE=paste("Plastic12"),PROB=data12$probability )
data_plastic24=data.frame(LONG=data24$lng, LAT=data24$lat, PLACE=paste("Plastic24"),PROB=data24$probability )
data_plastic36=data.frame(LONG=data36$lng, LAT=data36$lat, PLACE=paste("Plastic36"),PROB=data36$probability )


# Initialize the leaflet map:
leaflet() %>% 
  setView(lng=14, lat=42, zoom=5 ) %>%
  
  # Add two tiles
  addProviderTiles("NASAGIBS.ViirsEarthAtNight2012", group="background 1") %>%
  #addTiles(options = providerTileOptions(noWrap = TRUE), group="background 1") %>%
  
  # Add 2 marker groups
  addCircleMarkers(data=data_plastic, lng=~LONG , lat=~LAT, radius=20 , color="",  fillColor="white", stroke = TRUE, fillOpacity = 0.8, group="1st month") %>%
  addCircleMarkers(data=data_plastic2, lng=~LONG , lat=~LAT, radius=~sqrt(PROB)*20 , color="",  fillColor="white", stroke = TRUE, fillOpacity = 0.8, group="2nd month") %>%
  addCircleMarkers(data=data_plastic4, lng=~LONG , lat=~LAT, radius=~sqrt(PROB)*20 , color="",  fillColor="white", stroke = TRUE, fillOpacity = 0.8, group="4th month") %>%
  addCircleMarkers(data=data_plastic6, lng=~LONG , lat=~LAT, radius=~sqrt(PROB)*20 , color="",  fillColor="white", stroke = TRUE, fillOpacity = 0.8, group="6th month") %>%
  addCircleMarkers(data=data_plastic8, lng=~LONG , lat=~LAT, radius=~sqrt(PROB)*20 , color="",  fillColor="white", stroke = TRUE, fillOpacity = 0.8, group="8th month") %>%
  addCircleMarkers(data=data_plastic10, lng=~LONG , lat=~LAT, radius=~sqrt(PROB)*20 , color="",  fillColor="white", stroke = TRUE, fillOpacity = 0.8, group="10th month") %>%
  addCircleMarkers(data=data_plastic12, lng=~LONG , lat=~LAT, radius=~sqrt(PROB)*20, color="",  fillColor="white", stroke = TRUE, fillOpacity = 0.8, group="1st year") %>%
  addCircleMarkers(data=data_plastic24, lng=~LONG , lat=~LAT, radius=~sqrt(PROB)*20, color="",  fillColor="white", stroke = TRUE, fillOpacity = 0.8, group="2nd year") %>%
  addCircleMarkers(data=data_plastic36, lng=~LONG , lat=~LAT, radius=~sqrt(PROB)*20, color="",  fillColor="white", stroke = TRUE, fillOpacity = 0.8, group="3rd year") %>%

  # Add the control widget
  addLayersControl(baseGroups = c("1st month","2nd month","4th month","6th month","8th month","10th month","1st year", "2nd year","3rd year"), options = layersControlOptions(collapsed = F))

  

