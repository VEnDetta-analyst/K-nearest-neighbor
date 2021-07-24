schools <- read.csv("https://ibm.box.com/shared/static/uummw8ijp41gn3nfkuipi78xnalkss4c.csv", sep = ",")
str(schools)
head(schools)
#taking subset from data 
schools.sub=subset(schools, select=c(name,province.name..english,latitude,longitude))
#changing column names of data
colnames(schools.sub)=c("name","province","lat","long")
#To visualise data on map
library(leaflet)
# Establish the limits of our default visualization
lower_lon = -140
upper_lon = -50
lower_lat = 40
upper_lat = 65
# Establish the center of our default visualization
center_lon = (lower_lon + upper_lon)/2
center_lat = (lower_lat + upper_lat)/2
# Set the zoom of our default visualization
zoom = 4
schools_map <- leaflet(schools.sub) %>%
  setView(center_lon,center_lat, zoom)%>% 
  addProviderTiles("OpenStreetMap.BlackAndWhite")%>% # set the map that we want to use as background
  addCircleMarkers(lng = schools.sub$long, 
                   lat = schools.sub$lat, 
                   popup = schools.sub$name, # pop-ups will show the name of school if you click on a data point
                   fillColor = "Black", # colors of the markers will be black
                   fillOpacity = 1, # the shapes will have maximum opacity
                   radius = 4, # radius determine the size of each shape
                   stroke = F) # no stroke will be drawn in each data point
library(htmlwidgets)
library(IRdisplay)
saveWidget(schools_map, file="schools_map.html", selfcontained = F) #saving the leaflet map in html
display_html(paste("<iframe src=' ", 'schools_map.html', " ' width='100%' height='400'","/>")) #display the map !
#Implementation of k nearest neighbors
set.seed(1234)
ind=sample(2,nrow(schools.sub),replace = TRUE, prob = c(0.7,0.3))
schools.train=schools.sub[ind==1,3:4]
schools.test=schools.sub[ind==2,3:4]
schools.trainLabels=schools.sub[ind==1,2]
schools.testLabels=schools.sub[ind==2,2]
prov_pred=knn(train=schools.train,test=schools.test,cl=schools.trainLabels,k=3)
#To find no of incorrect prediction & accuarcy
correct_provinces = which(prov_pred == schools.testLabels, arr.ind = TRUE)
incorrect_provinces <- which(prov_pred != schools.testLabels, arr.ind = TRUE)
length(incorrect_provinces)
length(correct_provinces)/length(schools.testLabels)
