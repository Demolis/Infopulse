# Downloading raster map
library(jpeg)
image <- readJPEG("France.jpg")
image
Xras = 800; Yras = 731
Reg <- c("Paris","Orlean","Lion","Tulusa")
par(mar = c(0, 0, 0, 0))
?par
plot(1,xlim = c(0, Xras), ylim = c(0, Yras), xlab = "", ylab = "")
lim <- par()
lim$usr
rasterImage(image, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])

xy <- locator(4)
xy
Estimates <- c(89,54,64,32)
mycex = 8*(Estimates - min(Estimates))/max(Estimates) + 2
colpts = rgb(0.2, 0.5, 0.4, alpha = 0.6)
points(xy$x, xy$y,cex = mycex, col = 1, pch = 21, bg = colpts)

# Distances
df <- data.frame(xy)
dist(df)



# Calc the distances in France:
library(sp)
library(maptools)
library(ggplot2)
library(rgeos)

gadm<-readRDS("FRA_adm0.rds") #source https://gadm.org/download_country.html
france <- fortify(gadm)
str(france)
summary(as.factor(france$id))
m <- ggplot() + geom_map(data = france, 
                         aes(map_id = id), 
                         map = france,
                         fill = "white", color = "black") +
  expand_limits(x = france$long, y = france$lat) +
  coord_map("mercator") + 
  xlab("Lon") + ylab("Lat") + theme_bw()
m

mp <- m + geom_point(data = data.frame(Lat = c(45.0, 47.2,45.5, 48.5),
                                       Lon = c(0, 3.0, 7.0, -2.0)),
                     aes(Lon, Lat), color = I("red"), size = 3)
mp
#or==============================================
library(highcharter)
hcmap("countries/fr/custom/fr-all-mainland", showInLegend = FALSE)%>%
hc_add_series(data = data.frame(name = c(1,2,3,4),
  lat = c(45.0, 47.2,45.5, 48.5),
  lon = c(0, 3.0, 7.0, -2.0),
  z=c(1,1,1,1)), type = "mapbubble", name = "Data",maxSize="1%") %>% 
  hc_mapNavigation(enabled = TRUE) 

library(geosphere)
install.packages("geosphere")
# its in meters
distHaversine(c(0, 45.0), c(-2, 47.2)) 

distVincentyEllipsoid(c(0, 45.0), c(-2, 47.2)) 

coords <- cbind(c(0, 3.0, 7.0, -2.0),
                c(45.0, 47.2,45.5, 48.5))
coords

Dist <- apply(coords, 1, 
              FUN = function(eachPoint) distHaversine(eachPoint, coords)/1000)
Dist

Dist <- Dist[lower.tri(Dist)]
Dist

mean(Dist)

