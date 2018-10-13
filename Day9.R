# Downloading raster map
library(jpeg)
image <- readJPEG("France.jpg")
Xras = 800; Yras = 731
Reg <- c("Paris","Orlean","Lion","Tulusa")
par(mar = c(1, 1, 1, 1))
?par
plot(1,xlim = c(0, Xras), ylim = c(0, Yras), xlab = "", ylab = "")
lim <- par()
lim$usr
rasterImage(image, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])


xy <- locator(4)
xy
Estimates <- c(89,54,64,32)
mycex = 10*(Estimates - min(Estimates))/max(Estimates) + 2
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
?expand_limits
m <- ggplot() + geom_map(data = france, 
                         aes(map_id = id), 
                         map = france,
                         fill = "white", color = "black") +
  expand_limits(x = france$long, y = france$lat) +
  coord_map("mercator") + 
  xlab("Lon") + ylab("Lat") + theme_bw()
print(m)
mp <- m + geom_point(data = data.frame(Lat = c(45.0, 47.2,45.5, 48.5),
                                       Lon = c(0, 3.0, 7.0, -2.0)),
                     aes(Lon, Lat), color = I("red"), size = 3)
print(mp)


install.packages('geosphere',dep=T)
library(geosphere)
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

#-----------------------------------------------------------------------	
#   Spatial distribution of points
#-----------------------------------------------------------------------	
install.packages('spatstat',dep=T)
library(spatstat)


plotBl <- read.table(file = "bloss.txt", header = T, sep = "\t")
str(plotBl)
# the size 
xmin <- floor(min(plotBl$x))
xmax <- ceiling(max(plotBl$x))
ymin <- floor(min(plotBl$y))
ymax <- ceiling(max(plotBl$y))

# Create ppp (point pattern):
ppp_object <- ppp(x = plotBl$x, y = plotBl$y, 
                  marks = data.frame(age = plotBl$age,
                                     blossoms = plotBl$blossoms),     # marks 
                  window = owin(c(xmin, xmax), c(ymin, ymax),  # window
                                unitname = c("metre","metres")))   

# Square
A <- area.owin(ppp_object$window)
A
# number of points
N <- ppp_object$n

# the size of 1 cell
L <- sqrt(2*A/N)
L



# Build the cells:
quadnum <- round(10/L)
quadnum
ppp_quadrats <- quadrats(ppp_object, nx = quadnum, ny = quadnum)

# The number of points in cells:
quadcount <- quadratcount(ppp_object, tess = ppp_quadrats)
quadcount <- round(quadcount, 1)

# Let's shov number of points in the squares

# centers of cells:
xgrid <- quadrats(ppp_object, nx = quadnum, ny = quadnum)$xgrid
ygrid <- quadrats(ppp_object, nx = quadnum, ny = quadnum)$ygrid
image(xgrid, ygrid,
      t(quadcount[order(1:quadnum, decreasing = T), ]),
      col = colorRampPalette(c("white", "green"))(15),
      axes=F, asp=1, xlab="", ylab="",
      main="Число точек в квадратах")
plot(quadcount, add=T, cex=0.7)      

# add the points:
plot(ppp_object, which.marks = "age",
     chars = c(19, 24), cex = 0.7, add = T)

# CSR test for probabilities:
quadrat_test_result <- quadrat.test(ppp_object, nx = 9, ny = 9)
quadrat_test_result

# Density map:
gene_ppp <- ppp_object[ppp_object$marks$age=="gene"]

# bandwidth by Silverman:
sigma<-(sd(gene_ppp$x) + sd(gene_ppp$y))/2 
iqr<-(IQR(gene_ppp$x) + IQR(gene_ppp$y))/2 
bandwidth <- 0.9*min(sigma, iqr)*gene_ppp$n^(-1/5)
bandwidth
gene_intensity <- density.ppp(gene_ppp, sigma =2)
plot(gene_intensity,   main = "Плотность gene, экз/кв.м")
points(gene_ppp, pch = 19, cex = 0.6)


#determine the area with some properties

for_relrisk_example <- ppp_object
marks(for_relrisk_example) <- ppp_object$marks$age

# the probability of age pre:
critvalue<-0.5
p <- relrisk(for_relrisk_example, critvalue) 
plot(p, main = "Доля группы pre", col = colorRampPalette(
  c("antiquewhite", "aquamarine3","navyblue"))(100))

# add contour lines:
contour(p, nlevels = 5, lwd = seq(from = 0.1, to = 3, length.out = 5), add = T)



#-----------------------------------------------------------------------	
#   Google Maps
#-----------------------------------------------------------------------	
install.packages('googleVis',dep=T)
library(googleVis)


Earthquake<- data.frame(Coords = c("38:142", "18:-73", "-36:-73"),
                     Location = c("Japan Earthquake", "Haiti  Earthquake", "Chily  Earthquake"))
?gvisMap
Map <- gvisMap(Earthquake, "Coords", "Location",
               chartid = "Earthquake")
plot(Map)
print(Map, tag = "chart")

# Web-graphics:
data(Fruits)
Fruits

M <- gvisMotionChart(Fruits, idvar = "Fruit", timevar = "Year")
plot(M)
print(M, tag = "chart")

