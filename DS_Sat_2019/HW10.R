#Task 1
library(spatstat)


plotConfl <- read.csv(file = "conflicts.csv", sep = ";",dec=",")
str(plotConfl)
plotConfl <- plotConfl%>% group_by(country, longitude, latitude)%>%summarise(type=min(type_of_violence),year=max(year))
# the size 
xmin <- floor(min(plotConfl$longitude))
xmax <- ceiling(max(plotConfl$longitude))
ymin <- floor(min(plotConfl$latitude))
ymax <- ceiling(max(plotConfl$latitude))

#Построить пространственное распределение конфликтов в Европе и в Украине.


# Create ppp (point pattern):
ppp_object <- ppp(x = plotConfl$longitude, y = plotConfl$latitude, 
                  marks = data.frame(year = as.factor(plotConfl$year),
                                     type =  plotConfl$type),     
                  window = owin(c(xmin, xmax), c(ymin, ymax)))   

# Square
A <- area.owin(ppp_object$window)
A
# number of points
N <- ppp_object$n

# the size of 1 cell
L <- sqrt(2*A/N)
L



# Build the cells:
quadnum_x <- round((xmax-xmin)/L)
quadnum_y <- round((ymax-ymin)/L)

ppp_quadrats <- quadrats(ppp_object, nx = quadnum_x, ny = quadnum_y)

# The number of points in cells:
quadcount <- quadratcount(ppp_object, tess = ppp_quadrats)
quadcount <- round(quadcount, 1)

# Let's show number of points in the squares

# centers of cells:
xgrid <- quadrats(ppp_object, nx = quadnum_x, ny = quadnum_y)$xgrid
ygrid <- quadrats(ppp_object, nx = quadnum_x, ny = quadnum_y)$ygrid
image(xgrid, ygrid,
      t(quadcount[order(1:quadnum_y, decreasing = T), ]),
      col = colorRampPalette(c("white", "green"))(10),
      axes=F, asp=1, xlab="", ylab="",
      main="Number of points")
plot(quadcount, add=T, cex=0.7)      

# add the points:
plot(ppp_object, which.marks = "year",
     chars = c(10, 11,12,13,14,15), add = T)
#Для Европы:
#  - построить изоклины для распределения конфликтов. 
for_relrisk_example <- ppp_object
for_relrisk_example$marks$type[for_relrisk_example$marks$type==2]<-3

marks(for_relrisk_example) <-as.factor( for_relrisk_example$marks$type)
sigma<-0.1
p <- relrisk(for_relrisk_example,sigma) 
plot(p, main = "Probability", col = colorRampPalette(
  c("antiquewhite", "aquamarine3","navyblue"))(100))

# add contour lines:
contour(p, nlevels = 5, lwd = seq(from = 0.1, to = 1, length.out = 5), add = T)




#Для Украины:
#  определить какой регион представлен на реальной карте;
#классифицировать точки по годам;
#построить плотностное распределение.
plotConflU <- plotConfl[plotConfl$country=="Ukraine",]
xmin <- floor(min(plotConflU$longitude))
xmax <- ceiling(max(plotConflU$longitude))
ymin <- floor(min(plotConflU$latitude))
ymax <- ceiling(max(plotConflU$latitude))

#Построить пространственное распределение конфликтов в Европе и в Украине.


# Create ppp (point pattern):
ppp_objectU <- ppp(x = plotConflU$longitude, y = plotConflU$latitude, 
                  marks = data.frame(year = as.factor(plotConflU$year),
                                     type =  plotConflU$type),     
                  window = owin(c(xmin, xmax), c(ymin, ymax)))   

# Square
A <- area.owin(ppp_objectU$window)
A
# number of points
N <- ppp_objectU$n

# the size of 1 cell
L <- sqrt(2*A/N)
L



# Build the cells:
quadnum_x <- round((xmax-xmin)/L)
quadnum_y <- round((ymax-ymin)/L)

ppp_quadrats <- quadrats(ppp_objectU, nx = quadnum_x, ny = quadnum_y)

# The number of points in cells:
quadcount <- quadratcount(ppp_objectU, tess = ppp_quadrats)
quadcount <- round(quadcount, 1)

# Let's show number of points in the squares

# centers of cells:
xgrid <- quadrats(ppp_objectU, nx = quadnum_x, ny = quadnum_y)$xgrid
ygrid <- quadrats(ppp_objectU, nx = quadnum_x, ny = quadnum_y)$ygrid
image(xgrid, ygrid,
      t(quadcount[order(1:quadnum_y, decreasing = T), ]),
      col = colorRampPalette(c("white", "green"))(10),
      axes=F, asp=1, xlab="", ylab="",
      main="Number of points")
plot(quadcount, add=T, cex=0.7)      

# add the points:
plot(ppp_objectU, which.marks = "year",
     chars = c(10, 11,12,13,14,15), add = T)

# bandwidth by Silverman:
sigma<-(sd(ppp_objectU$x) + sd(ppp_objectU$y))/2 
iqr<-(IQR(ppp_objectU$x) + IQR(ppp_objectU$y))/2 
bandwidth <- 0.9*min(sigma, iqr)*ppp_objectU$n^(-1/5)
bandwidth
intensity <- density.ppp(ppp_objectU, sigma =bandwidth)
plot(intensity,   main = "Intensity")
points(ppp_objectU, which.marks = "year",
       pch = c(10, 11,12,13,14,15), cex = 0.6)






