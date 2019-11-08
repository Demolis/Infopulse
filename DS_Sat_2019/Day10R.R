

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

# Let's show number of points in the squares

# centers of cells:
xgrid <- quadrats(ppp_object, nx = quadnum, ny = quadnum)$xgrid
ygrid <- quadrats(ppp_object, nx = quadnum, ny = quadnum)$ygrid
image(xgrid, ygrid,
      t(quadcount[order(1:quadnum, decreasing = T), ]),
      col = colorRampPalette(c("white", "green"))(10),
      axes=F, asp=1, xlab="", ylab="",
      main="Number of points")
plot(quadcount, add=T, cex=0.7)      

# add the points:
plot(ppp_object, which.marks = "age",
     chars = c(10, 12), cex = c(0.1,1), add = T)

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
gene_intensity <- density.ppp(gene_ppp, sigma =bandwidth)
plot(gene_intensity,   main = "Gene Intensity")
points(gene_ppp, pch = 19, cex = 0.6)


#determine the area with some properties

for_relrisk_example <- ppp_object
marks(for_relrisk_example) <- ppp_object$marks$age

# the probability of age pre:
sigma<-0.8
p <- relrisk(for_relrisk_example,sigma) 
plot(p, main = "Probability of pre", col = colorRampPalette(
  c("antiquewhite", "aquamarine3","navyblue"))(100))

# add contour lines:
contour(p, nlevels = 5, lwd = seq(from = 0.1, to = 3, length.out = 5), add = T)

#-----------------------------------------------------------------------	
#   Maps and Shape Files in R
#-----------------------------------------------------------------------	
library(sp)
library(maptools)
library(ggplot2)
library(rgeos)

# Shape file:
Regions <- readShapePoly("Fra_adm1.shp")

slotNames(Regions)
Regions@data    
str(Regions@polygons)
Regions@data$NAME_1

# Region of France:
spplot(Regions,
       "NAME_1", 
       scales = list(draw = T), 
       col.regions = rainbow(n = 22) ) 
library(RColorBrewer)
spplot(Regions,
       "NAME_1", 
       scales = list(draw = T), col.regions = brewer.pal(4, "Set3"),
       par.settings = list(axis.line = list(col = NA)))

# We want to add some data
# The order is important

Regions@data$Value = rnorm(22)
mypalette <- colorRampPalette(c("seagreen", "whitesmoke"))

# mypalette - is a function:
mypalette
spplot(Regions, "Value",
       col.regions = mypalette(20),  
       col = "transparent", # without borders
       par.settings = list(axis.line = list(col = NA)))

#the same in ggplot2


counties <- fortify(Regions, region = "NAME_1")
str(counties)
ggplot() + geom_map(data = counties,
                    aes(map_id = id), 
                    map = counties) + 
  expand_limits(x = counties$long, y = counties$lat) +
  coord_map("polyconic")

fake_data <- as.data.frame(Regions@data)
ggplot() + geom_map(data = fake_data, aes(map_id = NAME_1, fill = Value),                   
                    map = counties) + expand_limits(x = counties$long, y = counties$lat) + coord_map("polyconic")

# In other colors:
library(scales) 
ggplot() + geom_map(data = fake_data,
                    aes(map_id = NAME_1, fill = Value),
                    colour = "gray",
                    map = counties) + 
  expand_limits(x = counties$long, y = counties$lat) +
  scale_fill_gradient2(low = muted("blue"), 
                       midpoint = 0,
                       mid = "white",
                       high = muted("red"),
                       limits = c(min(fake_data$Value),
                                  max(fake_data$Value))) +
  coord_map("polyconic")

#the same in highcharter
library(highcharter)
mapdata <- get_data_from_map(download_map_data("countries/fr/fr-all-all"))
library(dplyr)
head(mapdata)
data_fake <- mapdata %>% 
  select(code = `hc-a2`) %>% 
  mutate(value = rnorm(nrow(.),1000,500))

hcmap("countries/fr/fr-all-all", data = data_fake, value = "value",
      joinBy = c("hc-a2", "code"), name = "Fake data",
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "#FAFAFA", borderWidth = 0.1,
      tooltip = list(valueDecimals = 2, valuePrefix = "", valueSuffix = " mln")) %>% 
      hc_mapNavigation(enabled = TRUE) 



