library('dplyr')
library('tidyr')
library('ggplot2')

salaries_SF <- read.csv("Salaries.csv")

str(salaries_SF)
ssf <- salaries_SF

dc <- c('Id', 'Notes', 'Agency')
#ssf <- subset(ssf, select = -c(1, 11, 12))
ssf <- ssf %>% select( -one_of(dc))
# Names & Job Title
ssf_names <- select(ssf, c(1:2))

ssf_names %>% 
  mutate_all(funs(factor(.)))

# Pay and Benefits
ssf_pay <- select(ssf, c(3:8)) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate_if(is.character, as.double)

ssf <- bind_cols(ssf_names, ssf_pay, select(ssf, c(9:10)))
ssfreg<-ssf[1:500,]

#----------------------------------- to build many ggplot graphics in cycle
  
for (i in colnames(ssfclust)[3:5]){
  print(  ggplot(data=ssfclust,aes(x=eval(parse(text=i)), y=TotalPay))+
            geom_point()+labs(x=i)) 
}


#---------------------------------- to build 3d picture from 4d model
mod2<-lm(TotalPay~OvertimePay+OtherPay+BasePay, data=ssf)

library(scatterplot3d)
s3d <- scatterplot3d(ssfreg$OtherPay, ssfreg$BasePay, ssfreg$TotalPay,highlight.3d = T, type = "h",
                     lab = c(2, 3)) # lab: number of tickmarks on x-/y-axes
s3d$plane3d(mod2$coefficients[1],mod2$coefficients[3],mod2$coefficients[4]) # draws the fitted plane lm



#----------------------- for spider chart https://www.r-graph-gallery.com/142-basic-radar-chart/
