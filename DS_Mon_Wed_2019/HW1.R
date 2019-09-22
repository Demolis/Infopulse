#HW 1 Day 1 Sl 10

mas1<-rnorm(100,0,5)
mas2<-rnorm(100,10, sd = 27)
mean(mas1)
mean(mas2)
sd(mas1)
sd(mas2)
plot(mas1,mas2)
hist(mas1)
hist(mas2)
mas1*3
mas2[mas2>mean(mas2)]<-mas2[mas2>mean(mas2)]-18
