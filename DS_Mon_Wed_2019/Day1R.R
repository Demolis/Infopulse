
#Comments :-P

1 + 5
1 + "a"

par<-8 # or =, but <- is better
par
"a"->par

pi
?pi
iris

## list of variables
ls
rm(par)

## logical type
logic_var<-3==2
logic_var

logic_var<-3>2 & 54<2
logic_var

logic_var<-3>2 | 54<2
logic_var

## numeric type
class(par)
par<-11.4
is.numeric(par)
par<-as.integer(par)
class(par)

par > 0
cos(par)
pars<-sqrt(par)+34/32
round(par)
par %% 3
par %/% 3
par ^ 3

## characters
var1<-"Hello"
var2<-'Hello'
var1 == var2

## Tasks
##

## arrays
arr_1 <- c(1,2,3,4,5,6,7,8,9,10)
arr_2 <- 1:10 # also functions seq() and rep()
class(arr_1)
arr_1[2]
arr_1[2:4]
arr_1[arr_1>7]
res<-arr_1 == 4
res
cos(arr_1)
arr_1[-c(3,5:8)]

arr_3 <- c(arr_2, "23")
length(arr_1)


## statistics
mean(1:10)
mean(arr_1)
quantile(arr_1)
summary(arr_1)

## plots
?plot
plot(1:10, col="red")
plot(1:10,2)
mas<-c(5,6,2,1)
plot(1:4,mas)

hist(mas, col="blue",main="Bla")
?hist
boxplot(mas)
boxplot(c(mas,13))
