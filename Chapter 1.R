getwd()

#calculations

2+2
2*2

#assignment operator

x<-4
x

y<-c(2, 7, 4, 1)
y

#Products

scalor= x*y

term_by_term=y*y

#alternative
y^2

#Vector Multiplication

outer_product=y%*%t(y)

inner_prod=t(y)%*%y

ln=log(y)

exp=exp(y)

y-mean(y)

#standardization
std= (y-mean(y))/sd(y) 

#Sequences

1:10
-10:10

seq(from=1, to=100, by=7)

#matrices

A<- matrix (1:16, 4, 4)
A

dim(A)

length (y)

#indexing

A[,1]
A[4,4]
y[c(1,4)]

A[,1] [A[,1]>2]

A[,1]>2 #boolean

y[A[,1]>2] #non-sense

#Data Objects

#Array

myFirst.Array <- array(c(1: 8), dim=c(2,2,2))
myFirst.Array

#Lists

myFirst.List <-list("Some_Numbers"=c(66, 76, 55, 12, 4, 66, 8, 99),"Animals"=c("Rabbit", "Cat", "Elephant"),"My_Series"=c(30:1))
myFirst.List

str(myFirst.List)

#Data Frame

myFirst.Dataframe<- data.frame("Credit_Default"=c(0, 0, 1, 0, 1, 1), "Age"=c(35, 41, 55, 36, 44, 26), "Loan_in_1000_EUR"=c(55, 65, 23, 12, 98, 76))
myFirst.Dataframe

#Simple Regression Analysis using R

auto.data <- read.csv(file="autodata.csv")

gasolin.consumption<-auto.data$MPG.city
car.weight<-auto.data$Weight

head(cbind(gasolin.consumption, car.weight))

plot(y=gasolin.consumption, x=car.weight, xlab="Car-Weight (US-Pounds)", ylab="Consumption (Miles/Gallon)", main="Buy Light-Weight Cars!")

lm.result<-lm(gasolin.consumption ~ car.weight)
lm.summary<- summary(lm.result)
lm.summary

alpha<- lm.summary$coefficients[1]
beta<- lm.summary$coefficients[2]

lm.summary$coefficients

plot(y=gasolin.consumption, x=car.weight, xlab="Car-Weight (US-Pounds)", ylab="Consumption (Miles/Gallon)", main="Buy Light-Weight Cars!") 
abline (a=alpha, b=beta, col="red")










