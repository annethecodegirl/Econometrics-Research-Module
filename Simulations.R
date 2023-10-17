#Simulation with Bernoulli

set.seed(163)


rbern<- function (n,p){
  
  as.numeric(runif(n,0,1)<=p)

}


#meandraw_2bern=replicate(10, mean(rbern(n=2, p=0.3)))

meandraw_2bern=c()
for(i in 1:10){
  meandraw_2bern[i]<-mean(rbern(n=2, p=0.3))
}

hist(meandraw_2bern, main="Histogram of sample averages", xlab="Sample Mean", xlim=c(0,1))



Solutions - Bernoulli
# Take random sample of size 2 and calulate its mean
meandraw_2bern=replicate(10, mean(rbern(n=2, p=0.3)))

hist(meandraw_2bern, main="Histrogram of sample averages",
     xlab="Sample Mean", xlim=c(0,1))