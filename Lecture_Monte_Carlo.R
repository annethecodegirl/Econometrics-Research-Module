#Estimate the value of pi
#with loop
circle_points<-0
square_points<-0
interval<-0

R<-10000

while(interval<R)
{
  x<-runif(1, min=0, max=1)
  y<-runif(1, min=0, max=1)
  d=x^2+y^2

if(d<=1){
  circle_points=circle_points+1}

square_points=square_points+1
interval=interval+1
}
pi_estimate <- 4 * (circle_points / square_points)

cat("Estimated value of pi:", pi_estimate, "\n")

#with vectors (ofz. soln.)

s=10^6

U<-runif(s, min=0, max=1)
V<-runif(s, min=0, max=1)
pi_sim=4*mean(U^2+V^2<=1)

pi
pi_sim

#Estimator is a mapping from the random sample
#Mueller Box: Draw from uniform distribution

#The inverse transformation method
#The Quantile Function: inverse of the distribution function
#It exists for the uniform distribution: continuity property

#Generate the quantile function for the uniform distribution

set.seed(163)

#continuation IN Simulation.R







