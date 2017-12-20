# (counter)example to EMV illustrating direct linear dependence
V = 15 # vintages
T = 10 # maximum age

# stacked format in order vintage, age, period
iVintage = kronecker(diag(V),rep(1,T))
iAge = kronecker(rep(1,V),diag(T))
iPeriod <- diag(T)
for (v in 2:V){
  iPeriod <- rbind(
    cbind(iPeriod, matrix(0,nrow(iPeriod),1)),
    cbind(matrix(0,T,ncol(iPeriod)-T+1), diag(T)) )
} 

# check whether the DVs work
c <- iVintage %*% (as.vector(c(1:V)))
a <- iAge %*% (as.vector(c(1:T)))
p <- iPeriod %*% (as.vector(c(1:(V+T-1))))
d = cbind(c,a,p)                  # dataset in original format
D = cbind(iVintage,iAge,iPeriod)  # dummy variables

# report the rank
library(pracma)
r = rank(D)
message("Matrix rank is ", r, " while the number of columns equals ",ncol(D))
print(sprintf("Matrix rank is %d while the number of columns equals %d.", r, ncol(D)))
n <- nullspace(D) # null space holds all linear combinations of D columns such that D * n = 0

