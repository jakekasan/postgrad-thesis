# get time series
x<-numeric()
x[1]<-0
for(i in 2:100){
  x[i]<-x[i-1]*0.95+rnorm(1,0,1)
}

# get differenced series

x_diff<-numeric()

for(i in 2:(length(x))){
  x_diff[i]<-x[i]-x[i-1]
}

x_lagged<-numeric()

x_dl<-numeric()


for(i in 3:length(x)){
  x_dl[i]<-x_diff[i]-x_diff[i-1]
  x_lagged[i]<-x[(i-2)]
}

#plot(x,type='l')

x_diff<-x_diff[-(1:2)]
x_lagged<-x_lagged[-(1:2)]
x_dl<-x_dl[-(1:2)]

x_diff.lm<-lm(x_diff ~ x_lagged + x_dl)
summary(x_diff.lm)



x1<-numeric(50)
x2<-numeric(50)
x3<-numeric(50)
x1[1]<-0
x2[1]<-0
x3[1]<-0
for(i in 2:length(x1)){
  x1[i]<-x1[i-1]*0.9+rnorm(1,0,1)
  x2[i]<-x2[i-1]*0.9+rnorm(1,0,1)
  x3[i]<-x3[i-1]*0.9+rnorm(1,0,1)
}

df<-data.frame(x1,x2,x3)

a<-c(df[2:length(df)] - df[1:(length(df)-1)])

