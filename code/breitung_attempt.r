getData<-function(T,n){
	df<-data.frame(1:T)
	for(i in 1:n){
		temp<-numeric()
		temp[1]<-0+rnorm(1,0,1)
		#temp[2]<-0.8*temp[1]+rnorm(1,0,1)
		#temp[3]<-0.8*temp[2]-0.7*temp[1]+rnorm(1,0,1)
		for(j in 2:T){
			temp[j]<-0.999*temp[j-1]+rnorm(1,0,1)
			#temp[j]<-0.8*temp[j-1]-0.7*temp[j-2]+0.3*temp[j-3]+rnorm(1,0,1)
		}
		df[,i]<-c(temp)
	}
	return(df)
}

x1<-c(1:100)
x2<-x1**1.1
x<-matrix(x1)
y<-x*0.3+rnorm(length(x),0,1)
y.lm<-lm.fit(x,y)

y.lm

z<-data.frame(x,y)

x<-getData(20000,1)


v <- c(c( .004, .003, .002, .002, .001, .001, .001,0.000,0.000,0.000,0.000,0.000,0.000),
       c(1.049,1.035,1.027,1.021,1.017,1.014,1.011,1.008,1.007,1.006,1.005,1.001,1.000),
       c(-.554,-.546,-.541,-.537,-.533,-.531,-.527,-.524,-.521,-.520,-.518,-.509,-.500),
       c(0.919,0.889,0.867,0.850,0.837,0.826,0.810,0.798,0.789,0.782,0.776,0.742,0.707),
       c(-.703,-.674,-.653,-.637,-.624,-.614,-.598,-.587,-.578,-.571,-.566,-.533,-.500),
       c(1.003,0.949,0.906,0.871,0.842,0.818,0.780,0.751,0.728,0.710,0.695,0.603,0.500)
       )

Tn <- c(  25,  30,  35,  40,  45,  50,  60,   70,   80,   90,  100,  250,   500)

adj.levinlin <- array(v, dim=c(13,2,3),
                      dimnames = list(Tn, c("mu","sigma"),
                        c("none", "intercept", "trend")))

x<-x[,1]


# difference it

x_diff<-c(NA,x[2:length(x)]-x[1:(length(x)-1)])
x_diff

x_lags<-sapply(1:1,function(x,y) c(rep(NA,x),y[(1:(length(y)-x))]),x)

x_dl<-sapply(1:1,function(x,y) c(rep(NA,x),y[(1:(length(y)-x))]),x_diff)

y.lm<-lm(x_diff ~ x_lags + x_dl)

summary(y.lm)