x1<-numeric(10)
x2<-numeric(20)
x3<-numeric(100)
x4<-numeric(500)
x5<-numeric(1000000)
x1[1]<-0
x2[1]<-0
x3[1]<-0
x4[1]<-0
x5[1]<-0
gen<-function(x){
	for(i in 2:length(x)){
		x[i]<-0.99999*x[i-1]+rnorm(1,0,1)
	}
	return(x)
}

x1<-gen(x1)
x2<-gen(x2)
x3<-gen(x3)
x4<-gen(x4)
x5<-gen(x5)

plot.ts(x1,x2,x3,x4,plot.type="multiple",type='l')

plot(x1,type='l')
plot(x2,type='l')
plot(x3,type='l')
plot(x4,type='l')
#plot(x5,type='l')

acf(x1)
acf(x2)
acf(x3)
acf(x4)

pacf(x1)
pacf(x2)
pacf(x3)
pacf(x4)


df<-data.frame(x1,x2,x3,x4)

plot(df)

library(ggplot)
.libPaths("F:/nupak")

a<-0

for(i in 1:100){
	a<-a+i
}

a



ortho<-function(object){
	new_object<-matrix(ncol=ncol(object),nrow=nrow(object))
	new_object[,1]<-object[,1]
	for(i in 2:length(object)){
		temp<-object[,i]
		for(j in 1:(i-1)){
			temp<-temp-matlib::Proj(object[,i],new_object[,j])
		new_object[,i]<-temp
		}
	}

	print(zapsmall(crossprod(new_object)))

	return(new_object)
}

z<-ortho(panel)

