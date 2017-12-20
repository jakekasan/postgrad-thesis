# import function

list_all<-function(object){
	names<-object[[1]]
	for(i in 2:(dim(object)[1])){
		if(object[[i]]!=object[[i-1]]){
			names[length(names)+1]<-object[[i]]
		}
	}
	return(names)
	
}

get_data<-function(name,object,index,target){
	temp<-numeric()
	for(i in 1:length(object[,1])){
		if(object[i,index]==name){
			temp[length(temp)+1]<-object[i,target]
		}
	}
	return(temp)
}

get_graphs<-function(object){
	levs<-levels(mydata[,1])
	for(i in levs){
		#print(length(get_data(i,object,1,length(object))))
		temp<-(get_data(i,object,1,length(object)))
		time<-get_data(i,object,1,4)
		#plot(time,temp,main="Title",type='l',xlab="Quarter",ylab="deltaY")
		#print(summary(lm(temp ~ time)))
		#readline()
		#png(paste(i,"acf.png",sep="-"))
		#acf(temp, main=i)
		#dev.off()
		#cat("ACF for ", i)
		#readline()
		#wait for input
		#png(paste(i,"pacf.png",sep="-"))
		#pacf(temp, main=i)
		#dev.off()
		#cat("PACF for ", i)
		#readline()

		#Individual plots

		png(paste(i,"plot.png",sep="-"))
		plot(temp, main=i,type='l')
		dev.off()
		cat("Plot for ", i)
		#readline()
		#wait for input
		#png(paste(i,"pacf.png",sep="-"))
		#pacf(temp, main=i)
		#dev.off()
		#cat("PACF for ", i)
		#readline()
		# wait for input
	}
}

get_diff<-function(object){
	temp<-numeric()
	temp[1]<-0
	for(i in 2:length(object)){
		temp[i]<-object[i]-object[i-1]
	}
	return(temp)
}

get_graphs(mydata)