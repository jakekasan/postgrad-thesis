brownian<-function(obs){
	x<-numeric()
	x[1]<-0
	for(i in 2:obs){
		x[i]<-x[(i-1)]*0.99 + rnorm(1,0,1)
	}
	return(x)
}

gen_panel<-function(start,increment,quarters){
	x<-numeric()
	b<-brownian(quarters)
	x[1]<-start
	for(i in 2:quarters){
		x[i]<- b[i]
		# max(x[(i-1)]-increment+((increment/2)*(b[i]/(max(b)))),0)
		
	}

	new_x<-data.frame(x)
	return(x)
}



# GENERATE PANELS

temp<-list()
for(i in 1:30){
	temp[i]<-list(gen_panel(0,1000,(36-(i))))
}
temp


create_df<-function(data){
	for(i in 2:length(data)){
		df<-data.frame(data[[i]])
		for(j in 1:(i-1)){
			df<-cbind(df,data[[j]][-(1:(length(data[[j]])-length(data[[i]])))])
		}


		
		print("data frames:")
		print(1:i)
		llc<-plm::purtest(df,data=NULL,pmax=1,exo="none",test="levinlin",lags="AIC")
		print(llc)
	
	}

}

create_df(temp)

for(i in 1:length(temp)){
	# do something
}




# list ordering


a<-c(8,3,7,1,9,4,6,2)

for(i in 1:length(temp)){
	print(length(temp[[i]]))
}

