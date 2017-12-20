setup<-function(rho,t,n){
	panel<-list()


	for(i in 1:n){
		temp<-numeric()
		temp[1]<-0
		for(j in 2:t){
			temp[j]<-rho*temp[(j-1)]+rnorm(1,0,1)
		}
		panel[i]<-list(temp)
	}

	df_temp<-data.frame(panel)
	colnames(df_temp)<-1:n
	return(df_temp)
}

data_variables<-setup(0.4)

ur_test_list<-c("levinlin","madwu")

ts_test<-function(variable){
	tst<-numeric()
	count<-1
	for(i in 1:length(variable)){
		#variable[i]
		# run adf
		adf<-tseries::adf.test(variable[[i]],k=1)
		#print(adf$p.value)
		tst[count]<-adf$p.value
		count<-count+1
	}
	
	#print("Now panel data...")

	for(i in ur_test_list){
		test<-plm::purtest(variable,data=NULL,pmax=1,exo="none",test=i,lags="AIC")
		#print(test$statistic$p.value)
		tst[count]<-test$statistic$p.value
		count<-count+1
	}

	return(tst)

	
	
	
}

ts_test(a)

run_range<-function(){
	t<-250
	results<-data.frame(ts_test(setup(0.8)))
	for(i in 81:100){
		num<-(i/100)
		var<-setup(num,t)
		temp<-ts_test(var)
		results<-cbind(results,temp)
	}
	print(length(results))
	colnames(results)<-80:100
	return(results)
}

mc_sim<-function(rho,num,t,n){
	x<-numeric()
	for(i in 1:num){
		temp<-plm::purtest(setup(rho,t,n),data=NULL,pmax=1,exo="none",test="hadri",lags="AIC")
		x[i]<-temp$statistic$p.value
	}
	good<-0
	bad<-0
	for(i in 1:length(x)){
		if(x[i]<0.05){
			good<-good+1
		} else {
			bad<-bad+1
		}
	}
	success<-good/(good+bad)
	print(success)
	#return(x)
}

for(i in 10:50){
	mc_sim(0.60,100,i)
}


results<-run_range()



plm::purtest(data_variables,pmax=4,exo="none",test="levinlin",lags="SIC")

