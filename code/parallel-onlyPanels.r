setup<-function(rho,t,n,intercept=0,trend=0){
  panel<-list()
  
  
  for(i in 1:n){
    temp<-numeric(t)
    temp[1]<-0+intercept+trend
    for(j in 2:t){
      temp[j]<-rho*temp[(j-1)]+intercept+trend*j+rnorm(1,0,1)
    }
    panel[i]<-list(temp)
  }
  
  df_temp<-data.frame(panel)
  colnames(df_temp)<-1:n
  return(df_temp)
}

##################################################
#########		LIBRARIES			########
##################################################

.libPaths("F:/nupak")
library(parallel)
library(doParallel)
library(foreach)



##################################################
#########		CLUSTER INIT		########
##################################################


no_cores<-detectCores()-1
cluster<-makeCluster(no_cores)
registerDoParallel(cluster)
clusterEvalQ(cluster,.libPaths(mylibpath))
clusterEvalQ(cluster,library(tseries))
clusterEvalQ(cluster,library(plm))
clusterEvalQ(cluster,library(foreach))
clusterExport(cluster,"setup")
clusterExport(cluster,"mylibpath")
clusterExport(cluster,"my.mw")
clusterExport(cluster,"myadf")
clusterExport(cluster,"adf.lag.find")
clusterExport(cluster,"lagit")

par_panel_mc<-function(obs,ind,mc,rho){


	# t_start,t_end,n_start,n_end,mc,rho_start,rho_end
	

	message("Started computing at ", Sys.time())

	starting_time<-proc.time()[[3]]

	# Data Frame format: Rho, Obs, Ind, ADF-mean, ADF-sd, PP-mean, PP-sd, LLC-mean, LLC-sd, MW-mean, MW-sd, MC?
	
	total<-(length(obs))*(length(ind))*(length(rho))
	#results<-data.frame(rho=numeric(total),observations=numeric(total),individuals=numeric(total),adf_mean=numeric(total),adf_sd=numeric(total),pp_mean=numeric(total),pp_sd=numeric(total),llc_mean=numeric(total),llc_sd=numeric(total),mw_mean=numeric(total),mw_sd=numeric(total),ips_mean=numeric(total),ips_sd=numeric(total))
	results<-data.frame(rho=numeric(total),observations=numeric(total),individuals=numeric(total),llc_mean=numeric(total),llc_sd=numeric(total),mw_mean=numeric(total),mw_sd=numeric(total),ips_mean=numeric(total),ips_sd=numeric(total))
	#results<-data.frame(rho=numeric(),observations=numeric(),individuals=numeric(),adf_mean=numeric(),adf_sd=numeric(),pp_mean=numeric(),pp_sd=numeric(),llc_mean=numeric(),llc_sd=numeric(),mw_mean=numeric(),mw_sd=numeric())
	
	
	count<-1
	for(r in rho){
		rho<-1-(r/1000)
		for(a in obs){
	      	for(b in ind){

				#adfs<-numeric(mc*b)
				#pps<-numeric(mc*b)
				llc<-numeric(mc)
				mw<-numeric(mc)
				ips<-numeric(mc)

				#new_var<-matrix(ncol=3,nrow=mc)

				new_var<-foreach(i=1:mc,.packages=c("plm","tseries"),.combine=rbind) %dopar% {
					panel<-setup(rho,a,b, trend = 1,intercept = 1)
					#a_temp<-numeric(length(panel))
					#p_temp<-numeric(length(panel))
					#for(j in 1:length(panel)){
					#	test<-tseries::adf.test(panel[[j]])
					#	test2<-tseries::pp.test(panel[[j]])
					#	a_temp[j*i]<-tseries::adf.test(panel[[j]])$p.value
      		#  p_temp[j*i]<-tseries::pp.test(panel[[j]])$p.value
      		#}
     					#test3<-plm::purtest(panel,data=NULL,exo="trend",pmax=1,test="levinlin",lags="AIC")
      				#test4<-plm::purtest(panel,data=NULL,exo="trend",pmax=1,test="madwu",lags="AIC")
     					#test5<-plm::purtest(panel,data=NULL,exo="trend",pmax=1,test="ips",lags="AIC")
					llc<-(plm::purtest(panel,data=NULL,exo="trend",pmax=1,test="levinlin",lags="AIC"))$statistic$p.value[[1]]
      		#mw<-my.mw(panel,exo="none")
					mw<-0
					#mw<-(plm::purtest(panel,data=NULL,exo="trend",pmax=1,test="madwu",lags="AIC"))$statistic$p.value[[1]]
					ips<-(plm::purtest(panel,data=NULL,exo="trend",pmax=1,test="ips",lags="AIC"))$statistic$p.value[[1]]


					#llc<-test3$statistic$p.value[[1]]
      				#mw<-test4$statistic$p.value[[1]]
					#ips<-test5$statistic$p.value[[1]]
					#a_mean<-mean(a_temp)
					#p_mean<-mean(p_temp)
					
					end<-c(llc,mw,ips) #,a_temp,p_temp) #,a_mean,p_mean)
					rm(llc);rm(mw);rm(ips)
					
					return(end)
				}
				#print(dim(new_var))
				llc<-unname(new_var[,1])
				#new_var<-new_var[,-1]
				mw<-unname(new_var[,2])
				ips<-unname(new_var[,3])
				rm(new_var)
				#new_var<-new_var[,-(1:4)]
				#print(dim(new_var))
				#len<-length(new_var[1,])
				#half<-len/2
				#adfs<-unname(rbind(new_var[,1:half]))[1,]
				#new_var<-new_var[,-(1:len)]
				#pps<-unname(rbind(new_var[,(half+1):len]))[1,]
				#df<-data.frame(rho=rho,observations=a,individuals=b,llc_mean=mean(llc),llc_sd=sqrt(var(llc)),mw_mean=mean(mw),mw_sd=sqrt(var(mw)),ips_mean=mean(ips),ips_sd=sqrt(var(ips)))
      	#df<-data.frame(rho=rho,observations=a,individuals=b,adf_mean=mean(adfs),adf_sd=sqrt(var(adfs)),pp_mean=mean(pps),pp_sd=sqrt(var(pps)),llc_mean=mean(llc),llc_sd=sqrt(var(llc)),mw_mean=mean(mw),mw_sd=sqrt(var(mw)),ips_mean=mean(ips),ips_sd=sqrt(var(ips)))
      	df<-data.frame(rho=rho,observations=a,individuals=b,llc_mean=mean(llc),llc_sd=sqrt(var(llc)),mw_mean=mean(mw),mw_sd=sqrt(var(mw)),ips_mean=mean(ips),ips_sd=sqrt(var(ips)))
      	results[count,]<-df
				rm(df)
				#gc()
				message("Finished ", count ," of ", (length(obs))*(length(ind))*(length(rho)))
				count<-count+1
      		}
		}
	}

	finished_time<-proc.time()[[3]]
	time_to_complete<-finished_time - starting_time
	message("Finished ",total*mc," computations in ", time_to_complete)
	return(results)
}

par_test<-par_panel_mc(c(8:25),c(2:50),100,750)

stopCluster(cluster)
stopImplicitCluster()
write.csv(par_test,"t8-25n2-50mc100-rho75-wIPS.csv")
write.csv(par_test,"temp_file.csv")

#########################################
#####		Clustering			#####
#########################################

test_function<-function(x) {
	panel<-setup(rho,a,b)
	for(j in 1:length(panel)){
		test<-tseries::adf.test(panel[[j]])
		test2<-tseries::pp.test(panel[[j]])
		adfs[length(adfs)+1]<-test$p.value
       	pps[length(pps)+1]<-test$p.value
      }
     	test3<-plm::purtest(panel,data=NULL,exo="none",pmax=1,test="levinlin",lags="AIC")
      test4<-plm::purtest(panel,data=NULL,exo="none",pmax=1,test="madwu",lags="AIC")
     	llc[x]<-test3$statistic$p.value[[1]]
      mw[x]<-test4$statistic$p.value[[1]]
	
	message("Instance ",count," of ",total)
	count<-count+1
}


single_test<-function(test,n,t,rho){
	results<-numeric()
	results<-foreach(i=1:10000) %dopar% {
		result<-(plm::purtest(setup(rho,t,n,3,2),data=NULL,test=test,exo="trend",pmax=1,lags="AIC"))$statistic$statistic[[1]]
		return(result)
	}
	hist(numerize(results))
}
numerize<-function(object){
	temp<-numeric()
	for(i in 1:length(object)){
		temp[i]<-object[[i]]
	}
	return(temp)
}
single_test("levinlin",25,50,0.9)

stopCluster(cluster)

list_of_results<-numeric()
clusterExport(cluster,"list_of_results")
clusterExport(cluster,"setup")
clusterExport(cluster,"x")
clusterExport(cluster,"a")
clusterEvalQ(cluster,library(tseries))
a<-0

x<-function(x){
	temp<-setup(0.9,20,2)
	temp<-plm::purtest(temp,data=NULL,test="levinlin",lags="AIC",pmax=1,exo="none")$statistic$p.value[[1]]$p.value
	return(temp)
}
what<-parLapply(cluster,1:10,x)

counter<-numeric()

results<-foreach(i=1:20,.combine=cbind) %dopar% {
	temp<-setup(0.9,20,2)
	for(j in length(temp)){
		counter[length(counter)+1]<-tseries::adf.test(temp[,j])$p.value
	}
	llc<-plm::purtest(temp,data=NULL,test="levinlin",exo="none",lags="AIC",pmax=1)
	return(llc$statistic$p.value[[1]])
	
}

x<-numeric()
x[1]<-0
for(i in 2:100000){
	x[i]<-1*x[i-1]+rnorm(1,0,1)

}

plot(x,type='l')


for(i in 2){
	print(i)
}


