setup<-function(rho,t,n){
  panel<-list()
  
  
  for(i in 1:n){
    temp<-numeric()
    temp[1]<-0.00001
    for(j in 2:t){
      temp[j]<-rho*temp[(j-1)]+rnorm(1,0,1)
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
clusterEvalQ(cluster,.libPaths("F:/nupak"))
clusterEvalQ(cluster,library(tseries))
clusterEvalQ(cluster,library(plm))
clusterEvalQ(cluster,library(foreach))
clusterExport(cluster,"setup")

par_panel_mc<-function(t_start,t_end,n_start,n_end,mc,rho_start,rho_end){

	message("Started computing at ", Sys.time())

	starting_time<-proc.time()[[3]]

	# Data Frame format: Rho, Obs, Ind, ADF-mean, ADF-sd, PP-mean, PP-sd, LLC-mean, LLC-sd, MW-mean, MW-sd, MC?

	results<-data.frame(rho=numeric(),observations=numeric(),individuals=numeric(),adf_mean=numeric(),adf_sd=numeric(),pp_mean=numeric(),pp_sd=numeric(),llc_mean=numeric(),llc_sd=numeric(),mw_mean=numeric(),mw_sd=numeric())
	total<-(t_end-t_start+1)*(n_end-n_start+1)*mc*(rho_end-rho_start+1)
	count<-1
	for(r in rho_start:rho_end){
		rho<-1-(r/100)
		for(a in t_start:t_end){
	      	for(b in n_start:n_end){

				adfs<-numeric()
				pps<-numeric()
				llc<-numeric()
				mw<-numeric()

				new_var<-foreach(i=1:mc,.packages=c("plm","tseries"),.combine=rbind) %dopar% {
					panel<-setup(rho,a,b)
					a_temp<-numeric()
					p_temp<-numeric()
					for(j in 1:length(panel)){
						test<-tseries::adf.test(panel[[j]])
						test2<-tseries::pp.test(panel[[j]])
						a_temp[length(a_temp)+1]<-test$p.value
      		      		p_temp[length(p_temp)+1]<-test2$p.value
      		    		}
     					test3<-plm::purtest(panel,data=NULL,exo="none",pmax=1,test="levinlin",lags="AIC")
      				test4<-plm::purtest(panel,data=NULL,exo="none",pmax=1,test="madwu",lags="AIC")
     					llc<-test3$statistic$p.value[[1]]
      				mw<-test4$statistic$p.value[[1]]

					a_mean<-mean(a_temp)
					p_mean<-mean(p_temp)
					
					end<-c(llc,mw,a_mean,p_mean)
					return(end)
				}
				llc<-unname(new_var[,1])
				new_var<-new_var[,-1]
				mw<-unname(new_var[,1])
				new_var<-new_var[,-1]
				len<-length(new_var[1,])/2
				adfs<-unname(rbind(new_var[,1:len]))[1,]
				new_var<-new_var[,-(1:len)]
				pps<-unname(rbind(new_var))[1,]

      			df<-data.frame(rho=rho,observations=a,individuals=b,adf_mean=mean(adfs),adf_sd=sqrt(var(adfs)),pp_mean=mean(pps),pp_sd=sqrt(var(pps)),llc_mean=mean(llc),llc_sd=sqrt(var(llc)),mw_mean=mean(mw),mw_sd=sqrt(var(mw)))
				results[(nrow(results)+1),]<-df
				message("Finished ", count ," of ", (t_end-t_start+1)*(n_end-n_start+1)*(rho_end-rho_start+1))
				count<-count+1

      		}
		}
	}

	finished_time<-proc.time()[[3]]
	time_to_complete<-finished_time - starting_time
	message("Finished ",total," computations in ", time_to_complete)
	return(results)
}

par_test<-par_panel_mc(8,25,2,50,100,11,15)

stopCluster(cluster)

write.csv(par_test,"t8-25n2-50mc100-rho089-85.csv")

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



