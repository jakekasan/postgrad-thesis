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



panel_mc<-function(t_start,t_end,n_start,n_end,mc,rho_start,rho_end){
  
  # Data Frame format: Rho, Obs, Ind, ADF-mean, ADF-sd, PP-mean, PP-sd, LLC-mean, LLC-sd, MW-mean, MW-sd, MC?
  
  results<-data.frame(rho=numeric(),observations=numeric(),individuals=numeric(),adf_mean=numeric(),adf_sd=numeric(),pp_mean=numeric(),pp_sd=numeric(),llc_mean=numeric(),llc_sd=numeric(),mw_mean=numeric(),mw_sd=numeric())
  total<-(t_end-t_start+1)*(n_end-n_start+1)*mc*(rho_end-rho_start+1)
  count<-1
  for(r in rho_start:rho_end){
    rho<-1-(r/100)
    for(a in t_start:t_end){
      for(b in n_start:n_end){
        # declaring ADF mean+sd
        adf_means<-numeric()
        adf_sds<-numeric()
        
        # declaring PP mean+sd
        pp_means<-numeric()
        pp_sds<-numeric()
        llc<-numeric()
        mw<-numeric()
        for(i in 1:mc){
          panel<-setup(rho,a,b)
          adfs<-numeric()
          pps<-numeric()
          for(j in 1:length(panel)){
            test<-tseries::adf.test(panel[[j]])
            test2<-tseries::pp.test(panel[[j]])
            adfs[length(adfs)+1]<-test$p.value
            pps[length(pps)+1]<-test$p.value
            #adfs[j]<-test$p.value
            #pps[j]<-test2$p.value
            
          }
          test3<-plm::purtest(panel,data=NULL,exo="none",pmax=1,test="levinlin",lags="AIC")
          test4<-plm::purtest(panel,data=NULL,exo="none",pmax=1,test="madwu",lags="AIC")
          llc[i]<-test3$statistic$p.value[[1]]
          mw[i]<-test4$statistic$p.value[[1]]

          message("Instance ",count," of ",total)
          count<-count+1
          
        }
      
        df<-data.frame(rho=rho,observations=a,individuals=b,adf_mean=mean(adfs),adf_sd=sqrt(var(adfs)),pp_mean=mean(pps),pp_sd=sqrt(var(pps)),llc_mean=mean(llc),llc_sd=sqrt(var(llc)),mw_mean=mean(mw),mw_sd=sqrt(var(mw)))
        results[nrow(results)+1,]<-df
        
        #results<-rbind(results,df)
        
      }
    }
  }
  return(results)
}

batch<-function(){
  for(i in 0:5){
    temp<-panel_mc(8,150,2,20,1000,(i*10),(i*10)+9)
    name<-paste("result-pt",i,sep="")
    filename<-paste(name,".csv",sep="")
    write.csv(temp,filename)
  }
}

tempfilename<-paste("bigtrial2",".csv",sep="")
write.csv(mctest,tempfilename)


####################################################
####			Parallel Code			####
####################################################

library(parallel)

no_cores<-detectCores()-10
cluster<-makeCluster(no_cores)
stopCluster(cluster)

clusterExport(cluster,"setup")
clusterEvalQ(cluster,.libPaths("F:/nupak"))
clusterEvalQ(cluster,library(tseries))
clusterExport(cluster,"par_result")

test<-setup(0.9,20,4)

len<-length(test)

par_result<-numeric()

par_function<-function(x){
	test_holder<-numeric()
	temp<-setup(0.9,20,4)
	for(i in 1:length(temp)){
		a_test<-tseries::adf.test(temp[[i]])
		test_holder[i]<-a_test$p.value
	}
	par_result[x]<-mean(test_holder)
}

parLapply(cluster,1:1000,par_function)
lapply(1:1000,par_function)


print(par_result)
