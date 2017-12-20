run.mw<-function(ind,obs,rho,mc){
  total<-length(rho)*length(ind)*length(obs)
  total2<-total*mc
  results<-data.frame(rho = numeric(total),obs = numeric(total),ind = numeric(total),mw_mean = numeric(total),mw_sd = numeric(total))
  count<-1
  count2<-1
  for(r in rho){
    for(n in ind){
      for(t in obs){
        temp<-numeric(mc)
        #for(i in c(1:mc)){
        #  panel<-setup(r,t,n,intercept = 0, trend = 0)
        #  temp[i]<-my.mw(panel,exo="none",pmax=1)
        #  message("Finished ", count," of ", length(rho)*length(ind)*length(obs)*mc)
        #  count<-count+1
        #}
      
        temp<-foreach(i=1:mc,.packages=c("plm","tseries"),.combine=rbind) %dopar% {
          panel<-setup(r,t,n,intercept = 0, trend = 0)
          return(my.mw(panel,exo="none",pmax=1))
        }
        message("Finished ",count2," of ",total)
        results[count2,]<-data.frame(rho = r, obs = t, ind = n, mw_mean = mean(temp), mw_sd = sqrt(var(temp)))
        count2<-count2+1
        
      }
    }
  }
  return(results)
}

experiment<-run.mw(c(2:50),c(8:25),0.75,100)

write.csv(experiment,"t8-25n2-50mc100-rho75-onlyMW.csv")

ncol(setup(0.7,20,2))
z<-c(1:10)
z[(1:-1)]

a<-series(0.99,100,exo="exo")

tseries::adf.test(a,k=1)
myadf(a,pmax=1,exo="exo")

