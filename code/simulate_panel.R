generate<-function(rho,n,t){
	temp<-list()
	for(i in 1:n){
		tempts<-numeric()
		tempts[1]<-0
		for(j in 2:t){
			tempts[j]<-rho*tempts[j-1]+rnorm(1,0,1)
		}
		temp[i]<-list(tempts)
	}
	df<-data.frame(temp)
	return(df)
}

runtest<-function(n_start,n_end,t_start,t_end,mc,rho){
	for(i in n_start:n_end){
		for(j in t_start:t_end){
			message("\n===================================================")
			message("\t\tN: ",i,"\tT: ",j,"\n")
			a_list<-list()
			l<-numeric()
			for(k in 1:mc){
				c(a,b) := quicktest(generate(rho,i,j))
				a_list[k]<-list(a)
				l[k]<-b
			}

			printer(a_list,l)
		}
	}	
}

quicktest<-function(panel){
	adf_p<-numeric()
	for(i in 1:length(panel)){
		adf<-tseries::adf.test(panel[[i]])
		#message("ADF: ",adf$p.value)
		adf_p[i]<-adf$p.value
	}
	llc<-plm::purtest(panel,data=NULL,test="levinlin",exo="none",lags="AIC")
	#message("LLC: ",llc$statistic$p.value[[1]])

	return(list(adf_p,llc$statistic$p.value[[1]]))
	
}

':=' <- function(lhs,rhs){
	frame <- parent.frame()
	lhs <- as.list(substitute(lhs))
	if (length(lhs) > 1){
		lhs <- lhs[-1]
	}
	if(length(lhs) == 1){
		do.call(`=`, list(lhs[[1]],rhs), envir=frame)
		return(invisible(NULL))
	}
	if (is.function(rhs) || is(rhs, 'formula')){
		rhs<-list(rhs)
	}
	if (length(lhs) > length(rhs)){
		rhs<-c(rhs,rep(list(NULL),length(lhs)-length(rhs)))
	}
	for (i in 1:length(lhs)){
		do.call(`=`,list(lhs[[i]],rhs[[i]]),envir=frame)
	}
	return(invisible(NULL))
}



printer<-function(a_p,llc_p){
	sat10<-0
	nsat10<-0
	sat5<-0
	nsat5<-0
	sat1<-0
	nsat1<-0

	#print(a_p[1])
	#print(llc_p)

	for(i in 1:length(a_p)){
		for(j in 1:length(a_p[[i]])){
			temp<-a_p[[i]]
			#print(temp[j])
			if(temp[j]<=0.01){
				sat1<-sat1+1
			} else {
				nsat1<-nsat1+1
			}
			if(temp[j]<=0.05){
				sat5<-sat5+1
			} else {
				nsat5<-nsat5+1
			}
			if(temp[j]<=0.1){
				sat10<-sat10+1
			} else {
				nsat10<-nsat10+1
			}

		}
	}

	message("Augmented Dickey-Fuller\t\t\t10%\t5%\t1%")
	message("Stationary:\t\t\t\t",sat10,"\t",sat5,"\t",sat1)
	message("Non-Stationary:\t\t\t\t",nsat10,"\t",nsat5,"\t",nsat1)

	slat10<-0
	nslat10<-0
	slat5<-0
	nslat5<-0
	slat1<-0
	nslat1<-0
	for(i in 1:length(llc_p)){
		if(llc_p[[i]]<=0.01){
			slat1<-slat1+1
		} else {
			nslat1<-nslat1+1
		}
		if(llc_p[[i]]<=0.05){
			slat5<-slat5+1
		} else {
			nslat5<-nslat5+1
		}
		if(llc_p[[i]]<=0.1){
			slat10<-slat10+1
		} else {
			nslat10<-nslat10+1
		}
	}

	message("\nLevin-Lin-Chu\t\t\t\t10%\t5%\t1%")
	message("Stationary:\t\t\t\t",slat10,"\t",slat5,"\t",slat1)
	message("Non-Stationary:\t\t\t\t",nslat10,"\t",nslat5,"\t",nslat1)
	message()
}

runtest(10,12,1100,1110,100,0.99)

randomized_panel<-function(panel,n,t,plags){

	for(h in 1:t){
		temp<-panel
		for(i in 1:n){
			if(length(panel)==2){
				warning("Stopping loop, only 2 individuals.")
			} else {
				temp<-temp[-(round(runif(1,1,length(temp))))]
			}
		}
	
		test<-plm::purtest(temp,data=NULL,test="levinlin",pmax=plags,exo="trend",lags="AIC")
		print(test$statistic$p.value[[1]])
	}

}