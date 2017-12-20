series<-function(r,t,exo="none"){
	x<-numeric(t)
	e<-rnorm(t,0,1)
	ifelse(exo!="none",intercept<-rep(1,t),intercept<-rep(0,t))
	ifelse(exo=="trend",trend<-c(1:t)/100,trend<-rep(0,t)/100)
	x[1]<-0
	for(i in 2:t){
		x[i]<-r*x[i-1]+e[i]
	}

	x<-x+intercept+trend

	return(x)
}

a<-series(0.999,20)
#plot(a,type='l')
b<-series(0.99,20)
c<-series(0.999,20)
d<-series(0.999,20)
e<-series(0.999,20)

x<-data.frame(a,b,c,d,e)

#y<-0.3*a+0.4*b+0.5*c+0.6*d+0.9*e

#test<-mat.fit(as.matrix(x),y)

#lm(a ~ b)

#y<-0.5*x$a+0.5*x$b+3

#myadf(a)

#y<-NA
#diff(y)

lagit<-function(Dy,pmax){
	dLy<-sapply(1:pmax,function(x,y) c(rep(0,x),y[1:(length(y)-x)]),Dy)
	return(dLy)
}

thing<-myadf(a,pmax=1,exo="trend")
thing2<-tseries::adf.test(a,k=1)
#thing$DF_stat
#thing$p.value
#thing2


#try<-summary(lm(y ~ as.matrix(x)))

myadf<-function(y,pmax=10,aux=FALSE,exo="none"){
  lags<-adf.lag.find(y,exo=exo,pmax=pmax)
	Dy<-c(0,y[2:length(y)]-y[1:(length(y)-1)])
	Ly<-c(0,y[1:(length(y)-1)])
	dLy<-as.matrix(lagit(Dy,lags))
	#dLy<-as.matrix(lagit(Dy,adf.lag.find(y,lags)))
	Dy<-Dy[(lags+1):length(Dy)]
	Ly<-Ly[(lags+1):length(Ly)]
	dLy<-dLy[(lags+1):(dim(dLy)[1]),]

	ifelse(exo!="none",intercept<-rep(1,length(Dy)),intercept<-rep(0,length(Dy)))
	ifelse(exo=="trend",trend<-1:length(Dy),trend<-rep(0,length(Dy)))
	#intercept[1]<-0
	#trend[1]<-0
	#print(data.frame(Dy,Ly,dLy,intercept,trend))
	adf.lm<-lm(Dy ~ Ly + intercept + trend + dLy)	
	adf.lm.sum<-invisible(summary(adf.lm))
	adf.coefs<-coef(adf.lm.sum)
	#print(adf.coefs)
	#message(adf.coefs[,1][[2]])
	rho<-adf.coefs[,1][[2]]
	se<-coef(adf.lm.sum)[,2][[2]]
	df.stat<-adf.lm.sum$coefficients[2,3]
	adf.stat<-rho/se
	results<-list(DF_stat=adf.stat)
	sigma<-adf.lm.sum$sigma
	#rho<-adf.lm.sum$coef[1]
	#sdrho<-adf.lm$se[1]
	ifelse((rho==0 | se == 0), trho<-0, trho<-rho/se)
	results$sigma<-sigma
	results$rho<-rho
	results$sdrho<-se
	results$trho<-trho
	results$adf.coefs<-adf.coefs
	mymu<-adj.levinlin.value(length(y),exo)[1]
	mysig<-adj.levinlin.value(length(y),exo)[2]
	#message("Finding p.value for critical: ",trho, " ",rho, " ", se)
	p.value<-find.val(trho,exo = exo, t = length(y))
	results$p.value=p.value
	if(aux){
		dy.lm<-lm(Dy ~ dLy)
		ly.lm<-lm(Ly ~ dLy)
		X<-cbind(dLy,intercept,trend)
		res.d<-lm.fit(X,Dy)$residuals/sigma
		res.l<-lm.fit(X,Ly)$residuals/sigma
		dy.lm.sum<-summary(dy.lm)
		ly.lm.sum<-summary(ly.lm)
		delta_res<-dy.lm.sum$residuals/sigma
		level_res<-ly.lm.sum$residuals/sigma
		delta_res<-res.d
		level_res<-res.l
		results$residuals<-data.frame(delta_res,level_res)
		return(results)
	} else {
		#results$residuals<-adf.lm.sum$residuals
		return(results)
	}
}

myadf(a,exo="trend",aux=FALSE)
tseries::adf.test(a,k=1)

mat.fit<-function(x,y,dfcor=FALSE){
	z<-lm.fit(x,y)
	s<-summary(z)
	#print(s)
	p <- z$rank
	Qr <- z$qr
	n <- NROW(Qr$qr)
	rdf <- n - p
	p1 <- 1L:p
	r <- z$residuals
	rss <- sum(r^2)
	resvar <- ifelse(dfcor,rss/rdf,rss/n) # purtest used a dfcor variable here...
	sigma <- sqrt(resvar)
	R <- chol2inv(Qr$qr[p1,p1,drop = FALSE])
	thecoef <- z$coefficients[Qr$pivot[p1]]
	these <- sigma*sqrt(diag(R))
	#message("The standard error: ",these)
	#print(str(z))
	return(list(coef = thecoef, se = these, sigma = sigma, rss = rss, n = n, K = p, rdf = rdf))
}

myllc<-function(object,pmax=10,exo="none"){
	L<-dim(object)[1]
	n<-dim(object)[2]
	#message("L: ",L,"\nN: ",n)
	panel_adf<-mapply(function(x,y) myadf(x, y, aux=TRUE), object, rep(1,n), SIMPLIFY=FALSE)
	level_res<-(unlist(lapply(panel_adf, function(x) x$residuals$level_res)))
	delta_res<-(unlist(lapply(panel_adf, function(x) x$residuals$delta_res)))
	values<-adj.levinlin.value(L,exo)
	#message("Mymu: ",values[1],"\nMysig: ", values[2])
	mymu <-values[1]
	mysig <- values[2]
	sigmaST<-sapply(panel_adf, function(x) x[["sigma"]])
	sigmaLT<-sqrt(sapply(object,longrunvar,exo,q=NULL))
	si <- sigmaLT/sigmaST
	sbar <- mean(si)
	z<-mat.fit(as.matrix(level_res),delta_res,dfcor=FALSE)
	tildeT<-L-1-1
	sigmaeps2<-z$rss/(n*tildeT)
	rho<-z$coef
	sdrho<-z$se
	trho<-rho/sdrho
	stat<-c(z = (trho - n * tildeT * sbar / sigmaeps2 * sdrho * mymu)/mysig)
	names(stat) <- "z"
	message(stat)
	pvalue <- 2*pnorm(abs(stat), lower.tail = FALSE)
	message("trho: ",trho,"\nn: ",n,"\ntildeT: ",tildeT,"\nsbar: ",sbar,"\nsigmaeps2: ", sigmaeps2,"\nsdrho: ",sdrho,"\nmymu: ",mymu,"\nmysig: ",mysig,"\nrho: ",rho)
	message("Test statistic is: ", stat)
	message("The p.value is: ", pvalue)
}

#blah<-myllc(x)
this<-plm::purtest(x,test="levinlin",pmax=1,exo="none",lags="AIC")
#str(blah)
#this$adjval

#blah

#test.data<-x

#panel_adf<-mapply(function(x,y) myadf(x, y, aux=TRUE), test.data, 1, SIMPLIFY=FALSE)

######### LEVIN LIN DIST


Tn <- c(  25,  30,  35,  40,  45,  50,  60,   70,   80,   90,  100,  250,   500) 

 
 v <- c(c( .004, .003, .002, .002, .001, .001, .001,0.000,0.000,0.000,0.000,0.000,0.000), 
        c(1.049,1.035,1.027,1.021,1.017,1.014,1.011,1.008,1.007,1.006,1.005,1.001,1.000), 
        c(-.554,-.546,-.541,-.537,-.533,-.531,-.527,-.524,-.521,-.520,-.518,-.509,-.500), 
        c(0.919,0.889,0.867,0.850,0.837,0.826,0.810,0.798,0.789,0.782,0.776,0.742,0.707), 
        c(-.703,-.674,-.653,-.637,-.624,-.614,-.598,-.587,-.578,-.571,-.566,-.533,-.500), 
        c(1.003,0.949,0.906,0.871,0.842,0.818,0.780,0.751,0.728,0.710,0.695,0.603,0.500) 
        ) 

adj.levinlin <- array(v, dim=c(13,2,3),dimnames = list(Tn, c("mu","sigma"), c("none", "intercept", "trend")))

adj.levinlin.value<-function(t,exo = c("none","intercept","trend")){
	theTs <- as.numeric(rownames(adj.levinlin))
	#print(theTs)
	Ts <- selectT(t, theTs)
	if (length(Ts) == 1){
		#print(Ts)
		return(adj.levinlin[as.character(Ts),,exo])
	} else {
		#print(Ts)
		low<-adj.levinlin[as.character(Ts[1]),,exo]
		high<-adj.levinlin[as.character(Ts[2]),,exo]
		return(low + (1 - Ts[1])/(Ts[2] - Ts[1])*(high - low))
	}
}

selectT <- function(x, Ts){
	#print(x)
	if (x %in% Ts) {
		#print("x is in Ts")
		return(x)
	}
	if (x < Ts[1]){
		#print("oogah")
		return(Ts[1])
	}
	if (x > Ts[length(Ts)]){
		#print("boogah")
		return(Ts[length(ts)])
	}
	pos <- which((Ts - x) > 0)[1]
	#print(pos)
	#print("grrrr")
	return(Ts[c(pos-1,pos)])
}

longrunvar<-function(x,exo="none",q=NULL){
	T <- length(x)
	if(is.null(q)) q<-round(3.21*T^(1/3))
	#print(q)
	dx <- x[2:T]-x[1:(T-1)]
	if(exo == "intercept") dx <- dx - mean(dx)
	if(exo == "trend") dx <- lm.fit(cbind(1,1:length(dx)), dx)$residuals
	dx <- c(NA, dx)
	1/(T-1)*sum(dx[-1]^2) + 2 * sum(sapply(1:q, function(L) sum(dx[2:(T-L)] * dx[(L+2):T]) / (T-1) * (1-L/(q+1))))

}


my.mw<-function(object,choi=FALSE,exo="none",pmax=10){
	N<-ncol(object)

	p.values<-numeric(N)
	for(i in 1:N){
	  #temporary<-myadf(object[,i],exo=exo,pmax=pmax)$p.value
	  temporary<-tseries::adf.test(object[,i],k=pmax)$p.value
	  #message("The p.value given is: ", temporary, " at i: ",i," and N: ",N, " while the dims: ",summary(p.values))
		if(is.na(temporary)){
		  print("BAZINGA!!!!!!!!!")
		}
	  p.values[i]<-log(temporary)
		#message(p.values[i])
	}
	p.values<-sum(p.values)

	
	if(choi){
		stat<--(p.values+N)/sqrt(N)
		p.value.final<-pnorm(stat,0,1,lower.tail=FALSE)
	} else {
		stat<-(-2)*(p.values)
		#print(stat)
		p.value.final<-pchisq(stat,2*N,lower.tail=FALSE)
	}
	#message("The p-value is: ", p.value.final)
	return(p.value.final)
	
}

#test<-tseries::adf.test(rnorm(20,0,1))

#tseries::adf.test(x[,1],k=1)

#my.mw(x,choi=FALSE)
#plm::purtest(x,test="madwu",lags="AIC",pmax=1)


coint<-function(object){
	N<-ncol(object)
	T<-nrow(object)
	results<-matrix(ncol=N,nrow=N)
	res1<-numeric(T)
	res2<-numeric(T)
	res3<-numeric(T)
	for(i in 1:N){
		for(j in 1:N){
			if(j == i){
				results[i,j]<-tseries::adf.test(object[,i])$p.value
			} else {
				res1<-myadf(object[,i])$residuals
				res2<-myadf(object[,j])$residuals
				#print(length(res2))
				res3<-summary(lm(res1 ~ res2))$residuals
				results[i,j]<-tseries::adf.test(res3)$p.value
			}
		}
	}
	return(results)
}

adf.p.value<-function(critical,n){
	adf.table <- cbind(c(4.38, 4.15, 4.04, 3.99, 3.98, 3.96), 
	                   c(3.95, 3.80, 3.73, 3.69, 3.68, 3.66), 
	                   c(3.60, 3.50, 3.45, 3.43, 3.42, 3.41), 
	                   c(3.24, 3.18, 3.15, 3.13, 3.13, 3.12), 
	                   c(1.14, 1.19, 1.22, 1.23, 1.24, 1.25), 
	                   c(0.80, 0.87, 0.90, 0.92, 0.93, 0.94), 
	                   c(0.50, 0.58, 0.62, 0.64, 0.65, 0.66), 
	                   c(0.15, 0.24, 0.28, 0.31, 0.32, 0.33)) 

	adf.table<-(-adf.table)
	adf.table.n<-dim(adf.table)[2]
	adf.table.T<-c(25, 50, 100, 250, 500, 100000)
	adf.table.p<-c(0.01,0.025,0.05,0.10,0.9,0.95,0.975,0.99)
	adf.table.ipl<-numeric(adf.table.n)
	for(i in 1:adf.table.n){
		adf.table.ipl[i]<-approx(adf.table.T,adf.table[,i],n,rule=2)$y
	}
	interpol<-approx(adf.table.ipl, adf.table.p,critical,rule=2)$y
#	if(is.na(approx(adf.table.ipl, adf.table.p,critical,rule=1)$y)){
#		if(interpol == min(adf.table.p)){
#			# p-value is smaller than printed p-value
#		} else {
#			# p-value is greater than printed p-value
#		}
#	}
	return(interpol)
}


library(parallel)
library(doParallel)
library(foreach)

no_cores<-detectCores()-1
cluster<-makeCluster(no_cores)
registerDoParallel(cluster)
clusterEvalQ(cluster,.libPaths("F:/nupak"))
clusterEvalQ(cluster,library(tseries))
clusterEvalQ(cluster,library(plm))
clusterEvalQ(cluster,library(foreach))


adf.gen<-function(mc=10000,t=100,exo="none",para=FALSE){
	results<-numeric(mc)
	if(para==TRUE){
	    results<-foreach(a=1:mc,.combine=rbind) %dopar% {
		    x<-numeric(t)
		    x[1]<-0
		    ifelse(exo!="none",intercept<-rep(1,t),intercept<-rep(0,t))
		    ifelse(exo=="trend",trend<-1:t,trend<-rep(0,t))
		    for(i in 2:t){
		            x[i]<-x[i-1]+rnorm(1,0,1)+intercept[i]+trend[i]
		    }
		    dx<-c(0,x[2:length(x)]-x[1:(length(x)-1)])
		    lx<-c(0,x[1:(length(x)-1)])
		    ldx<-c(0,dx[2:length(dx)]-dx[1:(length(dx)-1)])
		    adf.lm<-lm(dx ~ lx + ldx + intercept + trend)
		    adf<-summary(adf.lm)$coef[2,3]
    		    return(adf)
	    }
	}
	if(para==FALSE){
	    ifelse(exo!="none",intercept<-rep(1,t),intercept<-rep(0,t))
        	   ifelse(exo=="trend",trend<-1:t,trend<-rep(0,t))
	    for(i in 1:mc){
        	       x<-numeric(t)
        		x[1]<-0

        		for(i in 2:t){
        			x[i]<-x[i-1]+rnorm(1,0,1)+intercept[i]+trend[i]
        		}
        		dx<-c(0,x[2:length(x)]-x[1:(length(x)-1)])
        		lx<-c(0,x[1:(length(x)-1)])
        		ldx<-c(0,dx[2:length(dx)]-dx[1:(length(dx)-1)])
        		adf.lm<-lm(dx ~ lx + ldx + intercept + trend)
        		adf<-summary(adf.lm)$coef[2,3]
        		results[i]<-adf
        		}
        	
	}
	plot(density(results),lwd=2,col=c("deeppink2"))
	#hist(results,breaks=20)
	return(results)
}

brownian<-function(t,reps,exo="none"){
    df.stat<-numeric(reps)
    zero<-function(t){
        u<-rnorm(t)
        W<-1/sqrt(t)*cumsum(u)
        return((W[t]^2-1)/(2*sqrt(mean(W^2))))
    }
    
    intercept<-function(t){
        u<-rnorm(t)
        W<-1/sqrt(t)*cumsum(u)
        W_mu<-W-mean(W)
        return((W_mu[t]^2-W_mu[1]^2-1)/(2*sqrt(mean(W_mu^2))))
    }
    
    s<-seq(0,1,length.out = t)
    
    trend<-function(t,s){
        u<-rnorm(t)
        W<-1/sqrt(t)*cumsum(u)
        W_tau<-W-(4-6*s)*mean(W)-(12*s-6)*mean(s*W)
        return((W_tau[t]^2-W_tau[1]^2-1)/(2*sqrt(mean(W_tau^2))))
    }
    
    
    
    for(i in 1:reps){
        if(exo=="none"){
            df.stat[i]<-zero(t)
        } else if(exo =="intercept"){
            df.stat[i]<-intercept(t)
        } else {
            df.stat[i]<-trend(t,s)
        }
    }
    return(df.stat)
}


plot(density(brownian(500,50000)),lwd=2,col=c("deeppink2"))

adf.value.zero<-list()
adf.value.intercept<-list()
adf.value.trend<-list()

for(t in seq(10,100,10)){
    adf.value.zero[(length(adf.value.zero)+1)]<-list(brownian(t,10000,exo="none"))
    adf.value.intercept[(length(adf.value.intercept)+1)]<-list(brownian(t,10000,exo="intercept"))
    adf.value.trend[(length(adf.value.trend)+1)]<-list(brownian(t,10000,exo="trend"))
    message("finished ",t)
}


hist(adf.value.intercept[[5]])

str(adf.value.intercept)


for(i in adf.value.trend){
    #print(summary(i))
    plot(density(i),lwd=2,col=c("deeppink2"),xlim=c(-5,5),ylim=c(0,0.5))
    readline()
}

find.val<-function(what,exo="zero",t=100){
    possible<-seq(10,100,10)
    if(!(t %in% possible)){
        t<-possible[which.min(abs(t-possible))]
    }
    t<-t/10
    if(exo=="none"){
        where<-adf.value.zero[[t]]
    } else if(exo=="intercept"){
        where<-adf.value.intercept[[t]]
    } else {
        where<-adf.value.trend[[t]]
    }
    sorted<-sort(where)
    
    return(which.min(abs(sorted-what))/length(sorted))
}


integrate(test(14),-Inf,-2.6)$value

plot(density(adf.value.intercept[[1]]))

test<-ecdf(adf.value.zero[[1]])

y<-rnorm(100,0,1)
dy<-c(0,y[2:length(y)]-y[1:(length(y)-1)])
ly<-c(0,y[1:(length(y)-1)])
dly<-c(0,dy[1:(length(y)-1)])

reg<-lm(dy ~ ly + dly)

adf.gen(t=100,exo="none")



adf.lag.find<-function(Y,exo="none", pmax=10,signi=1.8){
	dY<-c(0,diff(Y))
	lY<-c(0,Y[1:(length(Y)-1)])
	ldY<-c(0,dY[1:(length(dY)-1)])
	ifelse(exo!="none",intercept<-rep(1,length(Y)),intercept<-rep(0,length(Y)))
	ifelse(exo=="trend",trend<-1:length(Y),trend<-rep(0,length(Y)))
	searching<-TRUE
	i<-0
	while(searching){
		lags<-pmax-i
		ldY<-lagit(dY,(lags))
		x<-as.matrix(cbind(lY,trend,intercept,ldY))
		adf.fit<-lm(dY ~ x)
		covfefe<-summary(adf.fit)$coefficients
		covfefe<-covfefe[length(covfefe[,3]),3]
		#print(summary(adf.fit))
		
		#message("COEF: ",covfefe)
		if(lags == 0 || covfefe > signi){
			searching<-FALSE
		} else {
			i<-i+1
		}
	}
	if(lags == 0) {
		lags<-1
	}
	return(lags)
}

adf.lag.find(c,pmax=200,signi=1.96)

myadf(c,pmax=adf.lag.find(c,pmax=5,signi=1.96))

lev<-plm::purtest(x,test = "levinlin",pmax = 1,exo = "none", lags = "Hall")
lev2<-myllc(x,pmax=1,exo="none")
lev

madwa<-plm::purtest(x,test = "madwu",pmax = 1, exo = "none", lags = "Hall")
madwa2<-my.mw(x)
