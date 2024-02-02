
## IRFA 2023 WGDL
## Title: Co-Movement between Commodity and Equity Markets Revisited - an Application of the Thick Pen Method

anscombe_test <-function (x, alternative=c("two.sided","less","greater"))
  {
    DNAME <- deparse(substitute(x))
    x <- sort(x[complete.cases(x)])
    n <- length(x)
    s <- match.arg(alternative)
    alter <- switch(s, two.sided=0, less=1, greater=2)
    b <- n*sum( (x-mean(x))^4 )/(sum( (x-mean(x))^2 )^2);
    eb2 <- 3*(n-1)/(n+1);
    vb2 <- 24*n*(n-2)*(n-3)/ ((n+1)^2*(n+3)*(n+5));
    m3 <- (6*(n^2-5*n+2)/((n+7)*(n+9)))*sqrt((6*(n+3)*(n+5))/(n*(n-2)*(n-3)));
    a <- 6 + (8/m3) * (2/m3 + sqrt(1 + 4/m3^2));
    xx <- (b-eb2)/sqrt(vb2);
    cube_root <- function (x) sign(x) * abs(x)^(1/3);
    z <- (1 - 2/(9 * a) - cube_root(((1 - 2/a)/(1 + xx * sqrt(2/(a - 4))))))/sqrt(2/(9 * a));
    pval <- pnorm(z, lower.tail = FALSE)
    if (alter == 0) {
      pval <- 2*pval
      if (pval > 1) pval<-2-pval
      alt <- "kurtosis is not equal to 3"
    }
    else if (alter == 1)
    {
      alt <- "kurtosis is greater than 3"
    }
    else
    {
      pval <- 1-pval
      alt <- "kurtosis is lower than 3"
    }
    RVAL <- list(statistic = c(kurt = b, z = z), p.value = pval, 
                 alternative = alt, method = "Anscombe-Glynn kurtosis test",
                 data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
  }

# function for descriptive statistics

descstat.comove = function(x) {
  sss = NULL
  for (i in 1:ncol(x)) {
    ss = c(mean(x[,i])*100,max(x[,i]),min(x[,i]),sd(x[,i])*100)
    ss = format(round(ss,4),nsmall=4)
    at1 = moments::agostino.test(x[,i],)
    at = at1$statistic[1]
    at = ifelse(at1$p.value<=0.01,paste(format(round(at,4),nsmall=2),"***"),format(round(at,4),nsmall=4))
    ant1 = anscombe_test(x[,i])
    ant = ant1$statistic[1]
    ant = ifelse(ant1$p.value<=0.01,paste(format(round(ant,4),nsmall=4),"***"),format(round(ant,4),nsmall=4))
    ss = c(ss,at,ant,length(x[,1]))
    sss = rbind(sss,ss)
  }
  rownames(sss) = colnames(x)
  colnames(sss) = c("Mean (%)","Max","Min","Std. Dev.(%)","Skewness","Kurtosis","No.of obs.")
  sss
}


#auxilary functions
myoperator<-function(x,y,tau=2,operator,...)#equi-spaced time series
  #auxilary function for TPT calculation of the current  article
  #x:  vector of x-variable values (typically time)
  #y: vector of y-variable values (value of the time series)
  #tau: thickness value
  #operator: min or max function  
{
  T<-length(y)
  
  b<-tau+1  
  
  y<-c(y,rep(NA,b-1))
  #simialr trick to subsampling, but 'y' needs to be extended, 
  #so that the resulting time series is of lenght n and not n-b+1
  
  X<-matrix(rep(y,b),byrow=FALSE,ncol=b) # series are in columns; X is tall
  X[upper.tri(X)]<-NaN #remove the top-right corner; use NaN in case x has NA's
  
  #create auxilary matrix with the bottom-left corner of NAs
  Y<-ifelse(is.nan(X),FALSE,TRUE) #top-right corner of NAs
  Y<-Y[,ncol(Y):1] #reverse rows
  Y<-Y[nrow(Y):1,] #reverse columns
  Y[upper.tri(Y)]<-FALSE # Y has both, top-right and bottom-left with FALSE and ow TRUE 
  X<-matrix(X[Y],ncol=b,byrow=FALSE)#X should be n-tau+1 by tau
  rm(Y)
  
  result<-apply(X,1,operator,na.rm=TRUE)
  
  return(result)
}

