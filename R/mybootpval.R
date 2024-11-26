#' Bootstrap p-value Calculation
#'
#' This function calculates the p-value based on bootstrapping for a given sample.
#' It allows for one-sided or two-sided hypothesis testing and provides a confidence interval
#' for the mean difference based on bootstrap resampling.
#'
#' @param x A numeric vector containing the sample data.
#' @param conf.level The confidence level for the confidence interval (default is 0.95).
#' @param iter The number of bootstrap iterations (default is 3000).
#' @param mu0 The null hypothesis value for the population mean (default is 0).
#' @param test A character string indicating the type of test. Options are:
#'        - "two" for a two-sided test,
#'        - "upper" for an upper one-sided test,
#'        - "lower" for a lower one-sided test (default is "two").
#'
#' @return A list containing:
#'         \item{pvalue}{The calculated p-value based on the bootstrap resampling.}
#'         \item{tcalc}{The calculated t-statistic for the observed data.}
#'         \item{n}{The sample size of the data.}
#'         \item{x}{The input sample data.}
#'         \item{test}{The test type used ("two", "upper", or "lower").}
#'         \item{ci}{The confidence interval for the resampled means.}
#'
#' @importFrom graphics title
#'
#' @export
#'
#' @examples
#' # Example usage for two-sided test
#' bootpval(x = rnorm(30), conf.level = 0.95, iter = 3000, mu0 = 0, test = "two")
#'
#' # Example usage for upper one-sided test
#' bootpval(x = rnorm(30), conf.level = 0.95, iter = 3000, mu0 = 0, test = "upper")
#'
#' # Example usage for lower one-sided test
#' bootpval(x = rnorm(30), conf.level = 0.95, iter = 3000, mu0 = 0, test = "lower")

bootpval<-function(x,conf.level=0.95,iter=3000,mu0=0, test="two"){
  n=length(x)
  y=x-mean(x)+mu0  # transform the data so that it is centered at the NULL
  rs.mat<-c()    #rs.mat will become a resample matrix -- now it is an empty vector
  xrs.mat<-c()
  for(i in 1:iter){ # for loop - the loop will go around iter times
    rs.mat<-cbind(rs.mat,sample(y,n,replace=TRUE)) #sampling from y cbind -- column bind -- binds the vectors together by columns
    xrs.mat<-cbind(xrs.mat,sample(x,n,replace=TRUE)) #sampling from x cbind -- column bind -- binds the vectors together by columns
  }

  tstat<-function(z){ # The value of t when the NULL is assumed true (xbar-muo)/z/sqrt(n)
    sqrt(n)*(mean(z)-mu0)/sd(z)
  }

  tcalc=tstat(x) # t for the data collected
  ytstat=apply(rs.mat,2,tstat) # tstat of resampled y's, ytstat is a vector and will have iter values in it
  xstat=apply(xrs.mat,2,mean)  # mean of resampled x's
  alpha=1-conf.level # calculating alpha
  ci=quantile(xstat,c(alpha/2,1-alpha/2))# Nice way to form a confidence interval
  pvalue=ifelse(test=="two",length(ytstat[ytstat>abs(tcalc) | ytstat < -abs(tcalc)])/iter,
                ifelse(test=="upper",length(ytstat[ytstat>tcalc])/iter,
                       length(ytstat[ytstat<xstat])/iter))

  h=hist(ytstat,plot=FALSE)
  mid=h$mid
  if(test=="two"){
    ncoll=length(mid[mid<= -abs(tcalc)])
    ncolr=length(mid[mid>=  abs(tcalc)])
    col=c(rep("Green",ncoll),rep("Gray",length(mid)-ncoll-ncolr),rep("Green",ncolr))
  }
  if(test=="upper"){
    ncolr=length(mid[mid>=  abs(tcalc)])
    col=c(rep("Gray",length(mid)-ncolr),rep("Green",ncolr))
  }

  if(test=="lower"){
    ncoll=length(mid[mid<=  -abs(tcalc)])
    col=c(rep("Green",ncoll),rep("Gray",length(mid)-ncoll))
  }
  hist(ytstat,col=col,freq=FALSE,las=1,main="",xlab=expression(T[stat]))
  #segments(ci[1],0,ci[2],0,lwd=2)
  pround=round(pvalue,4)
  title(substitute(paste(P[value],"=",pround)))
  return(list(pvalue=pvalue,tcalc=tcalc,n=n,x=x,test=test,ci=ci))
}
