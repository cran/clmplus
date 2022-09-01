## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----tailor made functions, include=FALSE-------------------------------------
t2c <- function(x){
  "
  Function to transform an upper run-off triangle into a half-square.
    
  This function takes an upper run-off triangle as input.
  
  It returns a half square.
  "
  I= dim(x)[1]
  J= dim(x)[2]
  
  mx=matrix(NA,nrow=I,ncol=J)
  for(i in 1:(I)){
    for(j in 1:(J)){
      if(i+j<=J+1){
        mx[j,(i+j-1)]=x[i,j]
      }
    }
  }
  return(mx)
}


c2t <- function(x){
  "
  Function to transform a square into an upper run-off triangle.
  
  This function takes a half square as input.
  
  It returns an upper run-off triangle. 
  "
  I= dim(x)[1]
  J= dim(x)[2]
  
  mx=matrix(NA,nrow=I,ncol=J)
  for(i in 1:(I)){
    for(j in 1:(J)){
      if(i+j<=J+1){
        mx[i,j]=x[j,(i+j-1)]
      }
    }
  }
  return(mx)
}



## ----sand box, message=FALSE, warning=FALSE-----------------------------------
library(ChainLadder)

data("AutoBI")
dataset=AutoBI$AutoBIPaid 
dataset

colnames(dataset)=c(0:(dim(dataset)[1]-1))
rownames(dataset)=c(0:(dim(dataset)[1]-1))


## ----life representation, echo=FALSE------------------------------------------
t2c(dataset)

## ----rtt data, include=FALSE--------------------------------------------------
library(clmplus)
rtt <- RtTriangle(cumulative.payments.triangle = dataset)


## ----amodel, message=FALSE, warning=FALSE-------------------------------------
a.model=clmplus(RtTriangle =  rtt, 
             hazard.model = "a")


## ----mack, message=FALSE, warning=FALSE---------------------------------------
mck.chl <- MackChainLadder(dataset)
ultimate.chl=mck.chl$FullTriangle[,dim(mck.chl$FullTriangle)[2]]
diagonal=rev(t2c(mck.chl$FullTriangle)[,dim(mck.chl$FullTriangle)[2]])

## ----clm replicated-----------------------------------------------------------
data.frame(ultimate.cost.mack=ultimate.chl,
           ultimate.cost.fchl=a.model$ultimate.cost,
           reserve.mack=ultimate.chl-diagonal,
           reserve.fchl=a.model$reserve
           )

cat('\n Total reserve:',
    sum(a.model$reserve))


## ----apc clm------------------------------------------------------------------
library(apc)

ds.apc = apc.data.list(cum2incr(dataset),
                       data.format = "CL")

ac.model.apc = apc.fit.model(ds.apc,
                         model.family = "od.poisson.response",
                         model.design = "AC")


## ----show comparison----------------------------------------------------------

ac.model.apc$coefficients.canonical[,'Estimate']

ac.fcst.apc = apc.forecast.ac(ac.model.apc)

data.frame(reserve.mack=ultimate.chl-diagonal,
           reserve.apc=c(0,ac.fcst.apc$response.forecast.coh[,'forecast']),
           reserve.fchl=a.model$reserve
           
           )



## ----fitted ax amodel---------------------------------------------------------
a.model$model.fit$ax

## ----plot effects ax----------------------------------------------------------
p.a=plot(a.model)

## ----amodel residuals---------------------------------------------------------
#make it triangular
plotresiduals(a.model)

## ----message=FALSE, warning=FALSE---------------------------------------------
ac.model <- clmplus(rtt, hazard.model="ac")
plotresiduals(ac.model)

## ----apapc models, message=FALSE, warning=FALSE-------------------------------
ap.model = clmplus(rtt,hazard.model = "ap")
apc.model = clmplus(rtt,hazard.model = "apc")

## ----residuals apmodel--------------------------------------------------------
plotresiduals(ap.model)

## ----residuals apcmodel-------------------------------------------------------
plotresiduals(apc.model)

