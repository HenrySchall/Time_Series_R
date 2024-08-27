#####
##   OUTROS MODELOS DA FAMÍLIA GARCH
#####

## 01 - IGARCH
## 02 - GARCH-M
## 03 - EGARCH
## 04 - TGARCH


source("/cloud/project/install_and_load_packages.R")

#####
##   GERAÇÃO DAS SÉRIES ARMA - GARCH
#####

# Fixar a raiz para os dados gerados na simulação serem iguais em qualquer computador
set.seed(123)

# GARCH(1,1) - specify omega/alpha/beta
spec = garchSpec(model = list(omega = 1, alpha = 0.5, beta = 0.4), cond.dist="sged")

#####
##   COMPORTAMENTO DA SÉRIE SIMULADA
#####
x = garchSim(spec, n = 1000)
summary(x)
plot(x)

#####
##   ESTIMAÇÃO DOS MODELOS
#####

Igarch(x)
# type = 1 for Variance-in-mean
# type = 2 for volatility-in-mean
# type = 3 for log(variance)-in-mean
garchM(x, type = 3)
y = log(x + abs(min(x)) + 1)
Egarch(y)
Tgarch11(x)


#####
##  TSAY - FUNÇÃO IGARCH
#####

"Igarch" <- function(rtn,include.mean=F,volcnt=F){
  # Estimation of a Gaussian IGARCH(1,1) model.
  # rtn: return series 
  # include.mean: flag for the constant in the mean equation.
  # volcnt: flag for the constant term of the volatility equation.
  #### default is the RiskMetrics model
  #
  Idata <<- rtn
  Flag <<- c(include.mean,volcnt)
  #
  Mean=mean(Idata); Var = var(Idata); S = 1e-6
  if((volcnt)&&(include.mean)){
    params=c(mu = Mean,omega=0.1*Var,beta=0.85)
    lowerBounds = c(mu = -10*abs(Mean), omega= S^2, beta= S)
    upperBounds = c(mu = 10*abs(Mean), omega = 100*Var, beta = 1-S)
  }
  if((volcnt)&&(!include.mean)){
    params=c(omega=0.1*Var, beta=0.85)
    lowerBounds=c(omega=S^2,beta=S)
    upperBounds=c(omega=100*Var,beta=1-S)
  }
  #
  if((!volcnt)&&(include.mean)){
    params=c(mu = Mean, beta= 0.8)
    lowerBounds = c(mu = -10*abs(Mean), beta= S)
    upperBounds = c(mu = 10*abs(Mean), beta = 1-S)
  }
  if((!volcnt)&&(!include.mean)){
    params=c(beta=0.85)
    lowerBounds=c(beta=S)
    upperBounds=c(beta=1-S)
  }
  # Step 3: set conditional distribution function:
  igarchDist = function(z,hh){dnorm(x = z/hh)/hh}
  # Step 4: Compose log-likelihood function:
  igarchLLH = function(parm){
    include.mean=Flag[1]
    volcnt=Flag[2]
    mu=0; omega = 0
    if((include.mean)&&(volcnt)){
      my=parm[1]; omega=parm[2]; beta=parm[3]}
    if((!include.mean)&&(volcnt)){
      omega=parm[1];beta=parm[2]}
    if((!include.mean)&&(!volcnt))beta=parm[1]
    if((include.mean)&&(!volcnt)){mu=parm[1]; beta=parm[2]}
    #
    z = (Idata - mu); Meanz = mean(z^2)
    e= omega + (1-beta)* c(Meanz, z[-length(Idata)]^2)
    h = filter(e, beta, "r", init=Meanz)
    hh = sqrt(abs(h))
    llh = -sum(log(igarchDist(z, hh)))
    llh
  }
  # Step 5: Estimate Parameters and Compute Numerically Hessian:
  fit = nlminb(start = params, objective = igarchLLH,
               lower = lowerBounds, upper = upperBounds)
  ##lower = lowerBounds, upper = upperBounds, control = list(trace=3))
  epsilon = 0.0001 * fit$par
  cat("Estimates: ",fit$par,"\n")
  npar=length(params)
  Hessian = matrix(0, ncol = npar, nrow = npar)
  for (i in 1:npar) {
    for (j in 1:npar) {
      x1 = x2 = x3 = x4  = fit$par
      x1[i] = x1[i] + epsilon[i]; x1[j] = x1[j] + epsilon[j]
      x2[i] = x2[i] + epsilon[i]; x2[j] = x2[j] - epsilon[j]
      x3[i] = x3[i] - epsilon[i]; x3[j] = x3[j] + epsilon[j]
      x4[i] = x4[i] - epsilon[i]; x4[j] = x4[j] - epsilon[j]
      Hessian[i, j] = (igarchLLH(x1)-igarchLLH(x2)-igarchLLH(x3)+igarchLLH(x4))/
        (4*epsilon[i]*epsilon[j])
    }
  }
  cat("Maximized log-likehood: ",igarchLLH(fit$par),"\n")
  # Step 6: Create and Print Summary Report:
  se.coef = sqrt(diag(solve(Hessian)))
  tval = fit$par/se.coef
  matcoef = cbind(fit$par, se.coef, tval, 2*(1-pnorm(abs(tval))))
  dimnames(matcoef) = list(names(tval), c(" Estimate",
                                          " Std. Error", " t value", "Pr(>|t|)"))
  cat("\nCoefficient(s):\n")
  printCoefmat(matcoef, digits = 6, signif.stars = TRUE)
  
  if((include.mean)&&(volcnt)){
    mu=fit$par[1]; omega=fit$par[2]; beta = fit$par[3]
  }
  if((include.mean)&&(!volcnt)){
    mu = fit$par[1]; beta = fit$par[2]; omega = 0
  }
  if((!include.mean)&&(volcnt)){
    mu=0; omega=fit$par[1]; beta=fit$par[2]
  }
  if((!include.mean)&&(!volcnt)){
    mu=0; omega=0; beta=fit$par[1]
  }
  z=Idata-mu; Mz = mean(z^2)
  e= omega + (1-beta)*c(Mz,z[-length(z)]^2)
  h = filter(e,beta,"r",init=Mz)
  vol = sqrt(abs(h))
  
  Igarch <- list(par=fit$par,volatility = vol)
}


#####
##  TSAY - GARCH-M
#####

"garchM" <- function(rtn,type=1){
  # Estimation of a Gaussian GARCH(1,1)-M model.
  ##### The program uses GARCH(1,1) results as initial values.
  # rtn: return series 
  # type = 1 for Variance-in-mean
  #      = 2 for volatility-in-mean
  #      = 3 for log(variance)-in-mean
  #
  if(is.matrix(rtn))rtn=c(rtn[,1])
  garchMdata <<- rtn
  # obtain initial estimates
  m1=garch11FIT(garchMdata)
  est=as.numeric(m1$par); v1=m1$ht  ## v1 is sigma.t-square
  Mean=est[1]; cc=est[2]; ar=est[3]; ma=est[4]; S=1e-6
  if(type==2)v1=sqrt(v1)
  if(type==3)v1=log(v1)
  #### Obtain initial estimate of the parameters for the mean equation
  m2=lm(rtn~v1)
  Cnst=as.numeric(m2$coefficients[1])
  gam=as.numeric(m2$coefficients[2])
  params=c(mu=Cnst,gamma=gam, omega=cc, alpha=ar,beta=ma)
  lowBounds=c(mu=-5*abs(Mean),gamma=-20*abs(gam), omega=S, alpha=S, beta=ma*0.6)
  uppBounds=c(mu=5*abs(Mean),gamma=100*abs(gam), omega=cc*5 ,alpha=3*ar,beta=1-S)
  ### Pass model information via defining global variable
  Vtmp <<- c(type,v1[1])
  #
  fit=nlminb(start = params, objective= glkM, lower=lowBounds, upper=uppBounds)
  ##,control=list(trace=3,rel.tol=1e-5))
  epsilon = 0.0001 * fit$par
  npar=length(params)
  Hessian = matrix(0, ncol = npar, nrow = npar)
  for (i in 1:npar) {
    for (j in 1:npar) {
      x1 = x2 = x3 = x4  = fit$par
      x1[i] = x1[i] + epsilon[i]; x1[j] = x1[j] + epsilon[j]
      x2[i] = x2[i] + epsilon[i]; x2[j] = x2[j] - epsilon[j]
      x3[i] = x3[i] - epsilon[i]; x3[j] = x3[j] + epsilon[j]
      x4[i] = x4[i] - epsilon[i]; x4[j] = x4[j] - epsilon[j]
      Hessian[i, j] = (glkM(x1)-glkM(x2)-glkM(x3)+glkM(x4))/
        (4*epsilon[i]*epsilon[j])
    }
  }
  cat("Maximized log-likehood: ",-glkM(fit$par),"\n")
  # Step 6: Create and Print Summary Report:
  se.coef = sqrt(diag(solve(Hessian)))
  tval = fit$par/se.coef
  matcoef = cbind(fit$par, se.coef, tval, 2*(1-pnorm(abs(tval))))
  dimnames(matcoef) = list(names(tval), c(" Estimate",
                                          " Std. Error", " t value", "Pr(>|t|)"))
  cat("\nCoefficient(s):\n")
  printCoefmat(matcoef, digits = 6, signif.stars = TRUE)
  
  m3=ResiVol(fit$par)
  
  garchM <- list(residuals=m3$residuals,sigma.t=m3$sigma.t)
}

glkM = function(pars){
  rtn <- garchMdata
  mu=pars[1]; gamma=pars[2]; omega=pars[3]; alpha=pars[4]; beta=pars[5]
  type=Vtmp[1]
  nT=length(rtn)
  # use conditional variance
  if(type==1){
    ht=Vtmp[2]
    et=rtn[1]-mu-gamma*ht
    at=c(et)
    for (i in 2:nT){
      sig2t=omega+alpha*at[i-1]^2+beta*ht[i-1]
      ept = rtn[i]-mu-gamma*sig2t
      at=c(at,ept)
      ht=c(ht,sig2t)
    }
  }
  # use volatility
  if(type==2){
    ht=Vtmp[2]^2
    et=rtn[1]-mu-gamma*Vtmp[2]
    at=c(et)
    for (i in 2:nT){
      sig2t=omega+alpha*at[i-1]^2+beta*ht[i-1]
      ept=rtn[i]-mu-gamma*sqrt(sig2t)
      at=c(at,ept)
      ht=c(ht,sig2t)
    }
  }
  # use log(variance)
  if(type==3){
    ht=exp(Vtmp[2])
    et=rtn[1]-mu-gamma*Vtmp[2]
    at=c(et)
    for (i in 2:nT){
      sig2t=omega+alpha*at[i-1]^2+beta*ht[i-1]
      ept=rtn[i]-mu-gamma*log(abs(sig2t))
      at=c(at,ept)
      ht=c(ht,sig2t)
    }
  }
  #
  hh=sqrt(abs(ht))
  glk=-sum(log(dnorm(x=at/hh)/hh))
  
  glk
}

ResiVol = function(pars){
  rtn <- garchMdata
  mu=pars[1]; gamma=pars[2]; omega=pars[3]; alpha=pars[4]; beta=pars[5]
  type=Vtmp[1]
  nT=length(rtn)
  # use conditional variance
  if(type==1){
    ht=Vtmp[2]
    et=rtn[1]-mu-gamma*ht
    at=c(et)
    for (i in 2:nT){
      sig2t=omega+alpha*at[i-1]^2+beta*ht[i-1]
      ept = rtn[i]-mu-gamma*sig2t
      at=c(at,ept)
      ht=c(ht,sig2t)
    }
  }
  # use volatility
  if(type==2){
    ht=Vtmp[2]^2
    et=rtn[1]-mu-gamma*Vtmp[2]
    at=c(et)
    for (i in 2:nT){
      sig2t=omega+alpha*at[i-1]^2+beta*ht[i-1]
      ept=rtn[i]-mu-gamma*sqrt(sig2t)
      at=c(at,ept)
      ht=c(ht,sig2t)
    }
  }
  # use log(variance)
  if(type==3){
    ht=exp(Vtmp[2])
    et=rtn[1]-mu-gamma*Vtmp[2]
    at=c(et)
    for (i in 2:nT){
      sig2t=omega+alpha*at[i-1]^2+beta*ht[i-1]
      ept=rtn[i]-mu-gamma*log(abs(sig2t))
      at=c(at,ept)
      ht=c(ht,sig2t)
    }
  }
  #
  
  ResiVol <- list(residuals=at,sigma.t=sqrt(ht))
}

garch11FIT = function(x){
  # Step 1: Initialize Time Series Globally:
  tx <<- x
  # Step 2: Initialize Model Parameters and Bounds:
  Mean = mean(tx); Var = var(tx); S = 1e-6
  params = c(mu = Mean, omega = 0.1*Var, alpha = 0.1, beta = 0.8)
  lowerBounds = c(mu = -10*abs(Mean), omega = S^2, alpha = S, beta = S)
  upperBounds = c(mu = 10*abs(Mean), omega = 100*Var, alpha = 1-S, beta = 1-S)
  # Step 3: Set Conditional Distribution Function:
  garchDist = function(z, hh) { dnorm(x = z/hh)/hh }
  # Step 4: Compose log-Likelihood Function:
  garchLLH = function(parm) {
    mu = parm[1]; omega = parm[2]; alpha = parm[3]; beta = parm[4]
    z = tx-mu; Mean = mean(z^2)
    # Use Filter Representation:
    e = omega + alpha * c(Mean, z[-length(tx)]^2)
    h = filter(e, beta, "r", init = Mean)
    hh = sqrt(abs(h))
    llh = -sum(log(garchDist(z, hh)))
    llh }
  #####print(garchLLH(params))
  # Step 5: Estimate Parameters and Compute Numerically Hessian:
  fit = nlminb(start = params, objective = garchLLH,
               lower = lowerBounds, upper = upperBounds)
  #
  est=fit$par
  # compute the sigma.t^2 series
  z=tx-est[1]; Mean=mean(z^2)
  e=est[2]+est[3]*c(Mean,z[-length(tx)]^2)
  h=filter(e,est[4],"r",init=Mean)
  
  garch11Fit <- list(par=est,ht=h)
}


#####
##  TSAY - EGARCH
#####

"Egarch" <- function(rtn){
  # Estimation of an EGARCH(1,1) model. Assume normal innovations
  # rtn: return series 
  #
  write(rtn,file='tmp.txt',ncol=1)
  # obtain initial estimates
  mu=mean(rtn)
  par=c(mu,0.1,0.1,0.1,0.7)
  #
  #
  #mm=optim(par,glk,method="Nelder-Mead",hessian=T)
  low=c(-10,-5,0,-1,0)
  upp=c(10,5,1,0,1)
  mm=optim(par,glk,method="L-BFGS-B",hessian=T,lower=low,upper=upp)
  ## Print the results
  par=mm$par
  H=mm$hessian
  Hi = solve(H)
  cat(" ","\n")
  cat("Estimation results of EGARCH(1,1) model:","\n")
  cat("estimates: ",par,"\n")
  se=sqrt(diag(Hi))
  cat("std.errors: ",se,"\n")
  tra=par/se
  cat("t-ratio: ",tra,"\n")
  # compute the volatility series and residuals
  ht=var(rtn)
  T=length(rtn)
  if(T > 40)ht=var(rtn[1:40])
  at=rtn-par[1]
  for (i in 2:T){
    eptm1=at[i-1]/sqrt(ht[i-1])
    lnht=par[2]+par[3]*(abs(eptm1)+par[4]*eptm1)+par[5]*log(ht[i-1])
    sig2t=exp(lnht)
    ht=c(ht,sig2t)
  }
  sigma.t=sqrt(ht)
  Egarch <- list(residuals=at,volatility=sigma.t)
}

glk <- function(par){
  rtn=read.table("tmp.txt")[,1]
  glk=0
  ht=var(rtn)
  T=length(rtn)
  if(T > 40)ht=var(rtn[1:40])
  at=rtn[1]-par[1]
  for (i in 2:T){
    ept=rtn[i]-par[1]
    at=c(at,ept)
    eptm1=at[i-1]/sqrt(ht[i-1])
    lnht=par[2]+par[3]*(abs(eptm1)+par[4]*eptm1)+par[5]*log(ht[i-1])
    sig2t=exp(lnht)
    ht=c(ht,sig2t)
    glk=glk + 0.5*(lnht + ept^2/sig2t)
  }
  glk
}


#####
##  TSAY - TGARCH
#####

Tgarch11 = function(x,cond.dist="norm")
{
  # Estimation of TGARCH(1,1) model with Gaussian or Student-t innovations
  # Step 1: Initialize Time Series Globally:
  Tx <<- x
  # Step 2: Initialize Model Parameters and Bounds:
  Meanx = mean(Tx); Varx = var(Tx); S = 1e-6
  if(cond.dist=="std"){
    params = c(mu = Meanx, omega = 0.1*Varx, alpha = 0.1, gam1= 0.02, beta = 0.81, shape=6)
    lowerBounds = c(mu = -10*abs(Meanx), omega = S^2, alpha = S, gam1=S, beta = S, shape=3)
    upperBounds = c(mu = 10*abs(Meanx), omega = 100*Varx, alpha = 1-S, gam1 = 1-S, beta = 1-S, shape=30)
  }
  else{
    params = c(mu = Meanx, omega = 0.1*Varx, alpha = 0.1, gam1= 0.02, beta = 0.81)
    lowerBounds = c(mu = -10*abs(Meanx), omega = S^2, alpha = S, gam1=S, beta = S)
    upperBounds = c(mu = 10*abs(Meanx), omega = 10*Varx, alpha = 1-S, gam1 = 1-S, beta = 1-S)
  }
  # Step 3: Set Conditional Distribution Function:
  garchDist = function(z, hh, cond.dist, nu1) { 
    if(cond.dist=="std"){LL=dstd(x = z/hh, nu=nu1)/hh}
    else{
      LL=dnorm(x = z/hh)/hh }
    LL
  }
  # Step 4: Compose log-Likelihood Function:
  garchLLH = function(parm) {
    mu = parm[1]; omega = parm[2]; alpha = parm[3]; gam1=parm[4]; beta = parm[5]
    shape = 0; 
    if(length(parm)==6){
      shape=parm[6]
      cond.dist="std"
    }
    else
    {cond.dist="norm"}
    z = (Tx-mu); Mean = mean(z^2)
    zm1=c(0,z[-length(z)])
    idx=seq(zm1)[zm1 < 0]; z1=rep(0,length(z)); z1[idx]=1
    # Use Filter Representation:
    e = omega + alpha * c(Mean, z[-length(z)]^2) + gam1*z1*c(Mean,z[-length(z)]^2)
    h = filter(e, beta, "r", init = Mean)
    hh = sqrt(abs(h))
    llh = -sum(log(garchDist(z, hh, cond.dist, shape)))
    llh }
  # Step 5: Estimate Parameters and Compute Numerically Hessian:
  fit = nlminb(start = params, objective = garchLLH,
               lower = lowerBounds, upper = upperBounds) ### control = list(trace=3))
  epsilon = 0.0001 * fit$par
  npar=length(params)
  Hessian = matrix(0, ncol = npar, nrow = npar)
  for (i in 1:npar) {
    for (j in 1:npar) {
      x1 = x2 = x3 = x4  = fit$par
      x1[i] = x1[i] + epsilon[i]; x1[j] = x1[j] + epsilon[j]
      x2[i] = x2[i] + epsilon[i]; x2[j] = x2[j] - epsilon[j]
      x3[i] = x3[i] - epsilon[i]; x3[j] = x3[j] + epsilon[j]
      x4[i] = x4[i] - epsilon[i]; x4[j] = x4[j] - epsilon[j]
      Hessian[i, j] = (garchLLH(x1)-garchLLH(x2)-garchLLH(x3)+garchLLH(x4))/
        (4*epsilon[i]*epsilon[j])
    }
  }
  cat("Log likelihood at MLEs: ","\n")
  print(-garchLLH(fit$par))
  # Step 6: Create and Print Summary Report:
  se.coef = sqrt(diag(solve(Hessian)))
  tval = fit$par/se.coef
  matcoef = cbind(fit$par, se.coef, tval, 2*(1-pnorm(abs(tval))))
  dimnames(matcoef) = list(names(tval), c(" Estimate",
                                          " Std. Error", " t value", "Pr(>|t|)"))
  cat("\nCoefficient(s):\n")
  printCoefmat(matcoef, digits = 6, signif.stars = TRUE)
  # compute output
  est=fit$par
  mu = est[1]; omega = est[2]; alpha = est[3]; gam1=est[4]; beta = est[5]
  z=(Tx-mu); Mean = mean(z^2)
  zm1=c(0,z[-length(z)])
  idx=seq(zm1)[zm1 < 0]; z1=rep(0,length(z)); z1[idx]=1
  e = omega + alpha * c(Mean, z[-length(z)]^2) + gam1*z1*c(Mean,z[-length(z)]^2)
  h = filter(e, beta, "r", init = Mean)
  sigma.t = sqrt(abs(h))
  
  Tgarch11 <- list(residuals = z, volatility = sigma.t, par=est)
}
