# All calculations, simulations, tables and figures in the main
# article may be reproduced by running the following code in R. Save
# the code below to a file e.g. ‘code.R’, open R and then use the
# command:
#
# source("code.R",echo=T)
#
# at the R prompt, ensuring the file ‘code.R’ is in the working
# directory.
lam0=(210/470)*13
lam1=(260/470)*13
cbind(lam0,lam1)
logLRobs=2*(dpois(13,13,log=T)-
              (dpois(13,lam0,log=T)+dpois(0,lam1,log=T)))
logLRobs
1-pchisq(logLRobs,df=1)
set.seed(20180226)
logLRsim=0
for (i in 1:20000000){
  x=rpois(1,lam0)
  y=rpois(1,lam1)
  lam0sim=(x+y)/(210+260)
  logL0=dpois(x,lam0sim*210,log=T)+dpois(y,lam0sim*260,log=T)
  logL1=dpois(x,x,log=T)+dpois(y,y,log=T)
  logLRsim[i]=2*(logL1-logL0)
}
no.exceeding=sum(logLRsim>=logLRobs)
no.exceeding
mu0=(14/(210+260))*210
mu1=(14/(210+260))*260
cbind(mu0,mu1)
logLRperturb=2*((dpois(13,13,log=T)+dpois(1,1,log=T))-
                  (dpois(13,mu0,log=T)+dpois(1,mu1,log=T)))
logLRperturb
1-pchisq(logLRperturb,df=1)
mon=c(9,1,6,8,10,12,9,8,8,10,3,1,4)
yr=c(1981,1984,1987,1987,1987,1987,1988,1990,1991,1992,1993,1996,1996)
months=12*(yr-1979)+mon
rbind(mon,yr,months)
scan.stat=function(months,window){
  sum.window=0
  for (j in 1:(210-window+1)){
    sum.window[j]=sum((j<=months)&(months<j+window))
  }
  max(sum.window)
}
n.sim=10000
max.window=18
max.stat.mat=matrix(0,n.sim,max.window)
for (j in 1:n.sim){
  N=rpois(1,13)
  months.sim=sort(sample(1:210,size=N,repl=F))
  for (i in 1:max.window){
    max.stat.mat[j,i]=scan.stat(months.sim,i)
  }
}
p.vals=0
stat.obs=0
window=1:max.window
for (k in window) {
  stat.obs[k]=scan.stat(months,k)
  p.vals[k]=mean(max.stat.mat[,k]>=stat.obs[k])
}
cbind(window,stat.obs,p.vals)
unadj.pval=min(p.vals)
unadj.pval
M.sim=10000
pvals.sim=0
min.pval.sim=0
for (a in 1:M.sim){
  N=rpois(1,13)
  months.sim=sort(sample(1:210,size=N,repl=F))
  stat.obs.sim=0
  for (b in window){
    stat.obs.sim[b]=scan.stat(months.sim,b)
    pvals.sim[b]=mean(max.stat.mat[,b]>=stat.obs.sim[b])
  }
  min.pval.sim[a]=min(pvals.sim)
}
adj.pval=mean(min.pval.sim<=unadj.pval)
adj.pval
tabl=cbind(window,stat.obs,p.vals)
tabl
hist(min.pval.sim,pr=T,breaks=(0:20)/20,
     main="Simulated minimum p-values (observed in marked in red)")
abline(v=unadj.pval,col="red")