# data y simulated from a logistic regression model 
# with with three predictors, n=10000
x = matrix(rnorm(30000),10000,3)
lp = 0 + x[,1] - 1.42*x[2] + .67*x[,3] + 1.1*x[,1]*x[,2] - 1.5*x[,1]*x[,3] +2.2*x[,2]*x[,3] + x[,1]*x[,2]*x[,3]
p = 1/(1+exp(-lp))

# y is true class labels
y = runif(10000)<p

# fit a logistic regression model
mod = glm(y~x[,1]*x[,2]*x[,3],family="binomial")

# using a cutoff of cut, calculate sensitivity, specificity, and classification rate
perf = function(cut, mod, y)
{
   yhat = (mod$fit>cut)
   w = which(y==1)
   sensitivity = mean( yhat[w] == 1 ) 
   specificity = mean( yhat[-w] == 0 ) 
   c.rate = mean( y==yhat ) 
   d = cbind(sensitivity,specificity)-c(1,1)
   d = sqrt( d[1]^2 + d[2]^2 ) 
   out = t(as.matrix(c(sensitivity, specificity, c.rate,d)))
   colnames(out) = c("sensitivity", "specificity", "c.rate", "distance")
   return(out)
}

s = seq(.01,.99,length=1000)
OUT = matrix(0,1000,4)
for(i in 1:1000) OUT[i,]=perf(s[i],mod,y)
plot(s,OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
lines(s,OUT[,4],col="darkred",lwd=2)
box()
legend(0,.25,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Classification Rate","Distance"))