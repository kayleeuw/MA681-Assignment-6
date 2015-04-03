metrop = function(N,b){
     x = rep(0,N)
     for(i in 2:N){
          y = rnorm(1,x[i-1],b)
          r = (1+x^2)/(1+y^2)
          u = runif(1)
          if(u < r)x[i] = y  else x[i] = x[i-1]
          }
     return(x)
     }


N = 1000
postscript("metrop.cauchy1.eps",horizontal=F,onefile=F,print.it=F)
par(mfrow=c(3,1))
x1 = metrop(N,.1);
x2 = metrop(N,1);
x3 = metrop(N,10);
a = max(abs(c(x1,x2,x3)))
plot(x1,type="l",main= "x1: b = .1", xlab="",ylab="",xaxt="n",yaxt="n",bty="l")
plot(x2,type="l",main= "x2: b = 1"   ,xlab=" ",ylab="",xaxt="n",yaxt="n",bty="l")
plot(x3,type="l",main= "x3: b=10"  ,xlab="",ylab="",xaxt="n",yaxt="n",bty="l")
dev.off()

plot(x1,type="l", main= "x1: b = .1", xlab="",ylab="",xaxt="n",yaxt="n",bty="l")
plot(x2,type="l", main= "x2: b = 1", xlab="",ylab="",xaxt="n",yaxt="n",bty="l")
plot(x3,type="l", main= "x3: b = 10", xlab="",ylab="",xaxt="n",yaxt="n",bty="l")



grid = seq(-a-1,a+1,length=100)
f    = dcauchy(grid)
b    = max(f)
b    = .7


hist(x1,prob=T,xlab="x1",ylab="",main="",xlim=c(-a-1,a+1),
     xaxt="n",yaxt="n",density=5)
lines(grid,f,lwd=3)
hist(x2,prob=T,xlab="x2",ylab="",main="",xlim=c(-a-1,a+1),
     xaxt="n",yaxt="n",density=5)
lines(grid,f,lwd=3)
hist(x3,prob=T,xlab="x3",ylab="",main="",xlim=c(-a-1,a+1),
     xaxt="n",yaxt="n",density=5)
lines(grid,f,lwd=3)


postscript("metrop.cauchy2.eps",horizontal=F,onefile=F,print.it=F)
par(mfrow=c(3,1))
hist(x1,prob=T,xlab="",ylab="",main="",xlim=c(-a-1,a+1),
     xaxt="n",yaxt="n",density=5)
lines(grid,f,lwd=3)
hist(x2,prob=T,xlab="",ylab="",main="",xlim=c(-a-1,a+1),
     xaxt="n",yaxt="n",density=5)
lines(grid,f,lwd=3)
hist(x3,prob=T,xlab="",ylab="",main="",xlim=c(-a-1,a+1),
     xaxt="n",yaxt="n",density=5)
lines(grid,f,lwd=3)
dev.off()

labelm="mean(x1), mean(x2), mean(x3)"
mean=c(mean(x1),mean(x2),mean(x3))
labelv="var(x1), var(x2), var(x3)"
var=c(var(x1),var(x2),var(x3))

print(labelm)
print(mean)




