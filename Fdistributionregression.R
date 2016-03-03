#Definition of problem

#Number of data points
n<-1000

#Number of tests
m<-10000

x.orig<-rnorm(n)
x<-x.orig-mean(x.orig)
H<-x%*%solve(t(x)%*%x)%*%x
Randomy<-rnorm(n*m)
Y.orig<-matrix(Randomy,n,m)

#Calculation of Means and Residuals
Ybar.col <- colMeans(Y.orig)
Ybar.mat <- t(matrix(Ybar.col,m,n))
Y <- Y.orig-Ybar.mat
resids <- (diag(n)-H)%*%Y
sxx <- sum((x)^2)

#Sxy

x.matrix <- matrix(x,n,m)

sxy.col<-colSums((x.matrix)*(Y))

#Paramater estimates
b1.col<-sxy.col/sxx

var(b1.col)
var(b0.col)

Yhat.col<-H%*%Y
resid.col<-Y-Yhat.col
resid.col2<-resid.col*resid.col
s2.col<-colSums(resid.col2)/(n-2)

SSR.rcol<-colSums((Yhat.col)^2)
F<-SSR.rcol/s2.col

plot(density(F, adjust = 0.4, from = 0), ylim = c(0,5), xlim = c(0.05,20))

test.lm<-lm(Y[,1]~x)

curve(df(x, 1, 999), add = TRUE, col = "red")
