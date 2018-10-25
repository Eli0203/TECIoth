##by: Eliana Vallejo Arango
## functions
###########optimization method Levenberg-Marquardt
optim_l_m<-function(f1,epsi,lambda,x0,N=500){
i <- 1; x1 <- x0
while (i<=N) {
  x1 <- x0-(solve(lambda*diag(length(x0)) + (hessian(f1, x0)))%*%grad(f1, x0))
   i <- i + 1
  if (sum((x1-x0)^2) < epsi) break
  x0 <- x1
   }
results<-list(x0,f1(x0),i)
names(results)<-c("par","objective","iter")
return(results)
}
