########################################
##practica 1
##by: Eliana Vallejo Arango
##metodo de la seccion aurea
###libraries
##Functions
golden_sec<-function(f,a1,epsi){
  b1<-a1+4;alpha<-(1+sqrt(5))/2
  x1<-a1+((1-alpha)*(b1-a1))
  x2<-a1+(alpha*(b1-a1))
  f1<-f(x1);f2<-f(x1)
  while(abs(b1-a1)>epsi){
    if(f1>f2){
      a1<-x1; x1<-x2
      x2<-a1+(alpha*(b1-a1))
      f2<-f(x2)
    }else{
      a1<-a1; b1<-x1
      x1<-a1+((1-alpha)*(b1-a1))
      x2<-x1
      f1<-f(x1)
    }
  }
  return(data.frame(var=c("a","b","function a","function b"),value=c(a1,b1,f(a1),f(b1))))
}
