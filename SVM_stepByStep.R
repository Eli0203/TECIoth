# SET-UP
pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

# INCLUDES
pkgTest('nloptr')

# Metodo que estima un modelo SVM
# Necesita como parametro:
# - X, matriz de caracteristicas (filas = observaciones, columnas = atributos)
# - Y, vector de etiquetas {-1, 1} (tantos elementos como observaciones)
# - C, coeficiente de penalizaci???n de errores
svm <- function(X,Y,C){
  ####################
  # PON AQU??? TU C???DIGO
  ####################
  p<-ncol(X)
  n<-nrow(X)
  f<-function(w){
    w_s<-sapply(1:p,function(i)w[i]^2)
    epsi<-sapply(1:n,function(i)w[i+p+1])
    out<-(sum(w_s)/2)+C*sum(epsi)
    return(out)
  }
  
  ##constraints
  ineq<-function(w){
    out<-numeric()
    for(i in 1:n){
      wp_x<-sapply(1:p,function(j)w[j]*X[i,j])
      out[i]<-Y[i]*(sum(wp_x)-w[p+1])-1+w[p+1+i]
    }
    return(out)
  }
  lb<-c(rep(-Inf,p+1),rep(0,n))
  # Para la optimizaci???n del modelo SVM se recomienda usar:
  res<-auglag(x0 = runif(p+1+n), fn = f, hin=ineq,gr=NULL,lower=lb,upper = NULL,localsolver = c("SLSQP"),control = list(maxeval = 1000))
  w<-res$par[1:p]
  b<-res$par[p+1]
  # Este metodo devuelve un modelo SVM
  # definido como una lista con dos elementos:
  # - el vector w
  # - el coeficiente b
  return(list(w=w, b=b))
}

# Funci???n que devuelve las etiquetas predecidas a partir de:
# - mod, modelos SVM (estimado con el metodo svm)
# - X, matriz de caracteristicas (filas = observaciones, columnas = atributos)
predict <- function(mod, X){
  as.numeric(sign(mod$w %*% t(X) + mod$b))
}

# Funci???n que devuelve la distancia al hiperplano a partir de:
# - mod, modelos SVM (estimado con el metodo svm)
# - X, matriz de caracteristicas (filas = observaciones, columnas = atributos)
predict2 <- function(mod, X){
  as.numeric((mod$w %*% t(X) + mod$b))
}

#########################
# Ejemplo 1 : puntos separables linealmente
#########################
X <- matrix(c(1, 4,
              2, 3,
              3, 5,
              5, 2,
              6, 1,
              7, 3), ncol = 2, byrow = T)
Y <- rep(c(-1,1), each = 3)
mod <- svm(X, Y, 1)
Y.hat <- predict(mod, X)
paste("Precision: ", mean(Y.hat == Y))

# Este conjunto de datos es completamente separable.
# La precisi???n deber???a ser 1.
# El hiperplano ???ptimo es:
# - w = (0.5714286, -0.2857143)
# - b = -1.285714

#########################
# Ejemplo 2 : digitos manuscritos
#########################

# Funci???n para dibujar el digito en posici???n 'num'
plotDigit <- function(num){
  m1 <- matrix(as.numeric(X[num,]),16,16, byrow = F)
  image(m1[,ncol(m1):1], axes = FALSE, col = grey(seq(1, 0, length = 2)))
}

# Cargar fichero datos
semeion <- read.csv("semeion.csv")
X <- as.matrix(semeion[,1:256]) # Matriz de atributos
Y <- as.matrix(semeion[,257:266]) # Matriz de etiquetas
N <- semeion[,267] # N???mero representado en el digito

plotDigit(1) # Ejemplo: Dibuja el elemento en posici???n 1

# Entrenar un modelo para cada digito del 0 al 9
# ATENCI???N: este bucle puede ser MUY lento (~10 horas)
mods <- list()
for(digit in 0:9){
  mods[[digit+1]] <- svm(X, Y[,digit+1], 1)
  save.image(file = "workspace.RData") #Guardando el estado de la memor???a por si acaso...
}

# Obteniendo predicci???n
# Se considera la distancia del punto a los hiperplanos y se
# elije el n???mero correspondiente a la distancia m???xima.
N.hat <- unlist(apply(X, 1, function(x){
  res <- array()
  for(digit in 0:9){
    res[digit+1] <- predict2(mods[[digit+1]], matrix(x, nrow=1))
  }
  return(which.max(res)-1)
}))

paste("Precision: ", mean(N.hat == N))
# La precisi???n obtenida deber???a ser 1

