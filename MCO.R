#Cargamos la libreria para leer archivos excel
library(readxl)
base2 <- read_excel("Sexto Semestre 2021/Modelos Lineales Matlab/base2.xlsx", 
                    col_names = FALSE)

names(base2) = c('Pais','EP','AG','PD','GS')
attach(base2)
df <- base2[,2:5]

Y = matrix(base2$EP)
X1 = matrix(data = c(base2$AG,base2$PD,base2$GS),nrow = 20,ncol = 3)
X2 = matrix(data = c(base2$AG,log(base2$PD),base2$GS),nrow = 20,ncol = 3)
X3 = matrix(data = c(base2$AG,base2$GS),nrow = 20,ncol = 2)

#Funcion para encontrar los betas
Beta <- function(X,y,R,r){
  #-----------------------------------------------------------------------------
  #Funcion que encuentra el valor de los coeficientes que acompaña a las varia-
  #bles independientes de una regresión lineal cuando se asume que el intercepto
  #es igual a 0. Aparte hace un analisis de hipotesis para saber si estos
  #coeficientes son significativos o no.
  #-----------------------------------------------------------------------------
  #Input: X: Matriz nxK que representa a las variables independientes o expli
  #          catorias.
  #       y: Vector nx1 que representa a las variables dependientes.
  #       c: Matriz 1x1 que toma dos valores:
  #          c = 1; cuando el modelo tiene una constante.
  #          c = 0; cuando el modelo no tiene constante.
  #       b_bar: Vector Kx1 que contiene las hipotesis nulas para el testeo.
  #       R: Matriz rxK que tiene las ecuaciones para la prueba de hipotesis.
  #       r: Vector rx1 que tiene los valores de las hipotesis nulas.
  #-----------------------------------------------------------------------------
  #Output: BT: data frame que contiene el valor de los coeficientes, una prueba
  #            los resultados de la prueba de hipotesis, el test F y el R cua-
  #            drado.
  #-----------------------------------------------------------------------------
  
  H = c()
  K = ncol(X)
  n = nrow(X)
  B = solve(t(X)%*%X)%*%t(X)%*%Y
  
  Y_hat = X%*%B
  e = Y-Y_hat
  R2 = sum(Y_hat^2)/sum(Y^2)
  e2 = sum(e^2)
  K = ncol(X)
  S2 = e2/(n-K)
  V_b = S2*solve(t(X)%*%X)
  t = c()
  for (i in c(1:K)) {
    t = c(t,B[i,1]/sqrt(V_b)[i,i])
    if ((-qt(0.975,n-K)<B[i,1]/sqrt(V_b)[i,i])&(qt(0.975,n-K)>B[i,1]/sqrt(V_b)[i,i])){
      H = c(H,'No se rechaza H0')}
    else{
      H = c(H,'Se rechaza H0')
    }
  }
  num_r = nrow(r)
  F_Test = (t(R%*%B-r)%*%solve(R%*%V_b%*%t(R))%*%(R%*%B-r))/num_r
  Betas = c(1:K)
  BT = data.frame(Betas,B,t,H,F_Test,R2)
  return(BT)}

R_1 = matrix(c(1,0,0,0,1,0,0,0,1),nrow = 3,ncol = 3)
r_1 = matrix(c(0,0,0),ncol = 1,nrow = 3)
R_2 = matrix(c(1,0,0,1),ncol = 2,nrow = 2)
r_2 = matrix(c(0,0),ncol = 1,nrow = 2)
data1 = Beta(X1,y,R_1,r_1)
data2 = Beta(X2,y,R_1,r_1)
data3 = Beta(X3,y,R_2,r_2)

cor(df)

attach(base2)
data_1 = lm(EP~AG+PD+GS)
data_2 = lm(EP~AG+log(PD)+GS)
summary(data_1)
summary(data_2)


mean(data_1$residuals)
