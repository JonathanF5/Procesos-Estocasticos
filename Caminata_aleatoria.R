#######################################################
########### PROGRAMA CAMINATA SIMPLE ##################
#######################################################

#### Trayectoria Caminata Aleatoria ----

#Funci�n que simula una trayectoria
#para una caminata aleatoria simple
#n=n�mero de pasos
#p=probabilidad de incrementar en uno en cada paso.
#y=posici�n inicial

caminata_aleatoria <- function(n,p,y){
  r <- sample(x = c(1,-1), size = n, replace = TRUE, prob = c(p, 1-p))
  for(i in 1:n){
    y[i+1] <- y[i] + r[i]
  }
  x <- 0:n
  r <- c(0,r)
  plot(x,y,type='o',pch=16,cex=0.9,
       main='Caminata Aleatoria Simple',
       ylab='Espacio de Estados',
       xlab='Espacio Parametral')
  abline(h=0,v=0,col='red')
  cbind(x,y,r)
}

# Considere una caminata aleatoria de 1000 pasos con probabilidad
# de incremento de una unidad de 0.5, empezando en la posici�n
# 0.
n <- 100
p <- 0.5
y <- 0

caminata_aleatoria(n,p,y)

## Promedio y Varianza
# Te�rico
n*(p-(1-p))
4*n*p*(1-p)

# Simulaci�n
caminata <- caminata_aleatoria(n,p,y)
caminata <- as.data.frame(caminata)
mean(caminata$r)
var(caminata$r)*n

#### FIN Trayectoria Caminata Aleatoria ----

#### Esperanza y Varianza Caminata Aleatoria ----

#Funci�n que simula m trayectorias
#distintas de una caminata aleatoria
#simple que comienza en cero.
#Se dan n pasos y se observa la posici�n,
#al final se saca el promedio.
#El resultado responde la pregunta 
#�Cu�l es la E(X_n)? 
#(te�ricamente E(X_n)=n*(p-q))
#p=probabilidad de incrementar en uno en cada paso.

esp_var_Xn=function(n,p,m){
  sim <- 0; q <- 1-p
  for(i in 1:m){ 
    e <- sample(x = c(1,-1), size = n, replace = TRUE, prob = c(p, 1-p))
    X <- 0
    for(j in 1:n){
      X[j+1] <- X[j]+e[j]}
    sim[i] <- X[n+1]}
  a <- paste("E(X_n) te�rico = ", n*(p-q))
  b <- paste("E(X_n) Monte Carlo = ", mean(sim))
  c <- paste("Var(X_n) te�rico = ", 4*n*p*(1-p))
  d <- paste("Var(X_n) Monte Carlo = ", round(var(sim),3))
  rbind(a,b,c,d)
}
##EJEMPLO:

esp_var_Xn(10,.7,20000)

#### FIN Esperanza y Varianza Caminata Aleatoria ----

#### Probabilidad Regreso Origen ----

#Probabilidad de un eventual 
#regreso al estado de partida
#Te�ricamente es 1-|p-q|.
#n=n�mero de simulaciones.
#u=posici�n inicial
#p=probabilidad de incrementar en uno en cada paso.
#LIM=n�mero l�mite de pasos que generamos
#Si LIM y n son muy grandes, la aproximaci�n debe 
#ser muy buena.

proba_regreso=function(n,u,p,LIM){
  q=1-p; pr=1-abs(p-q); N=0
  for(i in 1:n){
    paso1 <- sample(x = c(1,-1), size = 1, replace = TRUE, prob = c(p, 1-p))
    aux <- u+paso1 
    j <- 0
    while(aux!=u && j!=LIM){ 
      e <- sample(x = c(1,-1), size = 1, replace = TRUE, prob = c(p, 1-p))
      aux <- aux+e
      j <- j+1
    }
    N[i] <- (j<LIM)#N[i] es cero o uno dependiendo si se regres� o no se regres�.
  }
  a <- paste("Probabilidad de regreso a la posici�n de origen MC = ",mean(N))
  b <- paste("Probabilidad de regreso a la posisic�n de origen te�rica = ",pr)
  rbind(a,b)
}

##EJEMPLO:

proba_regreso(100,0,.3,10000)

#### FIN Probabilidad Regreso Origen ----

#### Ruina del Jugador ----

#ruina_jugador realiza un juego de apuestas
#y muestra la trayectoria hasta que el 
#jugador A gane o pierda.
#u=posici�n inicial
#N=capital conjunto de los jugadores A y B
#p=probabilidad de incrementar en uno en cada paso.

ruina_jugador <- function(u,N,p){
  k <- u; j <- 0; C <- u
  while(k>0 && k<N){ 
    e <- sample(x = c(1,-1), size = 1, replace = TRUE, prob = c(p, 1-p))
    y <- k+e; k <- y; j <- j+1
    C <- c(C,k)
  }
  plot(0:j,C,type="o",ylim=c(0,N))
  abline(h=c(0,N),v=c(0,j))
  a <- j/2; b <- .8*N
  text(a,b,labels=paste('tau =',j),pos=3,cex=2,col="blue")
  j
}

##EJEMPLO

ruina_jugador(20,100,.6)

#### FIN Ruina del Jugador ----

#### Ruina del Jugador: Tiempo de espera simulaci�n ----

#tiempo_absorci�n hace una sola simulaci�n
#del juego de apuesta y nos da su duraci�n.

tiempo_absorcion <- function(u,N,p){
  k <- u; j <- 0
  while(k>0 && k<N){ 
    e <- sample(x = c(1,-1), size = 1, replace = TRUE, prob = c(p, 1-p))
    y <- k+e; k <- y; j <- j+1}
  j
}

## EJEMPLO

tiempo_absorcion(20,100,.6)

#### FIN Ruina del Jugador: Tiempo de espera simulaci�n ----

#### Ruina del Jugador: Tiempo de espera exacto ----

#ta_teorico calcula de forma exacta el
#tiempo esperado del juego de apuestas.
#u=posici�n inicial
#N=capital conjunto de los jugadores A y B
#p=probabilidad de incrementar en uno en cada paso.

ta_teorico <- function(u,N,p){
  q <- 1-p
  if(p==.5){Res <- u*(N-u)}
  if(p!=.5){Res <- (1/(q-p))*(u-N*((1-(q/p)^u)/(1-(q/p)^N)))}
  Res
}

## EJEMPLO
ta_teorico(20,100,.6)

#### FIN Ruina del Jugador: Tiempo de espera exacto ----

#### Comparaci�n: Tiempo de espera exacto y de simulaci�n ---- 

#ta_mc hace "n" simulaciones del juego de
#apuestas y al final saca el promedio de
#duraci�n y lo compara con el te�rico.
#u=posici�n inicial
#N=capital conjunto de los jugadores A y B
#p=probabilidad de incrementar en uno en cada paso.

ta_mc <- function(u,N,p,n){
  Ta <- numeric(n)
  for(i in 1:n){Ta[i] <- tiempo_absorcion(u,N,p)}
  paste("Teorico=",ta_teorico(u,N,p),"MonteCarlo=",mean(Ta))
}

##Ejemplo

ta_mc(20,100,.6,100)

#### FIN Comparaci�n: Tiempo de espera exacto y de simulaci�n ---- 
