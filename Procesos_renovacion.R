
#### INICIO Procesos de renovación ----

# Simulación de N_t: Número de renovaciones que se han
# producido en el momento t.
set.seed(999)
t <- 6
x <- runif(100,0,2); x # Distribución del tiempo entre llegadas
y <- cumsum(x); y
z <- which.max(y > t) - 1 # menor n para el que T_n > 6
z

# Repite la simulación de N_t, para dar una estimación de E[N_t]
# es decir, el número esperado de renovaciones hasta el tiempo t.
muestra <- rep(0,1000)
set.seed(999)
for (i in 1:1000){
  x <- runif(100,0,2)
  y <- cumsum(x)
  muestra[i] <- which.max(y > 6) - 1
}
muestra
mean(muestra)

# N_t/t : Tasa de renovación promedio, converge con probabilidad
# 1 al valor 1/u, con u=E[t_n]: Tiempo medio entre renovaciones 

conv <- function(error,t,u=1){
  muestra <- rep(0,10000)
  for (i in 1:10000){
    x <- runif(100000,0,2)
    y <- cumsum(x)
    muestra[i] <- which.max(y > t) - 1 #
  }
  mean(abs((muestra/t) - (1/u)) < error)
}
conv(0.01,100)
conv(0.01,500)
conv(0.01,1000)
conv(0.01,10000)

#### FIN Procesos de renovación ----

#### INICIO Procesos de renovación con recompensas ----
# X_n : Ganancia en la renovación con distribución N(3,0.5)
ganancia <- rep(0,1000)
for (i in 1:1000){
  x <- runif(100,0,2)
  z <- rnorm(100,3,0.5)
  y <- cumsum(x)
  t <- cumsum(z)
  h <- which.max(y > 6) - 1
  ganancia[i] <- t[h]
}
ganancia
mean(ganancia)

#### FIN Procesos de renovación con recompensas ----

#### INICIO Procesos de renovación alternantes ----
t <- rep(0,200)
tiempos <- rep(0,200)
estado1 <- runif(100,0,2)
estado2 <- rexp(100,1.5)
for(i in 1:100){
  t[2*i-1] <- estado1[i]
  t[2*i] <- estado2[i]
  tiempos <- cumsum(t)}
tiempos

total1 <- sum(estado1)
total <- sum(estado1)+sum(estado2)
total1/total

#### FIN Procesos de renovación alternantes ----