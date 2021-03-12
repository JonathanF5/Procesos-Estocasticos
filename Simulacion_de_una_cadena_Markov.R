## matriz potencia (mat,k) mat^k
#
matrixpower <- function(mat,k) {
  if (k == 0) return (diag(dim(mat)[1])) 
  if (k == 1) return(mat)
  if (k > 1) return( mat %*% matrixpower(mat, k-1))
}

# Algoritmo para simular una cadena de Markov
# 
# Entradas: (i) Distribución inicial ??, (ii) matriz de transición P, (iii) número n de pasos.
# Salidas: X_0,X_1,.,X_n
# 
# Algoritmo:
#   Generar X_0 de acuerdo con ??
# PARA i=1,.,n
# Suponga que X_(i-1)=j
# Establecer que p=j-ésima fila de P
# Generar X_i de acuerdo con p
# FIN DE PARA

## Simula una cadena de Markov en tiempo discreto
# Simula n pasos de una cadena de Markov
# markov (init, mat, n, states)
# Genera X0, ..., Xn para una cadena de Markov con distribucion
# inicial init y matriz de transición mat
# Los estados pueden ser un vector de caracteres de estados; 
# el valor predeterminado es 1, .... k
markov <- function(init,mat,n,labels) { 
  if (missing(labels)) labels <- 1:length(init)
  simlist <- numeric(n+1)
  states <- 1:length(init)
  simlist[1] <- sample(states,1,prob=init)
  for (i in 2:(n+1)) 
  { simlist[i] <- sample(states,1,prob=mat[simlist[i-1],]) }
  labels[simlist]
}

## Simula la ruina del jugador
# gamble(k, n, p)
# k: estado inicial del jugador
# n: monto a ganar $n o arruinarse
# p: Probabilidad de ganar $1 en cada jugada
# La función devuelve 1 si el jugador finalmente se arruina o
# devuelve 0 si el jugador finalmente gana $n
gamble <- function(k,n,p) {
  stake <- k
  while (stake > 0 & stake < n) {
    bet <- sample(c(-1,1),1,prob=c(1-p,p))
    stake <- stake + bet
  }
  if (stake == 0) return(1) else return(0)
}   

# EJEMPLOS

# Simule las primeras 20 letras (vocal / consonante) de la cadena de 
# Márkov del poema de Pushkin
mat <- matrix(c(0.175,0.526,0.825,0.474),nrow=2)
markov(c(0.5,0.5),mat,20,c("v","c"))

# Simule 50 pasos de la caminata aleatoria en la figura 1 del apartado
# de EJEMPLOS DE CADENA MARKOV. 
# Repita la simulación 10 veces. ¿Cuántas de tus simulaciones terminan 
# en el vértice c. ¿Compare con la probabilidad exacta a largo plazo?
mat <- matrix(c(0,1/4,0,0,0,0,1,0,1/4,1/4,1/3,0,0,1/4,0,1/4,1/3,1/2,
                + 0,1/4,1/4,0,1/3,1/2,0,1/4,1/4,1/4,0,0,0,0,1/4,1/4,0,0),nrow=6)
markov(rep(1/6,6), mat, 50, c("a", "b", "c", "d", "e", "f"))
a <- replicate(10,markov(rep(1/6,6), mat, 50, c("a", "b", "c", "d", "e", "f"))[50])
a
matrixpower(mat,100)
# La probabilidad a largo plazo de visitar el vértice c es 4/18

# Simule la ruina del jugador para una apuesta inicial de $2, 
# en un juego justo. 
# (a) Estime la probabilidad de que el jugador se arruine antes de 
# ganar $5. 
a <- replicate(10000, gamble(k=2,n=5,p=0.5))
# La probabilidad de arruinarse es de 
mean(a)

# (b) Construya la matriz de transición para la cadena de Márkov 
# asociada. Estime la probabilidad deseada en (a) tomando altas 
# potencias matriciales. 
mat <- matrix(c(1,0.5,0,0,0,0,0,0,0.5,0,0,0,0,0.5,0,0.5,0,0,0,0,0.5,0,0.5,0,0,0,0,0.5,0,0,0,0,0,0,0.5,1), nrow = 6)
rownames(mat) <- 0:5
colnames(mat) <- 0:5
a <- round(matrixpower(mat,200),2)
a[3,1]

# (c) Compare sus resultados con la probabilidad exacta.
# la probabilidad deseada es (n-k)/n = 
(5-2)/5