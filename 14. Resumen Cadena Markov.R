library (igraph)

#### Cadena de Markov de un paso ----

# Ejemplo 1: CM Independiente
P <- matrix(c(0.4,0.2,0.2,0.2,
              0.4,0.2,0.2,0.2,
              0.4,0.2,0.2,0.2,
              0.4,0.2,0.2,0.2),nrow=4,byrow=T)
states <- c("Bus","Taxi","Auto","Bicicleta")
rownames(P) <- states
colnames(P) <- states
P

# Ejemplo 2: CM Dependiente
P <- matrix(c(0.4,0.6,0.0,0.0,
              0.0,0.0,1.0,0.0,
              0.0,0.0,0.0,1.0,
              1.0,0.0,0.0,0.0),nrow=4,byrow=T)
states <- c("Bus","Taxi","Auto","Bicicleta")
rownames(P) <- states
colnames(P) <- states
P

# Ejemplo 2: CM de Pase aleatorio en un gráfico
### 3.4 Tipos especiales de grafos
# Grafo completo
Comp.g <- graph.full(5)
plot.igraph(Comp.g) # verificar que todos tengan la misma gráfica
# Grafo regular
Reg.g <- sample_k_regular(5,2)
plot.igraph(Reg.g)
# Lista de adyacencia :
get.adjlist(Reg.g)
# Lista de aristas :
get.edgelist(Reg.g)
# Matrices de adyacencia :
a <- matrix(c(matrix(get.adjacency(Reg.g))), nrow = 5, byrow = T); a
data.frame(vertice=c(1,2,3,4,5),Pij=1/apply(a, 1, sum))
P <- matrix(rep(0,25),nrow = 5, byrow = T)
for (i in 1:5) {
  for (j in 1:5) {
    P[i,j] <- ifelse(test = a[i,j] == 0, yes = 0, no = 0.5)
  }
}
P



# Computaciones Básicas
matrixpower <- function(mat,k) {
  if (k == 0) return (diag(dim(mat)[1])) 
  if (k == 1) return(mat)
  if (k > 1) return( mat %*% matrixpower(mat, k-1))
}



# Relación Chapman-Kolmogorov
# P^(m+n) = P^m + P^n
m <- 10
n <- 20
A <- matrixpower(P,m+n)
B <- matrixpower(P,m) %*% matrixpower(P,n)
A-B



# Distribución de X_n
# P(X_n = j) = (a P^n)j
n <- 10
j <- 1 # Nos preguntamos cuál es la probabilidad de estar en el paso 10 en el estado o vértice 1
a <- c(1,0,0,0,0)
P_n <- a %*% matrixpower(P,n); P_n
P_n[j]

# Distribución conjunta
# P(X_n_1 = i_1, X_n = i_2, X_n = i_3) = (a P^n_1)i_1 (P^(n_2-n_1))i_1,i_2
# (P^(n_3-n_2))i_2,i_3
# r = P(X_n_1 = i_1, X_n = i_2, X_n = i_3)
# b = (a P^n_1)i_1
# c = (P^(n_2-n_1))i_1,i_2
# d = (P^(n_3-n_2))i_2,i_3

n_1 <- 2
n_2 <- 5
n_3 <- 7
i_1 <- 1
i_2 <- 5
i_3 <- 1
a <- c(1,0,0,0,0)
b <- (a %*% matrixpower(P,n_1))[i_1]
c <- matrixpower(P,n_2-n_1)[i_1,i_2]
d <- matrixpower(P,n_3-n_2)[i_2,i_3]
r <- b*c*d; r

#### FIN Cadena de Markov de un paso ----

#### Cadena de Markov a largo plazo ----

# Distribución limitante
# También se puede interpretar como "Proporción de tiempo en cada estado"
iterar.P <- function(a, P, n) {
  res <- matrix(NA, n+1, ncol = length(a))
  res[1,] <- a
  for (i in seq_len(n))
    res[i+1,] <- a <- a %*% P
  res
}
n <- 30
a <- c(0.325,0.275,0.25,0.05,0.1)
P

y1 <- iterar.P(a,P,n)
y2 <- iterar.P(c(0.275,0.325,0.25,0.05,0.1),P,n)

matplot(0:n, y1, type="l", lty=1, xlab="Step", ylab="y", las=1)
matlines(0:n, y2, lty=2)

matrixpower(P,75)

a <- c(1,0,0,0,0)
P_n <- a %*% matrixpower(P,75); P_n



# Distribución estacionaria de una cadena de Markov
stationary <- function(mat) {
  x = eigen(t(mat))$vectors[,1]
  as.double(x/sum(x))
}
## Ejemplo
lambda <- stationary(P)
lambda
lambda %*% P



# Matrices Regulares
i <- 2
c <- 0
d <- 0
x <- list(P)
M.regular <- function(i,c,d,x) {
  while (d == 0 & c == 0) {
    x[[i]] <- matrixpower(P,i)
    d <- prod(x[[i]] > 0)
    c <- prod(x[[i-1]] == x[[i]])
    
    if (d != 0)
      return(paste("La matriz es regular con n igual a",i,".","Por lo tanto, la CM tiene una distribución limitante."))
    
    if (c != 0)
      return(paste("La matriz no es regular. La búsqueda terminó en n igual a",i))
    
    i <- i + 1
  }
}

M.regular(i,c,d,x)



# Tiempo de retorno esperado
# Valor esperado de tiempo de retorno
markov <- function(init,mat,n,labels) { 
  if (missing(labels)) labels <- 1:length(init)
  simlist <- numeric(n+1)
  states <- 1:length(init)
  simlist[1] <- sample(states,1,prob=init)
  for (i in 2:(n+1)) 
  { simlist[i] <- sample(states,1,prob=mat[simlist[i-1],]) }
  labels[simlist]
}
P
states <- c("e.1","e.2","e.3","e.4","e.5")
colnames(P) <- states
rownames(P) <- states; P
init <- c(1,0,0,0,0)
markov(init,P,25,states)

trials <- 10000
simlist <- numeric(trials)
for (i in 1:trials) {
  path <- markov(init,P,25,states)
  # índice de búsqueda de la segunda aparición de "a"
  # resta 1 para tener en cuenta el tiempo 0
  returntime <- which(path == "e.1")[2] - 1
  simlist[i] <- returntime
}
# tiempo de retorno esperado al estado "e.1"
mean(simlist,na.rm = TRUE)

# tiempo de retorno esperado teórica al estado "e.1"
lambda <- stationary(P)
1/lambda



# CM Ergódica (CM irreducible y aperiódica)
# Ejemplo Rango de Página
# Probabilidades de Rango de Página
Q <- matrix(c(0,0,0,0,1/2,1/2,0,
              1/3,0,1/3,0,0,1/3,0,
              0,0,0,1/2,0,1/2,0,
              0,0,0,0,0,1,0,
              1/4,0,0,1/4,0,1/4,1/4,
              1/2,1/2,0,0,0,0,0,
              1/7,1/7,1/7,1/7,1/7,1/7,1/7),
            nrow=7,byrow=TRUE)
states <- c("a","b","c","d","e","f","g")
rownames(Q) <- states
colnames(Q) <- states
Q
A <- matrix(rep(1/7,49),nrow=7)
rownames(A) <- states
colnames(A) <- states
A
# Matriz de transición con un factor de amortiguamiento de p=0.85
P_1 <- 0.85*Q + 0.15*A
pr <- stationary(P_1)
pr # Probabilidad Estacionaria



# CADENAS ABSORBENTES
# Escaleras y rampas
# Carga de la matriz P
load(file = "Exports/Escalera.Rampas.RData")
apply(P, 1, sum)
init <- c(1,rep(0,100))# Inicia en el estado 0 (fuera del tablero)
## simulación del juego
markov(init,P,500,0:100)
## 1000 réplicas de la simulación del juego
simlist <- replicate(1000,which(markov(init,P,500,0:100) == 100)[1])
mean(simlist)

Q <- P[1:100, 1:100]
# Matriz fundamental
f <- solve(diag(100) - Q)
# Probabilidad de absorción
R <- P[-101,101]
f %*% R
## Tiempo de absorción
f %*% rep(1,100)

#### FIN Cadena de Markov a largo plazo ----