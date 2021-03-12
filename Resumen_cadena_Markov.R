library(igraph)
library(ggplot2)
library(tidyverse)
library(zoo)
stationary <- function(mat) {
  x = eigen(t(mat))$vectors[,1]
  as.double(x/sum(x))
}

# PageRank ----
Q <- matrix(c(0,0,0,0,1/2,1/2,0,
              1/3,0,1/3,0,0,1/3,0,
              0,0,0,1/2,0,1/2,0,
              0,0,0,0,0,1,0,
              1/4,0,0,1/4,0,1/4,1/4,
              1/2,1/2,0,0,0,0,0,
              1/7,1/7,1/7,1/7,1/7,1/7,1/7),
            nrow=7,byrow=TRUE)
states <- c("1","2","3","4","5","6","7")
rownames(Q) <- states
colnames(Q) <- states
Q
grap_1 <- graph.adjacency(t(Q), weighted = TRUE, mode = "directed")
plot(grap_1)
A <- matrix(rep(1/7,49),nrow=7)
rownames(A) <- states
colnames(A) <- states
A
# Matriz de transición con un factor de amortiguamiento de p=0.85
P <- 0.85*Q + 0.15*A
grap_2 <- graph.adjacency(t(P), weighted = TRUE, mode = "directed")
plot(grap_2)

# FIN PageRank ----

#### Computaciones Básicas ----
matrixpower <- function(mat,k) {
  if (k == 0) return (diag(dim(mat)[1])) 
  if (k == 1) return(mat)
  if (k > 1) return( mat %*% matrixpower(mat, k-1))
}
markov <- function(init,mat,n,labels) { 
  if (missing(labels)) labels <- 1:length(init)
  simlist <- numeric(n+1)
  states <- 1:length(init)
  simlist[1] <- sample(states,1,prob=init)
  for (i in 2:(n+1)) 
  { simlist[i] <- sample(states,1,prob=mat[simlist[i-1],]) }
  labels[simlist]
}

# Matriz de transición de n pasos
P
matrixpower(P,2)
matrixpower(P,3)
matrixpower(P,4)
matrixpower(P,5)
matrixpower(P,6)
matrixpower(P,7)
matrixpower(P,8)
matrixpower(P,9)
matrixpower(P,10)
matrixpower(P,20)

# Relación Chapman-Kolmogorov
matrixpower(P,5)[1,4]
(matrixpower(P,1) %*% matrixpower(P,4))[1,4]
(matrixpower(P,2) %*% matrixpower(P,3))[1,4]

# Distribución de X_n
# X_0 = alfa
X_0 <- c(1,0,0,0,0,0,0)
(X_1 <- X_0 %*% P)
(X_2 <- X_0 %*% matrixpower(P,2))
(X_3 <- X_0 %*% matrixpower(P,3))
(X_20 <- X_0 %*% matrixpower(P,20))

# Distribución conjunta
# P(X_2=1,X_3=4,X_20=7) = [X_0 P^2]_1 [P^(3-2)]_1,4 [P^(20-3)]_4,7

(X_0 %*% matrixpower(P,2))[1] * matrixpower(P,3-2)[1,4] *
  matrixpower(P,20-3)[4,7]

#### FIN Computaciones Básicas ----

#### Comportamiento a largo plazo ----

# Distribución limitante
# También se puede interpretar como "Proporción de tiempo en cada estado"
matrixpower(P,20)
matrixpower(P,30)
matrixpower(P,40)

iterar.P <- function(a, P, n) {
  res <- matrix(NA, n+1, ncol = length(a))
  res[1,] <- a
  for (i in seq_len(n))
    res[i+1,] <- a <- a %*% P
  res
}
n <- 30
a <- c(1,0,0,0,0,0,0)
P
y1 <- iterar.P(a,P,n)
colnames(y1) <- states
y1 %>% as.data.frame() %>% tbl_df()
autoplot(zoo(y1,order.by = 0:n), facet = NULL) + geom_point()
matrixpower(P,30)

a <- c(1,0,0,0,0,0,0)
(P_n <- a %*% matrixpower(P,30))

# Distribución estacionaria
lambda <- stationary(P)
lambda
lambda %*% P

# Matrices Regulares
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
i <- 2; c <- 0; d <- 0
x <- list(P)
M.regular(i,c,d,x)

# Recurrencia y Transitoriedad
library(markovchain)
# period(CM)
# communicatingClasses(CM)
# recurrentClasses(CM)
# transientClasses(CM)
# transientStates(CM)
# recurrentStates(CM)
# absorbingStates(CM)
# canonicForm(CM)
# summary(CM)
CM <- new("markovchain", states=c("1","2","3","4","5","6","7"),
          transitionMatrix=P, 
          name="CM")
recurrentClasses(CM)
recurrentStates(CM)
transientClasses(CM)
transientStates(CM)

# CM: Irreducibles, Tiempo de retorno esperado
summary(CM)
lambda <- stationary(P)
1/lambda

# Periodicidad
period(CM)

# CM Ergódica (CM irreducible y aperiódica)
summary(CM)

# Cadenas de Markov Absorbente
P <- matrix(c(0.03,0.91,0.00,0.00,0.06,0.00,
              0.00,0.03,0.91,0.00,0.06,0.00,
              0.00,0.00,0.03,0.93,0.04,0.00,
              0.00,0.00,0.00,0.03,0.04,0.93,
              0.00,0.00,0.00,0.00,1.00,0.00,
              0.00,0.00,0.00,0.00,0.00,1.00),nrow=6,byrow=T)
states <- c("1","2","3","4","Aband","Grad")
rownames(P) <- states
colnames(P) <- states
P
Q <- P[1:4,1:4]
R <- P[1:4,5:6]

# Probabilidad de Absorción
f <- solve(diag(4)-Q)
f %*% R
# Tiempo de Absorción
f %*% rep(1,4)

#### FIN Comportamiento a largo plazo ----