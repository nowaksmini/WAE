
library(cec2013)

mi <- 3
maxIterations <- 2
F <- 0.4

select <- function(P){
  0
}

sample <- function(P){
  c(0,0)
}

crossover <- function(P, M){
  P
}

tournament <- function(P, M){
  P
}

DifferentialEvolution <- function() {
  P <- array(0, c(mi))
  print(P)
  H <- P
  print(H)
  t <- 0
  stop <- 0
  while (stop < maxIterations){
    for (i in 1:mi){
      P[i] <- select(P)
      print(P[i])
      Parents <- sample(P)
      print(Parents)
      M <- P[i] + F * (Parents[0] - Parents[1])
      print(M)
      O <- crossover(P[i], M)
      print(O)
      H <- c(H,O)
      print(H)
      P[i] <- tournament(P[i], O)
      print(P[i])
    }
    stop <- stop + 1
  }
}

DifferentialEvolution()

