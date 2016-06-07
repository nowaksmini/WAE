source("Algorithms.R")


mi <- 100
dimension <- 2
maxIterations <- 10000

runTest <- function(selectionType, crossoverType, fileName) {
  results <- matrix(data=0, nrow=28, ncol=2)
  for (fun_nr in 1:28) {
    population <- DifferentialEvolution(crossoverType, selectionType, fun_nr)
    results[fun_nr, 1] <- fun_nr
    results[fun_nr, 2] <- mean(cec2013(fun_nr, population))
  }
  
  write(t(results), file=fileName, ncolumns=2, append=FALSE, sep="\t")
}

# Testy
# I
f <- 0.5
cr <- 0.5

runTest(SelectRandom, BinomialCrossover, "rand_bin.txt")
runTest(SelectRandom, ExponentialCrossover, "rand_exp.txt")
runTest(SelectAverage, BinomialCrossover, "avg_bin.txt")
runTest(SelectAverage, ExponentialCrossover, "avg_exp.txt")
runTest(SelectBest, BinomialCrossover, "best_bin.txt")
runTest(SelectBest, ExponentialCrossover, "best_exp.txt")

# II
cr <- 0.5

f <- 0.2
runTest(SelectRandom, BinomialCrossover, "F_02_rand_bin.txt")
runTest(SelectAverage, BinomialCrossover, "F_02_avg_bin.txt")

f <- 0.5
runTest(SelectRandom, BinomialCrossover, "F_05_rand_bin.txt")
runTest(SelectAverage, BinomialCrossover, "F_05_avg_bin.txt")

f <- 0.8
runTest(SelectRandom, BinomialCrossover, "F_08_rand_bin.txt")
runTest(SelectAverage, BinomialCrossover, "F_08_avg_bin.txt")

# III
f <- 0.5

cr <- 0.2
runTest(SelectRandom, BinomialCrossover, "cr_02_rand_bin.txt")
runTest(SelectAverage, BinomialCrossover, "cr_02_avg_bin.txt")

cr <- 0.5
runTest(SelectRandom, BinomialCrossover, "cr_05_rand_bin.txt")
runTest(SelectAverage, BinomialCrossover, "cr_05_avg_bin.txt")

cr <- 0.8
runTest(SelectRandom, BinomialCrossover, "cr_08_rand_bin.txt")
runTest(SelectAverage, BinomialCrossover, "cr_08_avg_bin.txt")



