#---------------------------------------------------------------------------
# Simulation of Data
# 29.04.2014

#---------------------------------------------------------------------------
# Simulate predictors from a uniform distribution
# N = 300 #

set.seed(2)
N300 = 300 # number of elements
dats300 <- as.data.frame(matrix(NA, N300, 5)) # prepare dataframe to store predictors in
colnames(dats300) <- paste0("X", 1:5) # name predictors X1:X5
for (i in 1:5) dats300[, i] <- runif(N300, min=0, max=1) # sample X1:X5 from a uniform distribution

# BERNOULLI #

set.seed(2)
Ybin300 <- rbinom(N300, size = 1, prob=plogis(-6.66 + 5*dats300$X1  - 10*(dats300$X1^2) + 10*dats300$X2 +10*dats300$X3*dats300$X4))
#min(table(Ybin300)) # effective sample size

#---------------------------------------------------------------------------
# Simulate predictors from a uniform distribution


# N = 250 #

set.seed(2)
N250 = 250 # number of elements
dats250 <- as.data.frame(matrix(NA, N250, 5)) # prepare dataframe to store predictors in
colnames(dats250) <- paste0("X", 1:5) # name predictors X1:X5
for (i in 1:5) dats250[, i] <- runif(N250) # sample X1:X5 from a uniform distribution

# GAUSSIAN #

set.seed(2)
Ynorm250 <- -5 + 5*dats250$X1 - 10*(dats250$X1)^2 + 10*dats250$X2 + 10*dats250$X3*dats250$X4 + rnorm(250, mean=0, sd=2)
#---------------------------------------------------------------------------

