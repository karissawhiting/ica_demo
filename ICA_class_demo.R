library(JADE) 
library(tuneR)
setwd("~/Repositories/ica_demo")


#####################################
#Import Sound Data

#set app to play waves- #mac

setWavPlayer("afplay")

#read in .wav files and subset so they are the same size
S1 <- readWave("./snapping.wav") #44100
S1 <- S1[1:474235]
S2 <- readWave("./hp_trim2.wav")
S2 <- S2[1:474235]
S3 <- readWave("./whistl_trim.wav")
S3 <- S3[1:474235]


#play audio files
play(S1)
play(S2)
play(S3)

#makes it quieter
#Stest<- S1/5
#play(Stest)

#plot right channel input
#plot(S1@right)

#create white noise file same duration as the rest of the clips
set.seed(321)
NOISE <- noise("white", duration = 474235)
play(NOISE)

#bind all the samples together in data frame
S <- cbind(S1@left, S2@left, S3@left, NOISE@left)

#scale the components to have unit variances
S <- scale(S, center = FALSE, scale = apply(S, 2, sd))
St <- ts(S)#, start = 0, frequency = 8000)

#create linear combo variables for mixing matrix
p <- 4
A <- matrix(runif(p^2, 0, 1), p, p)
A

#X = A*S. A is weights
#rows of X are linear mixtures of pure source signals
X <- tcrossprod(St, A)
Xt <- as.ts(X)

plot(St, main = "Sources")
plot(Xt, main = "Mixtures")

#center and rescale to canonical interval to be able to be played
x1 <- normalize(Wave(left = X[, 1], samp.rate = 44100, bit = 16),
                unit = "8")
x2 <- normalize(Wave(left = X[, 2], samp.rate = 44100, bit = 16),
                unit = "8")
x3 <- normalize(Wave(left = X[, 3], samp.rate = 44100, bit = 16),
                unit = "8")
x4 <- normalize(Wave(left = X[, 4], samp.rate = 44100, bit = 16),
                unit = "8")

#play mixed "observations"
play(x1)
play(x2)
play(x3)
play(x4)

#####################################
#ICA

#several different algorithms to perform ICA, depending on the criteria 
#used to determine the statistical independence and the method used to maximize it.
# All algorithms work to determine the ICs by calculating a demixing matrix, W, 
# which approximates the inverse of the mixing matrix, A−1
# The objective of ICA is therefore to calculate W, knowing only X

#JADE 
#uses fourth order moments (kurtosis) to measure non-guassianity. (proxy for independence)
#JADE assumption Gaussian distributions possess zero excess kurtosis,
#seeks an orthogonal rotation of the observed mixed vectors to estimate source vectors 
#which possess high values of excess kurtosis.

#SOBI 
#joint diagonalization of whole set of autocovariance matricies

jade <- JADE(X)
sobi <- SOBI(Xt)

#performance indicies
#Minimum Distance Index- compares the estimated unmixing matrix to the true A matrix
MD(coef(jade), A)
MD(coef(sobi), A)

#extract estimated sources
Z.nsstdjd <- bss.components(jade)
Z.nsstdjd <- bss.components(sobi)

#normalize to listen
NSSTDJDwave1 <- normalize(Wave(left = as.numeric(Z.nsstdjd[, 1]),samp.rate = 44100, bit = 16), unit = "8")
NSSTDJDwave2 <- normalize(Wave(left = as.numeric(Z.nsstdjd[, 2]), samp.rate = 44100, bit = 16), unit = "8")
NSSTDJDwave3 <- normalize(Wave(left = as.numeric(Z.nsstdjd[, 3]), samp.rate = 44100, bit = 16), unit = "8")
NSSTDJDwave4 <- normalize(Wave(left = as.numeric(Z.nsstdjd[, 4]), samp.rate = 44100, bit = 16), unit = "8")
play(NSSTDJDwave1)
play(NSSTDJDwave2)
play(NSSTDJDwave3)
play(NSSTDJDwave4)



