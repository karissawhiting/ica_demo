library(JADE)
library(tuneR)


S1 <- readWave("./isaak.wav") #44100
S1 <- S1[1:114624]
S2 <- readWave("./snap.wav")
S2 <- S2[1:114624]
S3 <- readWave("./karissa.wav")
S3 <- S3[1:114624]
file.remove("s1.wav", "s2.wav", "s3.wav")
play(S1)
play(S2)
play(S3)

set.seed(321)
NOISE <- noise("white", duration = 114624)
play(NOISE)

test<- Wave(S2@left)
play(test)
S <- cbind(S1@left, S2@left, S3@left, NOISE@left)
S <- scale(S, center = FALSE, scale = apply(S, 2, sd))
St <- ts(S)#, start = 0, frequency = 8000)
p <- 4
A <- matrix(runif(p^2, 0, 1), p, p)
A
X <- tcrossprod(St, A)
Xt <- as.ts(X)
Xt

plot(St, main = "Sources")
plot(Xt, main = "Mixtures")

setWavPlayer("/Applications/'QuickTime Player.app'/Contents/MacOS/'i'")
setWavPlayer("afplay")

x1 <- normalize(Wave(left = X[, 1], samp.rate = 44100, bit = 16),
                unit = "8")
x2 <- normalize(Wave(left = X[, 2], samp.rate = 44100, bit = 16),
                unit = "8")
x3 <- normalize(Wave(left = X[, 3], samp.rate = 44100, bit = 16),
                unit = "8")
x4 <- normalize(Wave(left = X[, 4], samp.rate = 44100, bit = 16),
                unit = "8")
play(x1)
play(x2)
play(x3)
play(x4)

#different sep methods
#each method has different assumptions
jade <- JADE(X)
sobi <- SOBI(Xt)
nsstdjd <- NSS.TD.JD(Xt)

#performance indicies
#nsstdjd performs the best
MD(coef(jade), A)
MD(coef(sobi), A)
MD(coef(nsstdjd), A)

Z.nsstdjd <- bss.components(nsstdjd)
NSSTDJDwave1 <- normalize(Wave(left = as.numeric(Z.nsstdjd[, 1]),samp.rate = 44100, bit = 16), unit = "8")
NSSTDJDwave2 <- normalize(Wave(left = as.numeric(Z.nsstdjd[, 2]), samp.rate = 44100, bit = 16), unit = "8")
NSSTDJDwave3 <- normalize(Wave(left = as.numeric(Z.nsstdjd[, 3]), samp.rate = 44100, bit = 16), unit = "8")
NSSTDJDwave4 <- normalize(Wave(left = as.numeric(Z.nsstdjd[, 4]), samp.rate = 44100, bit = 16), unit = "8")
play(NSSTDJDwave1)
play(NSSTDJDwave2)
play(NSSTDJDwave3)
play(NSSTDJDwave4)

