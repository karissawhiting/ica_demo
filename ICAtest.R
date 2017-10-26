install.packages("tuneR", repos = "http://cran.r-project.org")
library(tuneR)
library(fastICA)

snap<- readWave("./aud1.wav")
snap2<- snap
snapleft<- snap@left

test<- Wave(snapleft, samp.rate = 48000)
writeWave(test, "./aud5.wav")

a <- fastICA(snapleft, 2, alg.typ = "parallel", fun = "logcosh", alpha = 1,
             method = "R", row.norm = FALSE, maxit = 200,
             tol = 0.0001, verbose = TRUE)

normalize(snap, unit = "16", center = TRUE, level = 1, rescale = TRUE)
writeWave(snap2, "./aud2.wav")
writeWave(left_wav, "./aud3.wav")

###########################################
library(JADE)
library(tuneR)

download.file("http://research.ics.aalto.fi/ica/cocktail/source5.wav","s1.wav", mode = "wb")

download.file("http://research.ics.aalto.fi/ica/cocktail/source7.wav", "s2.wav", mode = "wb")

download.file("http://research.ics.aalto.fi/ica/cocktail/source9.wav", "s3.wav", mode = "wb")

S1 <- readWave("s1.wav")
S2 <- readWave("s2.wav")
S3 <- readWave("s3.wav")
file.remove("s1.wav", "s2.wav", "s3.wav")
play(S1)
play(S2)
play(S3)

set.seed(321)
NOISE <- noise("white", duration = 50000)
S <- cbind(S1@left, S2@left, S3@left, NOISE@left)
S <- scale(S, center = FALSE, scale = apply(S, 2, sd))
St <- ts(S, start = 0, frequency = 8000)
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

x1 <- normalize(Wave(left = X[, 1], samp.rate = 8000, bit = 8),
                   unit = "8")
x2 <- normalize(Wave(left = X[, 2], samp.rate = 8000, bit = 8),
                   unit = "8")
x3 <- normalize(Wave(left = X[, 3], samp.rate = 8000, bit = 8),
                   unit = "8")
x4 <- normalize(Wave(left = X[, 4], samp.rate = 8000, bit = 8),
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
NSSTDJDwave1 <- normalize(Wave(left = as.numeric(Z.nsstdjd[, 1]),samp.rate = 8000, bit = 8), unit = "8")
NSSTDJDwave2 <- normalize(Wave(left = as.numeric(Z.nsstdjd[, 2]), samp.rate = 8000, bit = 8), unit = "8")
NSSTDJDwave3 <- normalize(Wave(left = as.numeric(Z.nsstdjd[, 3]), samp.rate = 8000, bit = 8), unit = "8")
NSSTDJDwave4 <- normalize(Wave(left = as.numeric(Z.nsstdjd[, 4]), samp.rate = 8000, bit = 8), unit = "8")
play(NSSTDJDwave1)
play(NSSTDJDwave2)
play(NSSTDJDwave3)
play(NSSTDJDwave4)

