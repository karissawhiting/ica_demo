library(JADE)
library(tuneR)


S1 <- readWave("./sample1.wav") #44100
#S1 <- S1[1:114624]
S2 <- readWave("./sample2.wav")
#S2 <- S2[1:114624]
file.remove("s1.wav", "s2.wav", "s3.wav")
play(S1)
play(S2)


S <- cbind(S1@right, S2@right)
S <- scale(S, center = FALSE, scale = apply(S, 2, sd))
St <- ts(S)#, start = 0, frequency = 8000)


#plot(St, main = "Sources")
#plot(Xt, main = "Mixtures")

#different sep methods
#each method has different assumptions
jade <- JADE(S)
sobi <- SOBI(St)
nsstdjd <- NSS.TD.JD(St)

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
#play(NSSTDJDwave4)

