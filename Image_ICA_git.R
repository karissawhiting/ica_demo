### ICA Image test
### Shuang "Steve" Wu, 10/30/2017

source("https://bioconductor.org/biocLite.R")
biocLite("EBImage")
library(EBImage)

rm(list=ls())

### Import 4 images
test1=readImage("./Test1_g.jpg") 
test2=readImage("./Test2_g.jpg")
test3=readImage("./Test3_g.jpg")
test4=readImage("./Test4_g.jpg")

display(test1); display(test2); display(test3); display(test4) # Take a look at them

### A linear transformation of test1 and test2
img1=0.4*test1+0.6*test2
img2=0.7*test1+0.2*test2
#display(img1); display(img2)

## Convert the two transformations to vectors
img1_vec=as.vector(img1) 
img2_vec=as.vector(img2) 

## Construct the mixture data frame
S1 <- cbind(img1_vec, img2_vec)

# FastICA
ICA1=fastICA(S1, 2,  alg.typ = "parallel", fun = "logcosh", alpha = 1,
                 method = "C", row.norm = FALSE, maxit = 200,
                 tol = 0.0001, verbose = TRUE)
img1_ica=as.Image(matrix(ICA1.lc$S[,1], nrow=600)) # Reconstruct image
img2_ica=as.Image(matrix(ICA1.lc$S[,2], nrow=600))

ica1_img=combine(img1, img1_ica, test2,
                 img2,img2_ica, test1) # Combine images
plot(ica1_img, all=TRUE) # Plot all
names=c("Mixture", "ICA", "Source")
for(i in 1:3){ # Label each panel
  name=names[i]
  for(j in 1:2){
    text(x=600*(i-1)+20, y=600*(j-1)+20, label=paste(name, " ", j, sep = ""), 
         cex=1, adj = c(0,1), col = "red")
  }
}

### A nonlinear transformation test.
w = makeBrush(size = 21, shape = 'gaussian', sigma = 5) # Create brush
t2.f=filter2(test2, w); display(t2.f) # Blur image 2 to create non-linear transformation

img2.nl=0.2*test1+0.8*t2.f
display(img1); display(img2.nl)

img1_vec=as.vector(img1) # Convert to vector, default is by row
img2_vec.nl=as.vector(img2.nl)

S2 <- cbind(img1_vec, img2_vec.nl)
ICA2=fastICA(S2, 2,  alg.typ = "parallel", fun = "logcosh", alpha = 1,
          method = "C", row.norm = FALSE, maxit = 200,
          tol = 0.0001, verbose = TRUE)
img1_ica2=as.Image(matrix(ICA2$S[,1], nrow=600))
img2_ica2=as.Image(matrix(ICA2$S[,2], nrow=600))

ica1_img=combine(img1, img1_ica2, test2,
                 img2.nl,img2_ica2, test1) # Combine images
plot(ica1_img, all=TRUE) # Plot all
names=c("Mixture", "ICA", "Source")
for(i in 1:3){ # Label each panel
  name=names[i]
  for(j in 1:2){
    text(x=600*(i-1)+20, y=600*(j-1)+20, label=paste(name, " ", j, sep = ""), 
         cex=1, adj = c(0,1), col = "red")
  }
}

### A linear transformation of test1, 3 and 4
A=matrix(round(runif(9),digits = 1), nrow=3)
img1=0.6*test1+0.2*test3+0.2*test4
img2=0.2*test1+0.7*test3+0.1*test4
img3=0.1*test1+0.2*test3+0.7*test4
display(img1); display(img2); display(img3)

## Convert the two transformations to vectors
img1_vec=as.vector(img1) 
img2_vec=as.vector(img2) 
img3_vec=as.vector(img3) 

## Construct the mixture data frame
S3 <- cbind(img1_vec, img2_vec, img3_vec)

# FastICA
ICA3=fastICA(S3, 3,  alg.typ = "parallel", fun = "logcosh", alpha = 1,
             method = "C", row.norm = FALSE, maxit = 200,
             tol = 0.0001, verbose = TRUE)
img1_ica3=as.Image(matrix(ICA3$S[,1], nrow=600)) # Reconstruct image
img2_ica3=as.Image(matrix(ICA3$S[,2], nrow=600))
img3_ica3=as.Image(matrix(ICA3$S[,3], nrow=600))


par(mfrow=c(1,1))
par(mar=c(1,1,1,1))

ica3_img=combine(img1, img2, img3,
                img1_ica3, img2_ica3, img3_ica3,
                test1, test3, test4)
plot(ica3_img, all=TRUE)
names=c("Mixture", "ICA", "Source")
for(i in 1:3){
  name=names[i]
  for(j in 1:3){
    text(x=600*(j-1)+20, y=600*(i-1)+20, label=paste(name, " ", j, sep = ""), 
         cex=1, adj = c(0,1), col = "red")
  }
}
