options(java.parameters='-Xmx1g')
library(tsner)

mnist <- read.csv("mnist_test.csv")
X <- as.matrix(mnist)
storage.mode(X) <- "double"
dim(X) <- dim(mnist)

dims=2 
initial_dims=-1
perplexity=30
max_iter=1000
pca=FALSE
theta=0.5
verbose=FALSE

system.time(ds <- tsne(X, dims, initial_dims, perplexity, max_iter,
                        pca, theta, verbose))

plot(ds)
