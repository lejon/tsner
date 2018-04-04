options(java.parameters='-Xmx1g')
library(ggplot2)
library(tsner)

mnist_df <- read.csv("mnist_test.csv")
X <- mnist_df[,2:ncol(mnist_df)]

# Working with matrices is also possible
#X <- as.matrix(mnist)
#storage.mode(X) <- "double"
#dim(X) <- dim(mnist)

dims=2 
initial_dims=-1
perplexity=30
max_iter=1000
pca=FALSE
theta=0.5
verbose=FALSE

system.time(tsnedf <- tsne(X, dims, initial_dims, perplexity, max_iter,
                        pca, theta, verbose))

tsnedf$digit <- as.factor(mnist_df[,1])
ggplot(tsnedf,aes(x=V1,y=V2)) + geom_point(aes(color=digit)) + theme_bw()

