require(PairViz)

# Comparison of algorithms for generating tours on K9	


dev.new(width=6,height=6)
par(mar=c(2,2,2,1))
par(mfcol=c(3,2))

n <- 8	

e1 <- eseq(n)
e2 <- eulerian(n)
e3 <- hpaths(n,matrix=FALSE)
x <- 1:length(e1)


plot(x,e1,main=paste("Eseq",n),pch=20)
lines(x,e1,col="grey")

plot(x,e2,main=paste("Etour",n) ,pch=20)
lines(x,e2,col="grey")

plot(x,e3,main=paste("Hpaths",n) ,pch=20)
lines(x,e3,col="grey")

n <- 9	

e1 <- eseq(n)
e2 <- eulerian(n)
e3 <- hpaths(n,matrix=FALSE)
x <- 1:length(e1)
plot(x,e1,main=paste("Eseq",n),pch=20)
lines(x,e1,col="grey")

plot(x,e2,main=paste("Etour",n) ,pch=20)
lines(x,e2,col="grey")

plot(x,e3,main=paste("Hpaths",n) ,pch=20)
lines(x,e3,col="grey")



# Real data example
d <- eurodist
e1 <- eseq(length(labels(d)))
e2 <- eulerian(d)

require(gclus)
path1 <- order.hclust(-d,method="average")
#require(seriation)
#path1 <- get_order(seriate(d,method="gw"),1) # same as order.hclust
#path1 <- get_order(seriate(d,method="olo")) # same as order.hclust

e3 <- weighted_hpaths(d,path1= path1,as.matrix=TRUE)

dev.new(width=5.2,height=6)
par(mar=c(2,2,2,1))
par(cex=.5)
par(mfrow=c(3,1))

pw <- path_weights(dist2edge(d),e1)


plot(1:length(pw),pw,pch=20,cex=.5, main= "Algorithm eseq: Eurodist edge weights")

pw <- path_weights(dist2edge(d),e2)


plot(1:length(pw),pw,pch=20,cex=.5,main= "Weighted etour on Eurodist")

pw <- path_weights(dist2edge(d),e3)


plot(1:length(pw),pw,pch=20,cex=.5,main= "Weighted hamiltonians on Eurodist ")






