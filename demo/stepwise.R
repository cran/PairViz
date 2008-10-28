library(PairViz)

library(alr3)
data(sleep1)

data <- sleep1
rownames(data) <- sleep1[,11]


# logging the brain and body weights
data[,c(4,5)] <- log(data[,c(4,5)])
colnames(data) <- c("SWS","PS" ,"TS" ,"LogBodyWt", "LogBrainWt","Life","GP","P" ,"SE" , "D"  )

data <- na.omit(data[,c(1,2,4,5,6)])

y <- data[,4]
x <- data[,-4]
colnames(x) <-c("A","B","C","D")


preds <- list(1,2,3,4,c(1,2),c(1,3),c(1,4),c(2,3),c(2,4),c(3,4), 1:3,(1:4)[-3],(1:4)[-2],2:4,1:4)

names(preds) <- lapply(preds,function(p) do.call("paste",as.list(c(names(x)[p],sep=""))))




res <-sapply(preds,function(p) {
	d <- as.data.frame(x[,p])
	lm(y ~., data=d)$residuals})


sse <- apply(res,2,function(x) sum(x*x))

g <- new("graphNEL", nodes=colnames(res))

for (a in nodes(g)) {
	a1 <- strsplit(a,"")[[1]]
	for (b in nodes(g)){
	b1 <- strsplit(b,"")[[1]]
	
	if  (((length(a1)+1) == length(b1)) && !any(is.na(match(a1,b1)))){
		w <- sse[a] - sse[b]
	    names(w) <- NULL
	 	g <- addEdge(a,b,g,w)
	}}}

# g is not eulerian, 3 of the nodes are even, 4 are odd (ab ac bc abc)
eulerian(g,weighted=FALSE)
# A approximate eulerian is calculated by addition of one extra edge
# connecting last two nodes. Note that the start and target nodes are chosen from the odd # nodes.

eulerian(g)
# This version uses weights, and picks as a start a node connected to the loweset weight edge, which happens to be node a (an even node).

# To do a backwards selection version, specify start as "ABC"

ge <- mk_even_graph(g)
o <- eulerian(ge,start="ABCD")

# A approximate eulerian is calculated by addition of one extra edge
# connecting last two nodes

	
ew <- NULL	
for (i in 2:length(o)) ew <- c(ew,sse[o[i]] - sse[o[i-1]])


cols <- rep("grey30",length(ew))


for (i in 2:length(o)) {
	a <- o[i]
	b <- o[i-1]
	if (is.na(match(b,edges(g,a)[[1]]))) cols[i-1] <- "grey90"
	}

dev.new(width=7,height=3)

par(tcl = -.2, cex.axis=.4,mgp=c(3,.3,0))
pcp0 <- function(...){pv_pcp(...)
	abline(h=0,col="grey10",lwd=2)}

guided_pcp(res,path=match(o,nodes(g)),scale=FALSE,pathw=ew,bar.col=cols,pcpfn=pcp0,lwd=1,pcp.col="grey30",main="Sleep data: Model residuals.",pc.mar=c(1,1,2,1))


dev.new(width=4,height=2)

par(tcl = -.2, cex.axis=.4,mgp=c(3,.3,0))



pv_pcp(res,scale=FALSE, lwd=1,col="grey30",main="Sleep data: Model residuals.",mar=c(1.5,.5,2,1.5))

abline(h=0,col="grey10",lwd=2)

