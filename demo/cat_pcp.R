# This demo illustrates parallel coordinate-type displays for data with mixed categorical continuous variables. The displays are similar to Parallel Sets. http://www.vrvis.at/via/research/parsets/index.html
# For the example here to work, you must first load the following functions.

pcp_with_bars <- function (..., pcpbars, pcpbars.border="black",pcpbars.col=NULL,pcpbars.labels=NULL) {
    	
    	pv_pcp(...)
    	oldxpd <- par("xpd")
        par("xpd"=TRUE) 
        rect(pcpbars[,1],pcpbars[,2],pcpbars[,3],pcpbars[,4],col=pcpbars.col,
            border=pcpbars.border)
        if (!is.null(pcpbars.labels))
        text((pcpbars[,1]+pcpbars[,3])/2,(pcpbars[,2]+pcpbars[,4])/2,pcpbars.labels)
        par("xpd"=oldxpd)
     }
     

map2index <- function(x, shrink=0){
	xt <- table(x)
	newx <- rep(0,length(x))
	nm <- names(xt)
	start <- 1
	for (i in 1:length(xt)){
		n <- xt[i]
		d <- n*shrink
		newx[x==nm[i]] <- seq(1+d, n-d, length.out=n) + start-1
		start <- start+xt[i]
		}
	return(newx)
	}

calc_bars <- function(d,o,width=.2){
  ntot <- nrow(d)
  
  barl <- NULL
  barr <- NULL
  bart <- NULL
  barb <- NULL
  barlab <- NULL
  for (i in 1:length(o)){
	if (!is.na(o[i])){
	mtot <-  table(d[,o[i]])
    b <- 0
    for (k in 1:length(mtot)){
      barl <- c(barl, i-width/2)
      barr <- c(barr, i+width/2)
      barb <- c(barb,b)
      b <- b+mtot[k]/ntot
       bart <- c(bart,b)
       barlab <- c(barlab, names(mtot)[k])
     }
    }
   }
 ans <-cbind(barl,barb,barr,bart)
 colnames(ans) <- c("left","bottom","right","top")
 return(list(ans,barlab))
}
#---------------------------------
# Now the example

library(alr3)
data(donner)


d <- na.omit(donner[,c(1,2,3,6)])
d <- d[,c(2,3,4,1)]


colvar <- 2 # any of the categorical variables 1:3 will work here
d <- d[order(d[,colvar]),]
cols <- rainbow(4,alpha=0.4)[as.numeric(as.factor(d[,colvar]))]

dint <- d
dint[,1:3] <-apply(d[,1:3],2,map2index) #maps the categorical variables to 1..n


o <- hpaths(1:ncol(d),matrix=FALSE)
bwid <- .2
catorder <- o
catorder[catorder==4] <- NA
barlims <- calc_bars(d,catorder,bwid) # calculates bars for the categorical axes

dev.new(width=5,height=2.5)
par(mar=c(2,1,2,1))
par( cex.axis=.8,cex=.8)

pcp_with_bars(dint,order=o,horizontal=TRUE,axis.width=bwid,col=cols,lwd=2,pcpbars=barlims[[1]],pcpbars.labels=barlims[[2]],main="Donner Data")


# benefits: handles any number of categorical, and continuous
# see al pairwise relationships- eg, single and hired people are in older age groups and these are almost all male (1 exception).

#--------------------------


y <- as.matrix(as.data.frame(as.table(HairEyeColor)))


y <- apply(y[,-4],2, function(x) rep(x,times=y[,4]))


colvar <- 3
y <- y[order(y[,colvar]),]
cols <- rainbow(4,alpha=0.3)[as.numeric(as.factor(y[,colvar]))]


yint <-apply(y,2,map2index)

o <- c(1,2,3,1)  
barlims <- calc_bars(y,o,bwid)


par( cex.axis=.8,cex=.8)

pcp_with_bars(yint,order=o,horizontal=TRUE,axis.width=bwid,col=cols,lwd=2,pcpbars=barlims[[1]])




