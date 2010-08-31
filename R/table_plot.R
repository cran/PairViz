
fill_cols <- function(x,vals)	{
# returns a matrix same shape as x where each column contains the vector vals
    vals <- rep(vals,length.out=nrow(x))
	y <- vals[row(x)]
    y <-matrix(y,nrow=nrow(x))
   rownames(y) <- rownames(x)
   colnames(y) <- colnames(x)
   return(y)
}

fill_rows <- function(x,vals)	{
# returns a matrix same shape as x where each row contains the vector vals
    vals <- rep(vals,length.out=ncol(x))

	y <- vals[col(x)]
    y <- matrix(y,nrow=nrow(x))
    rownames(y) <- rownames(x)
    colnames(y) <- colnames(x)
    return(y)
}



	



table_plot <- function(rectw,recth,col="grey50",gapx=NULL,gapy=NULL,spacex=0.03,spacey=0.03, xjust="center",yjust="center",xruler=NULL,yruler=NULL,color.ruler="grey32",pch0=1,xlab=NULL,ylab=NULL,plot=TRUE,...){
	
	if (is.matrix(rectw) && !is.matrix(recth))
	  	  recth <- fill_cols(rectw,recth)
	if (is.matrix(recth) && !is.matrix(rectw))
	  	  rectw <- fill_rows(recth,rectw)
	  	  
	if (!is.matrix(recth) && !is.matrix(rectw)){
		x <- matrix(0,nrow=length(recth),ncol=length(rectw))
		rectw <- fill_rows(x,rectw)
		recth <- fill_rows(x,recth)
		
		}
	  	  
    
	
	bary <- row(rectw) -1
    barx <- col(rectw) -1
    
    spacex <- max(rowSums(rectw,na.rm=TRUE))*spacex
    spacey <- max(colSums(recth,na.rm=TRUE))*spacey
   	if (yjust=="center")  fy <- c(.5,.5)
     else if (yjust=="top") fy <- c(0,1)
    else fy <- c(1,0)	
    
    if (xjust=="center")  fx <- c(.5,.5)
     else if (xjust=="right") fx <- c(0,1)
    else fx <- c(1,0)
     
    if (is.null(gapy))
      for (i in (2:nrow(bary))) gapy <- c(gapy, max(fy[1]*recth[i-1,] + fy[2]*recth[i,],na.rm = TRUE ))
     if (is.null(gapx)) 
      for (j in (2:ncol(barx))) gapx <- c(gapx, max(fx[1]*rectw[,j-1] + fx[2]*rectw[,j],na.rm = TRUE ))
   
     if (yjust == "none")
     for (i in (2:nrow(bary))) bary[i,] <- bary[i-1,] + recth[i-1,] + spacey
     else
     for (i in (2:nrow(bary))) bary[i,] <- bary[i-1,] + gapy[i-1] + spacey
     
     if (xjust == "none")
     for (j in (2:ncol(barx))) barx[,j] <- barx[,j-1] + rectw[,j-1] + spacex
     else
     for (j in (2:ncol(barx))) barx[,j] <- barx[,j-1] + gapx[j-1] + spacex

     
    
     barl <- barx - rectw*fx[2]	
     barr <- barx + rectw*fx[1]	
     barb <- bary - recth*fy[2]	
     bart <- bary + recth*fy[1]	
     
     barx <- (barl+barr)/2
     bary <- (barb+bart)/2
     
     if (is.null(xlab)) xlab <- names(dimnames(rectw))[2]
     if (is.null(xlab)) xlab <- names(dimnames(recth))[2]
     if (is.null(xlab)) xlab <- ""
     if (is.null(ylab)) ylab <- names(dimnames(rectw))[1]
     if (is.null(ylab)) ylab <- names(dimnames(recth))[1]
      if (is.null(ylab)) ylab <- ""
     
     if (plot){
        plot(c(min(barl,na.rm=TRUE),max(barr,na.rm=TRUE)),c(min(barb,na.rm=TRUE),max(bart,na.rm=TRUE)),type="n",axes=FALSE,xlab=xlab,ylab=ylab,...)
     if ("center" %in% xruler)
      for (i in 1:nrow(barx)) lines(barx[i,],bary[i,],col=color.ruler)
     if ("bottom" %in% xruler)
      for (i in 1:nrow(barx)) lines(barx[i,],barb[i,],col=color.ruler)
     if ("top" %in% xruler)
      for (i in 1:nrow(barx)) lines(barx[i,],bart[i,],col=color.ruler)
     if ("center" %in% yruler)
      for (j in 1:ncol(bary)) lines(barx[,j],bary[,j],col=color.ruler)
      if ("left" %in% yruler)
        for (j in 1:ncol(bary)) lines(barl[,j],bary[,j],col=color.ruler)
      if ("right" %in% yruler)
        for (j in 1:ncol(bary)) lines(barr[,j],bary[,j],col=color.ruler)
      
      if (!is.null(pch0)){
      	cell0 <- barl == barr & barb == bart
      	if (any(cell0))
         points(barl[cell0], barb[cell0],col=col,pch=pch0)
       }
     rect(barl, barb,barr,bart,col=col)
     
     if (!is.null(colnames(rectw)))
        axis(1,labels=colnames(rectw),at=barx[1,],tcl=0,lwd=0)
     if (!is.null(rownames(rectw)))
        axis(2,labels=rownames(rectw),at=bary[,1],tcl=0,lwd=0,las=2)
      return(NULL)
      }
      else{ 
      	ans <-cbind(as.vector(barl),as.vector(barb),as.vector(barr),as.vector(bart))
      	ans <- na.omit(ans)
        colnames(ans) <- c("left","bottom","right","top")
       return(ans)
	   }
	}
# revise with plot separated from calc