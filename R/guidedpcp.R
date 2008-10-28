pv_pcp <- 
function (data, order = NULL, panel.colors = NULL, col = 1, lty = 1, 
    horizontal = TRUE, mar = NULL, scale=TRUE,axis.width=0,...) 
{
    if (is.null(mar)) 
        if (horizontal == TRUE) 
            mar <- c(2, 2, 2, 2) + 0.1
        else mar <- c(2, 5, 2, 2) + 0.1
    par("mar"=mar)
    if (!is.null(order)) {
        data <- data[, order]
        if (is.matrix(panel.colors)) 
            panel.colors <- panel.colors[order, order]
    }
    if (is.matrix(panel.colors)) 
        panel.colors <- panel.colors[col(panel.colors) == row(panel.colors) + 1]
    if (is.vector(panel.colors)) 
        if (ncol(data) - 1 != length(panel.colors)) 
            stop("dimensions do not match")
    if (scale==TRUE) x <- apply(data, 2, function(x) (x - min(x))/(max(x) - min(x)))
    else x <- data
    p <- ncol(x)
     indx <- 1:p
    if ((length(axis.width)==1) && (axis.width == 0))
      bx <- x
    else {
      bx <- x[,rep(1:p,times=rep(2,times=p))]
      if (length(axis.width)==1)
        indx <- as.vector(sapply(indx,function(x)c(x-axis.width/2,x+axis.width/2)))
      else indx <- as.vector(t(cbind(indx-axis.width/2,indx+axis.width/2)))
    	}
     linesr <- range(bx)
     if (horizontal == TRUE) {
        matplot(indx, t(bx), xlab = "", ylab = "", axes = FALSE, 
            type = "n", xaxs="i",...)
        axis(1, at = 1:p, labels = colnames(x))
        if (!(is.null(panel.colors))) 
            for (i in 1:(p - 1)) rect(i, 0, i + 1, 1, lty = 0, 
                col = panel.colors[i])
        for (i in 1:p) lines(c(i, i), linesr, col = "grey70")
        
        matpoints(indx, t(bx), type = "l", col = col, lty = lty, 
            ...)
            }
    else {
        matplot(t(bx), rev(indx), xlab = "", ylab = "", axes = FALSE, 
            type = "n",yaxs="i", ...)
        axis(2, at = p:1, labels = colnames(x), las = 2)
        if (!(is.null(panel.colors))) 
            for (i in 1:(p - 1)) rect(0, i, 1, i + 1, lty = 0, 
                col = panel.colors[p - i])
        for (i in 1:p) lines(linesr, c(i, i), col = "grey70")
        matpoints(t(bx), rev(indx), type = "l", col = col, lty = lty, 
            ...)
    }
    invisible()
}

    
    
    
guided_pcp <- function(data, edgew=NULL, path = NULL, pathw=NULL,zoom=NULL,pcpfn=pv_pcp,
     pcp.col = 1,lwd=0.5, panel.colors=NULL,  pc.mar=c(1.5,2,2,2),  bar.col=1:9,bar.axes=FALSE, bar.mar=NULL, reorder.weights=TRUE,
    layout.heights=NULL, layout.widths=c(10,1),
     main=NULL,legend=FALSE,cex.legend = 1,legend.mar=c(1,4,1,1),...){
    
  	if (is.null(path)) path <- 1:ncol(data)
	if (is.function(path)) 
	   o <- find_path(-edgew,path,...)
	else o <- path
    if (is.null(pathw) || (nrow(as.matrix(pathw)) != length(o) -1))
       if (!is.null(edgew)) pathw <- path_weights(edgew,o,...)
       
    pathw <- as.matrix(pathw)
    
    if (!is.null(zoom)) {
    	o <- o[zoom]
        pathw <- pathw[zoom,] 
        pathw <- pathw[-nrow(pathw),]   
    	}
    if (reorder.weights==TRUE)
       w.ord <- order(apply(pathw,2,sum),decreasing=TRUE)
    else w.ord <- 1:ncol(pathw)
    
    if (legend ==TRUE){
       if (is.null(layout.heights)) 
        	layout.heights <- c(5,5,3)
        layout(matrix(c(1,1,2,4,3,3),nrow=3), heights=layout.heights, widths=layout.widths)
        }
     else {
       if (is.null(layout.heights)) layout.heights <- c(10,3)
     layout(matrix(c(1,2)), heights=layout.heights )
     }
     
    pcpfn(data,col=pcp.col,order=o,panel.colors=panel.colors,
    horizontal=TRUE,lwd=lwd,mar=pc.mar,main=main,xaxs="i",...)
    if (is.null(bar.mar)) {
    	bar.mar <- pc.mar
    	bar.mar[c(1,3)] <- bar.mar[c(1,3)]/3   	
    	}
    par("mar"=bar.mar)
     if (length(bar.col) == length(w.ord))
      bar.col <- bar.col[w.ord]
    p1 <- par("usr")
    p2 <- p1-1
    p2[3] <- min(apply(pathw,1,function(r) sum(pmin(r,0))))
    p2[4] <- max(apply(pathw,1,function(r) sum(pmax(r,0))))

    plot.new()
    par("usr"=p2)
    if (ncol(pathw)==1)
    barplot(pathw[,1],space=0,axisnames= FALSE,
        col=bar.col,axes=bar.axes,main="",xaxs="i",add=TRUE)
   else
    barplot(t(pathw[,w.ord]),space=0,axisnames= FALSE,
        col=bar.col,axes=bar.axes,main="",xaxs="i",add=TRUE)
   
     if (legend==TRUE) {
         par("mar"=legend.mar)
       barplot(rep(1,length(w.ord)),col=bar.col[w.ord],horiz=TRUE,space=0, axes=FALSE,
       names.arg=colnames(pathw[,w.ord]),las=2,cex.names=cex.legend)
       }


     invisible()
    }
