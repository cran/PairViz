


mk_hypercube_graph <- function(n)	{
  
  binary <- function(x,n){
     ans <- rep(0,n)
     y <- NULL
     while (x != 0){
     	y <- c(y,x %% 2)
     	x <- x %/% 2
      	}
      if (length(y) !=0)
        ans[1:length(y)] <- y
      return(ans)
      }
      
  nodeid <- NULL
  if (length(n) !=1) {
     nodeid <- n
      n <- length(nodeid)
      }
  nnodes <- 2^n
  id <- t(sapply(0:(2^n -1),binary,n))
  if (is.null(nodeid))
     nnames <- apply(id,1, function(x) do.call("paste",as.list(c(x,sep=""))))
  else
     nnames <- c("0", apply(id[-1,],1, 
      function(x) do.call("paste",as.list(c(nodeid[as.logical(rev(x))],sep="")))))

  g <- new("graphNEL", nodes=nnames)
  k <- 1
  for (i in 1:(length(nnames)-1)){
    x <- nnames[i]
    for (j in (i+1):length(nnames)) {
    	y <- nnames[j]
  	    if (sum(abs(id[i,] - id[j,])) ==1)
  	    g <-addEdge(x,y,g)
  	    k <- k+1
 	}
 }
 return(g)
}



mk_line_graph <- function(g){
	lnode_names <- edgeNames(g)
	nlnodes <- length(lnode_names)
	lnodes <- sapply(lnode_names, function(e)strsplit(e,"~")[[1]])
	ledges <- NULL
	lnode_names <- apply(lnodes,2, function(z) do.call("paste",as.list(c(z,sep=""))))
	
	for (i in 1:(nlnodes-1)) {
		a <- lnodes[,i] 
		for (j in 2:nlnodes){	
			b <- lnodes[,j]
			if (length(intersect(a,b)) ==1) 
			  ledges <- rbind(ledges,lnode_names[c(i,j)])
			}
	  }
	 newg <- new("graphNEL", nodes=lnode_names)
	 newg <- addEdge(ledges[,1],ledges[,2],newg)
	 return(newg)
		}
		
		
graph_product <- function(g,h, type="cartesian"){
	g1 <- nodes(g)
	h1 <- nodes(h)
	k1 <- cbind(rep(g1,times=length(h1)),rep(h1,each=length(g1)))
	k <- new("graphNEL", 
	   nodes=apply(k1,1, function(z) do.call("paste",as.list(c(z,sep=".")))))
	n <- nodes(k)
	if (type=="cartesian") {
	  for (i in 1:(length(n) -1))
	     for (j in (i+1):length(n))
	     	  if (((k1[i,1]== k1[j,1]) && isAdjacent(h, k1[i,2],k1[j,2]) ) ||
	   	    ((k1[i,2]== k1[j,2]) && isAdjacent(g, k1[i,1],k1[j,1]) ))
	   	      k <- addEdge(n[i],n[j],k)
	   	    }
	  else if (type=="strong"){
	  for (i in 1:(length(n) -1))
	     for (j in (i+1):length(n))
	     	  if (isAdjacent(g, k1[i,1],k1[j,1]) && isAdjacent(h, k1[i,2],k1[j,2]))	   	      k <- addEdge(n[i],n[j],k)
	   	    }
      else if (type=="tensor"){
	  for (i in 1:(length(n) -1))
	     for (j in (i+1):length(n))
	     	  if ((((k1[i,1]== k1[j,1]) || isAdjacent(g, k1[i,1],k1[j,1])) && 
	     	  isAdjacent(h, k1[i,2],k1[j,2])) ||
	     	   (((k1[i,2]== k1[j,2]) || isAdjacent(h, k1[i,2],k1[j,2])) && 
	     	   isAdjacent(g, k1[i,1],k1[j,1])))
	     	  	   	      k <- addEdge(n[i],n[j],k)
	   	    }

	  
	 		
	return(k)
	}
	
  
	
			
knn_graph <- function(g,k=2)	{
	nod <- nodes(g)
	modeg <- edgemode(g)
	edgemode(g) <- "directed"
	for (i in 1:length(nod)){
		n <- nod[i]
		a <- edges(g,n)[[1]]
        b <- edgeWeights(g,n)[[1]]
        if (length(b) > k){
           o <- order(b)[-(1:k)]
        g <- removeEdge(n,a[o],g)
        }
		}
	edgemode(g) <- modeg
	return(g)
    }
    
    
dn_graph <- function(g,d=1)	{
	e <- edgeMatrix(g,duplicates=FALSE)
	ew <- eWV(g,e)
	e <- matrix(nodes(g)[e],ncol=2,byrow=TRUE)
	x <- ew <=d
	return(ftM2graphNEL(e[x,],ew[x],edgemode="undirected"))
	}
	     


bipartite_graph <- function(n1,n2){
 f <- matrix(nrow=length(n1)*length(n2),ncol=2)
 f[,1] <- n1
 f[,2] <- rep(n2, each=length(n1))
 return(ftM2graphNEL(f,  edgemode="undirected"))
	}