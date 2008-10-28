####### Code for constructing figures of Section 3.3 (Hurley and Oldford, 2008) ###################


library(PairViz)


data <- mtcars[,c(1,3:7)]
cols <- c("red","green")[mtcars[,9]+1 ]    # transmission type, red=automatic

colnames(data) <- c("Mpg" , "Disp", "Hp"  , "Drat" ,"Wt" ,  "Qsec")

o <- hpaths(1:6,matrix=FALSE)


dev.new(width=7.5,height=2.5)
pc <- c(rep("#FFFFCC"    ,5),0, rep("wheat",5 ), 0,rep("grey90" ,5 ))


par(tcl = -.2, cex.axis=.6,mgp=c(3,.3,0))

pv_pcp(data,order=o,horizontal=TRUE,col=cols,lwd=0.5,
main = "Hamiltonian decomposition",panel.colors=pc)



# add a correlation guide and find "better" hamiltonians...
corw <- as.dist(cor(data))

o <- weighted_hpaths(-corw, matrix=FALSE)

corw <- dist2edge(corw)

edgew <- cbind(corw*(corw>0), corw*(corw<0))
dev.new(width=7.5,height=3)
par(tcl = -.2, cex.axis=.6,mgp=c(3,.3,0))

guided_pcp(data,edgew, path=o,pcp.col=cols,panel.colors=pc,
         main="Correlation guided Hamiltonian decomposition",bar.col = c("blue","purple"))
         
         
      

#================================================================
#_________utility functions__________




pc_scag_title <- function(title="Parallel Coordinates",sel_scag)
 {if (length(sel_scag)>1) 
   {if (length(sel_scag)==length(scags))
      {title <- paste(title, 
      	               "on all scagnostics.", 
      	               sep=" ")
   	} else {title <- paste(title, 
      	               "on scagnostics:", 
      	               sep=" ")
   		     for (scag in sel_scag) {
	         title <- paste(title,scag, sep=" ")}}
    } else {title <-  	paste(title, 
      	               "on scagnostics:", 
      	               sep=" ")
      	      title <- paste(title,sel_scag, sep=" ")}

	 title}



	 
trim <- function(sc, num) {
	sc[sc<num] <- 0
	return(sc)}
#____________________________________________
	 
	
library(scagnostics)
library(RColorBrewer)
library(alr3)

		 
# Sleep data setup
data(sleep1)
data <- na.omit(sleep1)

# logging the brain and body weights
data[,c(4,5)] <- log(data[,c(4,5)])
colnames(data) <- c("SWS","PS" ,"TS" ,"LogBodyWt", "LogBrainWt","Life","GP","P" ,"SE" , "D"  )
rownames(data) <- data[,11]
data <- data[,-11]


# colours for pc curves
cols <- rainbow(3,alpha=0.7)[cut(rank(data[,6]),3,labels=FALSE)]

# The base data for finding weights.
sc <- scagnostics(data)

# Scag names and colours
# These are preserved throughout in this order.
scags <- colnames(sc)
scag_cols <- brewer.pal(9, "Set1")
names(scag_cols) <- scags

#____________________________________________

dev.new(width=9.5,height=3)
par(tcl = -.2, cex.axis=1,mgp=c(3,.3,0))


guided_pcp(data,trim(sc,.7), path=eulerian,pcp.col=cols,
         main=pc_scag_title("Eulerian",scags),bar.col = scag_cols,legend=TRUE)

# reconstruct above plot, computing order and edge weights first
#o <- find_path(-trim(sc,.7),path=eulerian)
#pathw <- path_weights(trim(sc,.7),o)

#guided_pcp(data, path=o,pathw=pathw,pcp.col=cols,
#         main=pc_scag_title("Eulerian",scags),bar.col = scag_cols,legend=TRUE)

# zoom in on first 19 axes

dev.new(width=9.5,height=3)
par(tcl = -.2, cex.axis=.5,mgp=c(3,.3,0))

guided_pcp(data,trim(sc,.7), path=eulerian,pcp.col=cols, zoom=1:19,
         main="First 18 panels of Eulerian on all scagnostics",bar.col = scag_cols)
         
         


# ----or, omit legend=TRUE and draw the legend separately--------------
dev.new(width=1.5,height=3)
par(mar=c(0,4.5,0,.5))

barplot(rep(1,9),col=scag_cols,horiz=TRUE,space=0, axes=FALSE,names.arg=scags,las=2,cex.names=.8)

##############Hamiltonian plots######################################

sel_scag <- c("Outlying","Clumpy", "Sparse")
title <- pc_scag_title("Best Hamiltonian",sel_scag)

sc1 <- sc[,sel_scag]
class(sc1) <- class(sc) # required so that edge_index works correctly 

# o <- find_path(-sc1, order_best,maxexact=10) takes a few mins... answer is
o <- c(4 , 1 , 5 , 7,  6 , 9 , 3 , 8 ,10 , 2)

# ans above is reverse of one in paper, because this one has been re-oriented to have decreasing weights

# alternatively use
# o <- find_path(sc1, order_tsp) 



#------------------

dev.new(width=6,height=3)
par(tcl = -.2, cex.axis=.7,mgp=c(3,.3,0))

guided_pcp(data,sc1, path=o, pcp.col=cols, 
         main=title,bar.col = scag_cols[sel_scag])


#------------------
sel_scag <-  c("Striated","Sparse")
title <- pc_scag_title("Best hamiltonian",sel_scag)
sc1 <- sc[,sel_scag]
class(sc1) <- class(sc)


# o <- find_path(-sc1, order_best,maxexact=10) takes a few mins... answer is

o <- c( 6,  8 , 2,  9,  5  ,3 , 7 , 1, 10 , 4)
#  ans above is reverse of one in paper




#------------------
sel_scag <-  c("Monotonic", "Convex")
title <- pc_scag_title("Best hamiltonian",sel_scag)
sc1 <- sc[,sel_scag]
class(sc1) <- class(sc)

# o <-   find_path(-sc1, order_best,maxexact=10) takes a few mins... answer is
o <- c( 1 , 3 , 2 , 6 , 7 , 5 , 4 , 9 , 10 , 8 )


#------------------
title <- pc_scag_title("Second hamiltonian",sel_scag)

o <- find_path(-sc1, weighted_hpaths,path1=o)  


o <- o[2,]



