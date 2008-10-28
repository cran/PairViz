####### Code for constructing figures of Section 3.3 (Hurley and Oldford, 2008) ###################


require(PairViz)
mt9 <- as.matrix(mtcars)[c(7,28,27,31,30,1,12:14),1:7]
rownames(mt9) <- 1:9
cols <- c("red","blue","green","purple")

o <- hpaths(7)
# for consistency with the paper execute the following line
o <- o -1 ; o[o==0] <- 7 # changes the `join' from 1 to 7

o <- rbind(1:7,o)

cols <- c("red","green","blue","purple")
mains <- c("Dataset order H0", "Order H1","Order H2" ,"Order H3")
for (i in 1:nrow(o)) {
	dev.new(width=2,height=2)
	stars(mt9[,o[i,]], len = 0.8,col.stars=rep(cols[i], nrow(mt9)), 
	 key.loc = NULL,     main = mains[i],cex=.8,radius = FALSE)
	}
	
#------------------------------------
	
	
dev.new(width=3,height=3)
stars(mt9[,as.vector(t(o[-1,]))], len = 0.8,col.stars=rep("orange", nrow(mt9)), 
	 key.loc = NULL,     main = "Hamiltonian decomp, H1:H2:H3",cex=.8,radius = FALSE)

#------------------------------------

m <- cor(mt9)
o <- eulerian(-m)
o <- o[-length(o)] # tour is closed, last element is the same as first and is uneeded for star glyps


dev.new(width=3,height=3)
stars(mt9[,o], len = 0.8,col.stars=rep("magenta", nrow(mt9)), 
	 key.loc = NULL,     main = "Eulerian order",cex=.8,radius = FALSE)

#------------------------------------
dev.new(width=5,height=3) 
plot(hclust(dist(scale(mt9)), "average"))

#------------------------------------
dev.new(width=5,height=3) 

loc <- prcomp(mt9, scale=TRUE,retx=TRUE)$x

stars(mt9[,o], col.stars=rep("purple", nrow(mt9)),
locations=loc[,1:2]*3,
      main = "Eulerian",radius = FALSE)
