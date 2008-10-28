####### Code for constructing figures of Section 3.1 (Hurley and Oldford, 2008) ###################
	 			
library(PairViz)	 			
#############################

data(cancer)
bx <- with(cancer, split(sqrt(Survival),Organ))
a <-  aov(sqrt(Survival) ~ Organ,data=cancer)

dev.new(height=2.5, width=2.5)

plot(TukeyHSD(a,conf.level = 0.95,cex=0.5))

#############################
library(HH)
mm <- glht.mmc(a, linfct = mcp(Organ = "Tukey"))
dev.new(height=5.5, width=5.5)


par(mar=c(5,5,5,12))

plot(mm, x.offset=1)




#############################


dev.new(height=4.5, width=9.5)
par(cex.axis=.75, cex.main = 1.6, cex.lab=1.4)
par(mar=c(3,5,3,5))

mc_plot(bx,a,main="Pairwise comparisons of cancer types", ylab="Sqrt Survival")



###########draw in stages ##########


dev.new(height=4.5, width=9.5)
par(cex.axis=.75, cex.main = 1.6, cex.lab=1.4)
par(mar=c(3,5,3,5))

# suppress CIs
mc_plot(bx,a,main="Pairwise comparisons of cancer types", ylab="Sqrt Survival",levels=NULL)

# draw CIs

mc_plot(bx,a,main="Pairwise comparisons of cancer types", ylab="Sqrt Survival", sig.col=NULL)

# mark significant CIs
mc_plot(bx,a,main="Pairwise comparisons of cancer types", ylab="Sqrt Survival")
