# ----------------------------------------------------------------
# Praeamble
# ----------------------------------------------------------------

# Packages 
library('igraph')
library('RColorBrewer')

# ----------------------------------------------------------------
# Exercise 1
# ----------------------------------------------------------------
start = graph.adjacency(matrix(1,4,4) - diag(1,4), mode=c("undirected"))
b02 <- barabasi.game(10**2,m=4,start.graph = start,directed = FALSE); d02 = degree(b02)
b03 <- barabasi.game(10**3,m=4,start.graph = start,directed = FALSE); d03 = degree(b03)
b04 <- barabasi.game(10**4,m=4,start.graph = start,directed = FALSE); d04 = degree(b04)
lines(my.ecdf)
plot(density(d04),type='l',ylim=c(0,0.5),xlab="Degree","",xlim=c(0,250),col='blue')
par(new=TRUE)
plot(density(d03),type='l',ylim=c(0,0.5),xlab="Degree","",xlim=c(0,250),col="red")
par(new=TRUE)
plot(density(d02),type='l',ylim=c(0,0.5),xlab="Degree","",xlim=c(0,250),col='green')
legend('topright',legend=c('10**4','10**3','10**2'),lty=1,col=c('blue','red','green'))
title(main="Degree Distriubtions for Barabasi-Albert model")

ozone.ordered = sort(ozone)
plot(ozone.ordered, (1:n)/n, type = 's', ylim = c(0, 1), xlab = 'Sample Quantiles of Ozone', ylab = '', main = 'Empirical Cumluative Distribution\nOzone Pollution in New York')

ozone = airquality$Ozone
fun.ecdf <- ecdf(d04)
my.ecdf <- fun.ecdf(sort(d04))
x = seq(1,27,0.0026)[-1]
plot(x,my.ecdf,type='l',ylim=c(0,0.5),xlim=c(0,27),xlab="Degree","",col="red")
par(new=TRUE)
plot(density(d03),type='l',ylim=c(0,0.5),xlab="Degree","",xlim=c(0,250),col="red")
par(new=TRUE)
plot(density(d02),type='l',ylim=c(0,0.5),xlab="Degree","",xlim=c(0,250),col='green')
legend('topright',legend=c('10**4','10**3','10**2'),lty=1,col=c('blue','red','green'))
title(main="Degree Distriubtions for Barabasi-Albert model")


avg01 <- mean(transitivity(b02,type="local"))
avg02 <- mean(transitivity(b03,type="local"))
avg03 <- mean(transitivity(b04,type="local"))

outcome <- rep(NA,100000)

for(i in 10:100000){
  
  temp <- barabasi.game(i,m=4,start.graph = start,directed = FALSE)
  avg <- mean(transitivity(temp,type="local"))
  outcome[i] <- avg
  cat('\n',i)
  
}

set <- setdiff(outcome,NA)
x <- 5:(length(set)+4)
plot(x,set,type="l")
abline(h=0)
# ---------------------------------------------------------------
# Exercise 2
# ----------------------------------------------------------------

# (1) - Create two random undirected graphs with p=0.4 and 20 nodes
g01 <- erdos.renyi.game(20, p=0.4, directed = FALSE, loops = FALSE)
g02 <- erdos.renyi.game(20, p=0.4, directed = FALSE, loops = FALSE)
V(g01)$name <- paste(1:20)
V(g02)$name <- paste(21:40)

# Merge graphs and rewire edges
p <- 15/(ecount(g01)+ecount(g02))

g03 <- rewire( union(g01,g02), each_edge(prob = p))

plot(g03)

GN <- edge.betweenness.community(g03)
MM <- cluster_leading_eigen(g03)
colorRampPalette(brewer.pal(9,"Blues"))(100)
colorRampPalette(brewer.pal(9,"Greens"))(100)
plot(GN, g03, layout=layout_nicely(g03),mark.col=c("#B5D3E9","#B6E2B0"), mark.border=c('black','black'))
plot(MM, g03, layout=layout_with_fr(g03),mark.col=c("#B5D3E9","#B6E2B0"), mark.border=c('black','black'))

dendPlot(GN)

#Dendograms is plot illustrating the clustering by hierachical clustering methods. Those methods cluster recursivaly the previous
# cluster.

