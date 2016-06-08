# Packages 
library('igraph')
library('RColorBrewer')

# (1) - Create two random undirected graphs with p=0.4 and 20 nodes
g01 <- erdos.renyi.game(20, p=0.4, directed = FALSE, loops = FALSE)
g02 <- erdos.renyi.game(20, p=0.4, directed = FALSE, loops = FALSE)
V(g01)$name <- paste(1:20)
V(g02)$name <- paste(21:40)

# Merge graphs and rewire edges
p <- 15/ecount(g03)
g03 <- rewire( union(g01,g02), each_edge(prob = p))


GN <- edge.betweenness.community(g03)
MM <- optimal.community(g03)
colorRampPalette(brewer.pal(9,"Blues"))(100)
colorRampPalette(brewer.pal(9,"Greens"))(100)
plot(GN, g03, layout=layout_nicely(g03),mark.col=c("#B5D3E9","#B6E2B0"), mark.border=c('black','black'))
plot(MM, g03, layout=layout_with_fr(g03),mark.col=c("#B5D3E9","#B6E2B0"), mark.border=c('black','black'))

dendPlot(GN)

#Dendograms is plot illustrating the clustering by hierachical clustering methods. Those methods cluster recursivaly the previous
# cluster.