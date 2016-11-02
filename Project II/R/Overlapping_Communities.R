# ------------------------------------------------------------
# Compute label matrix 
# ------------------------------------------------------------

# Clear work space
rm(list = ls())

# Load Packages
if (!require("igraph"))     install.packages("igraph");     library(igraph)
if (!require("linkcomm"))   install.packages("linkcomm");   library(linkcomm)
if (!require("tictoc"))     install.packages("tictoc");     library(tictoc)
if (!require("tikzDevice")) install.packages("tikzDevice"); library(tikzDevice)

# Set working directory
setwd('CHANGE ME --- CHANGE ME --- CHANGE ME')

# Initialize color Set
co <- 1 / 255 

pers.green      <- rgb( co *  14 ,  co * 105 , co *  90 )
pers.blue       <- rgb( co *  22 ,  co *  54 , co *  92 )
pers.red        <- rgb( co *  99 ,  co *  37 , co *  35 )
pers.gray       <- rgb( co * 150 ,  co * 150 , co * 150 )
pers.orange     <- rgb( co * 186 ,  co *  85 , co *  59 )
pers.beige      <- rgb( co * 196 ,  co * 189 , co * 151 )

full.color.set  <- list( pers.green , pers.blue, pers.red,
                         pers.gray, pers.orange, pers.beige) 

# Load data
nodes       <- read.csv("flickr-nodes.csv", header = FALSE)
edges       <- read.csv("flickr-edges.csv", header=FALSE)
groups      <- as.vector(read.csv("flickr-groups.csv", header=FALSE))
group.edges <- as.vector(read.csv("flickr-group-edges.csv", header=FALSE))

# ------------------------------------------------------------
# Graph related information and label matrix
# ------------------------------------------------------------

# Create graph object
get.graph <- graph_from_data_frame(edges, vertices=nodes,directed = FALSE)

# Compute adjacency matrix
mat       <- get.adjacency(get.graph)

# Init label  matrix
labels <- matrix(0,80513,195)

# Compute labels from files
for( i in 1:80513){
  
  # Trace
  cat('\rComputing for i =', i)
  if (i == 80513) { cat(' [Done]\n') }
  
  # Fill matrix
  ind <- which(group.edges[,1] == i) 
  val <- group.edges[ind,2]
  labels[i,][val] = 1
  
  
}

# Compute Label density
label.density <- (length(labels[labels>0]) / (nrow(labels) * ncol(labels))) * 100

# Compute mean labels
mean.lab <- mean( rowSums( labels ) )

# Compute graph features
r             <- rowSums(labels)
graph.density <- graph.density(get.graph)

# ------------------------------------------------------------
# Community run time simuulation
# ------------------------------------------------------------

i <- 1
time <- c()
for( nodes in seq(100,900,200)){
  cat('iteration',i,'\n')

  tic()
  random.graph <- get.edgelist(erdos.renyi.game(nodes,p=0.1,directed=FALSE))
  system.time(getLinkCommunities( random.graph, directed = FALSE, check.duplicates = FALSE,
                      plot = FALSE ))

  
  exectime <- toc()
  exectime <- exectime$toc - exectime$tic
  time <- c(time,exectime)
  i <- i + 1

  }

# Data for plots
seq01  <- seq(100,900,200)
time02 <- time/60

# ------------------------------------------------------------
# Community computation
# ------------------------------------------------------------

# Sample random nodes - uniformly
rand.indices      <- sample(80513,8000,replace = FALSE)
V(get.graph)$name <- 1:vcount(get.graph)

# Construct graph and drop isolated nodes
g02   <- induced_subgraph(get.graph,vids=rand.indices)
drops <- as.numeric(names(degree(g02)[degree(g02)<1]))
g02   <- delete_vertices(g02,as.character(drops))

# Corresponding labels for random set - Drop empty columns
new.labels    <- labels[setdiff(rand.indices,drops),]
final.labels  <- new.labels[,which(colSums(new.labels)>0)]

# Find communities of new sub graph  
communities <- getLinkCommunities( get.edgelist(g02), directed = FALSE, check.duplicates = TRUE,plot = FALSE )

# Compute Dimensions
M  <- length(communities$clusters)
N  <- vcount(g02)

# New data
new.edge.list <- get.edgelist(g02)
new.edge.list <- data.frame(V1=as.numeric(new.edge.list[,1]),V2=as.numeric(new.edge.list[,2]))
nodes         <- unique(as.numeric(c(new.edge.list[,1],new.edge.list[,2])))

# ------------------------------------------------------------
# Plots
# ------------------------------------------------------------

#tikz("powerlaw.tex",width = 5, height = 5)
plot(table(degree(get.graph)),xaxt='n',xlab='Number of nodes',ylab='Number of degrees',main='',
     col = pers.blue)  
ytick<-seq(0, 6000, by=1000)
axis(side=1, at=ytick, labels = ytick)
#dev.off()

#tikz("powerlaw_2.tex",width = 5, height = 5)
plot(table(degree(g02)),,xaxt='n',xlab='Number of nodes',ylab='Number of degrees',main='',
     col = pers.blue)  
ytick<-seq(0, 500, by=100)
axis(side=1, at=ytick, labels = ytick)
#dev.off()

#tikz("exectime.tex",width = 5, height = 5)
matplot(seq01,time02,xlab="Number of nodes",ylab='Execution time (in minutes)',type='l',lty=2,
        lwd = 3,col=pers.blue)
#dev.off()
