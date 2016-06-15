# ----------------------------------------------------------------
# Praeamble
# ----------------------------------------------------------------

# Packages 
library('igraph')
library('RColorBrewer')
library('poweRlaw')
library('tikzDevice')

# help function
cdf_val <- function(vec){
  result <- as.list(environment(ecdf(vec)))
  return(result)
}

# Set Working Directory
setwd('/Users/felix/Documents/GSE/Term 3/14D009 Social and Economic Networks/Problemsets/14D009-Social-and-Economic-Networks/Problem Set 3')

# ----------------------------------------------------------------
# Exercise 1
# ----------------------------------------------------------------

# Compute starting condition
start = graph.adjacency(matrix(1,4,4) - diag(1,4), mode=c("undirected"))

# Compute BA Graph and corresponding degrees
b02 <- barabasi.game(10**2,m=4,start.graph = start,directed = FALSE); d02 = degree(b02)
b03 <- barabasi.game(10**3,m=4,start.graph = start,directed = FALSE); d03 = degree(b03)
b04 <- barabasi.game(10**4,m=4,start.graph = start,directed = FALSE); d04 = degree(b04)

# (a) and (b) Plot densities of degrees

# Densities
tikz("p01.tex", width = 3.5, height =3.5)

  plot(density(d04),type='l',ylim=c(0,0.4),xlab="Degree",ylab="Density",xlim=c(0,250),
       col='blue', lwd=2,cex.lab=0.8,cex.axis = 0.5,tck=-0.03,las=1,mgp = c(2, 0.5, 0),main="")
  
  par(new=TRUE)
  
  plot(density(d03),type='l',ylim=c(0,0.4),xlab="",ylab="",xlim=c(0,250),col="red", lwd=2,
       axes=FALSE,main="")
  
  par(new=TRUE)
  
  plot(density(d02),type='l',ylim=c(0,0.4),xlab="",ylab="",xlim=c(0,250),col='green', 
       lwd=2,axes=FALSE,main="")
  
  legend('topright',legend=c(expression(paste(10^2)),expression(paste(10^3)),expression(paste(10^4))),
         lty=1,col=c('green','red','blue'), bty = "n",cex=0.8)
  
dev.off()

# Power Law data
t00 <- table(d02)
t01 <- log(as.numeric(names(t00)))
t02 <- log(as.numeric(t00))

t10 <- table(d03)
t11 <- log(as.numeric(names(t10)))
t12 <- log(as.numeric(t10))

t20 <- table(d04)
t21 <- log(as.numeric(names(t20)))
t22 <- log(as.numeric(t20))

# Power law plot
tikz("p02.tex", width = 3.5, height =3.5)

  plot(t01,t02,xlim=c(1,6),ylim=c(0,8.5),col='red',pch=19,cex=0.5,xlab="log(degree)",ylab="log(frequency)",
       cex.lab=0.8,cex.axis = 0.5,tck=-0.03,las=1,mgp = c(1.2, 0.5, 0))
  
  par(new=TRUE)
  
  plot(t11,t12,xlim=c(1,6),ylim=c(0,8.5),col='blue',pch=19,cex=0.5,xlab="",ylab="",axes=FALSE)
  
  par(new=TRUE)
  
  plot(t21,t22,xlim=c(1,6),ylim=c(0,8.5),col='green',pch=19,cex=0.5,xlab="",ylab="",axes=FALSE)

  abline(12,-2,lty=2)
  
  legend('topright',legend=c(expression(paste(10^2)),expression(paste(10^3)),expression(paste(10^4))),
         lty=1,col=c('blue','red','green'), bty = "n",cex=0.8)
  
dev.off()

# (c) Plot the CDF
cdf04 <- cdf_val(d04)
cdf03 <- cdf_val(d03)
cdf02 <- cdf_val(d02)

# CDF Plots
tikz("p03.tex", width = 3.5, height =3.5)
  plot(cdf04, type="l", col = "green",ylim=c(0,1),xlab="",ylab="",cex.lab=0.8,
       cex.axis = 0.5,tck=-0.03,bty="l",las=1,mgp = c(2, 0.5, 0))
dev.off()

tikz("p04.tex", width = 3.5, height =3.5)
  plot(cdf03, type="l", col = "red",ylim=c(0,1),xlab="",ylab="",cex.lab=0.8,
       cex.axis = 0.5,tck=-0.03,bty="l",las=1,mgp = c(2, 0.5, 0))
dev.off()

tikz("p05.tex", width = 3.5, height =3.5)
  plot(cdf02, type="l", col = "blue",ylim=c(0,1),xlab="",ylab="",cex.lab=0.8,
       cex.axis = 0.5,tck=-0.03,bty="l",las=1,mgp = c(2, 0.5, 0))
dev.off()

# (d) Measure local clustering coefficient iterativly

avg01 <- round(mean(transitivity(b02,type="local")),3)
avg02 <- round(mean(transitivity(b03,type="local")),3)
avg03 <- round(mean(transitivity(b04,type="local")),3)

# Plot clustering as a function of N
iter <- c(4,seq(1000,100000,1000))
outcome <- rep(NA,length(iter))

i <- 1
for(j in iter){
  
  temp <- barabasi.game(j,m=4,start.graph = start,directed = FALSE)
  avg <- mean(transitivity(temp,type="local"))
  outcome[i] <- avg
  cat('\n',i)
  i <- i + 1
}

# Plot results
tikz("p06.tex",width = 6, height =6)
plot(iter,outcome,type="l",col="blue",xlab = "N",
     ylab = "Average clustering coefficient",cex.lab=0.8,
     cex.axis = 0.5,tck=-0.01,bty="l",las=1,mgp = c(2, 0.5, 0))
dev.off()


# (e) Measure degree dynamics of one of the initial nodes 

# Set number of time periods
iter <- 5:15000

# Init storage objects
d51  <- rep(NA,length(iter))
d52  <- rep(NA,length(iter))
d53  <- rep(NA,length(iter))
d54  <- rep(NA,length(iter))


# Run simmulation
for( i in iter){
  
  # Trace
  cat('\rComputing for i =', i)
  if (i == max(iter)) { cat(' [Done]\n') }
  
  # Set to controll the developement of the model
  set.seed(123)
  
  # Comptute temp graph for time t = i
  temp <- barabasi.game(i,m=4,start.graph = start,directed = FALSE)
  # Get degree
  temp.degree <- degree(temp)
  
  # Save degree for specific nodes
  d51[(i-4)] <- temp.degree[1]
  if(i > 99){
    d52[(i-4)] <- temp.degree[100]
  }
  if(i > 999){
    d53[(i-4)] <- temp.degree[1000]
  }
  if(i > 4999){
    d54[(i-4)] <- temp.degree[5000]
  }
}


# Create Plots
tikz("p07.tex",width = 6, height =6)
  plot(iter,d51,type="l",xlim=c(0,15000),ylim = c(0,140),xlab="Time",ylab="Degree",cex.lab=0.8,
       cex.axis = 0.5,tck=-0.01,bty="l",las=1,mgp = c(2, 0.5, 0),col='blue')
  par(new=TRUE)
  plot(iter,d52,type="l",xlim=c(0,15000),ylim = c(0,140),xlab="Time",ylab="Degree",cex.lab=0.8,
       cex.axis = 0.5,tck=-0.01,bty="l",las=1,mgp = c(2, 0.5, 0),col='red')
  par(new=TRUE)
  plot(iter,d53,type="l",xlim=c(0,15000),ylim = c(0,140),xlab="Time",ylab="Degree",cex.lab=0.8,
       cex.axis = 0.5,tck=-0.01,bty="l",las=1,mgp = c(2, 0.5, 0),col='green')
  par(new=TRUE)
  plot(iter,d54,type="l",xlim=c(0,15000),ylim = c(0,140),xlab="Time",ylab="Degree",cex.lab=0.8,
       cex.axis = 0.5,tck=-0.01,bty="l",las=1,mgp = c(2, 0.5, 0),col='black')
  legend('topleft',legend=c("Degree node 1","Degree node 100","Degree node 1000","Degree node 5000" ),
         lty=1,col=c('blue','red','green'), bty = "n",cex=0.6)
dev.off()

# ---------------------------------------------------------------
# Exercise 2
# ----------------------------------------------------------------


# (1) - Create two random undirected graphs with p=0.4 and 20 nodes
g01 <- erdos.renyi.game(20, p=0.4, directed = FALSE, loops = FALSE)
g02 <- erdos.renyi.game(20, p=0.4, directed = FALSE, loops = FALSE)

V(g01)$name <- paste(1:20)
V(g02)$name <- paste(21:40)

# Merge graphs and rewire edges
p   <- 15/(ecount(g01)+ecount(g02))
g03 <- rewire( union(g01,g02), each_edge(prob = p))

plot(g03)

GN <- edge.betweenness.community(g03)
MM <- cluster_leading_eigen(g03)
colorRampPalette(brewer.pal(9,"Blues"))(100)
colorRampPalette(brewer.pal(9,"Greens"))(100)

tikz("p08.tex",width = 3.5, height =3.5)
  plot(GN, g03, layout = layout_nicely(g03),mark.col=c("#B5D3E9","#B6E2B0"), mark.border=c('black','black'))
dev.off()

tikz("p09.tex",width = 3.5, height =3.5)
  plot(MM, g03, layout = layout_with_fr(g03),mark.col=c("#B5D3E9","#B6E2B0"), mark.border=c('black','black'))
dev.off()

tikz("p10.tex",width = 6, height =6)
  dendPlot(GN)
dev.off()
