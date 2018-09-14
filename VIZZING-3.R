
### Some manual cleaning, resulting from experimenting with the data vizzing!

EdgelistClean1 <- gsub("nwobhm", "new wave of british heavy metal", as.matrix(EdgelistClean))
EdgelistClean1 <- gsub("garage punk fusion genre", "garage punk", as.matrix(EdgelistClean))
EdgelistClean1 <- gsub("pub rock united kingdom", "pub rock", as.matrix(EdgelistClean))

EdgelistClean1[EdgelistClean1[, "To"] == "punk rock",]

Punk1 <- EdgelistClean1[EdgelistClean1[, "From"] == "punk rock",]
Punk2 <- EdgelistClean1[EdgelistClean1[, "To"] == "punk rock",]
punkbind <- rbind(Punk1, Punk2)
punkbind

HH1 <- EdgelistClean1[EdgelistClean1[, "From"] == "hip hop",]
HH2 <- EdgelistClean1[EdgelistClean1[, "To"] == "hip hop",]
hhbind <- rbind(HH1, HH2)

hhpunkbind <- rbind(hhbind, punkbind)

EdgelistClean1[EdgelistClean1[, "To"] == "pub rock united kingdom",]

#################################################################################################################

### A package that does network analysis

library(igraph)
library(lattice)
library(stringr)
library(httpuv)
library(threejs)
library(networkD3)
library(webshot)

head(EdgelistClean)

# Setting our edgelist as a graph object

g <- graph.edgelist(as.matrix(EdgelistClean1), directed = TRUE)
g_und <- graph.edgelist(as.matrix(EdgelistClean1), directed = FALSE)
g1 <- graph.edgelist(as.matrix(punkbind), directed = TRUE)
g2 <- graph.edgelist(as.matrix(hhpunkbind), directed = TRUE)

# Remember that in R, it seems that...

# Vertex/Vertices = Nodes
# Edge = Edge, or Lines

plot(g)

# Omg what a mess!
# Let's subset things

E(g)[[inc("punk rock")]] # prints something cool-ish in the console but does not produce any graph

# Plotting the network

plot(g1, vertex.size = 0, edge.arrow.size = 0.1, layout = layout_nicely(g1)) #testing with punk rock
plot(g, vertex.size = 0.5, vertex.label = NA, edge.arrow.size = 0.1, layout = layout_as_tree(g)) 

plot(g2, vertex.size = 0, edge.arrow.size = 0.1, layout = layout_nicely(g2))

# Calculating the outgoing edges of every node/vertice

g.outd <- degree(g, mode = c("out"))

# Hip-hop has the most outbounds, meaning it permitted the most genres to emerge

which.max(g.outd)

# Calculating the "betweenness", basically how important a node/vertice is in connecting other nodes together

g.b <- betweenness(g, directed = TRUE)

# This plotting below will print a network chart with vertices of different sizes (according to importance)

plot(g, vertex.label = NA, edge.width = 0.5, vertex.size = sqrt(g.b)/6, layout = layout_nicely, edge.arrow.size = 0.01)

# This below will print a 3D interactive clustered network
# !!! WARNING !!! : this makes my RStudio crash most of the time, but it works fine with R alone. Save everything before trying with RStudio

# Community detector, color by group
# I am using g_und (my edgelist, but undirected) here because everything below only works with undirected edgelists

# On this matter, I thank Bryan W. Lewis, the author of the threejs package who helped me out after a quick email contact!

m <- membership(cluster_louvain(g_und))

# m is now a vector of group membership ids for each vertex

n <- length(unique(m))
colors <- rainbow(n)
graphjs(g_und, main = "Musicorum Galaxy", vertex.size=0.025, bg = "black", vertex.color=colors[m], vertex.label=V(g_und)$name)

# Note that edge.width cannot be changed - package is designed so for performance reasons
# But suppose we'd like some genres to be more visible, depending on their importance, we can do this... if we want

Z = as_adjacency_matrix(g_und)
v = abs(eigen(Z)$vector[,1])
v = v / max(v)
graphjs(g_und, main = "Musicorum Galaxy", vertex.size=v, bg = "#01173F", vertex.color=colors[m], vertex.label=V(g_und)$name, edge.alpha = 0.5)

# But it still looks... meh. 
# I tried to make edges invisible but it does not seem possible as in all cases it will simply adapt to the background color and appear as such when in front of a vertex

# Now let's make it more digestible by making users expand the Musicorum Galaxy themselves by clicking on nodes!

# I absolutely took all of the following right here -> https://bwlewis.github.io/rthreejs/advanced/advanced.html

N  <- length(V(g_und))

# Vertex page rank values
pr <- page_rank(g_und)$vector

# order the page rank values
i <- order(pr, decreasing=TRUE)

# Vertex cluster membership
cl <- unclass(membership(cluster_louvain(g_und)))

# Find the index of the highest page rank vertex in each cluster
idx <- aggregate(seq(1:N)[i], by=list(cl[i]), FUN=head, 1)$x

# Create a default force-directed layout for the whole network - Note here that it is the letter l and the number 1
l1 <- norm_coords(layout_with_fr(g_und, dim=3))

# Collapse the layout to just the idx vertices - note the same thing here, it's the letter l and number 0
l0 <- Reduce(rbind,Map(function(i) l1[idx[i],], cl))

# Grouped vertex colors, setting all but idx vertices transparent
col <- rainbow(length(idx), alpha=0)[cl]
col[idx] <- rainbow(length(idx), alpha=1)

# animation layouts, one for each of the idx vertices, and
# animation color schemes, one scheme for each idx vertex

click <- Map(function(i)
{
  x <- l0
  x[cl == i, ] <- l1[cl == i, ]
  c <- col
  list(layout=x, vertex.color=c)
}, seq(idx))

names(click) <- paste(idx)

# Let's plot that all!

graphjs(g_und, layout=l0, click=click, vertex.size=v, vertex.color=col, fps=20, bg = "#01173F", font.main="78px Arial", vertex.label=V(g_und)$name)

# Hehe!

# Testing with visNetwork
#library(visNetwork)

#visNetwork(MyNodes, edges = g_und) %>%
#  visOptions(highlightNearest = TRUE, 
#             nodesIdSelection = TRUE) %>%
#  visPhysics(stabilization = TRUE) %>%
#  visNodes(color = col, size = v) %>%
#  visEdges(smooth = FALSE, width = 0.01, physics = FALSE, selectionWidth = 10) %>%
#  visInteraction(hideEdgesOnDrag = FALSE, dragNodes = FALSE)

# Now we have to save that somewhere, so we'll need the package htmlwdigets
# You also might need to install some thing called Pandoc, R will tell you in the console, but here anyway: http://pandoc.org/installing.html

library(htmlwidgets)

saveWidget(graphjs(g_und, layout=l0, click=click, vertex.size=v, vertex.color=col, fps=20, bg = "#01173F", font.main="78px Arial", vertex.label=V(g_und)$name), file = "C:/Users/moukm/Google Drive/Birmingham - BCU/Cours/ADM7001 Major project/Arborescence/R/Vizzes/musicorum.html")

# Tadam!

# We can also put the labels directly on the nodes - thanks again to Bryan Lewis' reactive help!

