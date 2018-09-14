# Networks testing

# Let's try another version, with the package networkD3 - dummy test - taken from here : https://www.r-graph-gallery.com/252-simple-interactive-network-with-networkd3/

# create data:

library(networkD3)
library(digest)
library(ggplot2)
library(GGally)
library(devtools)
library(RColorBrewer)
library(network)
library(visNetwork)
library(ggnetwork)
library(igraph)

set.seed(20)

links=data.frame(EdgelistClean1)

# Plot
graph=simpleNetwork(links)
saveNetwork(graph,file = '#252_interactive_network_chart1.html', selfcontained = T)

# Plot customisation dummy test
simpleNetwork(links,     
              Source = 1,                 # column number of source
              Target = 2,                 # column number of target
              height = 4800,              # height of frame area in pixels
              width = 4800,
              linkDistance = 500,         # distance between node. Increase this value to have more space between nodes
              charge = 0,              # numeric value indicating either the strength of the node repulsion (negative value) or attraction (positive value)
              fontSize = 22,              # size of the node names
              fontFamily = "serif",       # font og node names
              linkColour = "#666",        # colour of edges, MUST be a common colour for the whole graph
              nodeColour = "red",         # colour of nodes, MUST be a common colour for the whole graph
              opacity = 0.8,              # opacity of nodes. 0=transparent. 1=no transparency
              zoom = T                    # Can you zoom on the figure?
)

# To make the same interactive 3D from threejs but with accumulated clicked animations

# I absolutely took all of the following right here -> https://bwlewis.github.io/rthreejs/advanced/advanced.html

N  <- length(V(g_und))

# Vertex page rank values
pr <- page_rank(g_und)$vector

# Order the page rank values
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
  x <- x - l0
  c <- col
  c[cl == i] <- rainbow(length(idx), alpha=1)[i]
  list(layout=x, vertex.color=c, cumulative = TRUE)
}, seq(idx))

names(click) <- paste(idx)

# Let's plot that all!

graphjs(g_und, layout=l0, click=click, vertex.size=v, vertex.color=col, fps=20, bg = "#01173F", font.main="78px Arial", vertex.label=V(g_und)$name)

# Hehe!

# Now we have to save that somewhere, so we'll need the package htmlwdigets
# You also might need to install some thing called Pandoc, R will tell you in the console, but here anyway: http://pandoc.org/installing.html

library(htmlwidgets)

saveWidget(graphjs(g_und, layout=l0, click=click, vertex.size=v, vertex.color=col, fps=20, bg = "#01173F", font.main="78px Arial", vertex.label=V(g_und)$name), file = "C:/Users/moukm/Google Drive/Birmingham - BCU/Cours/ADM7001 Major project/Arborescence/R/Vizzes/musicorum.html")

# Tadam!

# Tree type with dendroNetwork - Nein exactly

hc <- hclust(dist(USArrests), "ave")

dendroNetwork(hc, height = 600)
dendroNetwork(hc, treeOrientation = "vertical")

dendroNetwork(hc, height = 600, linkType = "diagonal")
dendroNetwork(hc, treeOrientation = "vertical", linkType = "diagonal")

dendroNetwork(hc, textColour = c("red", "green", "orange")[cutree(hc, 3)],
              height = 600)
dendroNetwork(hc, textColour = c("red", "green", "orange")[cutree(hc, 3)],
              treeOrientation = "vertical")

# Tree type with chordNetwork - Nein

# With GGally and ggplot2
# kamadakawai is interesting as a mode
# spring too, but hard to see how different it is from the default fruchtermanreingold - this latter is more spaced

Wario <- c("St-Jerome", "Yukon", "Montreal", "Quebec", "Brum", "Yamachiche", "Walsall", "Walsall", "Brum", "Montreal", "Quebec")
Waluigi <- c("Yamachiche", "Walsall", "Walsall", "Brum", "Montreal", "Quebec", "St-Jerome", "Yukon", "Montreal", "Quebec", "Brum")
net <- cbind(Wario, Waluigi)


palette1 <- brewer.pal(4, "Greens")
display.brewer.pal(3, "Greens")

ggnet2(network(EdgelistClean1, directed = TRUE), arrow.gap = 0.05, edge.alpha = 1, arrow.size = 1, node.size = 0.1, color = "blue")
ggnet2(network(net, directed = TRUE), arrow.gap = 0.01, arrow.size = 1, node.size = 4, color = "blue")


# With visNetwork - template below

nodes <- jsonlite::fromJSON("https://raw.githubusercontent.com/datastorm-open/datastorm-open.github.io/master/visNetwork/data/nodes_miserables.json")

edges <- jsonlite::fromJSON("https://raw.githubusercontent.com/datastorm-open/datastorm-open.github.io/master/visNetwork/data/edges_miserables.json")


visNetwork(nodes, edges, height = "700px", width = "100%") %>%
  visOptions(selectedBy = "group", 
             highlightNearest = TRUE, 
             nodesIdSelection = TRUE) %>%
  visPhysics(stabilization = TRUE)

# With visNetwork - real case below

UniqueFrom <- unique(EdgelistClean1[,1])
UniqueTo <- unique(EdgelistClean1[,2])
UniqueFromTo <- c(UniqueFrom, UniqueTo)
UniqueNodes <- unique(UniqueFromTo)
UniqueNodesDF <- data.frame(UniqueNodes)
nrow(UniqueNodesDF)
UniqueNodesDFwithID <- cbind(UniqueNodesDF, UniqueNodesDF)
colnames(UniqueNodesDFwithID) <- c("id", "label")
head(UniqueNodesDFwithID)
MyNodes <- UniqueNodesDFwithID
EdgelistClean1_DF <- as.data.frame(EdgelistClean1)
colnames(EdgelistClean1_DF) <- c("from", "to")

# https://wesslen.github.io/text%20mining/topic-networks/ 

EdgelistClean1_igraph <- graph.edgelist(EdgelistClean1, directed = FALSE)
LouvainThing <- cluster_louvain(EdgelistClean1_igraph)
  
plot(LouvainThing, EdgelistClean1_igraph, edge.width = E(EdgelistClean1_igraph)$edge.width, vertex.size = 2, vertex.label = "")

V(EdgelistClean1_igraph)$community <- LouvainThing$membership
V(EdgelistClean1_igraph)$betweenness <- betweenness(EdgelistClean1_igraph, v = V(EdgelistClean1_igraph), directed = F)
V(EdgelistClean1_igraph)$degree <- degree(EdgelistClean1_igraph, v = V(EdgelistClean1_igraph))

data <- toVisNetworkData(EdgelistClean1_igraph)
nodes <- data[[1]]
edges <- data[[2]]

colBrewer <- brewer.pal(12, "Set3")[as.factor(nodes$community)]
nodes$shape <- "dot" 
nodes$size <- ((nodes$betweenness / max(nodes$betweenness))+.2)*100 # Node size
nodes$borderWidth <- 0.5 # Node border width
nodes$color.background <- colBrewer
nodes$color.border <- "black"
nodes$color.highlight.background <- "orange"
nodes$color.highlight.border <- "darkred"
edges$width <- 0.1
nodes$shadow <- FALSE

# Last step

visNetwork(nodes, edges) %>% 
  visOptions(highlightNearest = TRUE, selectedBy = "community", nodesIdSelection = TRUE) %>%
  visPhysics(stabilization = TRUE) %>%
  visEdges(smooth = FALSE, physics = FALSE, selectionWidth = 10, length = 1, hidden = TRUE) %>%
  visIgraphLayout() %>%
  visInteraction(hideEdgesOnDrag = FALSE, dragNodes = FALSE)

# Gne

visNetwork(MyNodes, edges = EdgelistClean1_DF) %>%
  visOptions(highlightNearest = TRUE, 
             nodesIdSelection = TRUE) %>%
  visPhysics(stabilization = TRUE) %>%
  visNodes(size = 10) %>%
  visEdges(smooth = FALSE, width = 0.01, physics = FALSE, selectionWidth = 10) %>%
  visInteraction(hideEdgesOnDrag = FALSE, dragNodes = FALSE)

ok[1:27] <- cluster_louvain(EdgelistClean1_igraph)
