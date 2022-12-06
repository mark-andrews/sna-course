library(tidyverse)
library(igraph)


# Create an Erdos Renyi network -------------------------------------------

g1 <- erdos.renyi.game(n = 100, p = 0.1)

# how many possible edges in g1?
choose(100, 2)
100 * 99 / 2

# how many nodes
vcount(g1)

# how many edges
ecount(g1)

# plot it
plot(g1, 
     layout = layout_with_fr, 
     vertex.label = NA, 
     vertex.size = 1)

plot(g1, 
     layout = layout_in_circle, 
     vertex.label = NA, 
     vertex.size = 1)


# Generate a large number of Erdos-Renyi network
# with N = 100, p = 0.1

N <- 100
p <- 0.1

g1_set <- 
  replicate(
  10 ^ 5,
  erdos.renyi.game(n = N, p = p)
)

# count the number of edges in all these network
g1_set_n_edges <- map_dbl(g1_set, ecount)

# what is the mean?
mean(g1_set_n_edges)

# compare to theory
p * choose(N, 2)

# what is the variance
var(g1_set_n_edges)
# compare to theory
p * (1-p) * choose(N, 2)

# visualize the distribution
ggplot(data = NULL, aes(x = g1_set_n_edges)) + geom_bar()
