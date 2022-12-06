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


# New Erdos-Renyi network; large ------------------------------------------

g2 <- erdos.renyi.game(n = 10000, p = 0.05)
plot(g2, layout = layout_with_fr, vertex.size = 1, vertex.label = NA)

g2_degree <- degree(g2)
ggplot(data = NULL, aes(x = g2_degree)) + geom_bar()

mean(g2_degree)
# compare to theoretical result
0.05 * (10000 - 1)

var(g2_degree)
# compare to theoretical result
0.05 * 0.95 * (10000 - 1)


# Read in Facebook data ---------------------------------------------------

g3fb <- graph_from_data_frame(
  read_delim(
    "https://raw.githubusercontent.com/mark-andrews/sna-course/main/data/facebook.txt",
    delim = ' '
  ), directed = FALSE)

# order, size
vcount(g3fb)
ecount(g3fb)
edge_density(g3fb)

# plot FB
plot(g3fb, vertex.size = 1, vertex.label = NA, layout = layout_with_fr)

# count the degrees
g3fb_degree <- degree(g3fb)
ggplot(data = NULL, aes(x = g3fb_degree)) + geom_bar()

g3fb_avg_deg <- mean(g3fb_degree)

ggplot(data = NULL,
       aes(x = rpois(10000, lambda = g3fb_avg_deg))
) + geom_bar()

# log-log probability plot
degree_distribution(g3fb) %>% 
  enframe(name = 'degree', value = 'prob') %>% 
  mutate(degree = degree - 1) %>% 
  ggplot(aes(x = degree, y = prob)) + 
  geom_point() +
  scale_x_continuous(trans = 'log10') +
  scale_y_continuous(trans = 'log10')



# Generate new Erdos-Renyi network ----------------------------------------

g4 <- erdos.renyi.game(vcount(g3fb),
                       ecount(g3fb),
                       type = 'gnm')
                 
vcount(g4)
vcount(g3fb)
ecount(g4)
ecount(g3fb)

# make a clique coefficient function
clique_coefs <- function(g){
  transitivity(g, type = 'local', isolates = 'zero')
}


# Plot the distribution of clique coefficients in both FB and random network
ggplot(data = NULL, aes(x = clique_coefs(g4))) + 
  geom_histogram(bins = 50)+
  ggtitle('Erdos-Renyi') 

ggplot(data = NULL, aes(x = clique_coefs(g3fb))) + 
  geom_histogram(bins = 50) +
  ggtitle('Facebook')


# distances ---------------------------------------------------------------

get_distances <- function(g){
  DD <- distances(g)
  DD[lower.tri(DD)]
}

# Plot the distribution of distances in both FB and random network
ggplot(data = NULL, aes(x = get_distances(g4))) + 
  geom_bar() +
  ggtitle('Erdos-Renyi: Distance') 

ggplot(data = NULL, aes(x = get_distances(g3fb))) + 
  geom_bar() +
  ggtitle('Facebook: Distance')
  