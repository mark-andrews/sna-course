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
  


# Giants, connectness, components, criticality ----------------------------

# a disconnected random network
g5 <- erdos.renyi.game(100, p = 1/200)
plot(g5, vertex.size = 5, vertex.label = NA, layout = layout_with_fr)

# get the components
components(g5)

# get the giant
get_giant <- function(N, k){
  p <- k/(N-1)
  g <- erdos.renyi.game(N, p)
  # get size of largest component
  # return as proportion of network size/order
  max(components(g)$csize)/N
}

get_giant(10000, 0.01)
get_giant(10000, 0.9)
get_giant(10000, 1.1)
get_giant(10000, 5)
get_giant(10000, 10)

k <- seq(1/1000, 10, by = 0.01)
# giant proportion
gprop <- map_dbl(k, ~get_giant(10000, .))

tibble(avgdeg = k,
       gprop = gprop) %>% 
  ggplot(aes(x = avgdeg, y = gprop)) + 
  geom_line() +
  geom_vline(xintercept = 1, colour = 'red') +
  geom_vline(xintercept = log(10000), colour = 'red')



# Small world nets --------------------------------------------------------

g6 <- sample_smallworld(dim = 1, size = 10, nei = 2, p = 0)
plot(g6, layout = layout_in_circle, vertex.label = NA)

g7 <- sample_smallworld(dim = 1, size = 25, nei = 3, p = 0)
plot(g7, layout = layout_in_circle, vertex.size = 2, vertex.label = NA)

degree(g6)

g8 <- sample_smallworld(dim = 1, size = 100, nei = 3, p = 0)
g8r <- erdos.renyi.game(n = vcount(g8),
                        ecount(g8),
                        type = 'gnm')
vcount(g8r)

# average clique coef in the two networks
clique_coefs(g8) %>% mean()
clique_coefs(g8r) %>% mean()

# average distances in the two nets
mean_distance(g8)
mean_distance(g8r)

diameter(g8)
diameter(g8r)

# rewiring probability of 10%
g9 <- sample_smallworld(dim = 1, size = 100, nei = 3, p = 0.1)
plot(g9, layout = layout_in_circle, vertex.size = 2, vertex.label = NA)


# Watts-Strogatz
g10 <- sample_smallworld(dim = 1, size = 1000, nei = 5, p = 0.1)
# Regular lattice
g10L <- sample_smallworld(dim = 1, size = 1000, nei = 5, p = 0.0)
# Erdos-Renyi
g10R <- erdos.renyi.game(n = vcount(g10),
                         ecount(g10),
                         type = 'gnm')

# clique coefficients
clique_coefs(g10) %>% mean()
clique_coefs(g10L) %>% mean()
clique_coefs(g10R) %>% mean()

# mean distances
mean_distance(g10)
mean_distance(g10L)
mean_distance(g10R)

diameter(g10)
diameter(g10L)
diameter(g10R)

# range of rewiring probability values
p <- seq(0, 0.25, length.out = 1000)
# for each probability calculate average clique coef in a Watts-Strogatz model
cc_range <- map_dbl(p,
    ~mean(clique_coefs(sample_smallworld(dim = 1, size = 1000, nei = 5, p = .)))
)

# plot it
tibble(p = p,
       cc = cc_range) %>% 
  ggplot(aes(x = p, y = cc)) + geom_point()


# for each probability calculate diameter in a Watts-Strogatz model
diam_range <- map_dbl(p,
      ~diameter(sample_smallworld(dim = 1, size = 1000, nei = 5, p = .))
)

# plot it
tibble(p = p,
       diam = diam_range) %>% 
  ggplot(aes(x = p, y = diam_range)) + geom_point()
