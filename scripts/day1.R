library(tidyverse)
library(igraph)


# Get some data -----------------------------------------------------------

karate_df <- read_delim("https://raw.githubusercontent.com/mark-andrews/sna-course/main/data/karate.txt",
                        delim = ' ',
                        col_names = c('x', 'y'))

g1 <- graph_from_data_frame(karate_df, directed = FALSE)
class(g1)


# Get the adjacency matrix ------------------------------------------------

A <- as_adjacency_matrix(g1) %>% as.matrix()
isSymmetric(A)
SparseM::image(A) # visualize sparse matrix


# Visualize the graph -----------------------------------------------------

set.seed(10101)
plot(g1)

# Using the Fruchterman-Reingold algorithm
plot(g1, layout = layout_with_fr)
plot(g1, layout = layout_with_fr, vertex.size = 10)

# Davidson-Harel
plot(g1, layout = layout_with_dh)

plot(g1, layout = layout_with_gem)

# Kamada Kawai 
plot(g1, layout = layout_with_kk)

# large graph layout 
plot(g1, layout = layout_with_lgl)

# star layout
plot(g1, layout = layout_as_star)

# circle layout
plot(g1, layout = layout_in_circle)

# tree layout
plot(g1, layout = layout_as_tree)



# Graph properties --------------------------------------------------------

gorder(g1) # number of vertices
# see dim(A)
gsize(g1)  # number of edges

vcount(g1) # number of vertices
ecount(g1) # number of edges

V(g1) # vertex list
E(g1) # edge list


# Density -----------------------------------------------------------------

edge_density(g1)

N <- vcount(g1)
L <- ecount(g1)
L / (N * (N-1)/2)
2 * L/(N * (N-1))


# Degree of the graph -----------------------------------------------------


# Degree of graph
degree(g1)

rowSums(A)
colSums(A)

g1_degree_df <- degree(g1) %>% enframe(name = 'vertex', value = 'degree')
g1_degree_df %>% summarise(min = min(degree),
                           max = max(degree),
                           median = median(degree),
                           mean = mean(degree),
                           iqr = IQR(degree))

# plot the degree distribution
ggplot(g1_degree_df, aes(x = degree)) + geom_bar()

degree_distribution(g1)


# Path, distances, diameter -----------------------------------------------

shortest_paths(g1, from = '5')$vpath
all_shortest_paths(g1, from = '5')$res

shortest.paths(g1)
distances(g1)

DD <- distances(g1)
mean(DD[lower.tri(DD)])

mean_distance(g1)

distance_table(g1)

diameter(g1) # diameter of the graph
max(DD)

# Clustering coefficients -------------------------------------------------

transitivity(g1, type = 'local', isolates = 'zero')
transitivity(g1, type = 'local', isolates = 'zero', vids = '2')

# local clustering coefficient
transitivity(g1, type = 'local', isolates = 'zero') %>% mean()

# global clustering coefficient
transitivity(g1, type = 'global')

# see all the triangles
triangles(g1)
matrix(triangles(g1), nrow = 3)

count_triangles(g1) 


# Centrality --------------------------------------------------------------

closeness(g1) # closeness centrality
closeness(g1) %>% which.max()
closeness(g1) %>% which.min()
