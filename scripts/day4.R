library(RSiena)

s501
g501 <- igraph::graph_from_adjacency_matrix(s501)
plot(g501)

s502
g502 <- igraph::graph_from_adjacency_matrix(s502)
plot(g502)

s503
g503 <- igraph::graph_from_adjacency_matrix(s503)
plot(g503)

s50a # alcohol
s50s # smoking


# Create the 3d adjacency matrix ------------------------------------------

# data representing the network change over time
friendship_data <- array(
  c(s501, s502, s503),
  dim = c(50, 50, 3)
)

# the "dependent" variable in the Siena model
friendship <- sienaDependent(friendship_data)

# the covariate data structures
smoking <- coCovar(s50s[,1])
drinking <- varCovar(s50a)

# siena model data
s50model_data <- sienaDataCreate(friendship, smoking, drinking)

# siena model effects
s50_effects <- getEffects(s50model_data)
s50_effects <- includeEffects(s50_effects, transTrip, cycle3)

# siena model algorithm
s50_algo <- sienaAlgorithmCreate(projname = 's50_dec8')

# pop up
s50_model <- siena07(s50_algo,
                     data = s50model_data,
                     effects = s50_effects)

# without pop up

s50_model <- siena07(s50_algo,
                     data = s50model_data,
                     effects = s50_effects,
                     batch = TRUE)

s50_model$tconv.max

# run it again if not converged, using previous state
s50_model <- siena07(s50_algo,
                     data = s50model_data,
                     effects = s50_effects,
                     prevAns = s50_model,
                     batch = TRUE)

s50_model$tconv.max

s50_model
