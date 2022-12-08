library(ergm)
library(tidyverse)

# list available data sets in package
data(package = 'ergm')

data(florentine) # loads "flomarriage"


# plot the network
plot(flomarriage,
     label = network.vertex.names(flomarriage)
)

# some descriptives
network.size(flomarriage)
network.edgecount(flomarriage)
network.density(flomarriage)

list.vertex.attributes(flomarriage)
get.vertex.attribute(flomarriage, 'wealth')
flomarriage %v% 'wealth'


# Model 1: Erdos-Renyi ----------------------------------------------------

# flomarriage ~ edges 
# ergm(flomarriage ~ edges)

summary(flomarriage ~ edges)

flomodel_v1 <- ergm(flomarriage ~ edges)

# summary
summary(flomodel_v1)

ilogit <- function(x) 1 / (1 + exp(-x))

# probability of an edge 
ilogit(coef(flomodel_v1))

# log likelihood
logLik(flomodel_v1)
-2 * logLik(flomodel_v1) + 2 * length(coef(flomodel_v1))
AIC(flomodel_v1)
-2 * logLik(flomodel_v1) + log(120) * length(coef(flomodel_v1))
BIC(flomodel_v1)


# Model 2: number of edges and triangles ----------------------------------

summary(flomarriage ~ edges + triangles)

flomodel_v2 <- ergm(flomarriage ~ edges + triangles)

summary(flomodel_v2)

b <- coef(flomodel_v2)

# What is the prob of an edge if it creates no new triangles
ilogit(b['edges'] * 1 + b['triangle'] * 0)
# What is the prob of an edge if it creates one new triangle
ilogit(b['edges'] * 1 + b['triangle'] * 1)
# What is the prob of an edge if it creates two new triangles
ilogit(b['edges'] * 1 + b['triangle'] * 2)

confint(flomodel_v2)

map_dbl(list(flomodel_v1, flomodel_v2), AIC)
