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


# model 3: using wealth covariate -----------------------------------------

summary(flomarriage ~ edges + nodecov('wealth'))

flomodel_v3 <- ergm(flomarriage ~ edges + nodecov('wealth'))
summary(flomodel_v3)

b <- coef(flomodel_v3)
# what's the prob of an edge if the combined wealth increases by 10
ilogit(b['edges'] + b['nodecov.wealth'] * 10)
# what's the prob of an edge if the combined wealth increases by 50
ilogit(b['edges'] + b['nodecov.wealth'] * 50)
# what's the prob of an edge if the combined wealth increases by 100
ilogit(b['edges'] + b['nodecov.wealth'] * 100)

map_dbl(list(flomodel_v1, flomodel_v2, flomodel_v3), AIC)


# New data ----------------------------------------------------------------

data("faux.mesa.high")

mesa <- faux.mesa.high

plot(mesa)
plot(mesa, vertex.col = 'Grade')

summary(mesa ~ edges + nodefactor('Grade', levels = TRUE))

grade <- mesa %v% 'Grade'
grade == 7
as.sociomatrix(mesa)[grade == 7,] %>% rowSums() %>% sum()
as.sociomatrix(mesa)[grade == 11,] %>% rowSums() %>% sum()

mesa_v1 <- ergm(mesa ~ edges + nodefactor('Grade'))
summary(mesa_v1)

b <- coef(mesa_v1)

ilogit(b['edges'] )
ilogit(b['edges'] + b['nodefactor.Grade.8'])



# Model: matching of factors ----------------------------------------------

summary(mesa ~ edges + nodematch('Grade'))

mesa_v2 <- ergm(mesa ~ edges + nodematch('Grade'))
summary(mesa_v2)

b <- coef(mesa_v2)

# the probaility of a new edge between two people of different grades
ilogit(b['edges'] + b['nodematch.Grade'] * 0)

# the probaility of a new edge between two people of the same grade
ilogit(b['edges'] + b['nodematch.Grade'] * 1)

