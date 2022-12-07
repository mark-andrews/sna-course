library(ergm)

data(package = 'ergm')

data(florentine) # loads "flomarriage"
plot(flomarriage,
     label = network.vertex.names(flomarriage)
)
