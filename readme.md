Social network analysis is an application of modern network science to the study of networks of individuals in human or animal societies.
Although almost unused prior to a few decades ago, in recent years, social network analysis has been successfully applied to research in a wide range of fields, including social psychology, political science, criminology, behavioural ecology, epidemiology, and many others.
This workshop will introduce attendees to the theory of modern network science and the practical application, using R, of these methods to the analysis of social network datasets found in the social and behavioural sciences.
We aim to provide a comprehensive introduction to the basic principles of network science theory, particularly focusing on graph theory and theory of random networks; an introduction to manipulating, visualizing, and describing social network data using R tools; statistical regression modelling of network data particularly using exponential random graph models, and stochastic actor-based models for network dynamics.
The R packages we use will include `igraph`, `ergm`, `RSiena`.


## Intended Audience

This course is aimed at anyone who analyses social network data in social or behavioural sciences.

## Teaching Format

This course will be largely hands-on and workshop based, with intensive use of R. However, theoretical concepts of modern network science and the theoretical background to the methods of statistical modelling of network data will delivered using slides in a lecture style.


## Assumed quantitative knowledge

We will assume some familiarity with some general mathematical concepts such as matrix algebra, calculus, probability distributions. However,
expertise with these concepts are not necessary. Anyone who has taken any undergraduate (Bachelor's) level course in mathematics, or even advanced high school level, can be assumed to have sufficient familiarity with these concepts.
Familiarity with the theory and practice of regression modelling, e.g. linear or generalized linear models, will also be assumed.

## Assumed computer background

We assume familiarity with using R for data manipulation, visualization, and statistical analyses.


## Equipment and software requirements

Attendees of the course must use a computer R and the necessary R packages installed. These packages will primarily include `igraph`, `ergm`, `RSienna`, `tidyverse`.

# Course programme


Module 1: *Introduction to graph theory and network science* (1 x 2hrs). The mathematical foundation for the statistical analysis of networks, social or otherwise, is *graph theory*, and in this context, a "graph" is the common technical or mathematical term for what we would otherwise simply call a network. In this module, we will introduce all the major concepts of graph theory: the mathematical definition of a graph (edges and vertices); kinds of graphs, including directed or undirected graphs, weighted or unweighted graphs, simple and biparite graphs; adjacency matrices, and graph sparsity, and graph connectedness; paths, distances, shortest paths, graph diameter; vertex degree, average vertex degree, and degree distributions; clustering coefficients. 

Module 2: *Descriptive analyses of graph structures* (1 x 2hrs). This module will introduce tools in R for creating, manipulating, and visualizing graphs, particularly using the powerful `igraph` package. We use `igraph` and some similar or related R packages to calculate and visualize graph-theoretic descriptive statistics from network data. These characteristics include degree distributions, node centrality, identifying nodes that are "hubs"" and "authorities", network cohesion, network density, diameter, clustering coefficients, assortativity coefficients, 

Module 3: *Random graphs (random networks), small world networks, scale-free networks, preferential attachment* (2 x 2hrs). This module aims to be thorough introduction to modern network science, an particularly provide an introduction to the major types of the mathematical models of real world networks. These models are the heart of modern network science. In a Erdős-Rényi random graph, edges between pairs of vertices exist independently at random with probability $p$. We will look at the properties of random graphs including degree distributions, evolution of their properties as they grow in size, and see the extent to which real world graphs share the properties of random graphs. We then consider simple modifications of the Erdős-Rényi random graph that lead to far more realistic models of real world networks. These are known as Watts-Strogatz *small world networks*. We will then turn to random network with *power law* degree distributions. These are known as scale-free networks and they are characterized by a small number of highly inter-connected *hubs*, which are very typical properties of social networks. We will then explore why these types of networks are so common in nature, which is that emerge from network *growth* and the *preferential attachment* of new vertices. Although our focus will be on the theory, we will explore also all of these important theoretical concepts using data analysis and demonstrations in R.

Module 4: *Statistical models of networks* (2 x 2hrs): Standard statistical models like the generalized linear models can be extended, albeit in non-trivial ways, to be used as statistical models for networks. In particular, the *exponential random graph models* (ERGMs) can be seen as counterparts to generalized linear models for use with network data. In this module, we provide a thorough introduction to the statistical theory behind ERGMs, and carry out analyses in practice using the `ergm` package. As a point of comparison with ERGMs, we will also consider Quadratic Assignment Procedure (QAP) regression and multiple regression quadratic assignment procedure (MRQAP), which are nonparameteric permutation based methods.

Module 5: *Modelling dynamic networks with stochastic actor-based models* (2 x 2hrs): Methods such as ERGMS can only be applied to networks that are not changing with time. However, social networks are dynamic by their nature, with relationships forming and changing with time. We can use *stochastic actor-based models for network dynamics* to model network dynamics on the basis of observed longitudinal data. This approach is essentially a combination of statistical modelling of networks, as with ERGMs, and longitudinal modelling. To perfom this analysis in R, we will use the `RSiena` package.


