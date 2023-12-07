### WEB MINING PROJECT ###


# INTRODUCTION:
library(igraph)
library(rgexf)
#setwd("C:/Users/ciard/Desktop/Informatica/2Magistrale/Materie/Web Mining/Laboratorio/Progetto")
setwd("~/Progetto Web Mining")
set.seed(111)


# Definition of male penguins:
males = c("Takatsuji", "Juujou", "Kamanza", "Ryou", "Nishi", "Kami", "Atsushii", "Sakura", 
          "Yanagi", "Joufuku", "Matsuya", "Oike", "Man", "Nako", "Kuruma", "Rokkaku",
          "Kuro", "Mikage", "Horikawa", "Oomiya", "Nijou", "Ebisu", "Aya", "Same", 
          "Takara", "Shijou", "Shinmachi", "Takakura", "Take", "Maru", "Naka", "Higashi", 
          "Yosha", "Bukkou", "Abira", "Sen")


# Reading data from various files:
matrix_couples <- read.csv("./data/csv/Penguins of Kyoto - Couples.csv")
matrix_enemies <- read.csv("./data/csv/Penguins of Kyoto - Enemies.csv")
matrix_complicated <- read.csv("./data/csv/Penguins of Kyoto - Complicated.csv")
matrix_friends <- read.csv("./data/csv/Penguins of Kyoto - Friends.csv")
matrix_ex <- read.csv("./data/csv/Penguins of Kyoto - Exes.csv") #(Not Bad)
matrix_family <- read.csv("./data/csv/Penguins of Kyoto - Family.csv") 


# Sometimes the same penguin is referred to by different names:
# Yosha = Yoshiya
# Moto = Moe

# Disambiguation of names:
names <-  c(matrix_family[, 1])
names[11] <- "Moe"
names[12] <- "Atsushii"
names[18] <- "Kagera"
names[56] <- "Abira"
names[53] <- "Yosha"


# Definition of female penguins:
females = setdiff(names, males)

# ADJACENCY MATRICES:

# Definition of the adjacency matrices related to the various graphs:
# Adjacency matrix for family:
matrix_family <- as.matrix(matrix_family)
matrix_family <- as.numeric(matrix_family[, -1])
matrix_family <- matrix(matrix_family, nrow=59, ncol=59)

# Adjacency matrix for ex:
matrix_ex <- as.matrix(matrix_ex)
matrix_ex <- as.numeric(matrix_ex[, -1])
matrix_ex <- matrix(matrix_ex, nrow=59, ncol=59)

# Adjacency matrix for couples:
matrix_couples <- as.matrix(matrix_couples)
matrix_couples <- as.numeric(matrix_couples[, -1])
matrix_couples <- matrix(matrix_couples, nrow=59, ncol=59)

# Adjacency matrix for enemies:
matrix_enemies <- as.matrix(matrix_enemies)
matrix_enemies <- as.numeric(matrix_enemies[, -1])
matrix_enemies <- matrix(matrix_enemies, nrow=59, ncol=59)

# Adjacency matrix for complicated:
# Complicated data refer to complicated relationships among penguins
matrix_complicated <- as.matrix(matrix_complicated)
matrix_complicated <- as.numeric(matrix_complicated[, -1])
matrix_complicated <- matrix(matrix_complicated, nrow=59, ncol=59)

# Adjacency matrix for friends:
matrix_friends <- as.matrix(matrix_friends)
matrix_friends <- as.numeric(matrix_friends[, -1])
matrix_friends <- matrix(matrix_friends, nrow=59, ncol=59)


# GRAPH CREATION AND PLOTTING:

#Gender Attribute
gender <- rep(0, length(names))
gender <- matrix(gender, ncol = 1)
gender
rownames(gender) <- names
gender[rownames(gender) %in% males] <- 1
gender
gender <- c(gender)
#gender <- as.data.frame(gender, row.names = NULL)



# From adjacency matrix to graph:
graph_family <- graph_from_adjacency_matrix(matrix_family, mode="directed")
graph_family <- set_vertex_attr(graph_family, 'Gender', index=V(graph_family), gender)

# Renaming nodes and edges names:
V(graph_family)$name = names
E(graph_family)$label = "Family"
E(graph_family)$color = "black"


# Window size:
{windows.options(width=20, height=10)
# Plot size in the window:
dev.new(width=20, height=10, unit="in") # Dimensioni plot in finestra

#only_odd_vertices <- which(V(graph_family)$Gender==0)
#length(only_odd_vertices)

#V(graph_family)$color <- ifelse(V(graph_family)$Gender == 0, "blue", "pink")
#plot(graph_family, layout = layout, vertex.label.dist = 3.5,
#     main = "Social network - with genders as colors")

# Plotting of graph:
# Color=blue if male; Color=pink if female:
pal = c("deeppink", "blue")
plot(graph_family, vertex.size=2, vertex.label.color=pal[as.numeric(as.factor(vertex_attr(graph_family, "Gender")))], edge.arrow.size = 0.2, edge.wigth = 1, edge.label=NA, layout=layout_with_kk)
# Legend of graph edges:
legend(-1.7,1, legend=c("Family"),
       fill = c("black")
)
}


# Same actions for all the other graphs:
graph_ex <- graph_from_adjacency_matrix(matrix_ex, mode="directed")
V(graph_ex)$name = names
E(graph_ex)$label = "Ex"
E(graph_ex)$color = "blue"

graph_ex <- set_vertex_attr(graph_ex, 'Gender', index=V(graph_ex), gender)

# Window size:
{windows.options(width=20, height=10)
# Plot size in the window:
dev.new(width=20, height=10, unit="in") # Dimensioni plot in finestra
# Plotting of graph:
pal = c("deeppink", "blue")
plot(graph_ex, vertex.size=2, vertex.label.color=pal[as.numeric(as.factor(vertex_attr(graph_ex, "Gender")))], edge.arrow.size = 0.2, edge.wigth = 1, edge.label=NA, layout=layout_with_kk)
# Legend of graph edges:
legend(-1.7,1, legend=c("Ex"),
       fill = c("blue")
)
}


graph_couples <- graph_from_adjacency_matrix(matrix_couples, mode="directed")
V(graph_couples)$name = names
E(graph_couples)$label = "Couples"
E(graph_couples)$color = "red"

graph_couples <- set_vertex_attr(graph_couples, 'Gender', index=V(graph_couples), gender)

# Window size:
{windows.options(width=20, height=10)
# Plot size in the window:
dev.new(width=20, height=10, unit="in") # Dimensioni plot in finestra
# Plotting of graph:
pal = c("deeppink", "blue")
set.seed(111)
plot(graph_couples, vertex.size=2, vertex.label.color=pal[as.numeric(as.factor(vertex_attr(graph_couples, "Gender")))], edge.arrow.size = 0.2, edge.wigth = 1, edge.label=NA)
# Legend of graph edges:
legend(-1.7,1, legend=c("Couples"),
       fill = c("red")
)
}


graph_enemies <- graph_from_adjacency_matrix(matrix_enemies, mode="directed")
V(graph_enemies)$name = names
E(graph_enemies)$label = "Enemies"
E(graph_enemies)$color = "green"

graph_enemies <- set_vertex_attr(graph_enemies, 'Gender', index=V(graph_enemies), gender)

# Window size:
{windows.options(width=20, height=10)
# Plot size in the window:
dev.new(width=20, height=10, unit="in") # Dimensioni plot in finestra
# Plotting of graph:
pal = c("deeppink", "blue")
plot(graph_enemies, vertex.size=2, vertex.label.color=pal[as.numeric(as.factor(vertex_attr(graph_enemies, "Gender")))], edge.arrow.size = 0.2, edge.wigth = 1, edge.label=NA, layout=layout_with_kk)
# Legend of graph edges:
legend(-1.7,1, legend=c("Enemies"),
       fill = c("green")
)}

graph_complicated <- graph_from_adjacency_matrix(matrix_complicated, mode="directed")
V(graph_complicated)$name = names
E(graph_complicated)$label = "Complicated"
E(graph_complicated)$color = "purple"

graph_complicated <- set_vertex_attr(graph_complicated, 'Gender', index=V(graph_complicated), gender)

# Window size:
{windows.options(width=20, height=10)
# Plot size in the window:
dev.new(width=20, height=10, unit="in") # Dimensioni plot in finestra
# Plotting of graph:
pal = c("deeppink", "blue")
plot(graph_complicated, vertex.size=2, vertex.label.color=pal[as.numeric(as.factor(vertex_attr(graph_complicated, "Gender")))], edge.arrow.size = 0.2, edge.wigth = 1, edge.label=NA, layout=layout_with_kk)
# Legend of graph edges:
legend(-1.7,1, legend=c("Complicated"),
       fill = c("purple")
)}


graph_friends <- graph_from_adjacency_matrix(matrix_friends, mode="directed")
V(graph_friends)$name = names
E(graph_friends)$label = "Friends"
E(graph_friends)$color = "yellow"

graph_friends <- set_vertex_attr(graph_friends, 'Gender', index=V(graph_friends), gender)

# Window size:
{windows.options(width=20, height=10)
# Plot size in the window:
dev.new(width=20, height=10, unit="in") # Dimensioni plot in finestra
# Plotting of graph:
pal = c("deeppink", "blue")
set.seed(111)
plot(graph_friends, vertex.size=2, vertex.label.color=pal[as.numeric(as.factor(vertex_attr(graph_friends, "Gender")))], edge.arrow.size = 0.2, edge.wigth = 1, edge.label=NA)
# Legend of graph edges:
legend(-1.7,1, legend=c("Friends"),
       fill = c("yellow1")
)
}



# MERGING OF THE VARIOUS GRAPHS:
par(mfrow = c(1, 1))

fusion <- rbind(as_data_frame(graph_enemies), as_data_frame(graph_complicated), as_data_frame(graph_family), as_data_frame(graph_ex), as_data_frame(graph_couples), as_data_frame(graph_friends))
graph_complete  <- graph_from_data_frame(fusion)
#dev.new() edge.label=E(G3b)$at, edge.color=ifelse(E(G3b)$at == "black", "red")
gender_complete <- rep(0, length(names))
gender_complete <- matrix(gender_complete, ncol = 1)
gender_complete
rownames(gender_complete) <- V(graph_complete)$name
gender_complete[rownames(gender_complete) %in% males] <- 1
gender_complete
gender_complete <- c(gender_complete)
graph_complete <- set_vertex_attr(graph_complete, 'Gender', index=V(graph_complete), gender_complete)

# Window size:
{windows.options(width=20, height=10)
# Plot size in the window:
dev.new(width=20, height=10, unit="in") # Dimensioni plot in finestra
# Graph plot:
pal = c("deeppink", "blue")
plot(graph_complete, vertex.size=2, vertex.label.color=pal[as.numeric(as.factor(vertex_attr(graph_complete, "Gender")))], edge.arrow.size = 0.2, edge.wigth = 1, edge.label=NA, layout=layout_with_kk)
# Legend of graph edges:
legend(-1.7,1, legend=c("Family", "Ex", "Couples", "Complicated", "Enemies", "Friends"),
       fill = c("black", "blue", "red", "purple","green", "yellow")
)
}

###### GRAPH ANALYSIS #######

### GRAPH METRICS: ###


# Counting the number of edges:
ecount(graph_family)
ecount(graph_ex)
ecount(graph_couples)

ecount(graph_enemies)   # Very few relationships
ecount(graph_friends)
ecount(graph_complicated) 

ecount(graph_complete)

# GRAPH DENSITY:
graph.density(graph_family) # 1.4% Observed Relation in respect to All Possible relation (Complete graph)
graph.density(graph_ex) # 0.9%
graph.density(graph_couples) # 1.2%
graph.density(graph_complete) # 4%
# Poorly connected networks

# RECIPROCITY:
reciprocity(graph_family) # val=0: Relationships represent parent-son relation, so it is impossible to have mutual relationship
reciprocity(graph_ex) # val=0.7741935: Mutual relationships represent "divorced" penguins
reciprocity(graph_couples)  # val=1: All relations are mutual in this graph
reciprocity(graph_complete)  #val=0.5755396

# TRANSITIVITY:
transitivity(graph_family)  # val=0: Impossible to have triangles, since this graph behaves like a family tree (no cycles)
transitivity(graph_ex)  # val=0
transitivity(graph_couples)  # val=NaN: Impossible to have triangles since we are talking about monogamous couples
transitivity(graph_complete)  # val=0.2171053


# DEGREE CENTRALITY:  (What are the central nodes?)
# Family Graph:
degree(graph_family, mode = 'in')
max(degree(graph_family, mode = 'in'))  #val=4
V(graph_family)$name[degree(graph_family, mode = 'in')==max(degree(graph_family, mode = 'in'))]  #Parents with more sons
degree(graph_family, mode = 'out')
max(degree(graph_family, mode = 'out')) # The value is 2 becouse the max number of parent that a penguin can have is 2
V(graph_family)$name[degree(graph_family, mode = 'out')==max(degree(graph_family, mode = 'out'))] 
degree(graph_family, mode = 'total')

# Ex Graph:
degree(graph_ex, mode = 'in')
max(degree(graph_ex, mode = 'in'))  # val=2
V(graph_ex)$name[degree(graph_ex, mode = 'in')==max(degree(graph_ex, mode = 'in'))]  
degree(graph_ex, mode = 'out')
max(degree(graph_ex, mode = 'out'))  # val=6
V(graph_ex)$name[degree(graph_ex, mode = 'out')==max(degree(graph_ex, mode = 'out'))] 
degree(graph_ex, mode = 'total')

# Couples Graph:
degree(graph_couples, mode = 'in')
max(degree(graph_couples, mode = 'in'))  # val=1
degree(graph_couples, mode = 'out')
max(degree(graph_couples, mode = 'out'))  # val=1
#The maximum value is 1 both as in degree and as out degree because in this graph 
#there are only bidirectional relationships between pairs of penguins.
#A penguin cannot be part of more than one couple
degree(graph_couples, mode = 'total')

# Complete Graph:
degree(graph_complete, mode = 'in')
max(degree(graph_complete, mode = 'in'))  # val=6
V(graph_complete)$name[degree(graph_complete, mode = 'in')==max(degree(graph_complete, mode = 'in'))]
degree(graph_complete, mode = 'out')
max(degree(graph_complete, mode = 'out'))  # val=10
V(graph_complete)$name[degree(graph_complete, mode = 'out')==max(degree(graph_complete, mode = 'out'))]
#Tera returns as a penguin with more relationships even in the complete graph, 
#certainly most of his relationships are parts of the graph of complicated relationships
degree(graph_complete, mode = 'total')
max(degree(graph_complete, mode = 'total'))  # val=14
V(graph_complete)$name[degree(graph_complete, mode = 'total')==max(degree(graph_complete, mode = 'total'))]

# ASSORTATIVITY:
# Family Graph:
clo = closeness(graph_family, normalized = T)
ord = order(clo, decreasing = T)
data.frame(Family = V(graph_family)$name[ord], Closeness = clo[ord])[1:3,]
assortativity(graph_family, V(graph_family)$Gender)  # val=-0.008883013


# Ex Graph:
clo = closeness(graph_ex, normalized = T)
ord = order(clo, decreasing = T)
data.frame(Ex = V(graph_ex)$name[ord], Closeness = clo[ord])[1:3,]
assortativity(graph_ex, V(graph_ex)$Gender)  # val=-0.7659417

# Couples Graph:
clo = closeness(graph_couples, normalized = T)
ord = order(clo, decreasing = T)
data.frame(Couples = V(graph_couples)$name[ord], Closeness = clo[ord])[1:3,]
assortativity(graph_couples, V(graph_couples)$Gender)  # val=-1

# Complete Graph:
clo = closeness(graph_complete, normalized = T)
ord = order(clo, decreasing = T)
data.frame(Complete = V(graph_complete)$name[ord], Closeness = clo[ord])[1:3,]
assortativity(graph_complete, V(graph_complete)$Gender)  # val=-0.516669


# CLOSENESS CENTRALITY

# In-Closeness centrality measures the degree to which a node 
# can be easily reached *from* other nodes 
# (i.e. using edges coming in towards the node) where 
# easily means shortest distance.

closeness(graph_family, mode = 'in') # Nodes that can reach a node x
maxValue = max(closeness(graph_family, mode = 'in'), na.rm = TRUE)  # val=1
V(graph_family)$name[(closeness(graph_family, mode = 'in')==maxValue) %in% TRUE]

clo = closeness(graph_family, normalized = T)
ord = order(clo, decreasing = T)
data.frame(Family = V(graph_family)$name[ord], Closeness = clo[ord])[1:3,]

#Out-Closeness centrality measures the degree to which a node 
# can easily reach other nodes 
# (i.e. using edges out from the node), and easily again 
# means shortest distance.

closeness(graph_family, mode = 'out') # Starting from node x, 
                                      # the nodes that can be reached
maxValue = max(closeness(graph_family, mode = 'out'), na.rm = TRUE) # val = 1
V(graph_family)$name[(closeness(graph_family, mode = 'out')==maxValue) %in% TRUE]
closeness(graph_family, mode = 'total')

# MODE:
# in -> in-closeness centrality
# out -> out-closeness centrality
# total -> in+out (or undirected)


closeness(graph_ex, mode = 'in')
maxValue = max(closeness(graph_ex, mode = 'in'), na.rm = TRUE)
V(graph_ex)$name[(closeness(graph_ex, mode = 'in')==maxValue) %in% TRUE]
closeness(graph_ex, mode = 'out')
maxValue = max(closeness(graph_ex, mode = 'out'), na.rm = TRUE)
V(graph_ex)$name[(closeness(graph_ex, mode = 'out')==maxValue) %in% TRUE]
closeness(graph_ex, mode = 'total')

# Graph Couples:
closeness(graph_couples, mode = 'in')
maxValue = max(closeness(graph_couples, mode = 'in'), na.rm = TRUE)
V(graph_couples)$name[(closeness(graph_couples, mode = 'in')==maxValue) %in% TRUE]
closeness(graph_couples, mode = 'out')
maxValue = max(closeness(graph_couples, mode = 'out'), na.rm = TRUE)
V(graph_couples)$name[(closeness(graph_couples, mode = 'out')==maxValue) %in% TRUE]
closeness(graph_couples, mode = 'total')


# Graph Complete is not interesting:
closeness(graph_complete, mode = 'in')  # Note that there are no isolated nodes in the complete graph
closeness(graph_complete, mode = 'out')
closeness(graph_complete, mode = 'total')


#BETWEENNESS CENTRALITY:
bet_family = betweenness(graph_family, directed = T, normalized = T)

bet_ex = betweenness(graph_ex, directed = T, normalized = T)

bet_couples = betweenness(graph_couples, directed = T, normalized = T)

bet_complete = betweenness(graph_complete, directed = T, normalized = T)

# Comparison of values:
zib = cbind(family=bet_family, ex=bet_ex, couples=bet_couples, complete=bet_complete)
summary(zib)


#EIGENVECTOR CENTRALITY:

eig_family = eigen_centrality(graph_family, scale=F)$vector
eig_ex = eigen_centrality(graph_ex, scale=F)$vector
eig_couples = eigen_centrality(graph_couples, scale=F)$vector
eig_complete = eigen_centrality(graph_complete, scale=F)$vector

zie = cbind(family=eig_family, ex=eig_ex, couples=eig_couples, complete=eig_complete)
summary(zie)


#MODELS ON GRAPHS
#Objective: Describe the dependence between variables

#Gender Attribute

matrix_complete <- get.adjacency(graph_complete, sparse=F)
diag(matrix_complete) = NA
p.MLE = mean(matrix_complete, na.rm = T)
p.MLE

matrix_family <- get.adjacency(graph_family, sparse=F)
diag(matrix_family) = NA
p.MLE = mean(matrix_family, na.rm = T)
p.MLE

matrix_ex <- get.adjacency(graph_ex, sparse=F)
diag(matrix_ex) = NA
p.MLE = mean(matrix_ex, na.rm = T)
p.MLE

matrix_couples <- get.adjacency(graph_couples, sparse=F)
diag(matrix_couples) = NA
p.MLE = mean(matrix_couples, na.rm = T)
p.MLE

library(ergm)

gender <- as.data.frame(gender, row.names = NULL)
gender_complete <- as.data.frame(gender_complete, row.names = NULL)


# -------- mod1: homogeneous binomial random graph model --------
net_complete = network(matrix_complete, directed = T)
net_complete %v% "Gender" = gender_complete$gender_complete

net_family = network(matrix_family, directed = T)
net_family %v% "Gender" = gender$gender

net_ex = network(matrix_ex, directed = T)
net_ex %v% "Gender" = gender$gender

net_couples = network(matrix_couples, directed = T)
net_couples %v% "Gender" = gender$gender

#************************************
# Let's estimate the NULL model: BRG
#************************************

mod0_complete = ergm(net_complete ~ edges)
# look in more depth
summary(mod0_complete)

mod0_family = ergm(net_family ~ edges)
summary(mod0_family)

mod0_ex = ergm(net_ex ~ edges)
summary(mod0_ex)

mod0_couples = ergm(net_couples ~ edges)
summary(mod0_couples)


# -------------------------------------------
# let us move towards the non-homogeneous BRG
# -------------------------------------------

mod1_complete = ergm(net_complete ~ edges + sender + receiver) 
summary(mod1_complete)
# Tutte i coefficienti dei receiver (eccetto quelli posti a -Inf) non sono significativi
# dunque tale modello probabilmente non sar? molto valido (per i sender il discorso ? simile)

mod1_family = ergm(net_family ~ edges + sender + receiver)
summary(mod1_family)
mod1_ex = ergm(net_ex ~ edges + sender + receiver)
summary(mod1_ex)
mod1_couples = ergm(net_couples ~ edges + sender + receiver)
summary(mod1_couples)


BIC(mod0_complete, mod1_complete)  #mod0 better
AIC(mod0_complete, mod1_complete)  #mod0 better

BIC(mod0_family, mod1_family)  #mod0 better
AIC(mod0_family, mod1_family)  #mod1 better

BIC(mod0_ex, mod1_ex) #mod0 better
AIC(mod0_ex, mod1_ex) #mod1 better

BIC(mod0_couples, mod1_couples) #mod0 better
AIC(mod0_couples, mod1_couples) #mod0 better

# -----------------------------------------------
# let us now consider the dyad independence model
# -----------------------------------------------

#Impossibile eseguire i modelli mod2 e mod3 anche su reti più piccole


mod3_complete = ergm(net_complete ~ edges + mutual, control = control.ergm(seed = 10))
summary(mod3_complete)
mod3_family= ergm(net_family ~ edges + mutual, control = control.ergm(seed = 10))  
summary(mod3_family) # -Inf a mutual perch? non vi sono relazioni bidirezionali
mod3_ex = ergm(net_ex ~ edges + mutual, control = control.ergm(seed = 10)) 
summary(mod3_ex)
mod3_couples = ergm(net_couples ~ edges + mutual, control = control.ergm(seed = 10))
summary(mod3_couples) #Unable to reach target effective size in iterations alotted.

BIC(mod0_complete, mod3_complete)   # mod3 better
AIC(mod0_complete, mod3_complete)   # mod3 better

BIC(mod0_family, mod3_family)    # mod0 better
AIC(mod0_family, mod3_family)    # mod0 better
# Mutual coefficient is -Inf, so AIC and BIC are not computable
# This is becouse there are no mutual attribute in net_family

BIC(mod0_ex, mod3_ex)    # mod3 better
AIC(mod0_ex, mod3_ex)    # mod3 better


mod4_complete = ergm(net_complete ~ edges + mutual + nodefactor("Gender") + nodematch("Gender"), control = control.ergm(seed = 10)) 
summary(mod4_complete)
mod4_family = ergm(net_family ~ edges + nodefactor("Gender") + nodematch("Gender"), control = control.ergm(seed = 10)) 
summary(mod4_family)
mod4_ex = ergm(net_ex ~ edges + mutual + nodefactor("Gender") + nodematch("Gender"), control = control.ergm(seed = 10)) 
summary(mod4_ex)
mod4_couples = ergm(net_couples ~ edges + nodefactor("Gender") + nodematch("Gender"), control = control.ergm(seed = 10)) 
summary(mod4_couples) #No homosexual relation

mod5_complete = ergm(net_complete ~ edges + mutual + nodematch("Gender"), control = control.ergm(seed = 10)) 
summary(mod5_complete)


BIC(mod3_complete, mod4_complete, mod5_complete)   # mod5 better
AIC(mod3_complete, mod4_complete, mod5_complete)   # mod5 better

BIC(mod4_family, mod0_family)    # mod0 better
AIC(mod4_family, mod0_family)    # mod0 better

BIC(mod4_ex, mod3_ex)    # mod3 better
AIC(mod4_ex, mod3_ex)    # mod4 better


mod8_complete = ergm(net_complete ~ edges + mutual + nodematch("Gender") +
               gwdsp(decay = 1,fixed = T), control = control.ergm(seed=10))
summary(mod8_complete)
mod8_family = ergm(net_family ~ edges +
               gwdsp(decay = 1,fixed = T), control = control.ergm(seed=10))
summary(mod8_family)
mod8_ex = ergm(net_ex ~ edges + mutual +
               gwdsp(decay = 1,fixed = T), control = control.ergm(seed=10))
summary(mod8_ex)
mod8_couples = ergm(net_couples ~ edges +
               gwdsp(decay = 1,fixed = T), control = control.ergm(seed=10))
summary(mod8_couples)

BIC(mod5_complete, mod8_complete)   # mod8 better
AIC(mod5_complete, mod8_complete)   # mod8 better

BIC(mod0_family, mod8_family)    # mod8 better
AIC(mod0_family, mod8_family)    # mod8 better

BIC(mod0_couples, mod8_couples)    # mod8 better
AIC(mod0_couples, mod8_couples)    # mod8 better

BIC(mod4_couples, mod8_couples)   # mod4 better
AIC(mod4_couples, mod8_couples)   # mod4 better

BIC(mod3_ex, mod4_ex, mod8_ex)    # mod3 better #######
AIC(mod3_ex, mod4_ex, mod8_ex)    # mod4 better

mcmc.diagnostics(mod8_complete)
mcmc.diagnostics(mod8_family)  # significativo e negativo
mcmc.diagnostics(mod8_couples)  # brutto perchè non ci posso essere clusters nelle coppie
mcmc.diagnostics(mod4_ex)  
mcmc.diagnostics(mod3_ex)


library(intergraph)

fnc = function(xx){
  ig = asIgraph(xx)
  rec = reciprocity(ig)
  tr = transitivity(ig)
  ideg = sd(degree(ig, mode = "in"))
  odeg = sd(degree(ig, mode = "out"))
  return(c(tr, ideg, odeg, rec))
}

evaluateMetrics = function(mod, graph){
  
  sim = simulate(mod, control = control.simulate.ergm(MCMC.burnin = 1000), nsim = 100, verbose = TRUE, seed = 10)

  null.distr = matrix(,100,4)
  for(b in 1:100){
    null.distr[b,]  = fnc(sim[[b]])
  }
  dev.new()
  par(mfrow = c(2,2))
  
  
  low = pmin(min(null.distr[,4]), reciprocity(graph)) - 0.05
  up = pmax(max(null.distr[,4]), reciprocity(graph)) + 0.05
  hist(unlist(null.distr[,4]), xlab = "reciprocity", main = paste("Histograms for", mod$call),  xlim = c(low, up)); abline(v = reciprocity(graph), col = "red")
  
  low = pmin(min(null.distr[,2]), sd(degree(graph, mode = "in"))) - 0.05
  up = pmax(max(null.distr[,2]), sd(degree(graph, mode = "in"))) + 0.05
  hist(unlist(null.distr[,2]), xlab = "in-degree",  xlim = c(low, up)); abline(v = sd(degree(graph, mode = "in")), col = "red")
  
  low = pmin(min(null.distr[,3]), sd(degree(graph, mode = "out"))) - 0.05
  up = pmax(max(null.distr[,3]), sd(degree(graph, mode = "out"))) + 0.05
  hist(unlist(null.distr[,3]), xlab = "out-degree", xlim = c(low, up)); abline(v = sd(degree(graph, mode = "out")), col = "red")
  
  low = pmin(min(null.distr[,1]), transitivity(graph)) - 0.05
  up = pmax(max(null.distr[,1]), transitivity(graph)) + 0.05
  hist(unlist(null.distr[,1]), xlab = "transitivity", xlim = c(low, up)); abline(v = transitivity(graph), col = "red")
  
  
}

evaluateMetrics(mod8_complete, graph_complete)
evaluateMetrics(mod8_family, graph_family)
evaluateMetrics(mod8_couples, graph_couples)
evaluateMetrics(mod4_ex, graph_ex)
evaluateMetrics(mod3_ex, graph_ex)

evaluateMetrics(mod5_complete, graph_complete)
evaluateMetrics(mod0_family, graph_family)
evaluateMetrics(mod0_couples, graph_couples)

# BLOCK MODELS:
library(sbm)
library(ggplot2)

matrix_complete = as_adjacency_matrix(graph_complete, names = FALSE)
matrix_complete = as.matrix(matrix_complete, nrow=59, ncol=59)

# plotMyMatrix consente di stampare la matrice di adiacenza
# nel caso in cui si abbiano valori binari rappresenta col 
# nero il valore 1 e col bianco lo 0
# ??????????????????????
# Graph family:


plotMyMatrix(matrix_family, dimLabels = list(row = 'Penguins', col = 'Penguins'), plotOptions= list(title='Family'))

# Graph ex:
plotMyMatrix(matrix_ex, dimLabels = list(row = 'Penguins', col = 'Penguins'), plotOptions= list(title='Ex'))

# Graph couples:
plotMyMatrix(matrix_couples, dimLabels = list(row = 'Penguins', col = 'Penguins'), plotOptions= list(title='Couples'))

# Graph enemies: # not considered
plotMyMatrix(matrix_enemies, dimLabels = list(row = 'Penguins', col = 'Penguins'), , plotOptions= list(title='Enemies'))

# Graph complicated: # not considered
plotMyMatrix(matrix_complicated, dimLabels = list(row = 'Penguins', col = 'Penguins'), plotOptions= list(title='Complicated'))

# Graph friends: # not considered
plotMyMatrix(matrix_friends, dimLabels = list(row = 'Penguins', col = 'Penguins'), plotOptions= list(title='Friends'))

# Graph complete:
plotMyMatrix(matrix_complete, dimLabels = list(row = 'Penguins', col = 'Penguins'), plotOptions= list(title='Complete'))

# Family:
sbm_family = estimateSimpleSBM(matrix_family, "bernoulli", dimLabels = 'Penguins', 
                         estimOptions = list(verbosity = 1))
sbm_family

# Ex:
sbm_ex = estimateSimpleSBM(matrix_ex, "bernoulli", dimLabels = 'Penguins', 
                               estimOptions = list(verbosity = 1))
sbm_ex

# Couples:
sbm_couples = estimateSimpleSBM(matrix_couples, "bernoulli", dimLabels = 'Penguins', 
                           estimOptions = list(verbosity = 1))
sbm_couples

# Complete:
sbm_complete = estimateSimpleSBM(matrix_complete, "bernoulli", dimLabels = 'Penguins', 
                                estimOptions = list(verbosity = 1))
sbm_complete


# Family:
# selected number of blocks
sbm_family$nbBlocks
# prior block probabilities
sbm_family$blockProp
# connectivity parameters
round(sbm_family$connectParam$mean,3)

# Ex:
# selected number of blocks
sbm_ex$nbBlocks
# prior block probabilities
sbm_ex$blockProp
# connectivity parameters
round(sbm_ex$connectParam$mean,3)


# Couples:
# selected number of blocks
sbm_couples$nbBlocks
# prior block probabilities
sbm_couples$blockProp
# connectivity parameters
round(sbm_couples$connectParam$mean,3)


# Complete:
# selected number of blocks
sbm_complete$nbBlocks
# prior block probabilities
sbm_complete$blockProp
# connectivity parameters
round(sbm_complete$connectParam$mean,3)


# Family:
plot(sbm_family, type = "data")
# nodes are ordered wrt to the block they belong to and blocks are highlighted
plot(sbm_family, type = "expected")
# fitted connection probabilities
plot(sbm_family, type = "meso")
# fitted connection probabilities


# Ex:
plot(sbm_ex, type = "data")
# nodes are ordered wrt to the block they belong to and blocks are highlighted
plot(sbm_ex, type = "expected")
# fitted connection probabilities
plot(sbm_ex, type = "meso")
# fitted connection probabilities


# Couples:
plot(sbm_couples, type = "data")
# nodes are ordered wrt to the block they belong to and blocks are highlighted
plot(sbm_couples, type = "expected")
# fitted connection probabilities
plot(sbm_couples, type = "meso")
# fitted connection probabilities


# Complete:
plot(sbm_complete, type = "data")
# nodes are ordered wrt to the block they belong to and blocks are highlighted
plot(sbm_complete, type = "expected")
# fitted connection probabilities
plot(sbm_complete, type = "meso")
# fitted connection probabilities


# Family:
sbm_family$storedModels
sbm_family$setModel(2)
sbm_family$memberships

# Ex:
sbm_ex$storedModels
sbm_ex$setModel(3)
sbm_ex$memberships

# Couples:
sbm_couples$storedModels
sbm_couples$memberships

# Complete:
sbm_complete$storedModels
sbm_complete$setModel(2)
sbm_complete$memberships

# Family:
sbm_family$nbBlocks
# prior block probabilities
sbm_family$blockProp
# connectivity parameters
round(sbm_family$connectParam$mean,3)

# Ex:
sbm_ex$nbBlocks
# prior block probabilities
sbm_ex$blockProp
# connectivity parameters
round(sbm_ex$connectParam$mean,3)


# Complete:
sbm_complete$nbBlocks
# prior block probabilities
sbm_complete$blockProp
# connectivity parameters
round(sbm_complete$connectParam$mean,3)


# Friends:
# Let us graphically represent the data matrix 
# reordering rows and cols according to the estimated block in the SBM
plot(sbm_family, type = "data", dimLabels = list(row = 'Penguins', col= 'Penguins'))

# or the average number of connections between trees
plot(sbm_family, type = "expected", dimLabels = list(row = 'Penguins', col= 'Penguins'))

# or 
plot(sbm_family, type = "meso", dimLabels = list(row = 'Penguins', col= 'Penguins'))
#Cluster pi? connessi con frecce pi? marcate


# Ex:
# Let us graphically represent the data matrix 
# reordering rows and cols according to the estimated block in the SBM
plot(sbm_ex, type = "data", dimLabels = list(row = 'Penguins', col= 'Penguins'))

# or the average number of connections between trees
plot(sbm_ex, type = "expected", dimLabels = list(row = 'Penguins', col= 'Penguins'))

# or 
plot(sbm_ex, type = "meso", dimLabels = list(row = 'Penguins', col= 'Penguins'))


# Complete:
# Let us graphically represent the data matrix 
# reordering rows and cols according to the estimated block in the SBM
plot(sbm_complete, type = "data", dimLabels = list(row = 'Penguins', col= 'Penguins'))

# or the average number of connections between trees
plot(sbm_complete, type = "expected", dimLabels = list(row = 'Penguins', col= 'Penguins'))

# or 
plot(sbm_complete, type = "meso", dimLabels = list(row = 'Penguins', col= 'Penguins'))
