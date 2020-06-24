library(igraph)
library(foreach)
library(Matrix)
library(RColorBrewer)
library(ggplot2)
library(markovchain)
library(plot3D)
library(plotly)
library(base)
require(fields)
require(ggplot2)
require(netdiffuseR)

n_step=10;
n_simulations=10;
population=1461;
M=2;

beta_in=0.8;
lambda_in=0.4;
delta=0.8; 
miu=0.2;
gamma_exp=0.5;  

standard_deviation_awareness=2; 
standard_deviation_homophily=4; #deviazione standard per il calcolo dell'homophily

reshuffle = TRUE
multiplex_adjacencies <- list()  
graphs <- list()
layout.old=NULL;

homophily_node<-c()
awareness_nodes<-c()
awareness_nodes_data<-c()
degree_nodes<-list() 
awareness_nodes_i<-c()
beta_nodes_i<-c()
k_nodes_i<-c()
list_nodes<- list()
list_nodes_aw<-list()
multiplex_omofilie<-list()
multiplex_product<-list()
multiplex_weights<-list()
multiplex_weights_mod<-list()
list_nodes_aw_data<-c()



graph_layer<- read.csv("/path.../Edgelist_users.csv", header = TRUE)
graph_layer1<-graph_layer %>% select(1, 2)
graph_layer1<-edgelist_to_adjmat(graph_layer1,
                                 simplify = TRUE,
                                 undirected = TRUE,
                                 self = FALSE,
                                 multiple =FALSE,
                                 keep.isolates = FALSE,
                                 recode.ids = TRUE)

matrix_adjacencies <-graph_layer1
matrix<-matrix(matrix_adjacencies,nrow=1461,ncol=1461)

if (reshuffle)
{
  nr<-dim(matrix)[1]
  samp=sample.int(nr)
  matrix=matrix[samp,samp]
}

graphs[[1]]=graph.adjacency(matrix, mode="undirected", weighted= NULL)

#if (i==1)
#{
layout.old<-layout.fruchterman.reingold(graphs[[1]], params=list(weights=E(graphs[[1]]) $weight))
#}

multiplex_adjacencies[[1]] = matrix
list_nodes[[1]] = data.frame(id_node = 1:population,  
                              color=(rnorm(population, mean=0, sd=standard_deviation_homophily)))
list_nodes_aw[[1]] = data.frame(id_node = 1:population,  
                                   color=(rnorm(population, mean=0, sd=standard_deviation_awareness)))
multiplex_omofilie[[1]] = matrix(, nrow = population, ncol = population)
multiplex_weights[[1]]=matrix(, nrow = population, ncol = population)
multiplex_weights_mod[[1]]=matrix(, nrow = population, ncol = population)
multiplex_product[[1]]=matrix(, nrow = population, ncol = population)
degree_nodes[[1]] = degree (graphs[[1]], v=V(graphs[[1]]), mode = c("all","out","in","total"), loops = TRUE,normalized = FALSE)



#second layer
graph_layer<-barabasi.game(population, power=1, m=NULL, out.dist = NULL, out.seq = NULL, out.pref = FALSE, zero.appeal = 1, directed = FALSE,
                           algorithm = c("psumtree","psumtree-multiple","bag"),start.graph = NULL)
matrix_adjacencies <-get.adjacency(graph_layer, type="both");
matrix<-matrix(matrix_adjacencies,nrow=population,ncol=population)
if (reshuffle)
{
  nr<-dim(matrix)[1]
  samp=sample.int(nr)
  matrix=matrix[samp,samp]
}

graphs[[2]]=graph.adjacency(matrix, mode="undirected", weighted= NULL)


layout.old<-layout.fruchterman.reingold(graphs[[2]], params=list(weights=E(graphs[[2]]) $weight))


multiplex_adjacencies[[2]] = matrix
list_nodes[[2]] = data.frame(id_node = 1:population,  
                              color=(rnorm(population, mean=0, sd=standard_deviation_homophily)))
list_nodes_aw[[2]] = list_nodes_aw[[1]]
multiplex_omofilie[[2]] = matrix(, nrow = population, ncol = population)
multiplex_weights[[2]]=matrix(, nrow = population, ncol = population)
multiplex_weights_mod[[2]]=matrix(, nrow = population, ncol = population)
multiplex_product[[2]]=matrix(, nrow = population, ncol = population)
degree_nodes[[2]] = degree (graph_layer, v=V(graph_layer), mode = c("all","out","in","total"), loops = TRUE,normalized = FALSE)

