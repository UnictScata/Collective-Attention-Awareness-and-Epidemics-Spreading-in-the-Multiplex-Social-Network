####baseline####
for(i in 1:M){
  for(j in 1:population)
  {
    homophily_node[j] = list_nodes[[i]]['color'][[1]][j]
    awareness_nodes[j]= list_nodes_aw[[1]]['color'][[1]][j]
    for(k in 1:population)
    {
      if(j==k){
        multiplex_product[[i]][j,k] = 1
        multiplex_weights[[i]][j,k]=1
      } else {
        homophily_node[k] = list_nodes[[i]]['color'][[1]][k]
        awareness_nodes[k]= list_nodes_aw[[1]]['color'][[1]][k]
        multiplex_product[[i]][j,k] = ((1/(1 + abs(homophily_node[j] - homophily_node[k])))*(abs(awareness_nodes[j] - awareness_nodes[k])))
         multiplex_weights[[i]][j,k] = (multiplex_product[[i]][j,k]) * (multiplex_adjacencies[[i]][j,k])
      }
    }
  }
}



strength_node<-matrix(,nrow=population,ncol=M)
inversepart_node<-matrix(,nrow=population,ncol=M)

for (i in 1:M){
  multiplex_adjacencies[[i]]=multiplex_weights[[i]]
  graphs[[i]]=graph.adjacency(multiplex_adjacencies[[i]], mode="undirected", weighted= TRUE,diag = FALSE)
  for (j in 1:population){
    strength_node[j,i]=sum(multiplex_adjacencies[[i]][j,])
    inversepart_node[j,i]=sum((multiplex_adjacencies[[i]][j,]/strength_node[j,i])^2)
  }
}


#######################################plot multiplex weighted network################################
sizeaw<-c()

for (i in 1:M)
{
  for(j in 1:population)
  { sizeaw[j]=2*abs(list_nodes_aw[[1]]['color'][[1]][j])
  
  
  }
  p1<-plot.igraph(graphs[[1]], vertex.label=NA, layout= layout.sphere,vertex.color='pink', vertex.size=sizeaw, edge.width=E(graphs[[1]])$weight, edge.color="black")
  p2<-plot.igraph(graphs[[2]], vertex.label=NA, layout=layout.fruchterman.reingold ,vertex.color='pink', vertex.size=sizeaw, edge.width=E(graphs[[2]])$weight, edge.color="black")
  
}



#######################################plot multiplex weighted network################################

x<-c()
x=list_nodes_aw[[1]]$color
y=x[order(x)]

colorstrip <- function(colors) {
  count <- length(colors)
  m <- matrix(1:count, count, 1)
  par(mai=c(0.1, 2, 0.8, 4), cex.axis=2, ann=T, tck=-1)
  image(m, col=colors, ylab="", axes=FALSE)
}

vcol<-c()
size<-c()
for(i in 1:M)
{
  for(j in 1:population)
  { size[j]=strength_node[j,i]
  }
  graphs[[i]]<-set.vertex.attribute(graphs[[i]], "aw",index=V(graphs[[i]]),y)
  get.vertex.attribute(graphs[[i]], "aw")
  vcol<-heat.colors(population, alpha = 1)
  
  V(graphs[[i]])$aw<-vcol[order(vcol,decreasing = TRUE)]
  V(graphs[[i]])$color<-V(graphs[[i]])$aw
  
  p3<-plot.igraph(graphs[[1]], vertex.label=NA, layout=layout_on_sphere,vertex.color=V(graphs[[1]])$color, vertex.size=size,edge.width=E(graphs[[1]])$weight)
  p4<-plot.igraph(graphs[[2]], vertex.label=NA, layout=layout.fruchterman.reingold,vertex.color=V(graphs[[2]])$color, vertex.size=size,edge.width=E(graphs[[2]])$weight)
  colorstrip(heat.colors(population, alpha = 1))
}

