library(plotly)
####awareness data-driven####
awareness_nodes_data<-c()
overlapping_degree_hashtag<-c()
coeff_part<-c() 

data_for_aw<- read.csv("/path.../degree_states.csv", header = TRUE)

overlapping_degree<-c()
for(j in 1:population){
  
  overlapping_degree[j]=degree_nodes[[1]][j]+degree_nodes[[2]][j]
}

for(j in 1:population)
{
  
  coeff_part[j] =(2*(1-((degree_nodes[[1]][j]/overlapping_degree[j])^2+(degree_nodes[[2]][j]/overlapping_degree[j])^2)))
}

overlapping_degree_hashtag<-data_for_aw$sum
entropy<-c()
for (i in 1:65){
  
  entropy[i]<- (data_for_aw$Degree.x/overlapping_degree_hashtag[i]) * log(data_for_aw$Degree.x/overlapping_degree_hashtag[i])
  + (data_for_aw$Degree.y/overlapping_degree_hashtag[i]) * log(data_for_aw$Degree.y/overlapping_degree_hashtag[i])
  + (data_for_aw$Degree.x.x/overlapping_degree_hashtag[i]) * log(data_for_aw$Degree.x.x/overlapping_degree_hashtag[i])
  + (data_for_aw$Degree.x.x.x/overlapping_degree_hashtag[i]) * log(data_for_aw$Degree.x.x.x/overlapping_degree_hashtag[i]) 
  + (data_for_aw$Degree.y.y/overlapping_degree_hashtag[i]) * log(data_for_aw$Degree.y.y/overlapping_degree_hashtag[i])
  + (data_for_aw$Degree.y.y.y/overlapping_degree_hashtag[i]) * log(data_for_aw$Degree.y.y.y/overlapping_degree_hashtag[i])
  +  (data_for_aw$Degree/overlapping_degree_hashtag[i]) * log(data_for_aw$Degree/overlapping_degree_hashtag[i]) 
  
}


entropy_sum<-sum(entropy)

list_nodes_aw_data<-c()

for (i in 1:population){
  
  list_nodes_aw_data[i]= (coeff_part[i]*entropy_sum)
  
}



list_nodes_aw_data<-list_nodes_aw_data/max(list_nodes_aw_data)

for(i in 1:M){
  for(j in 1:population)
  {
    homophily_node[j] = list_nodes[[i]]['color'][[1]][j]
    awareness_nodes_data[j]= list_nodes_aw_data[j]
    for(k in 1:population)
    {
      if(j==k){
        multiplex_product[[i]][j,k] = 1
        multiplex_weights[[i]][j,k]=1
      } else {
        homophily_node[k] = list_nodes[[i]]['color'][[1]][k]
        awareness_nodes_data[k]= list_nodes_aw_data[k]
        multiplex_product[[i]][j,k] = ((1/(1 + abs(homophily_node[j] - homophily_node[k])))*(abs(awareness_nodes_data[j] - awareness_nodes_data[k])))
        
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
list_nodes_aw[[1]]$color<-list_nodes_aw_data

sizeaw<-c()

for(j in 1:population)
{ sizeaw[j]=4*abs(list_nodes_aw[[1]]['color'][[1]][j])
}
p1<-plot.igraph(graphs[[1]], vertex.label=NA, layout= layout.sphere,vertex.color='pink', vertex.size=sizeaw, edge.width=E(graphs[[1]])$weight, edge.color="black")
p2<-plot.igraph(graphs[[2]], vertex.label=NA, layout=layout.fruchterman.reingold ,vertex.color='pink', vertex.size=sizeaw, edge.width=E(graphs[[2]])$weight, edge.color="black")




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
  { size[j]=strength_node[j,i]*4
  }
  graphs[[i]]<-set.vertex.attribute(graphs[[i]], "aw",index=V(graphs[[i]]),y)
  get.vertex.attribute(graphs[[i]], "aw")
  vcol<-heat.colors(population, alpha = 1)
  
  V(graphs[[i]])$aw<-vcol[order(vcol,decreasing = TRUE)]
  V(graphs[[i]])$color<-V(graphs[[i]])$aw
  
  p2<-plot.igraph(graphs[[i]], vertex.label=NA, layout=layout_on_sphere,vertex.color=V(graphs[[i]])$color, vertex.size=size,edge.width=E(graphs[[i]])$weight)
  p2<-plot.igraph(graphs[[i]], vertex.label=NA, layout=layout.fruchterman.reingold,vertex.color=V(graphs[[i]])$color, vertex.size=size,edge.width=E(graphs[[i]])$weight)
  colorstrip(heat.colors(population, alpha = 1))
}


datasetforplotaw<- data.frame(list_nodes_aw_data,coeff_part,overlapping_degree, strength_node, degree_nodes)
datasetforplotaw<- datasetforplotaw %>% 
  mutate(zscore = (overlapping_degree - mean(overlapping_degree))/sd(overlapping_degree))




###

fig <- plot_ly(data = datasetforplotaw, x = ~coeff_part, y = ~zscore, name = 'trace 0',
               marker = list(size = 10*list_nodes_aw[[1]]$color,
                             color = 'black',
                             line = list(color = 'black',
                                         width = 1)))


ax <- list(
  zeroline = TRUE,
  showline = TRUE,
  mirror = "ticks",
  gridcolor = toRGB("gray50"),
  gridwidth = 2,
  zeroline=FALSE,
  linecolor = toRGB("black"),
  linewidth = 6
)


fig <- fig %>% layout(gridcolor = 'black',
                      yaxis=ax,
                      xaxis=ax)
fig



fig <- plot_ly(data = datasetforplotaw, x = ~list_nodes_aw[[1]]$color, y = ~coeff_part,
               marker = list(size = overlapping_degree,
                             color = 'rgba(195, 152, 123, .12)',
                             line = list(color = 'rgba(152, 0, 0, .8)',
                                         width = 1)))

fig <- fig %>% layout(title = '  Multiplex Participation coefficient versus 
                      the Z-score of the total overlapping degree',
                      yaxis = list(zeroline = FALSE),
                      xaxis = list(zeroline = FALSE))

fig



fig <- plot_ly(data = datasetforplotaw, x = ~list_nodes_aw_data, y = ~degree_nodes, color = ~overlapping_degree)

fig



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
  
  p2<-plot.igraph(graphs[[i]], vertex.label=NA, layout=layout_on_sphere,vertex.color=V(graphs[[i]])$color, vertex.size=size,edge.width=E(graphs[[i]])$weight)
  colorstrip(heat.colors(population, alpha = 1))
}


