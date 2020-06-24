awareness_nodes<-matrix(,nrow=population,ncol=M)
beta_nodes<-matrix(,nrow=population,ncol=M)
for (i in 1:M){
  for(j in 1:population){
    awareness_nodes[j,i]=((strenght_nodo[j,i])/(1+strenght_nodo[j,i]))*lambda_in
    beta_nodes[j,i]=(1/(1+awareness_nodes[j,i]))*(beta_in*inversepart_nodo[j,i])
  }
}

n_step=100
n_simulations=10

p_IA<-c()
p_SU<-c()
p_SA<-c()

p_EU<-c()
p_EA<-c()
p_EF<-c()

p_SF<-c()
p_RA<-c()
p_RF<-c()

value_medium<-c()
sum_infected=0


pi= 0.8
pa= 0.4


pia=0.8
psu=0.4
psa=0.2
peu=0.5

rho_I<-c()
x<-c()
y<-c()


matrix_replica_rho<-list()
matrix_replica<-c()
matrix_rho<-matrix(,nrow=n_step,ncol=population)



for (t in 1:n_step)
{
  for (z in 1:n_simulations)
    
  {
    q_i<-c()
    r_i<-c()
    out1<-c()
    out2<-c()
    prod1<-list()
    prod2<-list()
    prod1=matrix(,nrow = population, ncol = population)
    prod2=matrix(,nrow = population, ncol = population)
    
    for (j in 1:population)
    {
      for(k in 1:population)
      {
        
        prod1[j,k]<-c(1-(multiplex_weights[[1]][j,k])*(pi)*(beta_nodes[k,1]))
        prod2[j,k]<-c(1-(multiplex_weights[[1]][j,k])*(pa)*(awareness_nodes[k,1]))
        
      }
      out1[j]<-prod(prod1[j,])
      out2[j]<-prod(prod2[j,])
      
    }
    
    for(k in 1:population)
    {
      
      q_i[k]=(1-beta_nodes[k,1])*out1
      r_i[k]=(1-awareness_nodes[k,1])*out2
    }
    
    for(j in 1:population){
      if (z==1){
        
        p_SA[j] = (q_i[j])*(psa)+(1-r_i[j])*(1-delta)*(psu)
        p_SU[j]=(r_i[j])*(psu)
        p_SF[j]= delta*(1-r_i[j])*(psu)
        p_IA[j]= gamma_exp*(1-q_i[j])*(1-miu)*(psa)+(1-miu)*(pia)
        p_EU[j]= (r_i[j])*(peu)
        p_EA[j]= (1-delta)*(1-r_i[j])*(peu)
        p_EF[j]= delta*(1-r_i[j])*(peu)
        p_RA[j]=miu*(1-delta)*(pia)+gamma_exp*miu*(1-q_i[j])*(1-delta)*(psa)
        p_RF[j]=miu*delta*(pia)+miu*delta*gamma_exp*(1-q_i[j])*(psa)
      } 
      
      else{
        
        p_SA[j] = (q_i[j])*(p_SA[j])+(1-r_i[j])*(1-delta)*(p_SU[j])
        p_SU[j]=(r_i[j])*(p_SU[j])
        p_SF[j]= delta*(1-r_i[j])*(p_SU[j])
        p_IA[j]= gamma_exp*(1-q_i[j])*(1-miu)*(p_SA[j])+(1-miu)*(p_IA[j])
        p_EU[j]= (r_i[j])*(p_EU[j])
        p_EA[j]= (1-delta)*(1-r_i[j])*(p_EU[j])
        p_EF[j]= delta*(1-r_i[j])*(p_EU[j])
        p_RA[j]=miu*(1-delta)*(p_IA[j])+gamma_exp*miu*(1-q_i[j])*(1-delta)*(p_SA[j])
        p_RF[j]=miu*delta*(p_IA[j])+miu*delta*gamma_exp*(1-q_i[j])*(p_SA[j])
        
      }
      
      
    }
    
    
    sum_infected = sum(p_IA[[t]])
    rho_I[z]= (sum_infected)
    
  }
  
  value_medium[t]=rho_I[t]
  matrix_rho[t,]=matrix(value_medium[t])
  
}


matrix_critical<-matrix( , nrow = population, ncol=population)
matrix_critical_stat<-matrix( , nrow = population, ncol=population)
eigenvalues<-c()
eigenvalues_stat<-c()

p_SA=p_SA/max(p_SA)

adjacency_or= matrix(,nrow = population, ncol = population)
for (j in 1:population)
{ 
  for(k in 1:population)
  { 
    
    if (multiplex_weights[[1]][k]!=0 | multiplex_weights[[2]][k]!=0 )
    { adjacency_or[j,k]= 1}
    else {adjacency_or[j,k]= 0}
  }
}

for(i in 1:population)
{
  for(j in 1:population)
  {
    matrix_critical[i,j]= (beta_nodes[i,2])*(1-miu)*gamma_exp*(p_SA[i])*adjacency_or[i,j]
    matrix_critical_stat[i,j]= (beta_in)*(1-miu)*gamma_exp*(p_SA[i])*adjacency_or[i,j]
  }
}
eigenvalues = eigen(matrix_critical)$values
denominator_critical=max((abs(Re(eigenvalues))))
point_criticall=(miu/denominator_critical)

eigenvalues_stat = eigen(matrix_critical_stat)$values
denominator_critical_stat=max((abs(Re(eigenvalues_stat))))
point_critical_stat=(miu/denominator_critical_stat)



########################scatter_plot3d##############################
for(j in 1:n_step)
{
  matrix_replica_rho[[j]]=rep(matrix_rho[j,1], times=population)
}

rate_infection<-c()
rate_awareness<-c()

for(t in 1:n_step){
  rate_infection<-c(c(rate_infection,beta_nodes[,1]))
  rate_awareness<-c(c(rate_awareness, awareness_nodes[,1]/max(awareness_nodes[,1])))
  matrix_replica<-c(c(matrix_replica, matrix_replica_rho[[t]]))
}

rate_inf_crit<-c()
rate_inf_crit=rep(point_critical, times=population*n_step)
rate_inf_crit_stat=rep(point_critical_stat, times=population*n_step)


z<-c()

rate_infection=rate_infection/max(rate_infection)
x<-rate_awareness
y<-rate_infection
z<-matrix_replica 

data_beta_lambda<-data.frame(x,y,z)
data_beta_lambda2<-data.matrix(data_beta_lambda)
figrho<-plot_ly(x=x,y=y,  z=z, color = z, colors= "RdBu", type ="scatter3d", mode= "markers", showlegend= FALSE) 
add_trace(x=x, y=rate_inf_crit, z=z,  type ="scatter3d",  mode= "markers", showlegend = FALSE)
add_trace(x=x, y=rate_inf_crit_stat, z=z, type ="scatter3d", mode= "markers", showlegend = FALSE)
