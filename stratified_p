stratified_p<-function(data_sam,str_props,str_sizes,str_names,asig="Neyman",error=0.02,alpha=0.05){
require(dplyr)

Nh=data_sam[,str_sizes] 
Wh=data_sam[,str_sizes]/sum(data_sam[,str_sizes],na.rm = T) 
ph=data_sam[,str_props]  
names=data_sam[,str_names] 

n_0=sum(Wh^2*ph*(1-ph))/((error/qnorm(alpha/2))^2)

###NEYMAN ASSIGNATION
wh_ney=Wh*sqrt(Nh*ph*(1-ph)/(Nh-1))/sum(Wh*sqrt(Nh*ph*(1-ph)/(Nh-1)))



print(paste("Sample Size of",round(n_0)))
res=data.frame(stratum=names,n_prop=n_0*Wh,n_ney=n_0*wh_ney,stringsAsFactors = F)
colnames(res)=c("stratum","n_prop","n_ney")
return(res)

}
  
