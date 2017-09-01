install.packages("vegan")
library("vegan")

vegan <- read.csv(file.choose())

set.seed(2)
community_matrix=matrix(
  sample(1:100,300,replace=T),nrow=10,
  dimnames=list(paste("community",1:10,sep=""),paste("sp",1:30,sep="")))

head(community_matrix)
head(vegan)
example_NMDS=metaMDS(vegan, # Our community-by-species matrix
                     k=2) # The number of reduced dimensions
example_NMDS=metaMDS(community_matrix,k=2,trymax=100) 
stressplot(example_NMDS)


plot(example_NMDS)

ordiplot(example_NMDS,type="n")
orditorp(example_NMDS,display="species",col="red",air=0.01)
orditorp(example_NMDS,display="sites",cex=1.25,air=0.01)

treat=c(rep("Treatment1",5),rep("Treatment2",5))
ordiplot(example_NMDS,type="n")
ordihull(example_NMDS,groups=treat,draw="polygon",col="grey90",label=F)
orditorp(example_NMDS,display="species",col="red",air=0.01)
orditorp(example_NMDS,display="sites",col=c(rep("green",5),rep("blue",5)),
         air=0.01,cex=1.25)
