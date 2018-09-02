library(igraph)
library(ggplot2)
library(dplyr)
#common_graph <- barabasi.game(50,2,directed = FALSE)
#common_graph<-erdos.renyi.game(5,0.4)
#Reading Text file
#common_graph <- read.table("F:/Code/R/datasets/grassland.txt")
#common_graph<-read_graph("C:/Users/Lenovo/Desktop/SUMMER PROJECT/dolphin.gml",format = c("gml"))

common_graph<-read_graph("C:/Users/Lenovo/Desktop/SUMMER PROJECT/football.gml",format = c("gml"))
#common_graph<-read_graph("C:/Users/Lenovo/Desktop/SUMMER PROJECT/karate.gml",format = c("gml"))
#common_graph<-read_graph("C:/Users/Lenovo/Desktop/SUMMER PROJECT/power.gml",format = c("gml"))

#   data1 <- get.data.frame(common_graph)
#   data <- subset(data1,select=-c(value,id))
data<-get.data.frame(common_graph)

exi_links<-get.adjacency(common_graph)

nodes<- nrow(exi_links)

nonexi<-matrix(data=-exi_links,nrow = nrow(exi_links))
nonexi<- nonexi+1

for(i in 1:nrow(nonexi))
  {
  nonexi[i,i]<-0
  }

nongraph<- graph_from_adjacency_matrix(nonexi,mode=c("undirected"))
nonexistlist<-get.data.frame(nongraph)

#data frames containing L and precision
CN_data<-common_neighbor(data,nodes)
jaccard_data<-jaccard_auc(data,nodes)
adamic_data<-adamic_auc(data,nodes)
T_data<-rbind(CN_data,jaccard_data,adamic_data)
print(T_data)

#plotting 3 graphs together
g<-ggplot(data=T_data,
          aes(x=lvalues,y=precision,col=ticker))+
  geom_line()+labs(title="AUP Curve-Football",x="L Values",y="Precision")
plot(g)





