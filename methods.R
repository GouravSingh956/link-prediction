
#common neighbour method
commonNode <- function(node1,node2,graph)
  {
  graph1<-graph_from_adjacency_matrix(graph,mode=c("undirected"))
  score<-cocitation(graph1,v=node1)
  score[,node2]
}

#jaccard coefficient
jaccard<-function(node1,node2,graph)
  {
  graph1<-graph_from_adjacency_matrix(graph,mode=c("undirected"))
  score1<-length(neighbors(graph1,v=node1))
  score2<-length(neighbors(graph1,v=node2))
  n_score<-commonNode(node1,node2,graph)
  d_score<-score1 + score2 - n_score
  score<-n_score/d_score
  score
}

#adamic coefficient
adamic<-function(node1,node2,graph){
  graph1<-graph_from_adjacency_matrix(graph,mode=c("undirected"))
  neighbor1<-neighbors(graph1,v=node1)
  neighbor2<-neighbors(graph1,v=node2)
  common<-intersection(neighbor1,neighbor2)
  score<-0
  for(i in common){
    score<-score+(1/log(length(neighbors(graph1,i))))
  }
  score
}