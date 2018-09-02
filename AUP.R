common_neighbor <- function(data,nodes){
  
  graphmat<-matrix(data=-1,nrow=nodes,ncol=nodes)
  no_edges=nrow(data)
  nrFolds <- 10
  # generate array containing fold-number for each sample (row)
  folds <- rep_len(1:nrFolds, nrow(data))
  
  t<-1
  pre<-c(0,0,0,0,0,0,0,0,0,0)
  L<-c(2,4,6,8,10,12,14,16,18,20)
  for(x in L)
  { precision=0
  result=0
  # actual cross validation
  for(k in 1:nrFolds) {
    # actual split of the data
    fold <- which(folds == k)
    traindata <- data[-fold,]
    testdata <-  data[fold,]
    graphmat<-matrix(data=-1,nrow=nodes,ncol=nodes)
    for(i in 1:nrow(traindata)){
      graphmat[traindata[i,1],traindata[i,2]]<-1
      graphmat[traindata[i,2],traindata[i,1]]<- 1
    }
    
    CNmat<-matrix(data=-1,nrow=nodes,ncol=nodes)
    for(i in 1:nrow(graphmat)){
      for(j in 1:ncol(graphmat)){
        if(graphmat[i,j]==-1){
          CNmat[i,j]<-commonNode(i,j,graphmat)
        }
      }
    }
    equal=0
    greater=0
    total=nrow(testdata)*nrow(nonexistlist)
    
    #implementing precision
    allrows<-rbind(testdata,nonexistlist)
    nallrows<-nrow(allrows)
    scorerow<-rep(0,nallrows)
    for(i in 1:nallrows){
      scorerow[i]<-CNmat[allrows[i,1],allrows[i,2]]
    }
    allrows$score<-scorerow
    n_allrows=allrows[order(allrows$score,decreasing=TRUE),]
    count=0
    #x<-nrow(testdata)
    top10<-n_allrows[1:x,]
    #top10<-orderscore[1:10]
    #for(i in 1:10){
    #  if(top10[i]<=nrow(testdata)){
    #   count=count+1
    # }
    #}
    for(i in 1:x)
    {
      for(j in 1:nrow(testdata))
      {
        if(top10[i,1]==testdata[j,1] && top10[i,2]==testdata[j,2])
        {
          count=count+1
        }
      }
    }
    precision=precision + count/x
    
    for(i in 1:nrow(testdata)){
      for(j in 1:nrow(nonexistlist)){
        if(CNmat[testdata[i,1],testdata[i,2]] > CNmat[nonexistlist[j,1],nonexistlist[j,2]])
        {
          greater=greater + 1
        }
        if(CNmat[testdata[i,1],testdata[i,2]] == CNmat[nonexistlist[j,1],nonexistlist[j,2]])
        {
          equal=equal+1
        }
      }
    }
    
    result=result+(greater+0.5*equal)/total
  }
  print(precision/10)
  pre[t]<-precision/10
  t=t+1
  
  }
  print(pre)
  print(result/10)
  data1<-data.frame(
    ticker=rep("CN",10),
    lvalues=L,
    precision=pre
  )
  data1
}

#for jaccard
jaccard_auc<- function(data,nodes){
  
  graphmat<-matrix(data=-1,nrow=nodes,ncol=nodes)
  no_edges=nrow(data)
  nrFolds <- 10
  # generate array containing fold-number for each sample (row)
  folds <- rep_len(1:nrFolds, nrow(data))
  
  t<-1
  pre<-c(0,0,0,0,0,0,0,0,0,0)
  L<-c(2,4,6,8,10,12,14,16,18,20)
  for(x in L)
  { precision=0
  result=0
  # actual cross validation
  for(k in 1:nrFolds) {
    # actual split of the data
    fold <- which(folds == k)
    traindata <- data[-fold,]
    testdata <-  data[fold,]
    graphmat<-matrix(data=-1,nrow=nodes,ncol=nodes)
    for(i in 1:nrow(traindata)){
      graphmat[traindata[i,1],traindata[i,2]]<-1
      graphmat[traindata[i,2],traindata[i,1]]<- 1
    }
    
    CNmat<-matrix(data=-1,nrow=nodes,ncol=nodes)
    for(i in 1:nrow(graphmat)){
      for(j in 1:ncol(graphmat)){
        if(graphmat[i,j]==-1){
          CNmat[i,j]<-jaccard(i,j,graphmat)
        }
      }
    }
    equal=0
    greater=0
    total=nrow(testdata)*nrow(nonexistlist)
    
    #implementing precision
    allrows<-rbind(testdata,nonexistlist)
    nallrows<-nrow(allrows)
    scorerow<-rep(0,nallrows)
    for(i in 1:nallrows){
      scorerow[i]<-CNmat[allrows[i,1],allrows[i,2]]
    }
    allrows$score<-scorerow
    n_allrows=allrows[order(allrows$score,decreasing=TRUE),]
    count=0
    #x<-nrow(testdata)
    top10<-n_allrows[1:x,]
    #top10<-orderscore[1:10]
    #for(i in 1:10){
    #  if(top10[i]<=nrow(testdata)){
    #   count=count+1
    # }
    #}
    for(i in 1:x)
    {
      for(j in 1:nrow(testdata))
      {
        if(top10[i,1]==testdata[j,1] && top10[i,2]==testdata[j,2])
        {
          count=count+1
        }
      }
    }
    precision=precision + count/x
    
    for(i in 1:nrow(testdata)){
      for(j in 1:nrow(nonexistlist)){
        if(CNmat[testdata[i,1],testdata[i,2]] > CNmat[nonexistlist[j,1],nonexistlist[j,2]])
        {
          greater=greater + 1
        }
        if(CNmat[testdata[i,1],testdata[i,2]] == CNmat[nonexistlist[j,1],nonexistlist[j,2]])
        {
          equal=equal+1
        }
      }
    }
    
    result=result+(greater+0.5*equal)/total
  }
  print(precision/10)
  pre[t]<-precision/10
  t=t+1
  
  }
  print(pre)
  print(result/10)
  data2<-data.frame(
    ticker=rep("jaccard",10),
    lvalues=L,
    precision=pre
  )
  data2
}

#for adamic
adamic_auc<- function(data,nodes){
  
  graphmat<-matrix(data=-1,nrow=nodes,ncol=nodes)
  no_edges=nrow(data)
  nrFolds <- 10
  # generate array containing fold-number for each sample (row)
  folds <- rep_len(1:nrFolds, nrow(data))
  
  t<-1
  pre<-c(0,0,0,0,0,0,0,0,0,0)
  L<-c(2,4,6,8,10,12,14,16,18,20)
  for(x in L)
  { precision=0
  result=0
  # actual cross validation
  for(k in 1:nrFolds) {
    # actual split of the data
    fold <- which(folds == k)
    traindata <- data[-fold,]
    testdata <-  data[fold,]
    graphmat<-matrix(data=-1,nrow=nodes,ncol=nodes)
    for(i in 1:nrow(traindata)){
      graphmat[traindata[i,1],traindata[i,2]]<-1
      graphmat[traindata[i,2],traindata[i,1]]<- 1
    }
    
    CNmat<-matrix(data=-1,nrow=nodes,ncol=nodes)
    for(i in 1:nrow(graphmat)){
      for(j in 1:ncol(graphmat)){
        if(graphmat[i,j]==-1){
          CNmat[i,j]<-adamic(i,j,graphmat)
        }
      }
    }
    equal=0
    greater=0
    total=nrow(testdata)*nrow(nonexistlist)
    
    #implementing precision
    allrows<-rbind(testdata,nonexistlist)
    nallrows<-nrow(allrows)
    scorerow<-rep(0,nallrows)
    for(i in 1:nallrows){
      scorerow[i]<-CNmat[allrows[i,1],allrows[i,2]]
    }
    allrows$score<-scorerow
    n_allrows=allrows[order(allrows$score,decreasing=TRUE),]
    count=0
    #x<-nrow(testdata)
    top10<-n_allrows[1:x,]
    #top10<-orderscore[1:10]
    #for(i in 1:10){
    #  if(top10[i]<=nrow(testdata)){
    #   count=count+1
    # }
    #}
    for(i in 1:x)
    {
      for(j in 1:nrow(testdata))
      {
        if(top10[i,1]==testdata[j,1] && top10[i,2]==testdata[j,2])
        {
          count=count+1
        }
      }
    }
    precision=precision + count/x
    
    for(i in 1:nrow(testdata)){
      for(j in 1:nrow(nonexistlist)){
        if(CNmat[testdata[i,1],testdata[i,2]] > CNmat[nonexistlist[j,1],nonexistlist[j,2]])
        {
          greater=greater + 1
        }
        if(CNmat[testdata[i,1],testdata[i,2]] == CNmat[nonexistlist[j,1],nonexistlist[j,2]])
        {
          equal=equal+1
        }
      }
    }
    
    result=result+(greater+0.5*equal)/total
  }
  print(precision/10)
  pre[t]<-precision/10
  t=t+1
  
  }
  print(pre)
  print(result/10)
  data3<-data.frame(
    ticker=rep("adamic",10),
    lvalues=L,
    precision=pre
  )
  data3
}
