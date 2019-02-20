for (t in 0:4) {
  if (t==0){
    
    E1<-normalise(runif(3))
    E2<-normalise(runif(3))
    E3<-normalise(runif(3))
    
      for (p in 1:100){
        E1<-rbind(E1, normalise(runif(3)))  
        E2<-rbind(E2, normalise(runif(3)))  
        E3<-rbind(E3, normalise(runif(3)))  
      }
    
    LearnPort <- rbind(c((E1[1,1]+E2[1,1]+E3[1,1])/3,(E1[1,2]+E2[1,2]+E3[1,2])/3,(E1[1,3]+E2[1,3]+E3[1,3])/3))
    
    ExpertMatrix <- matrix(1, nrow=3, ncol=3)
    apply(ExpertMatrix, c(1, 2), function(x) 1)
    
    E1Wealth=0
    E2Wealth=0
    E3Wealth=0
    LearnerWealth=0
  }
  else {
    
    realShareVector<-rbind(c(s1[t,1],s2[t,1],s3[t,1]))
    shareVector<-rbind(c(s1[t,2],s2[t,2],s3[t,2]))
    
    E1Wealth[t]<-wealth(realShareVector,E1[t,])
    E2Wealth[t]<-wealth(realShareVector,E2[t,])
    E3Wealth[t]<-wealth(realShareVector,E3[t,])
    LearnerWealth[t]<-wealth(realShareVector,LearnPort)
    
    if (sum(shareVector)==3) {
      ExpertMatrix=ExpertMatrix
    }
    else
    { 
      indexZero<-apply(shareVector, 1, function(y) which(y==0))
        
        for (i in 1:length(indexZero)){
          x<-indexZero[i]
          ExpertMatrix[1,x]<-(1-E1[t,x])*ExpertMatrix[1,x]
          ExpertMatrix[2,x]<-(1-E2[t,x])*ExpertMatrix[2,x]
          ExpertMatrix[3,x]<-(1-E3[t,x])*ExpertMatrix[3,x]
        }
      
      maxValues<-apply(ExpertMatrix,2,max)
      LearnPort<-normalise(maxValues)
      
    }
  }
}

sum(E1Wealth)
sum(E2Wealth)
sum(E3Wealth)
sum(LearnerWealth)