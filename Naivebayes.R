library(plyr)
train<-read.csv('D:\\ML\\Assignment4\\voting_train.data',header=FALSE)
test<-read.csv('D:\\ML\\Assignment4\\voting_test.data',header=FALSE)
#str(train)
train1 <- apply(train, 1,function(x) any(x=='?') )
test1 <- apply(test, 1,function(x) any(x=='?') )
voting_train<-train[!train1,]
voting_test<-test[!test1,]
#str(test1)
classOccurance=count(voting_train,"voting_train$V1")
total<-sum(classOccurance$freq)
classOcc=classOccurance
global_df<- data.frame();

get_probability<-function(i){
  count(voting_train,c("voting_train$V1",i))
  
}
#v<-ddply(voting_train,c()

for( i in colnames(voting_train)[-1]){
  x<-(get_probability(i))
  
  x$colname<-c(i)
  
  for (j in 1:nrow(x))
  {
    x$freq[j] <-x$freq[j]/classOcc[classOcc$voting_train.V1==x$voting_train.V1[j],]$freq
    #x$freq[j] <-x$freq[j]/classOcc[x$voting_train.V1[j]]
    
  }
  colnames(x)[2]<-"AttributeValue"
    global_df<-rbind(global_df,x);
}

for (j in 1:nrow(classOccurance))
{
  classOccurance$freq[j] <-classOccurance$freq[j]/total
  
}

hits<-0;
#d<-(global_df[global_df$colname=='V2' & global_df$AttributeValue=='n'&global_df$voting_train.V1=='democrat',])$freq
for( i in 1:nrow(voting_test)){
  democrat<-classOccurance[classOccurance$voting_train.V1=='democrat',]$freq;
  republican<-classOccurance[classOccurance$voting_train.V1=='republican',]$freq;
  for(j in colnames(voting_test[i,][-1])){
    val<-as.character(voting_test[i,j])
    d<-(global_df[global_df$colname==j & global_df$AttributeValue==val&global_df$voting_train.V1=='democrat',])$freq
    if(length(d)==0){
      d<-0;
      }
    
    r<-(global_df[global_df$colname==j & global_df$AttributeValue==val&global_df$voting_train.V1=='republican',])$freq
    if(length(r)==0){
      r<-0;
    }
    democrat<-democrat*d
    republican<-republican*r
    #print(j)
    #print(democrat)
    #print(republican)
    
  }
  if(democrat>republican && voting_test[i,][1]=='democrat'){
    hits<-hits+1;
  }else if(democrat<republican && voting_test[i,][1]=='republican'){
    hits<-hits+1;
  }
  #print(democrat)
  #print(voting_test[i])
  #global_df[global_df$colname=='V2' & global_df$voting_train.V1=='democrat',]
  
}
accuracy<-hits/nrow(voting_test)*100