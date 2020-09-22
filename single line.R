library(R.matlab)
library(gtools)
library(boot)
library(glmnet)
library("randomForest")
library(nnet)
library(MASS)
library(gbm)
library(caret)
library("plyr")
library(mlbench)
library(igraph)
library(dplyr)
library(kimisc)
library(shape)
library(data.table)
library(JOUSBoost)
library(tidyr)
library(neldermead)
library(yaml)
library(plyr)
library(grid)
library(mvtnorm)
library(libcoin)
library(partykit)
library(splitstackshape)
library(e1071)
library(stringr)

num.line=46
num.single.otg=690
num.obs=num.single.otg

connect<-readMat("/Users/jiahe/Desktop/Power System/bus 39/conncet_info.mat")
connect<-connect$connect.info


one.hop=NULL
onehop=NULL
for (i in 1:num.line)  {
  for (j in 1:2) {
    one.hop[[j]]=c(which(connect[,j]==connect[i,1]),which(connect[,j]==connect[i,2]))
  }
  onehop[[i]]=unique(unlist(one.hop))
}
##
##read final data

##################single line outage#################
##read single line outage cases###
setwd("/Users/jiahe/Desktop/Power System/bus 39/single/aligned")
file_list.single <- mixedsort(list.files())
singleoutage.case = lapply(file_list.single, function(x)readMat(x, header=FALSE))

#setwd("/Users/jiahe/Desktop/bus 39/single/not aligned")
#file_list.single <- mixedsort(list.files())
#singleoutage.case = lapply(file_list.single, function(x)readMat(x, header=FALSE))


##extract theta##
singleotg.case=NULL
for (i in 1:length(singleoutage.case)) {
  assign(paste0("singleoutage.case", i), singleoutage.case[[i]]$a)
  singleotg.case[[i]]=singleoutage.case[[i]]$a[,1:39] 
  singleotg.case[[i]] <-as.matrix(as.data.frame(singleotg.case[[i]]))
}


###corresponding line_outage index##
each.sinotg=15
outcome=rep(1:num.line,each=each.sinotg)
case_comb=singleotg.case


######gumble distribution
alpha=0.05
threshold=-log(-log(1-alpha))
threshold

ori.result=NULL
result=NULL
hah=NULL
for (i in 1:length(case_comb)) {
   scale=0.2
    y=NULL
    for (j in 1:num.line) {
    diff=matrix(case_comb[[i]][,connect[j,1]]-case_comb[[i]][,connect[j,2]],ncol=1)
    set.seed(1)
      ####add noise
    #consider mean(diff)
    diff=diff+rnorm(length(diff),0,scale*(abs(mean(diff))))
   ##consider sd(diff)
   #  diff=diff+rnorm(length(diff),0,scale*(abs(sd(diff))))
    y=cbind(y,diff)
    
  }
  ori.result[[i]]=y
  ori.result[[i]]<-apply(ori.result[[i]],2,fft)/length(ori.result[[i]])
  ori.result[[i]]<-apply(ori.result[[i]],2,Mod)
  ori.result[[i]]<-head(ori.result[[i]],nrow(ori.result[[i]])/2)[-1,]
  
  ##gumbel distribution result=max(|ori.result|^2)
  result[[i]]=ori.result[[i]]^2
  result[[i]]=scale(result[[i]],center=TRUE,scale=TRUE)
  hah[[i]]=apply(result[[i]],2, function(m) sort(m,index.return=TRUE,decreasing=TRUE))
}

indexa=NULL  #indexa[[1]] is the freq of all the obs when line1 out
for (j in 1:num.line) {
  mylist=NULL
  for(k in 1:num.obs) {
    mylist[[k]]=as.data.frame(t(hah[[k]][[j]]$ix[which(hah[[k]][[j]]$x>threshold)]))
  }
  indexa[[j]]=as.matrix(do.call(rbind.fill,mylist))
}

xsplit1 <- rep(1:46,each=each.sinotg)
#tmp1 splits each indexa[[i]] into 46
tmp1 <- lapply(indexa, function(m) split.data.frame(m,xsplit1)) 
##intersect within cases
single.oo=NULL
for (i in 1:length(tmp1)) {
  single.oo[[i]]=lapply(tmp1[[i]],function(m) alply(m,1))
  single.oo[[i]]=lapply(single.oo[[i]], function(m) Reduce(intersect,m))
}
union.allcase1=lapply(single.oo,function(m) Reduce(union,m))
union.allcase1=lapply(union.allcase1,na.omit)

mag=NULL
for(i in 1:num.line) {
  v=lapply(ori.result,function(m) m[,i][c(union.allcase1[[i]])])
  mag[[i]]=do.call(rbind,lapply(v,matrix,ncol=length(union.allcase1[[i]]),byrow=TRUE))
}

################
###############
####one hop
total=NULL
for (i in 1:num.line)  {
  total[[i]]=mag[onehop[[i]]]
  total[[i]]=do.call(cbind,lapply(total[[i]],matrix,nrow=num.obs))
  line.outage=as.factor(ifelse(outcome==i,1,0))
  total[[i]]=data.frame(total[[i]],line.outage)
  colnames(total[[i]])<-c(paste0("X", seq(1:(dim(total[[i]])[2]-1))),"outcome")
}

#############all 46##############
predictors=do.call(cbind,lapply(mag,matrix,nrow=num.obs))

total=NULL
for (i in 1:num.line)  {
  line.outage<-as.factor(ifelse(outcome==i,1,0))
  total[[i]]=data.frame(predictors,line.outage)
  colnames(total[[i]])<-c(paste0("X", seq(1:dim(predictors)[2])),"outcome")
}


######logistic
a=data.frame(outcome)
a$id=seq(1:num.obs)
rep=5
Result=matrix(NA,nrow=rep,ncol=4)
for (k in 1:rep) {
set.seed(k)
train <- do.call(rbind,lapply(split(a, a$outcome),
                              function(subdf) subdf[sample(1:nrow(subdf), 8),]))
train=train$id
final=matrix(NA,nrow=num.obs-length(train),ncol=num.line)
for (i in 1:num.line)   {
  glm.fit<-glm(outcome~.,data=total[[i]],subset=train,family=binomial)
  glm.probs=predict(glm.fit,total[[i]][-train,],type="response")
  glm.pred=rep("0",dim(total[[i]])[1]-length(train))
  glm.pred[glm.probs>.5]="1"
  final[,i]=as.numeric(glm.pred)
  table=table(pred=glm.pred,true=total[[i]][-train,]$outcome)
  print(table)
}

final=final+1
final=cbind( final,outcome[-train])
colnames( final)=c(paste0("line",seq(1:num.line)),"outage")

Result[k,]=Myfun_Single(final)
}


###rf

  a=data.frame(outcome)
  a$id=seq(1:num.obs)
  train <- do.call(rbind,lapply(split(a, a$outcome),
                                function(subdf) subdf[sample(1:nrow(subdf), 8),]))
  train=train$id
final=matrix(NA,nrow=num.obs-length(train),ncol=num.line)

for (i in 1:num.line)   {
  r<-randomForest(outcome~.,total[[i]],subset=train,importance=TRUE,do.trace=100,ntree=100)
  predictions<-predict(r,total[[i]][-train,])  
  final[,i]=as.numeric(predictions)
  table=table(pred=predictions,true=total[[i]][-train,]$outcome)
  print(i)
  print(table)
  }

  
final=cbind( final,outcome[-train])
colnames( final)=c(paste0("line",seq(1:num.line)),"outage")


























