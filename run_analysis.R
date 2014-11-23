run_analysis <- function(xtest, ytest, xtrain, ytrain, stest, strain, 
                        features, labels) {
#Step 1: Merge
  #Smash the xtest, xtrain, ytest, and ytrain datasets together
  train<-cbind(ytrain,strain,xtrain)  
  test<-cbind(ytest,stest,xtest)
  merge<-rbind(train,test)

#Step 2: Extract
  grab<-grep("mean()|std()",features[,2])
  extract<-merge[,c(1,2,grab+2)]

#Step 3: Activities
  activities<-merge(extract,labels,by="V1")

#Step 4: Labels
  #The first and last rows are redundant, remove the first.
  trim<-activities[,2:ncol(activities)]

  #The names in the features dataset contain problematic chars.  Auto fix that.
  colnames(trim)<-c("Subject",make.names(features[grab,2]),"Activity")

#Step 5: Tidy data
  DF<-as.data.frame(trim)
  tidy<-aggregate(DF[,2:80],by=list(DF$Subject,DF$Activity),FUN=mean)
  colnames(tidy)[1:2]<-cbind("Subject","Activity")
  return(tidy)



}