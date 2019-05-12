#Data mining amd analysis --- pt1

train <- read.csv("train.csv")
test<- read.csv("test.csv")

test$Survived<- "NULL"

data_combined <- rbind(train,test)

str(data_combined)

#Finding relation between Passenger's class vs survival
data_combined$Survived <- as.factor(data_combined$Survived)
data_combined$Pclass <- as.factor(data_combined$Pclass)

table(data_combined$Survived)

library(ggplot2)

ggplot(train, aes(x = factor(Pclass), fill = factor(Survived)))+
  geom_bar()
#it can be observed that Ist class passengers have better chances of surviving followed by IInd class and IIIrd class having the least chances

train$Name<-as.character(train$Name)
str(train)

head(train$Name)

length(unique(as.character(data_combined$Name)))

dup_names<-as.character(data_combined[which(duplicated(as.character(data_combined$Name))),"Name"])
dup_names

data_combined[which(data_combined$Name %in% dup_names),]

library(stringr)

miss<-data_combined[which(str_detect(data_combined$Name, "Miss")),]
#head(miss)
miss[1:6,]

mrs<- data_combined[which(str_detect(data_combined$Name,"Mrs")),]
mrs[1:6,]

male<-data_combined[which(data_combined$Sex=="male"),]
male[1:6,]

extractTitle <- function(name) {
  name<- as.character(name)
  
  if(length(grep("Miss",name)>0))
     {
     return("Miss")
}
  else if(length(grep("Mrs",name))>0)
  {
    return("Mrs")
  }
  
  else if (length(grep("Master",name))>0)
  {
    return("Master")
  }
  else if (length(grep("Mr",name))>0)
  {
    return("Mr")
  }
  else {
    return("Other")
  }
}

titles<-NULL
for (i in data_combined[1:891,"Name"]) {
  
  titles<-c(titles,extractTitle(i))
}

train$Titles<- titles

females_comb <- train[which(train$Sex=='female'),]
males_comb<- train[which(train$Sex=='male'),]

library(ggplot2)

ggplot(females_comb,aes(x=Titles, fill = factor(Survived)))+
  geom_bar()+
  facet_wrap(~Pclass)


ggplot(males_comb,aes(x=Titles, fill = factor(Survived)))+
  geom_bar()+
  facet_wrap(~Pclass)

#modelling will be done based on sex and titles to find the surviving chances



