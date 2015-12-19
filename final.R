
setwd("~/Desktop/F/data science/Amex")
train=read.csv("Training_Dataset.csv",header=T)
test=read.csv("Final_Dataset.csv",header=T)

colnames(train)=c("CitizenID","actual_vote","previous_vote","Centaur_Donations","Ebony_Donations","Tokugawa_Donations","Odyssey_Donations","Cosmos_Donations","Centaur_Share","Ebony_Share","Tokugawa_Share","Cosmos_Share","Odyssey_Share","Occupation","Region_Code","Family_Size","Age","Married","House","Political_Affiliations","Years_Curr_Address","elections_voted","Unique_Parties","Education","Centaur_Rallies","Ebony_Rallies","Tokugawa_Rallies","Odyssey_Rallies","Total_Rallies","Cosmos_Rallies","Additional_Document","Primary_Income")
colnames(test)=c("CitizenID","previous_vote","Centaur_Donations","Ebony_Donations","Tokugawa_Donations","Odyssey_Donations","Cosmos_Donations","Centaur_Share","Ebony_Share","Tokugawa_Share","Cosmos_Share","Odyssey_Share","Occupation","Region_Code","Family_Size","Age","Married","House","Political_Affiliations","Years_Curr_Address","elections_voted","Unique_Parties","Education","Centaur_Rallies","Ebony_Rallies","Tokugawa_Rallies","Odyssey_Rallies","Total_Rallies","Cosmos_Rallies","Additional_Document","Primary_Income")

#################LIBRARY##########
library(gbm)
library(rpart)
library(rpart.plot)
library(rattle)
library(dplyr)
library(caTools)
library(stringr)
library(party)
##################################

test$actual_vote="EBONY"
dat=rbind(train,test)

##################################
dat$Centaur_Donations=as.numeric(str_replace_all(as.character(dat$Centaur_Donations), "[^[:alnum:]]", ""))
dat$Ebony_Donations =as.numeric(str_replace_all(as.character(dat$Ebony_Donations), "[^[:alnum:]]", ""))
dat$Tokugawa_Donations=as.numeric(str_replace_all(as.character(dat$Tokugawa_Donations), "[^[:alnum:]]", ""))
dat$Odyssey_Donations=as.numeric(str_replace_all(as.character(dat$Odyssey_Donations), "[^[:alnum:]]", ""))
dat$Cosmos_Donations=as.numeric(str_replace_all(as.character(dat$Cosmos_Donations), "[^[:alnum:]]", ""))
##################################


dat$Centaur_Donations=as.numeric(dat$Centaur_Donations)
dat$Ebony_Donations=as.numeric(dat$Ebony_Donations)
dat$Cosmos_Donations=as.numeric(dat$Cosmos_Donations)
dat$Tokugawa_Donations=as.numeric(dat$Tokugawa_Donations)
dat$Odyssey_Donations=as.numeric(dat$Odyssey_Donations)

train=dat[1:42819,]
test=dat[42820:57092,]

train=train[sample(nrow(train)),]


fit_gbm=gbm(actual_vote~previous_vote+Centaur_Donations+Ebony_Donations+Tokugawa_Donations+Odyssey_Donations+Cosmos_Donations+Centaur_Share+Ebony_Share+Tokugawa_Share+Cosmos_Share+Odyssey_Share+Occupation+Region_Code+Age+House+Political_Affiliations+Years_Curr_Address+elections_voted+Unique_Parties+Education+Centaur_Rallies+Ebony_Rallies+Tokugawa_Rallies+Odyssey_Rallies+Total_Rallies+Cosmos_Rallies+Additional_Document+Primary_Income,data=train,distribution = "multinomial",n.trees=1200,interaction.depth = 5, bag.fraction = 0.5, shrinkage=0.01, train.fraction = 0.8, verbose = T, n.cores=2)


trees=gbm.perf(fit_gbm, plot.it=TRUE, method="test")


pred=predict(fit_gbm,test,type = "response")


name=c("CENTAUR","COSMOS","EBONY","TOKUGAWA")
p=apply(pred,1,which.max)
ans=name[p]

submit=data.frame(test$CitizenID,ans)
write.csv(submit, file="Call_Of_Data_IITGuwahati_84.csv", row.names = F)