setwd("C:\\Users\\user\\Desktop\\Great Lakes - Study Material\\Capstone\\Final Data")
mydata=read.csv("mydata.csv")
dim(mydata)
str(mydata)
prop.table(table(mydata$ComplaintReceived))
#highly imbalanced dataset

#Visualization & Data preparation
library(ggplot2)
a=ggplot(mydata, aes(x=Initials1, y=..density..,  fill=1))
a+geom_histogram(stat = "bin", bins = 15)+geom_density(alpha=0.5)+
  guides(fill=FALSE)+labs(y="Density", title="Density graph - Initials")+
  theme_bw()+facet_grid(~ComplaintReceived)+theme(plot.title = element_text(size = 10, hjust = 0.5))
table(mydata$Initials1)

#This looks like a variable which should be a factor variable with most of the data crowding around zero

mydata$Initials1=as.factor(mydata$Initials1)

a=ggplot(mydata, aes(x=FEPA1, y=..density.., fill=3))
a+geom_histogram(stat = "bin")+geom_density(alpha=0.5)+guides(fill=FALSE)+
  labs(x="FEPA", y="Density", title="Density Graph - FEPA")+
  theme_bw()+facet_grid(~ComplaintReceived)+
  theme(plot.title = element_text(size = 10, hjust = 0.5))
#This looks like a variable which should be a factor variable with most of the data crowding around zero

mydata$FEPA1=as.factor(mydata$FEPA1)

a=ggplot(mydata,aes(x=FEAO1, y=..density.., fill=1))
a+geom_histogram(stat = "bin")+geom_density(alpha=0.5)+guides(fill=FALSE)+
  labs(x="FEAO", y="Density", title="Density Graph - FEAO")+
  theme_bw()+theme(plot.title = element_text(size = 10, hjust = 0.5))
table(mydata$ComplaintReceived)

#This looks like a variable which should be a factor variable with all of the data crowding around zero
mydata$FEAO1=as.factor(mydata$FEAO1)

a=ggplot(mydata, aes(x=Waiver1, y=..density.., fill=1))
a+geom_histogram(stat="bin", bins = 20)+geom_density(alpha=0.5)+ labs(x="Waiver", y="Density", title="Density graph - Waiver")+
  theme_bw()+theme(plot.title = element_text(size = 10, hjust = 0.5))+
  facet_grid(~ComplaintReceived)+guides(fill=FALSE)

#This looks like a variable which should be a factor variable most of the data crowding around zero.
#Additionally looks like presence of Waiver is associated with complaints received

mydata$Waiver1=as.factor(mydata$Waiver1)

a=ggplot(mydata, aes(x=EDRV1, y=..density.., fill=1))
a+geom_histogram(stat="bin", bins = 20)+geom_density(alpha=0.5)+labs(x="EDRV", y="Density", title="Density graph - EDRV")+
  theme_bw()+theme(plot.title = element_text(size = 10, hjust = 0.5))+
  facet_grid(~ComplaintReceived)+guides(fill=FALSE)

#This looks like a variable which should be a factor variable most of the data crowding around zero and remaining around one.

mydata$EDRV1=as.factor(mydata$EDRV1)

a=ggplot(mydata, aes(x=Reversed, y=..density.., fill=1))
a+geom_histogram(stat="bin", bins = 20)+geom_density(alpha=0.5)+labs(x="Reversed", y="Density", title="Density graph - Reversed")+
  theme_bw()+theme(plot.title = element_text(size = 10, hjust = 0.5))+
  facet_grid(~ComplaintReceived)+guides(fill=FALSE)

#This looks like a variable which should be a factor variable most of the data crowding around zero and remaining around one.
#Additionally, presence of Reversed seems to be associated with complaints received in a bigger way

mydata$Reversed=as.factor(mydata$Reversed)
options(scipen = 999)
a=ggplot(mydata, aes(x=Surplus, y=..density.., fill=1))
a+geom_histogram(stat="bin", bins = 50)+geom_density(alpha=0.5)+labs(x="Surplus", y="Density", title="Density graph - Surplus")+
  theme_bw()+theme(plot.title = element_text(size = 10, hjust = 0.5))+
  facet_grid(~ComplaintReceived)+guides(fill=FALSE)
a=boxplot(mydata$Surplus)
length(a$out)
#Looks like a variable which should be numeric but with a high number of outliers. Close to 17% of observations are outliers
#Outlier treatment
summary(mydata$Surplus)
library(dplyr)
mydata%>%mutate(Dec=ntile(Surplus,10))%>%group_by(Dec, ComplaintReceived)%>%
  summarise(Max=max(Surplus), Min=min(Surplus), N=n())

unclass(mydata%>%mutate(Dec=ntile(Surplus,10))%>%count(Dec,ComplaintReceived)%>%filter(ComplaintReceived==1))[[3]]
#Seems like a variable with majority of values as zero.Eight deciles have min and max as zero which constitute 80% of the data.
#Complaints received only in the 9th and 10th deciles when there are surpluses greater than 0.

#Essentially there is a very high chance of a complaint/query when there is a surplus. This indicates that the customer thinks that the analysis is incorrect or the surplus is not being returned on time by the company.
#converting surplus to a factor variable. Chances of a complaint when there is no surplus is very low

mydata%>%filter(Surplus==0 & ComplaintReceived==1)%>%nrow()
mydata%>%filter(Surplus==0 & ComplaintReceived==0)%>%nrow()

mydata$Surplus=as.factor(ifelse(mydata$Surplus>0,1,0))

a=ggplot(mydata, aes(x=ShortageSpread, y=..density.., fill=1))
a+geom_histogram(stat="bin", bins = 50)+geom_density(alpha=0.5)+labs(x="ShortageSpread", y="Density", title="Density graph - ShortageSpread")+
  theme_bw()+theme(plot.title = element_text(size = 10, hjust = 0.5))+
  facet_grid(~ComplaintReceived)+guides(fill=FALSE)
a=boxplot(mydata$ShortageSpread)
length(a$out)

#Outlier treatment by winsorizing
library(DescTools)
mydata$ShortageSpread=Winsorize(mydata$ShortageSpread,probs = c(0,0.85))
boxplot(mydata$ShortageSpread)


a=ggplot(mydata, aes(x=Shortage, y=..density.., fill=1))
a+geom_histogram(stat="bin", bins = 50)+geom_density(alpha=0.5)+labs(x="ShortageSpread", y="Density", title="Density graph - Shortage")+
  theme_bw()+theme(plot.title = element_text(size = 10, hjust = 0.5))+
  facet_grid(~ComplaintReceived)+guides(fill=FALSE)

mydata%>%mutate(Dec=ntile(Shortage,10))%>%count(Dec,ComplaintReceived)%>%filter(ComplaintReceived==1)->Profiling
Profiling=rbind(c(1,1,0),Profiling)
Profiling$N=unclass(mydata%>%mutate(Dec=ntile(Shortage,10))%>%count(Dec))[[2]]
Profiling$CompPerc=Profiling$n/Profiling$N
Profiling$GreaterThan=unclass(mydata%>%mutate(Dec=ntile(Shortage,10))%>%group_by(Dec)%>%summarise(min(Shortage)))[[2]]
Profiling$LesserThan=unclass(mydata%>%mutate(Dec=ntile(Shortage,10))%>%group_by(Dec)%>%summarise(max(Shortage)))[[2]]
Profiling

#Leave shortage as it is though anytime there is a shortage, there is a higher chance of a complaint
table(mydata$LatestAnalysisType,mydata$ComplaintReceived)
a=ggplot(mydata,aes(x=LatestAnalysisType), alpha=0.5)
a+geom_bar(stat = "count",aes(fill=Surplus) ,position = "dodge")+
  facet_grid(Escrowed~factor(ComplaintReceived))+labs(x="",y="Count", title="Latest Analysis Matters?")+
  theme(legend.position = c(0.4,0.2), plot.title = element_text(size = 10, hjust = 0.5),
        legend.key.size = unit(0.5,"cm"), legend.title = element_text(size = 8),
        legend.text = element_text(size = 8), axis.text.x = element_text( angle=45,size = 8, vjust = 1, hjust = 1),
        legend.direction = "horizontal")

mydata$ComplaintReceived=as.factor(mydata$ComplaintReceived)

#With highly skewed numbers in favor of NonBK analysis a separate analysis only on NONBK analysis loans could be contemplated
library(caret)
#Model Building
set.seed(12345)
Index=createDataPartition(mydata$ComplaintReceived, list = FALSE, p=0.7)
train=mydata[Index,-1]
test=mydata[Index,-1]
Model1=glm(ComplaintReceived~Escrowed+LatestAnalysisType+ShortageSpread+Surplus+Reversed+
                      EDRV1+Waiver1+FEAO1+FEPA1+Initials1, 
                    data = train,family = binomial)

summary(Model1)
step(Model1, direction = "both")

Model2=glm(formula = ComplaintReceived ~ LatestAnalysisType + ShortageSpread + 
                    Surplus + Reversed + Waiver1 + FEAO1 + FEPA1 + Initials1, 
                  family = binomial, data = train)
summary(Model2)
exp(coef(Model2))

#Predition

Pred=predict(Model2, test, type = "response")
Pred.class=factor(ifelse(Pred>0.03,1,0))
CM=confusionMatrix(Pred.class,test$ComplaintReceived)
Acc=CM$overall[[1]]
Sens=CM$byClass[[1]]
Spec=CM$byClass[[2]]
library(ROCR)
Pred.St=prediction(Pred, test$ComplaintReceived)
Perf=performance(Pred.St,"tpr","fpr")
plot(Perf)
AUC=performance(Pred.St,"auc")
auc=AUC@y.values[[1]]

###########
set.seed(3465)
library(ROSE)
d.Rose=ROSE(ComplaintReceived~., data = mydata[,-1], seed = 1)$data
prop.table(table(d.Rose$ComplaintReceived))
names(d.Rose)
set.seed(123)
Index=createDataPartition(d.Rose$ComplaintReceived, p=0.70, list = FALSE, times = 1)
Train.R=d.Rose[Index,]
Test.R=d.Rose[-Index,]
library(randomForest)
set.seed(12345)

RF=randomForest(ComplaintReceived~., data = Train.R)
randomForest::varImpPlot(RF)
RF
Pred=predict(RF,Test.R)
CM=confusionMatrix(Pred,Test.R$ComplaintReceived)
CM$table
Acc_RF1=CM$overall[[1]]
Sensitivity_RF1=CM$byClass[[1]]
Specificity_RF1=CM$byClass[[2]]

F1sq_RF1=CM$byClass[[7]]
Pred.Storage=prediction(as.numeric(Pred),as.numeric(Test.R$ComplaintReceived))
AUC=performance(Pred.Storage,"auc")
AUC_RF1=AUC@y.values[[1]]

###########
#Tuning model parameters
set.seed(258)
str(Train.R)
tuneRF(Train.R[,2:12], Train.R[,1], mtryStart=2, ntreeTry=100, stepFactor=2, improve=0.05,
       trace=TRUE, plot=TRUE, doBest=FALSE)

#Least OOB error for mtry = 4
set.seed(12345)

RF_Final=randomForest(ComplaintReceived~., data = Train.R, mtry=4)
randomForest::varImpPlot(RF_Final)
RF_Final
Pred=predict(RF_Final,Test.R)
CM=confusionMatrix(Pred,Test.R$ComplaintReceived)
CM$table
Acc_RFinal=CM$overall[[1]]
Sen_RFinal=CM$byClass[[1]]
Spec_RFinal=CM$byClass[[2]]

F1sq_RFinal=CM$byClass[[7]]
Pred.Storage=prediction(as.numeric(Pred),as.numeric(Test.R$ComplaintReceived))
AUC=performance(Pred.Storage,"auc")
AUC_RFinal=AUC@y.values[[1]]
varImp(RF_Final)
varImpPlot(RF_Final)
########
#Let's try Adaboost and GBM
library(adabag)
objControl <- trainControl(method='cv', number=5, returnResamp='none', 
                           summaryFunction = twoClassSummary, 
                           classProbs = TRUE)

levels(Train.R[,1])=c("No","Yes")
# run model
GBMModel <- train(Train.R[,2:12], Train.R[,1], 
                  method='gbm', 
                  trControl=objControl,  
                  metric = "ROC",
                  preProc = c("center", "scale"))
GBMModel
levels(Test.R$ComplaintReceived)=c("No","Yes")
Pred=predict(GBMModel,Test.R[,2:12])
CM=confusionMatrix(Pred,Test.R$ComplaintReceived)
CM$table
Acc_GBM=CM$overall[[1]]
Sensitivity_GBM=CM$byClass[[1]]
Specificity_GBM=CM$byClass[[2]]

F1sq_GBM=CM$byClass[[7]]
Pred.Storage=prediction(as.numeric(Pred),as.numeric(Test.R$ComplaintReceived))
AUC=performance(Pred.Storage,"auc")
AUC_GBM=AUC@y.values[[1]]

Pred=predict(GBMModel,Test.R[,2:12], type = "prob")
Pred.prob=Pred[,2]

library(gains)
#Adding dummy customer IDs as replacement of what would have original customer IDs
Test.R$CustID=seq(1,46971,1)

gains(as.numeric(Test.R$ComplaintReceived),Pred.prob,groups = 10)

Test.R$prob = Pred.prob

quantile(Test.R$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

targeted=Test.R%>%filter(prob>0.94054635 & prob<=0.99999)%>%dplyr::select(CustID)



Compare=data.frame(Models=c("Logistic","RandomForest","RF-withTuning","Gradient Boosting"),
                   Accuracy=round(c(Acc,Acc_RF1,Acc_RFinal ,Acc_GBM),2),
                   Sensitivity=round(c(Sens,Sensitivity_RF1,Sen_RFinal,Sensitivity_GBM),2),
                   Specificity=round(c(Spec,Specificity_RF1,Spec_RFinal,Specificity_GBM),2),
                   AUC=round(c(auc,AUC_RF1,AUC_RFinal,AUC_GBM),2))
print(Compare)                   

library(reshape)
ggplot(melt(Compare,id.vars = "Models"),aes(Models,value, col=variable, group=variable))+geom_line()+
  geom_point(size=4,shape=21,fill="white")+scale_y_continuous(breaks = seq(0.4,1,0.05))+
  labs(x="",y="Values", title="Evaluation Metric Comparison", color="Metrics")+
  theme(legend.key = element_rect(colour = "black", fill = "light blue"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 15, hjust = 0.5))

ggplot(melt(Compare,id.vars = "Models"),aes(x=variable,value, fill=Models))+
  geom_bar(stat = "identity", position = "dodge")+coord_flip()+
  labs(x="",y="Values", title="Evaluation Metric Comparison", color="Metrics")+
  theme(legend.key = element_rect(colour = "black", fill = "light blue"),
        axis.text.y = element_text(size = 10, hjust = 1, face = "bold"),
        plot.title = element_text(size = 15, hjust = 0.5),
        legend.key.size = unit(0.5,"cm"),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey"))
