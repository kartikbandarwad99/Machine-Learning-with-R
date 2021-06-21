#installing the necessary packages
#install.packages("dplyr")
library(dplyr)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("caTools")
library(caTools)
#install.packages("moments")
library(moments)
#install.packages("e1071")
library(e1071)
#install.packages(rpart)
library(rpart)
#install.packages(ggpubr)
library(ggpubr)
library(tidyverse)

#importing the dataset
data <- read_csv("F:/simplilearn/live/R2/College_admission.csv")
head(data)
str(data)
#checking for null values
any(is.na(data))
#checking for outliers
ggplot(data,aes(gre))+geom_boxplot()+scale_x_continuous(breaks = seq(200,800,30))
data[data$gre<=320,]# there are few outliers in gre 

ggplot(data,aes(gpa))+geom_boxplot()+scale_x_continuous(breaks = seq(2, 4, .1))
data[data$gpa<=2.4,]#there are a few outliers in gpa

#Lets remove the outliers and check the shape of the dataset
df<-filter(data,gre>320,gpa>2.4)
dim(df)

# Only gre and gpa are numeric so convert the other variables to factors
df$admit<-as.factor(df$admit)
df$ses<-as.factor(df$ses)
df$Gender_Male<-as.factor(df$Gender_Male)
df$Race<-as.factor(df$Race)
df$rank<-as.factor(df$rank)

#checking if the data is normally distriuted for numerical variables
ggdensity(df$gre,fill = "red",main='Density of Graduate Exam Scores',xlab = 'Exam Scores')+
  stat_central_tendency(type = "mean", color='blue',)+
  stat_central_tendency(type = "median", color='green')
skewness(df$gre)#skewness for gre is -0.026 hence it is normally distributed

ggdensity(df$gpa,fill = "red",main='Density of Graduate Exam Scores',xlab = 'Exam Scores')+
  stat_central_tendency(type = "mean", color='blue',)+
  stat_central_tendency(type = "median", color='green')

# building logistic regression model
set.seed(101)
split<-sample.split(df$admit,SplitRatio = 0.75)
train<-subset(df,split==T)
test<-subset(df,split==F)

model<-glm(admit~.,family = binomial(link = 'logit'),data = train)
summary(model)
probs<-predict(model,test,type = 'response')
results<-ifelse(probs>0.5,1,0)
results
table(test$admit,results)#gives accuracy of 73.73%

# selecting only significant model for logistic regression

model<-glm(admit~rank+gre,family = binomial(link = 'logit'),data = train)
summary(model)
probs<-predict(model,test,type = 'response')
results<-ifelse(probs>0.5,1,0)
results
table(test$admit,results)#accuracy=68.68%

# SVM model
svm_model<- svm(admit~.,data=train,type='C-classification')
summary(svm_model)
predictions_svm<-predict(svm_model,test[-1])
confusion_matrix<-table(test$admit,predictions_svm)
confusion_matrix#accuracy=67.67%

#Decision tree model

model_dt<-rpart(admit~.,data=train,)
summary(model_dt)
predictions_dtree<-predict(model_dt,test[-1],type = 'class')
confusion_matrix<-table(test$admit,predictions_dtree)
confusion_matrix#accuracy=67.67%

# Logistic regression seems to be the best model among the others
#Admission Probability percentages plot
df %>%
  mutate(GreCat = ifelse(gre>580,"High",
                         ifelse(between(gre,440,580),"Medium","Low")))%>%
  mutate(admit_new = (ifelse(admit==1,"Yes","No")))%>%
  group_by(GreCat,admit_new)%>%
  summarise(Count = n())%>%
  mutate(AdmissProbPct =(round(Count/sum(Count)*100,2)))%>%
  arrange(AdmissProbPct)%>%
  ggplot(aes(admit_new,AdmissProbPct,color=GreCat))+
  geom_point()+
  geom_text(aes(label=paste0(AdmissProbPct,"%")),hjust=0,nudge_x = 0.03)+
  ggtitle("Admission Probablity of Student")+
  xlab("Admission Status") + ylab('Probablity Percentage')+
  theme_classic()
