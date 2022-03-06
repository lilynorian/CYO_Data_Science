#load necessary packages
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(caret)) install.packages('caret')
if (!require(cluster)) install.packages('cluster')
library(tidyverse)
library(ggplot2)
library(caret)
library(cluster)
#loading the data
#this is how I loaded the data from my computer, the data is included in the Github Repository for you to load
path<-"/Users/lilynorian/Desktop/Chicago_Crimes_2012_to_2017.csv"
dta<-read.csv(path)
head(dta)
#exploration
type_summary<-count(dta,Primary.Type) #the frequency of each primary crime type
nrow(type_summary) #there are 33 different primary crime types

dta%>%as_tibble()

#readable way to understand the ID, primary type, and district variables a bit more
dta%>% summarize(n_ID=n_distinct(ID),
                 n_Primary.Type=n_distinct(Primary.Type),
                 n_District=n_distinct(District))
colnames(dta)
dta<-dta[,1:16] #remove columns I decided are unnecessary for my analysis
head(dta)
ggplot(type_summary,aes(x=n,y=Primary.Type))+geom_bar(stat="identity")+ ggtitle("Primary.Type Frequency") #plot the frequency of each crime primary type
district_summary<-count(dta,District)
ggplot(district_summary,aes(x=District,y=n))+geom_bar(stat="identity")+ggtitle("Frequency of Crimes by District")  #plot the frequency of crimes in each district
#data cleaning
#remove NA rows and any duplicates based off of the unique ID column
dta<-na.omit(dta)
dta<-dta%>%distinct(ID,.keep_all=TRUE)
df<-data.frame(table(dta$Primary.Type,dta$District,dta$Location.Description))
colnames(df)<-c('Primary.Type','District','Location.Description','Freq')
#plot different relationships between these variables
ggplot(df,aes(x=District,y=Primary.Type))+geom_point(aes(size=Freq)) + ggtitle("Frequency of Crime Type in Each District")
ggplot(df,aes(x=District,y=Location.Description))+geom_point(aes(size=Freq)) +ggtitle("Frequency of Location in Each District")
ggplot(df,aes(x=Primary.Type,y=Location.Description))+geom_point(aes(size=Freq)) + ggtitle("Frequency of Location Given the Crime Type")

#explore relationships between District, Primary Types, and Arrest rates
dta%>%group_by(District)%>%
  summarize(p=mean(Arrest=='True'))%>%
  ggplot(aes(x=District,y=p))+
  geom_point() + ggtitle("Arrest Rates by District")
dta%>%group_by(District,Primary.Type)%>%
  summarize(p=mean(Arrest=='True'))%>%
  ggplot(aes(x=Primary.Type,y=p))+
  geom_point() + ("Arrest Rates by Primary Type") #difficult plot to read and understand as there are many points for each primary type
dta%>%group_by(District,Primary.Type)%>%count(Arrest=='True')

#attempting kmeans
y<-dta%>%group_by(Primary.Type,Arrest)%>%
  summarize(n=n())%>%
  spread(Arrest,n)
y<-y[,-1]%>%as.matrix()
y<-sweep(y,2,colMeans(y,na.rm=TRUE))
y<-sweep(y,1,rowMeans(y,na.rm=TRUE))
y_0<-y
y_0[is.na(y_0)]<-0
ky<-kmeans(y_0,centers=2,nstart=25)
z2<-data.frame(y_0,ky$cluster)
clusplot(z2,ky$cluster,color=TRUE,shade=F,labels=0,lines=0,main='k-Means Cluster Analysis-Primary.Type')

#identify the 10 most common crime types 
topCrimes<-dta%>%group_by(Primary.Type)%>%
  summarize(n=n())%>%
  arrange(desc(n))%>%
  top_n(10)%>%
  pull(Primary.Type)

#k-means clusterings focusing on districts and arrests for crimes that were one of the 10 most common crime types
arrests <- dta %>% 
  filter(Primary.Type %in% topCrimes) %>%
  group_by(District,Arrest) %>%
  summarize(n=n())%>%
  spread(Arrest,n)
arrests<-arrests[,-1]%>%as.matrix()
arrests<-sweep(arrests,2,colMeans(arrests,na.rm=TRUE))
arrests<-sweep(arrests,1,rowMeans(arrests,na.rm=TRUE))
arrests_0<-arrests
arrests_0[is.na(arrests_0)]<-0
karrests<-kmeans(arrests_0,centers=2,nstart=25)
z3<-data.frame(arrests_0,karrests$cluster)
clusplot(z3,karrests$cluster,color=TRUE,shade=F,labels=0,lines=0,main='k-Means Cluster Analysis')


#Part 2 of analysis

#predictions using regression
#create test and train set
set.seed(755)
test_index <- createDataPartition(y = dta$Arrest, times = 1, p = 0.2, 
                                  list = FALSE)
train_set <- dta[-test_index,]
test_set <- dta[test_index,]
#remove districts and crime types in the test set that do not appear in training set
test_set <- test_set %>% 
  semi_join(train_set, by = "District") %>%
  semi_join(train_set, by = "Primary.Type")

#show relationship between district and arrest rates
dta %>% 
  filter(Primary.Type %in% topCrimes)%>%
  group_by(District) %>%
  summarize(prop = mean(Arrest == "True")) %>%
  ggplot(aes(District, prop)) +
  geom_point() 

#equate the Arrest variable to binary 
train_set <- train_set %>% 
  filter(Primary.Type %in% topCrimes)%>%
  mutate(outcome= as.numeric(Arrest == "True"))

test_set<-test_set%>%
  filter(Primary.Type %in% topCrimes)%>%
  mutate(outcome = as.numeric(Arrest == "True"))
  
#predictions of arrests using just the average
mu<-mean(train_set$outcome)

RMSE <- function(true_outcome, predicted_outcome){
  sqrt(mean((true_outcome - predicted_outcome)^2))
}

naive_rmse <- RMSE(test_set$outcome, mu)
naive_rmse

#predictions using the average and average by district
district_avg<-train_set%>%
  group_by(District)%>%
  summarize(b_i = mean(outcome - mu))
predicted_outcome1 <- mu + test_set %>% 
  left_join(district_avg, by='District') %>%
  pull(b_i)
RMSE(predicted_outcome1, test_set$outcome)

#predictions using the average, average by district, and average by primary type
type_avg<-train_set%>%
  left_join(district_avg,by='District')%>%
  group_by(Primary.Type)%>%
  summarize(b_u=mean(outcome-mu-b_i))
predicted_outcome2 <- test_set %>% 
  left_join(district_avg, by='District') %>%
  left_join(type_avg, by='Primary.Type') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
RMSE(predicted_outcome2, test_set$outcome)

#explore relationship between location and arrest rates
dta%>%group_by(Location.Description)%>%
  summarise(p=mean(Arrest=="True"))%>%
  ggplot(aes(Location.Description,p))+
  geom_point()

#predictions using average, average by district, by type, and by location
location_avg<-train_set%>%
  left_join(district_avg,by='District')%>%
  left_join(type_avg,by='Primary.Type')%>%
  group_by(Location.Description)%>%
  summarize(b_p=mean(outcome-mu-b_i-b_u))
predicted_outcome3 <- test_set %>% 
  left_join(district_avg, by='District') %>%
  left_join(type_avg, by='Primary.Type') %>%
  left_join(location_avg,by='Location.Description')%>%
  mutate(pred = mu + b_i + b_u + b_p) %>%
  pull(pred)
RMSE(predicted_outcome3, test_set$outcome)

#the predictions variable shows the predictions in the last column with all other variables also included in the table
predictions<-test_set %>% 
  mutate(pred=round(predicted_outcome3))
unique(predictions$pred)
predictions$pred[predictions$pred==2]<-1
unique(predictions$pred)
predictions$pred[predictions$pred==0]<-'False'
predictions$pred[predictions$pred==1]<-'True'
