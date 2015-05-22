install.packages("data.table")
install.packages("dplyr")
install.packages("tidyr")

library(data.table)
library(dplyr)
library(tidyr)

##read in the test data, test subjects and test labels 
##and converts test data and tes labels to datatables
TEST=read.table("./UCI HAR Dataset/test/X_test.txt")
as.data.table(TEST)
SUBTEST=read.table("./UCI HAR Dataset/test/subject_test.txt")
YTEST=read.table("./UCI HAR Dataset/test/Y_test.txt")
as.data.table(YTEST)

##read in the train data, train subjects and train lables
##and converts to train data and train labels to datatables
TRAIN=read.table("./UCI HAR Dataset/train/X_train.txt")
as.data.table(TRAIN)
SUBTRAIN=read.table("./UCI HAR Dataset/train/subject_train.txt")
YTRAIN=read.table("./UCI HAR Dataset/train/Y_train.txt")
as.data.table(YTRAIN)


##creates a variable in YTEST and YTRAIN. This is to ensure that 
##that the test and train data can be distinguished in merged datasets.
YTEST<-YTEST%>%mutate(Data="Test")
YTRAIN<-YTRAIN%>%mutate(Data="Train")

#combines the test subjects, test labels and test data
##and the train subjects, train labels and train data to 2 data.tables
TESTC<-cbind(SUBTEST,YTEST, TEST)
TRAINC<-cbind(SUBTRAIN,YTRAIN, TRAIN)

##combines the test and train datasets together 

COMB<-rbind(TESTC, TRAINC)

##read in the features information as FEA and changes its class to vector 
##add the additional character elements: Subject, Activity and Data to 
##FEA to provide names for the first three columns of the data.table. 
##Ensure values of FEA are unique

FEA=read.table("./UCI HAR Dataset/features.txt", header=FALSE, sep = "")
FEA=as.vector(FEA[, 2])
FEA=c("Subject", "Activity","Data", FEA)
FEA<-make.names(FEA, unique=TRUE)

#Use the FEA data to assign names to the variables in the combined 
##data.table (COMB)
colnames(COMB)<-FEA

##selects columns with names containing either mean or std
SUB<-COMB %>%
  select(contains("mean"))
SUB2<-COMB %>%
   select(contains("std"))  

##Combines rows 1:3 of COMB with columns denoting means and standard deviations
COMB2<-cbind((COMB[, 1:3]), SUB, SUB2)


##read in the activity labels information and renames columns for easy
##merging. Merges the activity lables with subsetted Test/Train Date
ACT=read.table("./UCI HAR Dataset/activity_labels.txt", header=FALSE, sep = "")
colnames(ACT)<-c("Activity", "Activity_label")
COMB2<-merge(ACT,COMB2,by="Activity",all=TRUE)

##Removes column Activity (i.e. column with activities as 
##numbers not labels)
TIDY<-COMB2%>%select(-(Activity))%>%
          mutate(Subject=factor(Subject))

write.table(TIDY, "./UCI HAR Dataset/TidyData.txt", row.name=FALSE) 

##SUMMARISE COMB2 Data following the extraction of the column "Data" 
##and grouping by Activity_label and Subject

SUMMARY<-TIDY %>%
  select(-Data)%>%
  group_by(Activity_label, Subject) %>%
  summarise_each(funs(mean))

write.table(SUMMARY, "./UCI HAR Dataset/Summary.txt", row.name=FALSE) 