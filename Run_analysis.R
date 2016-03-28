# Tiding the *Human Activity Recognition Using Smartphones* Dataset
#I. Llatas  

#R scripts.

## Preprocess ##

#Load libraries, Download zip file and save the unzipped files

library(tidyr,warn.conflicts = FALSE,verbose=FALSE)
library(plyr,warn.conflicts=FALSE,verbose=FALSE)
library(dplyr,warn.conflicts = FALSE,verbose=FALSE)
library(data.table,warn.conflicts=FALSE,verbose=FALSE)

if(!file.exists("./data")){
        dir.create("./data")  
        #if data dir is in path, then you already downloaded and unzipped file
        fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(fileUrl,destfile="./data/ucihar.zip")
        unzip("./data/ucihar.zip") 
        file.copy("UCI HAR Dataset","./data",recursive=T)
        file.rename("./data/UCI HAR Dataset","./data/ucihar")
        file.remove("UCI HAR Dataset")  #tyding the workspace
}


#- Preprocessing of data in **features.txt** showed 561 
# features, but only 477 different names, corresponding to features 
# on the FFT of BodyAcc and BodyGyro, all containing in the name the string 
# "bandsEnergy".  I've read the downloaded files, but I cannot find a way to 
#assign proper X/Y/Z names to those columns.  

# Thus, I decided to select all columns in the Xtest/Xtrain data sets 
# BUT the ones with the aforementioned string.  

#I built a function, called join1.f to produce the intermediate tidy data frames.
# in this function the chuncks of data for each set are joined, creating four columns
# activity.code, activity.name, set, subject.id
########################
#Read into R and make 2 data tables, one for each set (train, test).

features<-read.table("data/ucihar/features.txt",stringsAsFactors = FALSE)

activities<-read.table("data/ucihar/activity_labels.txt",stringsAsFactors = FALSE)
names(activities)=c("code","act")

Xtrain<-read.table("data/ucihar/train/X_train.txt")
ytrain<-unlist(read.table("data/ucihar/train/y_train.txt"))
idtrain<-unlist(read.table("data/ucihar/train/subject_train.txt"))

Xtest<-read.table("data/ucihar/test/X_test.txt")
ytest<-unlist(read.table("data/ucihar/test/y_test.txt"))
idtest<-unlist(read.table("data/ucihar/test/subject_test.txt"))

join1.f<- function(set,id,y,X,act=activities,feat=features) {
        
############# Function for performing the tidying of each data set
############# All columns are used, but the ones with names containing string "BandsEnergy"
#############  (train or test)
#############  Depends on dplyr
        
        aux1<- grep("bandsEnergy",feat$V2) #find which rows contains the string 
        nombres<-feat[-aux1,2]  #find the names of reduced data
        aux2<-X[,-aux1]  #use only the columns without the string
        aux2<-aux2%>%mutate(set=set,subject.id=id,act.code=y,act.name=as.character(factor(y,levels=act$code,labels=act$act)))
        #select columns with no repeated names, add set, subject.id, acttivity code, and act.name
        names(aux2)[1:(ncol(aux2)-4)]<-nombres
        aux2  #return result
}

reducedTest<-join1.f("test",idtest,ytest,Xtest)
reducedTrain<-join1.f("train",idtrain,ytrain,Xtrain)

## Process##

## STEP 1.- 
## Merge the training and the test sets to create one data set into one data.frame called mergedHAR


mergedHAR<-bind_rows(reducedTrain,reducedTest)


## STEP 2:
# Extracts only the measurements on the mean and standard deviation for each measurement. To do this, ones needs to create a vector of names that match either the strings "mean()" or "sd()"

mean.names<-grep("mean\\(\\)",names(mergedHAR),value=TRUE)
std.names<-grep("std\\(\\)",names(mergedHAR),value=TRUE)
reducedNames<-c("set","subject.id","act.code", "act.name",mean.names,std.names)

reducedMergedHAR<-select(mergedHAR,one_of(reducedNames))

# STEP 3.-
# Step 3 and 4 (Uses descriptive activity names to name the activities in the data 
# set and Appropriately labels the data set with descriptive variable names) 
# were done in the preprocessing.

#STEP 4
# For the final step  (step 5) it is needed to  creates a second, independent tidy data set with the average of each variable for each activity and each subject in the data.table created in steps 1 to 4. The names of the data table have been modified to include a **xbarof_** string, to be informative.  
# This data set is then stored in a *txt* file, called "summarizedHAR.txt"

summarizedHAR<- reducedMergedHAR%>% group_by(subject.id,act.name,set)%>%summarize_each(funs(mean))

names(summarizedHAR)[5:ncol(summarizedHAR)]<-paste("xbarof_",names(summarizedHAR)[5:ncol(summarizedHAR)],sep="")

write.table(summarizedHAR,file="summarizedHAR.txt",row.names=FALSE)


# To upload the data to R use read.table command.