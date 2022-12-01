


library("readxl")
#airbnb_original=read_excel("/Users/demichen/Desktop/CIS9660/airbnb.xlsx")
airbnb_original=read.csv(file.choose(),header=T, stringsAsFactors=TRUE)


summary(airbnb_original)
str(airbnb_original)

#1. EDA

#check and drop duplicate listings
dups = airbnb_original[c(-2)]
airbnb_original = airbnb_original[!duplicated(dups),]

#Deleting Unnecessary fields/Columns
dropfiels=c("Number of Records","Review Scores Rating (bin)",'Name',
            "Host Id", "Zipcode")
airbnb=airbnb_original[ , !(names(airbnb_original) %in% dropfiels)]
names(airbnb) <- gsub(" ", "_", names(airbnb))

#Add column host_year
airbnb$Host_Since <-  as.Date(as.POSIXct(airbnb$Host_Since,'GMT'))
airbnb["Host_Year"] <- 2015 - as.numeric(format(airbnb$Host_Since,'%Y')) +1
airbnb = airbnb[, 2:9]

#PropertyType Ouliers
table(airbnb$Property_Type)
DropValue=c("Apartment","Bed & Breakfast","House","Loft","Townhouse")
airbnb= subset(airbnb,Property_Type %in% DropValue)


#Removing outliers in the response "Price"
boxplot(airbnb$Price, plot=TRUE)$out
outliers= boxplot(airbnb$Price, plot=FALSE)$out
airbnb=airbnb[-which(airbnb$Price %in% outliers),]


#Converting Variable into Factors
airbnb$Neighbourhood=as.factor(airbnb$Neighbourhood)
airbnb$Property_Type=as.factor(airbnb$Property_Type)
airbnb$Room_Type=as.factor(airbnb$Room_Type)

contrasts(airbnb$Neighbourhood)
contrasts(airbnb$Property_Type)
contrasts(airbnb$Room_Type)

#examine which column has missing values
missing_value = rep(0, 8)
for (i in 1:8)
{missing_value[i] = sum(is.na(airbnb[i]))}

#pairs(airbnb)

#Deleting all blank values
airbnb = na.omit(airbnb) 
str(airbnb)
quant = airbnb[-c(1:3)]
pairs(quant)

attach(airbnb)

plot(Neighbourhood , Price , col ="red", varwidth = T,
     xlab="Borough", ylab ="Price", main="Borough vs. Price")

plot(Room_Type , Price , col ="red", varwidth = T,
     xlab=" Room Type ", ylab ="Price ", main="Room Type vs. Price")

plot(Property_Type , Price , col ="red", varwidth = T,
     xlab=" Property Type ", ylab ="Price ", main="Property Type vs. Price")

#airbnb$Host_Since <-  as.Date(as.POSIXct(airbnb$Host_Since,'GMT'))
#airbnb["Host_Since_Year"] <- as.numeric(format(airbnb$Host_Since,'%Y'))
#dropfiels=c("Host_Since")
#airbnb=airbnb[ , !(names(airbnb) %in% dropfiels)]

#2.1 Regression Tree
library(tree)

#The model should only include quantitative variables as tree is binary only
set.seed(1)
train=sample(nrow(airbnb),nrow(airbnb)*0.8)
airbnb.train=airbnb[train,]
tree.airbnb=tree(Price~.,airbnb,subset =train)
plot(tree.airbnb)
text(tree.airbnb)
#The tree has 5 terminal nodes

airbnb.test=airbnb[-train,"Price"]
Price.test=Price[-train]
cv.airbnb=cv.tree(tree.airbnb,K=10) 
plot(cv.airbnb$size, cv.airbnb$dev, type = "b")

prune.airbnb=prune.tree(tree.airbnb,best=6)
plot(prune.airbnb)
text(prune.airbnb,pretty=0)

tree.pred=predict(prune.airbnb,airbnb[-train,])

TREEMSE=lapply((tree.pred-airbnb.test)^2, mean)


#2.2 Random Forest
library(randomForest)

set.seed(1)
rf.airbnb=randomForest(Price~., data=airbnb,
                       subset=train,mtry=2,importance=TRUE)
yhat.rf=predict(rf.airbnb,newdata=airbnb[-train,])

RFMSE=lapply((yhat.rf-airbnb.test)^2, mean)  

importance(rf.airbnb)
#the most significat variables are: Room_Type, Neighbourhood, Beds

varImpPlot(rf.airbnb)


#2.3: Linear Regression

#All Variables
LMModel1=lm(Price~.,data= airbnb)
summary(LMModel1)
plot(predict(LMModel1), residuals(LMModel1))
plot(predict(LMModel1), rstudent(LMModel1))
M1Train=lm(Price~.,data=airbnb.train)
summary(M1Train)
Price.predictM1=predict(M1Train,airbnb[-train,])
LRMSE2=lapply((airbnb[-train,"Price"]-Price.predictM1)^2, mean)  


#Using Vairables from Random Forest importance()
LMModel2=lm(Price~Beds+Review_Scores_Rating+
              Number_Of_Reviews ,data=airbnb)
summary(LMModel2)
plot(predict(LMModel2), residuals(LMModel2))
plot(predict(LMModel2), rstudent(LMModel2))
M2Train=lm(Price~Room_Type+Beds+Neighbourhood+Review_Scores_Rating,data=airbnb.train)
Price.predictM2=predict(M2Train,airbnb[-train,])
LRMSE=lapply((airbnb[-train,"Price"]-Price.predictM2)^2, mean)  



#Conclusion
LRMSE ##Predictors from RandomForest
LRMSE2 #All Predictors
LRMSE3 #All predictors (excpt for insignificat variable)
TREEMSE 
RFMSE


#Just for curiosity'

M3=lm(Price~Neighbourhood+Property_Type+Room_Type+Beds+Number_Of_Reviews+Review_Scores_Rating,data=airbnb)
summary(M3)
M3Train=lm(Price~Neighbourhood+Property_Type+Room_Type+Beds+Number_Of_Reviews+Review_Scores_Rating,data=airbnb.train)
Price.predictM3=predict(M3Train,airbnb[-train,])
LRMSE3=lapply((airbnb[-train,"Price"]-Price.predictM3)^2, mean)  
