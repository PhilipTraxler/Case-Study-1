---
  title: "Beer Study Notebook"
output: html_notebook
---
  
  
  ```{r}
# Beer Study
library(dlookr)
library(visdat)
library(plotly)
library(missRanger)
library(ggplot2)
library(magrittr)
library(ranger)
library(dplyr)
library(class)
library(caret)
library(e1071)
library(GGally)
library(stringi)
library(stringr)
library(xgboost)
library(car)
library(Matrix)
library(keras)
library(forecast)
library(neuralnet)
```
# THis is to add in all of the librarys we are using for the code.
```{r}
Beers = read.csv("/Users/phili/OneDrive/Documents/Module 8 SMU Masters/Beers.csv")
Breweries = read.csv("/Users/phili/OneDrive/Documents/Module 8 SMU Masters/Breweries.csv")
Breweries = Breweries %>% filter(!is.na(Name))
Beers2 = Beers
JoinedData_Unclean = left_join(Beers2,Breweries,bu = c("Brewery_id" = "Brew_ID"))
Beers2$IBU = imputate_na(Beers,IBU,ABV, method = "mice") # Filling in Missing IBU
Beers2$ABV = imputate_na(Beers,ABV,IBU, method = "mice") # Filling in Missing ABV
JoinedData = left_join(Beers2,Breweries,by = c("Brewery_id" = "Brew_ID"))

# Here we are joining in the data to then determine what our block of combined data will look like.
```
# This is to load, join, and fill in missing data from both the breweries and the beers data set.We had used the imputate.na() function to fill in the missing data with projections as opposed to random numbers.

```{r}
head(JoinedData, 6)
tail(JoinedData, 6)
plot_na_pareto(JoinedData, only_na = TRUE)
plot_na_intersect(JoinedData)
vis_miss(JoinedData)
```
# We are now looking to make sure that we didn't miss any of the data points and we have a solid block of data.
###Exploratory Data Analysis

#Adding Regions to Data frame
South = c(" TX"," OK"," AR"," LA"," MS"," AL"," GA"," FL"," TN"," KY"," WV"," VA"," NC"," SC", " MD"," DE")
Northeast = c(" ME"," NH"," VT"," MA"," CT"," RI"," NY"," NJ"," PA", " DC")
Midwest = c(" OH"," MI"," IL"," IN"," WI"," MN"," IA"," MO"," ND"," SD"," NE"," KS")
West = c(" AK"," HI"," WA"," OR"," CA"," NV"," ID"," UT"," AZ"," NM"," CO"," WY"," MT")

#Adding Regions to Medians
for (i in 1:nrow(Medians)) {
  
  if (Medians[i,1] %in% South){
    Medians$Region[i]<- "South"
  } 
  
  if (Medians[i,1] %in% Northeast){
    Medians$Region[i]<- "Northeast"
  } 
  
  if (Medians[i,1] %in% Midwest){
    Medians$Region[i]<- "Midwest"
  } 
  
  if (Medians[i,1] %in% West){
    Medians$Region[i]<- "West"
  } 
}

#Adding Regions to the Joined Data
for (i in 1:nrow(JoinedData)) {
  if (JoinedData[i,10] %in% South){
    JoinedData$Region[i]<- "South"
  }
  if (JoinedData[i,10] %in% Northeast){
    JoinedData$Region[i]<- "Northeast"
  }
  if (JoinedData[i,10] %in% Midwest){
    JoinedData$Region[i]<- "Midwest"
  }
  if (JoinedData[i,10] %in% West){
    JoinedData$Region[i]<- "West"
  }
}

#Adding Regions to the Breweries
for (i in 1:nrow(Breweries)) {
  if (Breweries[i,4] %in% South){
    Breweries$Region[i]<- "South"
  }
  if (Breweries[i,4] %in% Northeast){
    Breweries$Region[i]<- "Northeast"
  }
  if (Breweries[i,4] %in% Midwest){
    Breweries$Region[i]<- "Midwest"
  }
  if (Breweries[i,4] %in% West){
    Breweries$Region[i]<- "West"
  }
}

##1. Breweries Distribution
#Histogram of Breweries Increasing Frequency
ggplot(Breweries, aes(x= fct_infreq(State), color = Region)) +
  geom_histogram(stat="count") +
  ggtitle("Number of Breweries in the United States") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("State") + ylab("Number of Breweries") +
  geom_text(stat="count", aes(label=..count.., hjust =0.5, vjust=-1.1))

#Histogram by Region
ggplot(Breweries, aes(x = fct_infreq(State), fill = Region)) +
  geom_histogram(stat = "count") +
  ggtitle("Number of Breweries in the United States") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("State") + ylab("Number of Breweries") +
  geom_text(stat = "count", aes(label=..count.., hjust =-.5))+
  coord_flip() +
  facet_grid(Region ~ ., scales = "free", space = "free") +
  theme_wsj()

# Here we are looking at the number of breweries in each state.
MergedDataV2 <- read_excel("Desktop/MergedDataV2.xlsx")
#Variable comparisons
ggpairs(select(MergedDataV2,Avg_ABV,Avg_IBU,Consumer.Expenditure.Alcohol,Retail.Demand.Alcohol,Age.20s,Age.30s,Age.40s), cardinality_threshold = NULL)
ggplot(MergedDataV2, aes(x=Avg_ABV, y=Consumer.Expenditure.Alcohol)) + geom_point()
ggpairs(select(MergedDataV2,Avg_ABV,Age.20s,Age.30s,Age.40s))
ggplot(MergedDataV2, aes(x=Age.20s, y=Avg_IBU)) + geom_point()
ggplot(MergedDataV2, aes(x=Age.30s, y=Avg_IBU)) + geom_point()
```{r}
Medians = JoinedData %>% group_by(State) %>% summarize(median_ABV = median(ABV),median_IBU = median(IBU)) #Defining Medians
Medians %>% ggplot(aes(x = median_ABV, fill = State)) + geom_histogram() #Plotting Medians

# Here is where we are displaying the median ABV and IBU per state. In looking at the bitterness vs the ABV we are able to see a semi strong positive linear correlation.

JoinedData[which.max(JoinedData$IBU),] %>% select(State) #Max IBU by state
JoinedData[which.max(JoinedData$ABV),] %>% select(State) #Max ABV by state
# Here is where we are seeing that OR has the highest IBU and Colorado has the highest ABV.
```
# Now we are looking at the Joined Data to determine which state has the highest IBU and ABV.
```{r}
summary(JoinedData) # Summary of Data
JoinedData %>% ggplot(aes(x = ABV, y = IBU, color = State)) + geom_jitter() + geom_smooth() # Scatterplot
JoinedData %>% select(ABV,IBU) %>% ggpairs() # Deriving Correlation
IPA<-filter(JoinedData, grepl('IPA|(IPA)',Style))#Getting IPAs
Ale = JoinedData %>% filter(str_detect(Style,"Ale"))#Getting Ales
Ale = Ale[!grepl("IPA",Ale$Style),] #filtering out (IPA)
Ale$Category = "Ale"
IPA$Category = "IPA"
CombinedIPA_Ale = rbind(IPA,Ale)
CombinedIPA_Ale$ABV = as.numeric(CombinedIPA_Ale$ABV)
CombinedIPA_Ale$IBU = as.numeric(CombinedIPA_Ale$IBU)
```
# Here we are filting the data to only those classified as IPA and Ale.

```{r}
# KNN Classifier
set.seed(90)
splitPerc = .70
trainIndices = sample(1:dim(CombinedIPA_Ale)[1],round(splitPerc * dim(CombinedIPA_Ale)[1]))
train = CombinedIPA_Ale[trainIndices,]
test = CombinedIPA_Ale[-trainIndices,]
# k = 3
classifications = knn(train[,c(3,4)],test[,c(3,4)],train$Category, prob = TRUE, k = 3)
table(classifications,test$Category)
confusionMatrix(table(classifications,test$Category))
#Optimal Value of K
accs = data.frame(accuracy = numeric(50), k = numeric(50))
for(i in 1:50)
{
  classifications = knn(train[,c(3,4)],test[,c(3,4)],train$Category, prob = TRUE, k = i)
  table(classifications,test$Category)
  CM = confusionMatrix(table(classifications,test$Category))
  accs$accuracy[i] = CM$overall[1]
  accs$k[i] = i
}
plot(accs$k,accs$accuracy, type = "l", xlab = "k")
max(accs$accuracy)
```

# Using the KNN classifier to determine how many data points to aggregate in order to predict the classification of a new beer.
#In addition to understanding the KNN, we have found that we only needd 5 other beers to determine the correct classification of the new beer.
```{r}
# MLR
JoinedData2 = JoinedData %>% select(Ounces, ABV, IBU, Category)
fit = lm(ABV + Category + Ounces + IBU, data = AllData) # fitting the model
summary(fit) 
coefficients = coefficients(fit) #Building Coefficients
confit = confint(fit, level = 0.99) #Confidence interval
fitted = fitted(fit) # Fitting the results
residuals = residuals(fit) # Determining Residuals
anova = aov(fit) # Anova analysis of Model
vcov = vcov(fit) # Variable covariance of model
influence = influence(fit) #Determining influence of variables
plot(anova(fit)) # Plotting the anova values
plot(fitted(fit)) +xlab("Residual Distance") +ylab("Number of Occurances") +ggtitle("Residual Results")
```
# This has shown us that the ABV, Ounces, and IBU have been the most predicitve in determining the category (IPA, Ale, or Other) type of beer.