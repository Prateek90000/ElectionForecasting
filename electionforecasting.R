rm (list=ls())


install.packages("maps")
install.packages("devtools")
install.packages("ggmap")

#load libraries
library(maps)
library(devtools)  
library(ggmap)

register_google(key = "AIzaSyBlCZXGDK9dN3Vf_N1qdI6mPfFFCA34ubs")

#load the map
statesMap = map_data("state")

# Output structure 
str(statesMap)

z = table(statesMap$group)
table (z)

#Draw the map

ggplot(statesMap, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "white", color = "black") 

# Read data 
polling = read.csv("PollingImputed.csv")

# Split the data
Train = subset(polling, Year == 2004 | Year == 2008)
Test = subset(polling, Year == 2012)

colnames(Train)
# Logistic Regression
mod2 = glm(Republican~SurveyUSA+DiffCount + Rasmussen + PropR, data=Train, family="binomial")
# Make predictions
TestPrediction = predict(mod2, newdata=Test, type="response")
binaries = TestPrediction>0.5
ZerOnes = as.numeric(binaries)

pred = data.frame(Test$State,TestPrediction,binaries,ZerOnes)

pred$unclear = pred$TestPrediction>0.1 & pred$TestPrediction<0.9 
