#You must predict the total count of bikes rented during each hour covered by the test set, 
#using only information available prior to the rental period.
library(dplyr)
library(ggplot2)
library(ggthemes)
library(corrgram)
library(corrplot)

bike <- read.csv('bikeshare.csv')

pl.bike <- ggplot(bike, aes(x = temp, y = count)) + geom_point(alpha = 0.5, aes(color = temp))

#Plot count versus datetime as a scatterplot with a color gradient based on temperature. 
#You'll need to convert the datetime column into POSIXct before plotting.
bike$datetime <- as.POSIXct(bike$datetime)

pl.bike2 <- ggplot(bike, aes(x = datetime, y = count)) + geom_point(alpha = 0.5, (aes(color = temp))) + scale_color_gradient(low = 'green', high = 'red')

#What is the correlation between temp and count?
cor(bike[, c('temp','count')])

#Create a boxplot to look at seasonal data. y axis = count and x axis for each season
pl.seasons <- ggplot(bike, aes(x = factor(season), y = count)) + geom_boxplot(aes(color = factor(season)))

#Create an "hour" column that takes the hour from the datetime column. 
#You'll probably need to apply some function to the entire datetime column and reassign it. Hint below:
#time.stamp <- bike$datetime[4]
#format(time.stamp, "%H")

get.hour <- function(x){
  format(x, "%H")
}
#make a function to get the hour

bike$hour <- sapply(bike$datetime, get.hour) #apply the function to all rows of the datetime column of bike

#create a scatterplot of count vs hour for working days only
pl.count.v.hour <- ggplot(filter(bike, workingday==1), aes(x = hour, y = count)) + geom_point(aes(color = temp), position = position_jitter(w=1, h=0)) + scale_color_gradientn(colors=c('blue','green','yellow','red'))
pl.count.v.hourN <- ggplot(filter(bike, workingday==0), aes(x = hour, y = count)) + geom_point(aes(color = temp), position = position_jitter(w=1, h=0)) + scale_color_gradientn(colors=c('blue','green','yellow','red'))

temp.model <- lm(count ~ temp, data = bike)
summary(temp.model)

#How many bike rentals would we predict if the temperature was 25 degrees Celsius? Calculate this two ways:
#Using the values we just got above
print(25*9.1705 + 6.0462)

#Using the predict() function
temp.test <- data.frame(temp=c(25)) #making a data frame with just the value 25
predict(temp.model, temp.test)

#Use sapply() and as.numeric to change the hour column to a column of numeric values.
bike$hour <- sapply(bike$hour, as.numeric)

model2 <- lm(count ~ . -atemp -casual -registered -datetime, data = bike) #new model excluding (using the "-") certain variables