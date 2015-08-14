######## INSTALL NEEDED PACKAGES ###################

library(ggplot2)
install.packages("WriteXLS")
library(WriteXLS)

######################################################

## DATA IS FOUND IN DATA FOLDER ##
attach(Dance.Complete)
write.table(Dance.Complete, "/Users/AnhVuNguyen/Documents/MyData/YoutubeCount 04-16/danceComplete.txt", sep="\t")
WriteXLS("Dance.Complete", "/Users/AnhVuNguyen/Documents/MyData/YoutubeCount 04-16/danceComplete.xlsx",
         AdjWidth = TRUE, BoldHeaderRow = TRUE, row.names = TRUE)
#Most General Plots
plot(~View+Sub+Likes+Dislikes+Duration)
plot(~log(Dance.Complete$View)+log(Sub)+log(Likes)+log(Dislikes) + log(Duration)) #this seems to be a better choice for plot

#Other exploratory plots

plot(log(Duration), log(Likes/Dislikes)) # this would equate to Log(Duration) ~ Log(Likes) - Log(Dislikes), not what we want

/"
  Possible questions to ask include whether the duration of a video has any effect in the Views/Likes/Etc
  Whether there might be a correlation between Likes and Dislikes in this dataset
"/
  
#Likes/Dislikes standardized
#p <- qplot(log(Likes)/log(Dance.Complete$View),log(Dislikes)/log(Dance.Complete$View), xlim = c(0.15, 0.75), ylim = c(-0.31,0.5))

###########################
#Standardized over view
###########################
p <- qplot(log(Likes)/log(Dance.Complete$View),log(Dislikes)/log(Dance.Complete$View))
p + geom_abline(intercept = -0.35)

###########################
#Standardized over duration
###########################
pD <- qplot(log(Likes)/log(Dance.Complete$Duration),log(Dislikes)/log(Dance.Complete$Duration))
pD + geom_abline(intercept = -0.7)

###########################
#View and duration model 
###########################
vd <- ggplot(Dance.Complete, aes(y = log(Dance.Complete$View), x= log(Dance.Complete$Duration))) + geom_point(aes(color=log(Sub)), position = "jitter") 
vd + geom_text(label =Dance.Complete$View, size = 3, hjust =1, vjust = 1) 
vd + geom_abline(aes(intercept = 11.02327, slope = 0.15828), color = "red")  

boxplot(model3$residual, horizontal = TRUE, xlab = "Residual of model [ log(View) ~ log(Duration) ]")

###########################
#View and Date model 
###########################

#reference date origin = "1970-01-01"
months(as.Date(Dance.Complete$Date, origin="1970-01-01"))
boxplotView <- boxplot(log(Dance.Complete$View) ~ months(as.Date(Dance.Complete$Date, origin="1970-01-01")), ylab="Month", xlab="log(View)", col="cadetblue1")

###########################
#Like and Date model 
###########################

#Like seems to be more conscious choice on the user ends. A person might watch it but to actively engaged in the video, they might like it
boxplot(log(Dance.Complete$Likes) ~ months(as.Date(Dance.Complete$Date, origin="1970-01-01")), ylab="Month", xlab="log(Likes)", col="aquamarine", las = 2, horizontal = TRUE)

#estimate distribution of log(likes)
h <-hist(log(Dance.Complete$Likes))
xfit<-seq(min(log(Dance.Complete$Likes)),max(log(Dance.Complete$Likes)),length=400) 
yfit<-dnorm(xfit,mean=mean(log(Dance.Complete$Likes)),sd=sd(log(Dance.Complete$Likes))) 
yfit <- yfit*diff(h$mids[1:2])*length(log(Dance.Complete$Likes)) 
lines(xfit, yfit, col="blue", lwd=2)

###################################################################################################################################
#Exploratory modeling
###################################################################################################################################

###########################
##### FIRST MODEL #########
###########################
model <- lm(log(Dance.Complete$Likes) ~ log(Dance.Complete$Sub) + log(Dance.Complete$Duration), na.action=na.omit)
summary(model)
# even though p-value for Duration is 0.06, the threshold is only 0.05 so we might think again about whether it might be significant
# R-squared is 0.25, very small
# F-stat shows that pvalue is smaller than 0.05.
plot(model)
#Residual versus fitted seems to have a good distribution on top and below zero line
# Q-Q line is straight

############################
##### SECOND MODEL #########
############################
model2 <- lm(log(Dance.Complete$Likes) ~ log(Dance.Complete$Duration), na.action=na.omit)
summary(model2)

############################
##### THIRD MODEL #########
############################
model3 <- lm(log(Dance.Complete$View) ~ log(Dance.Complete$Duration), na.action=na.omit)
summary(model3)
plot(model3)
rstandard(model3) #plenty 

############################
##### FOURTH MODEL #########
############################
model4 <-lm(log(Likes)/log(Dance.Complete$View)~log(Dislikes)/log(Dance.Complete$View), na.action=na.omit)
summary(model4)
