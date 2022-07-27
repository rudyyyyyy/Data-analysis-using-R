
data=read.csv("dataset.csv",header=FALSE,sep = ",")
View(data)
# Display first 6 values to get an idea of the dataset
head(data)



#Graphical analysis
#Scatter plot with line of best fit
scatter.smooth(x=data$V1, y=data$V2, main="y versus x",xlab = "x",ylab="y")
cor(data$V1,data$V2)

# Check density if its normal distribution or not

polygon(density(data$V1),col="red")
polygon(density(data$V2),col="yellow")


#Building a Linear model(straight line)

plot(data$V1,data$V2)
abline(lm(data$V2~data$V1))
linmod=lm(data$V2~data$V1)
summary(linmod)
boxplot(linmod$residuals)
hist(linmod$residuals)

#Building a quadratic Model

V3=data$V1^2
data=cbind(data,V3)
quadmod=lm(V2~V1+V3,data=data)
summary(quadmod)
boxplot(quadmod$residuals)
hist(quadmod$residuals)

plot(data$V3,data$V2)
abline(quadmod,col="red")
plot(quadmod)



anova(linmod,quadmod)

predquad=predict(quadmod)




data=cbind(data,predquad)

predlin=predict(linmod)
data=cbind(data,predlin)
plot(data$V1,data$V2,main="Quadratic Model")
lines(predquad)
lines(predlin,col="red")



#building a logarithmic model

logmod=lm(log(data$V2)~data$V1)
summary(logmod)
boxplot(logmod$residuals)

hist(logmod$residuals)

