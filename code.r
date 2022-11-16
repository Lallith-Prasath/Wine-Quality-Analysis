wine = read.csv("C:\\Users\\lalli\\Downloads\\whiteWine.csv",header = TRUE, sep = ",")
wine
library("dplyr")
distinct(wine)
View(wine)
write.csv(wine,"C:\\Users\\lalli\\Downloads\\whiteWineC.csv", row.names = FALSE)
dim(wine)
str(wine)
summary(wine)
# 3.2. Plotting the marginal distributions of key numerical quantities of interest
p1 <- ggplot(wine, aes(x=fixed.acidity)) +geom_density()
p1 + geom_vline(aes(xintercept=mean(fixed.acidity)),color="red", linetype="dashed", size=1)+ggtitle("Fixed Acidity vs Density")+labs(x="Fixed Acidity",y="Density")
49
p2 <- ggplot(wine, aes(x=volatile.acidity)) + geom_density()
p2 + geom_vline(aes(xintercept=mean(volatile.acidity)),color="blue", linetype="dashed", size=1)+ggtitle("Volatile Acidity vs Density")+labs(x="Volatile Acidity",y="Density")
p3 <- ggplot(wine, aes(x=citric.acid)) + geom_density()
p3 + geom_vline(aes(xintercept=mean(citric.acid)),color="green", linetype="dashed", size=1)+ggtitle("Citric Acid vs Density")+labs(x="Citric Acid",y="Density")
p4 <- ggplot(wine, aes(x=residual.sugar)) + geom_density()
p4 + geom_vline(aes(xintercept=mean(residual.sugar)),color="blue", linetype="dashed", size=1)+ggtitle("Fixed Acidity vs Density")+labs(x="Residual Sugar",y="Density")
p5 <- ggplot(wine, aes(x=chlorides)) + geom_density()
p5 + geom_vline(aes(xintercept=mean(chlorides)),color="blue", linetype="dashed", size=1)+ggtitle("Chlorides vs Density")+labs(x="Chlorides",y="Density")
p6 <- ggplot(wine, aes(x=free.sulfur.dioxide)) + geom_density()
p6 + geom_vline(aes(xintercept=mean(free.sulfur.dioxide)),color="blue", linetype="dashed", size=1)+ggtitle("Suplur Dioxide vs Density")+labs(x="SuplurDioxide",y="Density")
p7 <- ggplot(wine, aes(x=total.sulfur.dioxide)) + geom_density()
p7 + geom_vline(aes(xintercept=mean(total.sulfur.dioxide)),color="blue", linetype="dashed", size=1)+ggtitle("Total Sulphur Dioxide vs Density")+labs(x="Total Sulphur Dioxide",y="Density")
p8 <- ggplot(wine, aes(x=density)) + geom_density()
p8 + geom_vline(aes(xintercept=mean(density)),color="blue", linetype="dashed", size=1)+ggtitle("Density vs Density")+labs(x="Density",y="Density")
50
p9 <- ggplot(wine, aes(x=pH)) + geom_density()
p9 + geom_vline(aes(xintercept=mean(pH)),color="blue", linetype="dashed", size=1)+ggtitle("pH vs Density")+labs(x="pH",y="Density")
p10 <- ggplot(wine, aes(x=sulphates)) + geom_density()
p10 + geom_vline(aes(xintercept=mean(sulphates)),color="blue", linetype="dashed", size=1)+ggtitle("Sulphates vs Density")+labs(x="Sulphates",y="Density")
p11 <- ggplot(wine, aes(x=alcohol)) + geom_density()
p11 + geom_vline(aes(xintercept=mean(alcohol)),color="blue", linetype="dashed", size=1)+ggtitle("Alcohol vs Density")+labs(x="Alcohol",y="Density")
install.packages("ggpubr")
library("ggpubr")
ggarrange(p1, p2, p3, p4, nrow = 2, ncol =2)
ggarrange(p5, p6, p7, p8, nrow = 2, ncol =2)
ggarrange(p9, p10, p11, nrow = 2, ncol =2)
b1 <- boxplot(wine$fixed.acidity, col="slategray2", pch=19,main="Fixed Acidity")
b2 <- boxplot(wine$volatile.acidity, col="slategray2", pch=19,main="Volatitle Acidity")
b3 <- boxplot(wine$citric.acid, col="slategray2", pch=19,main="Citric Acid")
b4 <- boxplot(wine$residual.sugar, col="slategray2", pch=19,main="Residual Sugar")
b5 <- boxplot(wine$chlorides, col="slategray2", pch=19,main="Chlorides")
b6 <- boxplot(wine$free.sulfur.dioxide, col="slategray2", pch=19,main="Free Suplhur Dioxide")
b7 <- boxplot(wine$total.sulfur.dioxide, col="slategray2", pch=19,main="Total Sulphur Dioxide")
51
b8 <- boxplot(wine$density, col="slategray2", pch=19,main="Density")
b9 <- boxplot(wine$pH, col="slategray2", pch=19,main="pH")
b10 <- boxplot(wine$sulphates, col="slategray2", pch=19,main="Sulphates")
b11 <- boxplot(wine$alcohol, col="slategray2", pch=19,main="Alcohol")
## 3.3. Inspecting quality against numerical variables of interest
g1 <- ggplot(wine, aes(factor(quality), fixed.acidity, fill=factor(quality))) + geom_boxplot() +labs(x = "quality", y = "fixed.acidity", title = "Boxplot of Quality vs. fixed.acidity") + theme(legend.position = 'none', plot.title = element_text(size = 9, hjust=0.5))
g1
g2 <- ggplot(wine, aes(factor(quality), volatile.acidity, fill=factor(quality))) + geom_boxplot() +labs(x = "quality", y = "volatile.acidity", title = "Boxplot of Quality vs. volatile.acidity") + theme(legend.position = 'none', plot.title = element_text(size = 9, hjust=0.5))
g2
g3 <- ggplot(wine, aes(factor(quality), citric.acid, fill=factor(quality))) + geom_boxplot() +labs(x = "quality", y = "citric.acid", title = "Boxplot of Quality vs. citric.acid") + theme(legend.position = 'none', plot.title = element_text(size = 9, hjust=0.5))
g3
g4 <- ggplot(wine, aes(factor(quality), residual.sugar, fill=factor(quality))) + geom_boxplot() +labs(x = "quality", y = "residual.sugar", title = "Boxplot of Quality vs. residual.sugar") + theme(legend.position = 'none', plot.title = element_text(size = 9, hjust=0.5))
g4
52
ggarrange(g1, g2, g3, g4, nrow = 2, ncol =2)
# It seems there’s positive relationship between citric acid and quality
# It seems there’s negative relationship between volatile acidity and quality
g5 <- ggplot(wine, aes(factor(quality), chlorides, fill=factor(quality))) + geom_boxplot() +labs(x = "Quality", y = "chlorides", title = "Boxplot of Quality vs. chlorides") + theme(legend.position = 'none', plot.title = element_text(size = 9, hjust=0.5))
g5
g6 <- ggplot(wine, aes(factor(quality), free.sulfur.dioxide, fill=factor(quality))) + geom_boxplot() +labs(x = "quality", y = "free.sulfur.dioxide", title = "Boxplot of quality vs. free.sulfur.dioxide") + theme(legend.position = 'none', plot.title = element_text(size = 9, hjust=0.5))
g6
g7 <- ggplot(wine, aes(factor(quality), total.sulfur.dioxide, fill=factor(quality))) + geom_boxplot() +labs(x = "quality", y = "total.sulfur.dioxide", title = "Boxplot of quality vs. total.sulfur.dioxide") + theme(legend.position = 'none', plot.title = element_text(size = 9, hjust=0.5))
g7
g8 <- ggplot(wine, aes(factor(quality), density, fill=factor(quality))) + geom_boxplot() +labs(x = "quality", y = "density", title = "Boxplot of quality vs. density") + theme(legend.position = 'none', plot.title = element_text(size = 9, hjust=0.5))
g8
ggarrange(g5, g6, g7, g8, nrow = 2, ncol =2)
# It seems there’s negative relationship between density acid and quality
g9 <- ggplot(wine, aes(factor(quality), pH, fill=factor(quality))) + geom_boxplot() +labs(x = "quality", y = "pH", title = "Boxplot of Quality vs. pH") + theme(legend.position = 'none', plot.title = element_text(size = 9, hjust=0.5))
g9
53
g10 <- ggplot(wine, aes(factor(quality), sulphates, fill=factor(quality))) + geom_boxplot() +labs(x = "quality", y = "sulphates", title = "Boxplot of quality vs. sulphates") + theme(legend.position = 'none', plot.title = element_text(size = 9, hjust=0.5))
g10
g11 <- ggplot(wine, aes(factor(quality), alcohol, fill=factor(quality))) + geom_boxplot() +labs(x = "quality", y = "alcohol", title = "Boxplot of quality vs. alcohol") + theme(legend.position = 'none', plot.title = element_text(size = 9, hjust=0.5))
g11
ggarrange(g9, g10, g11, nrow = 2, ncol =2)
# 3.4. Inspecting 3D
i1<- ggplot(wine, aes(x=factor(round(alcohol)), y=citric.acid)) + geom_boxplot(aes(colour = factor(quality))) +labs(title="Alcohol + Citric.Acid vs. Quality") + theme(plot.title=element_text(hjust=0.5))
i1
i2 <- ggplot(wine, aes(x=factor(round(alcohol)), y=volatile.acidity)) + geom_boxplot(aes(colour = factor(quality))) +labs(title="Alcohol + Volatile.Acidity vs. Quality") + theme(plot.title=element_text(hjust=0.5))
i2
i3 <- ggplot(wine, aes(x=factor(round(alcohol)), y=chlorides)) + geom_boxplot(aes(colour = factor(quality))) +labs(title="Alcohol + Chlorides vs. Quality") + ylim(0, 0.3)+theme(plot.title=element_text(hjust=0.5))
i3
54
#MODELLING
# 4. Modeling
## 4.1. Modeling with top 5 variables
## 4.1. Model 1 with top 5 highest correlation with TotalIncidents
lm0 <- lm(quality ~ alcohol + volatile.acidity + sulphates + citric.acid + total.sulfur.dioxide, data = wine)
summary(lm0)
lm1 <- lm(quality ~ alcohol + volatile.acidity + sulphates + total.sulfur.dioxide, data = wine)
summary(lm1)
lm0 <- lm(quality ~ alcohol + volatile.acidity + sulphates + citric.acid + total.sulfur.dioxide, data = wine)
summary(lm0)
lm1 <- lm(quality ~ alcohol + volatile.acidity + sulphates + total.sulfur.dioxide, data = wine)
summary(lm1)
#Using CV
# Define training control
set.seed(123)
train.control<- trainControl(method = "cv", number = 10) #cv Cross-Validation
# Train the model
55
model1 <- train(quality ~ alcohol + volatile.acidity + sulphates + total.sulfur.dioxide, data = wine, method = "lm", trControl = train.control)
# Summarize the results
summary(model1)
print(model1)
## 4.2. LASSO
library(glmnet)
x <- model.matrix(quality~., wine)[,-1]
y <- wine$quality
mod <- cv.glmnet(as.matrix(x), y, alpha=1)
#To see the coefficients with the minimum cross-validation error
#To see the coefficients with the largest value of lambda such that error is within 1 standard error of the minimum:
as.matrix(coef(mod, mod$lambda.min))
as.matrix(coef(mod, mod$lambda.1se))
#You can also select any other value of lambda that you want. Coefficients that are 0 have been dropped out of the model
CF <- as.matrix(coef(mod, mod$lambda.1se))
CF[CF!=0,]
56
#Using Model 2 using above independent variables
lm2 <- lm(quality ~ fixed.acidity + volatile.acidity + chlorides + total.sulfur.dioxide + sulphates + alcohol, data=wine)
summary(lm2)
#Using CV
# Define training control
set.seed(123)
train.control<- trainControl(method = "cv", number = 10) #cv Cross-Validation
# Train the model
model2<- train(quality ~ fixed.acidity + volatile.acidity + chlorides + total.sulfur.dioxide + sulphates + alcohol,data = wine, method = "lm",
               trControl = train.control)
# Summarize the results
summary(model2)
print(model2)
## 4.3 Random Forest Model 3
library(randomForest)
library(mlbench)
library(caret) # use createDataPartition() function
# partition
#Create Evaluation Sets
set.seed(123)
n = nrow(wine)
trainIndex = sample(1:n, size = round(0.7*n), replace=FALSE)
57
#Creates training and test set from observations
training = wine[trainIndex,]
testing = wine[-trainIndex,]
model3 <- randomForest(quality ~ ., training, mtry = 3,
                       importance = TRUE, na.action = na.omit)
print(model3)
#Plot the error vs the number of trees graph
plot(model3)
varImp(model3)
varImpPlot(model3,type=2)
# obtain MSE as of last element in fit$mse
# which should match the output from printout
model3$mse[length(model3$mse)]
# take square root to calculate RMSE for the model
sqrt(model3$mse[length(model3$mse)])
# now illustrate how to calculate RMSE on test data vs. training data
predValues<- predict(model3,testing)
# we can calculate it directly
sqrt(mean((testing$quality -predValues)^2)) #RMSE
mean(abs(testing$quality -predValues)) #MAE
58
#Evaluation dataframe
Model <- c("Model 1", "Model 2", "Model 3")
R_squared<- c(0.3479, 0.3546, 0.4850)
RMSE <- c(0.6549, 0.6515, 0.5843)
MAE <- c(0.5092899, 0.5063, 0.4222)
ml <- data.frame(Model, R_squared, RMSE, MAE)
#Plot
library(gridExtra)
p1 <- ggplot(ml, aes(Model, RMSE)) + geom_point(aes(colour = factor(Model), size = 4)) + labs(title="RMSE") + theme(plot.title=element_text(hjust=0.5), axis.title.y = element_blank(),axis.title.x = element_blank(), legend.position="none")
p2 <- ggplot(ml, aes(Model, R_squared)) + geom_point(aes(colour = factor(Model), size = 4)) + labs(title="R-Squared") + theme(plot.title=element_text(hjust=0.5), axis.title.y = element_blank(),axis.title.x = element_blank(), legend.position="none")
p3 <- ggplot(ml, aes(Model, MAE)) + geom_point(aes(colour = factor(Model), size = 4)) + labs(title="MAE") + theme(plot.title=element_text(hjust=0.5), axis.title.y = element_blank(),axis.title.x = element_blank(), legend.position="none")
grid.arrange(p2,p1,p3, ncol=3)
## 4.4. Clustering
#Cleaning data
df_new<- wine
59
df_new$quality_group<- ifelse(df_new$quality< 5, "1", ifelse((df_new$quality>= 5) & (df_new$quality<= 6), "2", "3"))
df_new[,12]<-NULL # Order column looks like meaningless
str(df_new)
df_new$quality_group<- as.numeric(df_new$quality_group)
str(df_new)
View(df_new)
library(tidyverse) # data manipulation
library(cluster) # clustering algorithms
library(factoextra) # clustering algorithms & visualization
#Standardize data
scale(df_new)
head(df_new)
#Elbow Method
library(factoextra)
library(NbClust)
set.seed(123)
a1 = fviz_nbclust(df_new, kmeans, method = "wss")
a1
60
#Using K-means
set.seed(123)
km <- kmeans(df_new, 2, nstart = 25)
df_new$cluster = km$cluster
View(df_new)
ggplot(df_new,aes(pH,alcohol,color=as.factor(cluster)))+geom_point()
ggplot(df_new,aes(pH,sulphates,color=as.factor(cluster)))+geom_point()
ggplot(df_new,aes(pH,total.sulfur.dioxide,color=as.factor(cluster)))+geom_point()
ggplot(df_new,aes(sulphates,total.sulfur.dioxide,color=as.factor(cluster)))+geom_point()
ggplot(df_new,aes(alcohol,sulphates,color=as.factor(cluster)))+geom_point()
ggplot(df_new,aes(alcohol,volatile.acidity,color=as.factor(cluster)))+geom_point()