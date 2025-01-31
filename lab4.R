#for the csv file BrainCancer.csv


#1 
data=read.csv("/home/ibab/R/BrainCancer.csv",header=TRUE)
print(data)

#2.a
print(dim(data))
#2.b
print(colnames(data))
#2.c
print(rownames(data))
#2.d
print(head(data,n=30))
#2.e
lapply(data,table)
#2.f
data$sex<- factor(data$sex,levels=c("Male","Female"))
is.factor(data$sex)
data$diagnosis<- factor(data$sex,levels=c("Meningioma","Hg glioma","LG glioma"))
is.factor(data$diagnosis)
data$loc<- factor(data$sex,levels=c("Infratentorial","Supratentorial"))
is.factor(data$loc)
data$stereo<- factor(data$sex,levels=c("SRS","SRT"))
is.factor(data$stereo)

#2.g
print(levels(data$loc))
print(levels(data$sex))
print(levels(data$diagnosis))

#2.h
print(nlevels(data$loc))
print(nlevels(data$sex))
print(nlevels(data$diagnosis))

#3.a
print(mean(data$gtv))
#3.b
print(mean(data$time))
#3.c
print(median(data$gtv))
#3.d
print(which.max(table(data$gtv)))
#3.e
print(sd(data$gtv))
#3.f
print(summary(data$gtv))
#3,g
hist(data$gtv)
#3.h
library(moments)
print(skewness(data$gtv))
#3.i
print(kurtosis(data$gtv))
#3.j
boxplot(data$gtv)
boxplot(data$gtv, range=0.1, horizontal=FALSE, border="darkblue", col= "orange")
boxplot(data$gtv, range=0.2, horizontal=FALSE, border="darkgreen", col= "pink")
boxplot(data$gtv, range=0.5, horizontal=FALSE, border="black", col= "purple")
#3.k
par(mfrow=c(1, 3))
boxplot(data$gtv)
boxplot(data$ki)
boxplot(data$time)

#4.a
gtv_subset <- data[data$gtv > 20, ]
dim(gtv_subset)
print(gtv_subset)

#4.b
filter=data[c( 1,3,8,9,13,14,18,21),]
print(filter)

#4.c
female_indices <- which(data$sex == "Female")
print(female_indices)
female_subset <- subset(data, sex == "Female")
print(female_subset)

#4.d
new_column <- data$gtv * data$ki / 234
new_dataframe <- data.frame(GTV = data$gtv, KI = data$ki, NewColumn = new_column)
print(new_dataframe)

#4.e
female_subset <- subset(data, sex == "Female")
write.csv(female_subset, "/home/ibab/R/lab4_female_BrainCancer.csv", row.names = FALSE)
data2=read.csv("/home/ibab/R/lab4_female_BrainCancer.csv",header=TRUE)
print(data2)





#for the file Heart.csv

#1 
data3=read.csv("/home/ibab/R/Heart.csv",header=TRUE)
print(data3)

#2.a
print(dim(data3))
#2.b
print(colnames(data3))
#2.c
print(rownames(data3))
#2.d
print(head(data3,n=30))
#2.e
lapply(data3,table)
#2.f
data3$ChestPain<- factor(data3$ChestPain,levels=c("typical","asymptomatic","nonanginal"))
is.factor(data$ChestPain)
data3$Thal<- factor(data3$Thal,levels=c("reversable","fixed","normal"))
is.factor(data3$Thal)
data3$AHD<- factor(data3$AHD,levels=c("Yes","No"))
is.factor(data3$AHD)


#2.g
print(levels(data3$ChestPain))
print(levels(data3$Thal))
print(levels(data3$AHD))

#2.h
print(nlevels(data3$ChestPain))
print(nlevels(data3$Thal))
print(nlevels(data3$AHD))

#3.a
print(mean(data3$Chol))
#3.b
print(mean(data3$Chol))
#3.c
print(median(data3$Chol))
#3.d
print(which.max(table(data3$Chol)))
#3.e
print(sd(data3$Chol))
#3.f
print(summary(data3$Chol))
#3,g
hist(data3$Chol)
#3.h
library(moments)
print(skewness(data3$Chol))
#3.i
print(kurtosis(data3$Chol))
#3.j
boxplot(data3$Chol)
boxplot(data3$Chol, range=0.1, horizontal=FALSE, border="darkblue", col= "orange")
boxplot(data3$Chol, range=0.2, horizontal=FALSE, border="darkgreen", col= "pink")
boxplot(data3$Chol, range=0.5, horizontal=FALSE, border="black", col= "purple")
#3.k
par(mfrow=c(1, 3))
boxplot(data3$Chol)
boxplot(data3$RestBP)
boxplot(data3$RestECG)

#4.a
Chol_subset <- data3[data3$Chol > 20, ]
dim(Chol_subset)
print(Chol_subset)

#4.b
filter=data3[c( 1,3,8,9,13,14,18,21),]
print(filter)

#4.c
female_indices <- which(data3$Sex == "0")
print(female_indices)
female_subset <- subset(data3, Sex == "0")
print(female_subset)

#4.d
new_column <- data3$RestBP * data3$RestECG / 234
new_dataframe <- data.frame(RestBP = data3$RestBP, RestECG = data3$RestECG, NewColumn = new_column)
print(new_dataframe)

#4.e
female_subset <- subset(data3, Sex == "0")
write.csv(female_subset, "/home/ibab/R/lab4_female_Heart.csv", row.names = FALSE)
data4=read.csv("/home/ibab/R/lab4_female_Heart.csv",header=TRUE)
print(data4)








