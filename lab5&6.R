#5.1
data=read.csv(file="/home/ibab/R/BrainCancer.csv",header=TRUE)
data$sex<- factor(data$sex,levels=c("Male","Female"))
is.factor(data$sex)
#5.2
print(nlevels(data$sex))
#5.3
print(levels(data$Diagnosis))

#6.1
num_rows <- nrow(data)
temperature_factor <- gl(n = 3, k = ceiling(num_rows / 3), length = num_rows, labels = c("Hot", "Cold", "Lukewarm"))
print(head(temperature_factor))
#6.2
brain_cancer_new <- data  
brain_cancer_new$Temperature <- temperature_factor  
print(brain_cancer_new)  

#7.1
tapply(data$gtv, data$ki, mean)
#7.2
tapply(data$gtv, data$ki, mean, trim=0.1)


#8
print(pmin(data$gtv,data$time,data$ki))
print(pmax(data$gtv,data$time,data$ki))
#x1<- c(1,2,9,7,10)
#x2<- c(4,76,64)
#print(pmin(x1,x2))

#9.1
#differnence between rank, sort, and order
ranks<- rank(data$gtv)
sorted<- sort(data$gtv)
ordered<- order(data$gtv)
view<- data.frame(data$gtv, ranks, sorted, ordered)
print(view)
#9.2
print(data$diagnosis[ordered])
ordered_data <- data.frame(gtv = data$gtv[ordered], diagnosis = data$diagnosis[ordered])
write.csv(ordered_data, "lab4_ordered_data.csv", row.names = FALSE)
print(head(ordered_data))

#10.1
filter1=data[1:6,3:8]
print(filter1)
#10.2
filter2=as.matrix(filter1)
print(attributes(filter2))
#10.3
newcol=data$ki+data$gtv+data$time
#10.4
newcoladded=data.frame(data,newcol)
print(newcoladded)
#10.5
newcol <- data$ki + data$gtv + data$time
newcoladded2 <- cbind(data, newcol)
print(newcoladded2)
#10.6
new_rows <- data[c(26:35),] 
newdata <- rbind(data, new_rows)
print(newdata)


#11
X <- matrix(c(1,0,2,5,3, 1,1,3,1,3, 3,1,0,2,2, 1,0,2,1,0), nrow=4, byrow=TRUE)
print(rownames(X)) 
print(colnames(X))
rownames(X) <- paste("Trial", 1:4, sep=" ")
colnames(X) <- c("aspirin", "paracetamol", "nurofen", "hedex", "placebo")
print(X)

dimnames(X) <- list(paste("Trial", 1:4, sep=" "), paste("drug", 1:5, sep=" "))
print(X)


#12.1
mean(X[,5])
#12.2
var(X[4,])
#12.3
rowSums(X) 
apply(X,1,sum)
#12.4
colSums(X)
apply(X,2,sum)
#12.5
rowMeans(X)
apply(X,1,mean)
#12.6
colMeans(X)
apply(X,2,mean)
#12.7
group <- c("A", "B", "B", "A")
rowsum_result <- rowsum(X, group)
print(rowsum_result)
print(row(X))  
print(col(X))
tapply_result <- tapply(X, list(group[row(X)], col(X)), sum)
print(tapply_result)
aggregate_result <- aggregate(X, list(group), sum)
print(aggregate_result)
#12.8
apply(X,2,sample)
#12.9
X <- rbind(X, apply(X,2,mean))
X <- cbind(X,apply(X,1,var))
headings <- c(paste("drug.",1:5,sep=""),"var") 
dimnames(X)<- list(NULL,headings) 
headings <- c(paste("Trial-",1:4,sep=''),"Mean") 
rownames(X) <- headings
print(X)


#13
#sweep() function
#to perform sweep action
data=read.csv("/home/ibab/R/BrainCancer.csv", header=TRUE)
eg_sweep = data.frame(data$ki,data$gtv,data$time)
cols <- apply(eg_sweep,2,mean)
print(cols)

cols.means <- matrix(rep(cols,rep(dim(eg_sweep)[1],dim(eg_sweep)[2])),
                     nrow=dim(eg_sweep)[1])
print(cols.means)
eg_sweep_alt <- eg_sweep - cols.means
print("Method 1")
print(eg_sweep_alt)

eg_sweep_alt2 <- sweep(eg_sweep,2,cols)
print("Method 2")
print(eg_sweep_alt2)

#sapply() used for vectors
eg_sapply <- sapply(3:7,seq)
print(attributes(eg_sapply))


#14:
data <- read.table("/home/ibab/Downloads/pgfull.txt", header = TRUE)
species <- data[, 1:54]
max_indices <- max.col(species)
print(max_indices)
species_names <- names(species)[max_indices]
print(species_names)
species_freq <- table(species_names)
print(species_freq)
min_indices <- max.col(-species)  #Negating the values in 'species'
print(min_indices)
#alt method:
min_indices <- apply(species, 1, which.min)
print(min_indices)


#15
apples <- c(4,4.5,4.2, 5.1,3.9)
oranges <- c(TRUE,TRUE,FALSE)
chalk <- c("limestone","marl","oolite","CaCO3")
cheese <- c(3.2-4.5i,12.8+2.2i)
items <- list(apples, oranges, chalk, cheese)
print(items)
#subscripts on lists have double square brackets
print(items[[3]])
print(items[[3]][3])

items[3] #this works
items[3][3] #this doesnt
print(names(items))
items <- list(first=apples,second=oranges,third=chalk,fourth=cheese)
print(items$fourth)
print(class(items))
print(lapply(items,length))
print(lapply(items,class))
print(lapply(items,mean))



