data=read.csv(file="D:/ACADEMIC/TEACHING/TEACHING_IN_IBAB/FY2024-2025/BDBP209/Labs/DATA/BrainCancer.csv", header=TRUE)
#print(data$sex)
print(class(data$sex))
data$sex <- factor(data$sex,levels=c("Male","Female"))
print(class(data$sex))
print(is.factor(data$sex))
print(nlevels(data$sex))
print(levels(data$sex))

#generating levels using gl() function
print(gl(4,3))
print(gl(4,3,43))
Temp <- gl(2,2,24,labels=c("Low","High"))
print(Temp)
Soft <- gl(3,8,24,labels=c("Hard","medium","soft"))
print(Soft)
fac_df <- data.frame(Temp,Soft)
print(fac_df)

## pmin, pmax
print(pmin(data$gtv,data$time,data$ki))
print(pmax(data$gtv,data$time,data$ki))
x1 <- c(1,2,9,7,10)
x2 <- c(4,76,54)
#print(pmin(x1,x2))

#diff between rank, sort and order
ranks <- rank(data$gtv)
sorted <- sort(data$gtv)
ordered <- order(data$gtv)
view <- data.frame(data$gtv,ranks,sorted,ordered)
print(view)

#data frame into matrix
filter1=data[1:6,3:8]
filter1mat=as.matrix(filter1)
print(filter1mat)
print(class(filter1mat))
print(mode(filter1mat))
print(attributes(filter1mat))
newcol = data$ki+data$gtv+data$time
newcoladded = data.frame(data,newcol)
print(newcoladded)
print(colnames(newcoladded))
newcoladded2=cbind(data,newcol)
print(colnames(newcoladded2))
filter2=data[c(1,3,8),]
newrowadded=rbind(data,filter2)
print(newrowadded)
print(colnames(newrowadded))

#names of rows and cols
X <- matrix(c(1,0,2,5,3,1,1,3,1,3,3,1,0,2,2,1,0,2,1,0),nrow=4)
print(X)
print(rownames(X))
print(colnames(X))
rownames(X) <- rownames(X,do.NULL=FALSE,prefix='Trial.')
print(rownames(X))
print(X)
drugs <- c("aspirin","paracetamol","nurofen","hedex","placebo")
colnames(X) <- drugs
print(X)
dimnames(X) <- list(NULL,paste("drug",1:5,sep=""))
print(X)

#calculations on rows/cols of a matrix
print(mean(X[,5]))
print(var(X[4,]))
print(rowSums(X)) #method 1 to get sum along rows
print(colSums(X))
print(apply(X,1,sum)) #method 2 to get rowSums
print(apply(X,2,sum))
print(apply(X,2,sqrt))
print(apply(X,2,function(x)x^2+x))
print(rowMeans(X))
print(colMeans(X))
print(apply(X,1,mean))
group=c("A","B","B","A")
print(rowsum(X,group))
print(row(X))
print(col(X))
print(tapply(X,list(group[row(X)],col(X)),sum))
print(aggregate(X,list(group),sum))
print(apply(X,2,sample))

X <- rbind(X,apply(X,2,mean))
print(X)
X <- cbind(X,apply(X,1,var))
print(X)
headings <- c(paste("drug.",1:5,sep=""),"var")
dimnames(X) <- list(NULL,headings)
print(X)
rowheadings <- c(paste("Trial-",1:4,sep=''),"mean")
dimnames(X) <- list(rowheadings,headings)
print(X)

#sweep() function
eg_sweep = data.frame(data$ki,data$gtv,data$time)
#method 1 to perform sweep action
cols <- apply(eg_sweep,2,mean)
print(cols)
col.means <- matrix(rep(cols,rep(dim(eg_sweep)[1],dim(eg_sweep)[2])),nrow=dim(eg_sweep)[1])
print(col.means)
eg_sweep_alt <- eg_sweep - col.means
print(eg_sweep_alt)
eg_sweep_alt2 <- sweep(eg_sweep,2,cols)
print(eg_sweep_alt2)

#sapply() used for vectors
eg_sapply <- sapply(3:7,seq)
print(attributes(eg_sapply))
print(class(eg_sapply))

#use max.col
pgdata <- read.table("D:/ACADEMIC/TEACHING/TEACHING_IN_IBAB/FY2024-2025/BDBP209/JNpractice/pgfull.txt",header=TRUE)
print(pgdata)
print(names(pgdata))
species <- pgdata[,1:54]
print(max.col(species))
print(names(species)[max.col(species)])
print(table(names(species)[max.col(species)]))
print(max.col(-species))

#Lists
apples <- c(4,4.5,2.5,1,3.9)
oranges <- c(TRUE,TRUE,FALSE)
chalk <- c("limestones","marl","oolite",'CaCO3')
cheese <- c(3.2-4.5i,12.8+2.2i)
#data.frame(apples,oranges,chalk)
items <- list(apples,oranges,cheese)
print(items)
print(items[[1]])
print(items[1])
print(items[[1]][2])
print(items[1][2])
print(names(items))
items <- list(first=apples,second=oranges,third=chalk,fourth=cheese)
print(names(items))
print(items$first)
print(lapply(items,length))
print(lapply(items,class))
print(lapply(items,mean))
print(summary(items))
print(attributes(items))
print(str(items))
