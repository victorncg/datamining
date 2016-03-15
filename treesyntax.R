# Load dataset
setwd ("C:/Users/Victor/~")

Data <- read.csv("Data.csv")

# Create categorical variable based on binary variable
index <- c(0,1)
values <- c("Non-hub","Hub")
Data$HUBCATEGORY <- values[match(Data$HUB,index)]

# Factor variable HUBCATEGORY
Data$HUBCATEGORY = factor(Data$HUBCATEGORY, c("Hub","Non-hub"))

attach (Data)

#====================================================================================
# Decision trees
# Transform variables from integer to numeric type
Datafix <- transform(Data,NPAS = as.numeric(NPAS),
                     AREAIR = as.numeric(AREAIR),
                     CPOP = as.numeric(CPOP),  
                     NAIRL = as.numeric(NAIRL),	
                     OPER = as.numeric(OPER),
                     NDES = as.numeric(NDES),	
                     IDES = as.numeric(IDES),
                     NTER = as.numeric(NTER),
                     ACCON = as.numeric(ACCON), M50 = as.numeric(M50)
                     )

# Randomize the data
set.seed(9850)
g <- runif(nrow(Datafix))
Datafixr <- Datafix[order(g),]

# Verify the structure of the data
str(Datafixr)


# Models for prediction
# Tree using function C5.0
mt <- C5.0(Datafixr[1:36,-11], Datafixr[1:36,11])
summary(mt)
plot(mt)
# Test the model 
pt <- predict(mt, Datafixr[37:51,])
pt
testtable <- table(Datafixr[37:51,11],pt)
testtable
Datapred1 <- Datafixr[37:51,]
Datapred1$Prediction <- pt

# Tree using function tree
Tree1 <- tree(HUBCATEGORY ~ NPAS + AREAIR + CPOP + M50 + ACCON + NAIRL + OPER + NDES + IDES + NTER,data = Datafixr[1:36,])
summary (Tree1)
plot(Tree1);text(Tree1, cex = 0.9)
Datatest1 <- Datafixr
Pred1 <- predict(Tree1,Datatest1[37:51,])
Pred1
Datatest1 <- data.frame(Datatest1[37:51,],Pred1)
Datatest1$Predicted <- ifelse(Datatest1$Hub>0.5,"Hub","Non-hub")
testtable1 <- table(Datatest1[,11],Datatest1[,14])
testtable1
