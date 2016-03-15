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
Tree1 <- C5.0(Datafixr[1:36,-11], Datafixr[1:36,11])
summary(Tree1)
plot(Tree1)
# Test the model 
pt <- predict(Tree1, Datafixr[37:51,])
pt
Datapred1 <- Datafixr[37:51,]
Datapred1$Prediction <- pt
testtable1 <- table(Datafixr[37:51,11],pt)
testtable1

# Tree using function tree
Tree2 <- tree(HUBCATEGORY ~ NPAS + AREAIR + CPOP + M50 + ACCON + NAIRL + OPER + NDES + IDES + NTER,data = Datafixr[1:36,])
summary (Tree2)
plot(Tree2);text(Tree2, cex = 0.9)
Datatest2 <- Datafixr
Pred2 <- predict(Tree2,Datatest2[37:51,])
Pred2
Datatest2 <- data.frame(Datatest2[37:51,],Pred2)
Datatest2$Predicted <- ifelse(Datatest2$Hub>0.5,"Hub","Non-hub")
testtable2 <- table(Datatest2[,11],Datatest2[,14])
testtable2
