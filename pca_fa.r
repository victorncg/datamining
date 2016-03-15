# Glass identification data
glassid <- read.xls("Glass Identification Data.xlsx")
# fa.parallel() function to determine number of compontents to extract
fa.parallel(glassid, fa="PC")
fa.parallel(glassid[,c(-1,-11)],fa="pc",n.iter=100,
show.legend = FALSE, main="Scree plot with parallel analysis")
# principal() function to extract the components
# Inputting the raw data without the ID column and specifying that four
# unrotated components should be extrated
# "varimax" rotation by default, so no rotation
pc<-principal(glassid[,c(-1,-11)],nfactors = 4,rotate = "none")
pc
# Rotating principal components
rc<-principal(glassid[,c(-1,-11)],nfactors=4,rotate = "varimax")
rc
# Obtaining principal component scores
spc<-principal(glassid[,c(-1,-11)],nfactors = 4,score=TRUE)
head(spc$scores)
spc
# Correlation
cor(glassid$Na,glassid$Mg,spc$score)
?factor.plot
summary(glassid)
# Graph an orthogonal solution
factor.plot(rc,labels = rownames(rc$loadings))
rc$loadings




#===============================================================
# Problem 3
# Harman23
# glassid <- read.admbFit("Herman23.cor")
# fa.parallel() function to determine number of compontents to extract
fa.parallel(Harman23.cor$cov,n.obs = 302,fa="fa",n.iter = 100,show.legend = FALSE,
main="Scree plot with parallel analysis")

# fa () function to extract the components
covariances <-Harman23.cor$cov
fa <- fa (covariances,nfactors=2,rotate = "none")
fa
# Factor extraction with orthogonal rotation
rotorfa <- fa (covariances,nfactors=2,rotate = "varimax")
rotorfa
# Factor extraction with oblique rotation
rotobfa <- fa (covariances,nfactors=2,rotate = "promax")
rotobfa
# Obtaining factor scores
fafs <- fa (covariances,nfactors=2,rotate = "none",score=TRUE)
head(fafs$score)
# Graphing orthogonal solution
factor.plot(rotorfa,labels = rownames(rotorfa$loadings))
# Graphing orthogonal solution
factor.plot(rotobfa,labels = rownames(rotobfa$loadings))
# Graphing orthogonal solution using fa.diagram ()
fa.diagram(rotobfa, simple = FALSE)



#===============================================================
# Problem 5
# Vertebral Column Data
verteb <- read.xls("Vertebral Column Data.xlsx")
verteb=verteb[1:10,-7]

# fa.parallel() function to determine number of compontents to extract
verteb$Class<-NULL
fa.parallel(verteb,n.iter = 100,show.legend = FALSE,main="Scree plot with parallel analysis")

# Perform multidimensional scaling
cmdscale(vertebm)
fa <- fa (covariances,nfactors=2,rotate = "none")
fa
# Factor extraction with orthogonal rotation
rotorfa <- fa (covariances,nfactors=2,rotate = "varimax")
rotorfa
# Factor extraction with oblique rotation
rotobfa <- fa (covariances,nfactors=2,rotate = "promax")
rotobfa
# Obtaining factor scores
fafs <- fa (covariances,nfactors=2,rotate = "none",score=TRUE)
head(fafs$score)
# Graphing orthogonal solution
factor.plot(rotorfa,labels = rownames(rotorfa$loadings))
# Graphing orthogonal solution
factor.plot(rotobfa,labels = rownames(rotobfa$loadings))
# Graphing orthogonal solution using fa.diagram ()
fa.diagram(rotobfa, simple = FALSE)

# Classical MDS
# N rows (objects) x p columns (variables)
# each row identified by a unique row name
d <- dist(verteb) # euclidean distances between the rows
fit <- cmdscale(d,eig=TRUE, k=2,add = TRUE) # k is the number of dim
fit # view results
# plot solution 
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",main="Metric MDS")
text(x, y, labels = row.names(mydata), cex=.7)


dt <- dist(vertebt) # euclidean distances between the rows
fitt <- cmdscale(dt,eig=TRUE, k=2,add = TRUE) # k is the number of dim
fitt # view results
# plot solution 
x <- fitt$points[,1]
y <- fitt$points[,2]
factor.plot(fitt$points,labels = rownames(fitt$points))
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",main ="Metric MDS",labels= rownames(vertebt))
text(x, y, labels = row.names(mydata), cex=.7)
