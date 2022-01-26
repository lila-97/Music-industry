setwd("~/uni/business/Business_project-main/songs_popularity/dataset3")

library(gam)
library(gbm)

dati <- read.csv("Spotify-2000.csv",stringsAsFactors = T)
dati <- dati[,-c(1,2,3)] # we don't care about index, title and artist
dim(dati)
str(dati)
dati$length <- as.numeric(dati$Length..Duration.)
dati <- dati[,-c(9)] #remove factor length
levels(dati$Top.Genre)

## too many genres, need to reduce:
pop <- c("acoustic pop","afropop","alternative pop","alternative pop rock","art pop",
         "australian pop","austropop","barbadian pop","baroque pop","belgian pop",
         "bow pop","brill building pop","britpop","bubblegum pop","canadian pop",
         "candy pop","chamber pop","classic italian pop","classic uk pop","danish pop",
         "danish pop rock","dutch pop","europop","german pop","irish pop","italian pop",
         "nederpop","new wave pop","operatic pop","uk pop")
indie <- c("alaska indie","australian indie folk","dutch indie","icelandic indie",
           "indie anthem-folk","indie pop","la pop","pop")
rock <- c("album rock","alternative rock","art rock","australian alternative rock",
          "australian rock","belgian rock","british alternative rock","canadian rock",
          "celtic rock","classic canadian rock","classic rock","classical rock","dutch rock",
          "garage rock","german alternative rock","german pop rock","glam rock","hard rock",
          "irish rock","modern rock","rock-and-roll","soft rock","yacht rock","celtic punk","pop punk","punk")
country <- c("alternative country","arkansas country","classic country pop","contemporary country")
dance <- c("alternative dance","australian dance","dance pop","dance rock","disco",
           "eurodance")
hip_hop <- c("alternative hip hop","atl hip hop","detroit hip hop","dutch hip hop",
             "east coast hip hop","gangster rap","hip pop")
metal <- c("alternative metal","dutch metal","finnish metal","glam metal")
other <- c("adult standards","australian americana","australian psych","basshall","bebop","big beat",
           "big room","boy band","british invasion","british singer-songwriter",
           "carnaval limburg","ccm","celtic","chanson","christelijk","classic schlager",
           "classic soundtrack","compositional ambient","downtempo","dutch americana",
           "dutch cabaret","dutch prog","gabba","irish singer-songwriter","j-core","laboratorio",
           "latin","latin alternative","levenslied","mellow gold","metropopolis","motown",
           "neo mellow","permanent wave","reggae","reggae fusion","scottish singer-songwriter",
           "stomp and holler","streektaal")
elettronica <- c("cyberpunk","diva house","edm","electro","electro house","electronica",
                 "electropop","happy hardcore","trance")
folk <- c("british folk","canadian folk","folk","folk-pop","modern folk rock")
soul <- c("british soul","chicago soul","classic soul","funk","g funk","neo soul",
          "acid jazz","contemporary vocal jazz","latin jazz")
blues <- c("blues","blues rock")

genere <- rep(0,length(dati$Top.Genre))
dati$genere = genere


for(i in 1:length(dati$Top.Genre)){
  if(dati$Top.Genre[i] %in% pop){
    dati$genere[i] = "pop"}
}
for(i in 1:length(dati$Top.Genre)){
  if(dati$Top.Genre[i] %in% indie){
    dati$genere[i] = "indie"}
}
for(i in 1:length(dati$Top.Genre)){
  if(dati$Top.Genre[i] %in% rock){
    dati$genere[i] = "rock"}
}
for(i in 1:length(dati$Top.Genre)){
  if(dati$Top.Genre[i] %in% country){
    dati$genere[i] = "country"}
}
for(i in 1:length(dati$Top.Genre)){
  if(dati$Top.Genre[i] %in% dance){
    dati$genere[i] = "dance"}
}
for(i in 1:length(dati$Top.Genre)){
  if(dati$Top.Genre[i] %in% hip_hop){
    dati$genere[i] = "hip hop"}
}
for(i in 1:length(dati$Top.Genre)){
  if(dati$Top.Genre[i] %in% metal){
    dati$genere[i] = "metal"}
}
for(i in 1:length(dati$Top.Genre)){
  if(dati$Top.Genre[i] %in% other){
    dati$genere[i] = "other"}
}
for(i in 1:length(dati$Top.Genre)){
  if(dati$Top.Genre[i] %in% elettronica){
    dati$genere[i] = "electro"}
}
for(i in 1:length(dati$Top.Genre)){
  if(dati$Top.Genre[i] %in% folk){
    dati$genere[i] = "folk"}
}
for(i in 1:length(dati$Top.Genre)){
  if(dati$Top.Genre[i] %in% soul){
    dati$genere[i] = "soul"}
}
for(i in 1:length(dati$Top.Genre)){
  if(dati$Top.Genre[i] %in% blues){
    dati$genere[i] = "blues"}
}

sum(dati$genere == 0) # all genres accounted for
dati <- dati[,-1] # remove top genre
dati$genere <- as.factor(dati$genere)
levels(dati$genere)
summary(dati$genere)
str(dati)

# Response variable
summary(dati$Popularity)
boxplot(dati$Popularity, col="orange", ylim=c(0,100), main="Popularity distribution", ylab="Popularity")
hist(dati$Popularity, col="orange", main="Songs", xlab="Popularity",freq = F)

# Explanatory variables
summary(dati)
par(mfrow=c(3,3))
for(i in c(seq(1,9))){
  hist(dati[,i], col="grey", main=paste(colnames(dati)[i]), xlab="")
}
par(mfrow=c(1,1))
dati$Speechiness <- log(dati$Speechiness)
hist(dati$Speechiness)

# Set train and test
set.seed(1)
train = sample (1:nrow(dati), 0.7*nrow(dati))
dati.train=dati[train ,]
dati.test=dati[-train ,]
dim(dati.train)
dim(dati.test)

m1 <- lm(Popularity~., data=dati.train)
summary(m1)

# Stepwise Regression
m2 <- step(m1, direction="both")
summary(m2)
plot(m2)

#Prediction
p.lm <- predict(m2, newdata=dati.test)
dati.test$Popularity[1:5]
p.lm[1:5]
dev.lm <- sum((p.lm-dati.test$Popularity)^2)
dev.lm
AIC(m2)

#Stepwise GAM
#Start with a linear model (df=1)
g1 <- gam(Popularity~., data=dati.train)

#Show the linear effects 
par(mfrow=c(3,4))
plot(g1, se=T) 
par(mfrow=c(1,1))

#Perform stepwise selection using gam scope
#Values for df should be greater than 1, with df=1 implying a linear fit

sc = gam.scope(dati.train[,-10], response=10, arg=c("df=2","df=3","df=4"))
g2<- step.Gam(g1, scope=sc, trace=T)
summary(g2)

AIC(g2)

par(mfrow=c(3,4))
plot(g2, se=T)

# if we want to see better some plot
par(mfrow=c(1,1))
plot(g2, se=T, ask=T)


#Prediction
p.gam <- predict(g2,newdata=dati.test)
dati.test$Popularity[1:5]
p.gam[1:5]
dev.gam <- sum((p.gam-dati.test$Popularity)^2)
dev.gam
dev.lm


# Gradient Boosting #######
# 1 Boosting- 
boost.songs=gbm(Popularity ~ ., data=dati.train, 
                 distribution="gaussian", n.trees=5000, interaction.depth=1)
#
par(mfrow=c(1,1))
#
#plot of training error
plot(boost.songs$train.error, type="l", ylab="training error")
#always decreasing with increasing number of trees
#
#
#relative influence plot
summary(boost.songs) 
#let us modify the graphical paramters to obtain a better plot
#
#more space on the left
#
# default vector of paramters
mai.old<-par()$mai
mai.old
#new vector
mai.new<-mai.old
#new space on the left
mai.new[2] <- 2.1 
mai.new
#modify graphical parameters
par(mai=mai.new)
summary(boost.songs, las=1) 
#las=1 horizontal names on y
summary(boost.songs, las=1, cBar=10) 
#cBar defines how many variables
#back to orginal window
par(mai=mai.old)


# test set prediction for every tree (1:5000)

yhat.boost=predict(boost.songs, newdata=dati.test, n.trees=1:5000)

# calculate the error for each iteration
#use 'apply' to perform a 'cycle for' 
# the first element is the matrix we want to use, 2 means 'by column', 
#and the third element indicates the function we want to calculate

err = apply(yhat.boost, 2, function(pred) mean((dati.test$Popularity - pred)^2))
#
plot(err, type="l")

# error comparison (train e test)
plot(boost.songs$train.error, type="l")
lines(err, type="l", col=2)
#minimum error in test set
best=which.min(err)
best
abline(v=best, lty=2, col=4)
#
min(err) #minimum error


# 2 Boosting - deeper trees
boost.songs=gbm(Popularity ~ ., data=dati.train, 
                 distribution="gaussian", n.trees=5000, interaction.depth=4)

plot(boost.songs$train.error, type="l")

par(mai=mai.new)

summary(boost.songs, las=1, cBar=10)
par(mai=mai.old)

yhat.boost=predict(boost.songs ,newdata=dati.test,n.trees=1:5000)
err = apply(yhat.boost,2,function(pred) mean((dati.test$Popularity-pred)^2))
plot(err, type="l")


plot(boost.songs$train.error, type="l")
lines(err, type="l", col=2)
best=which.min(err)
best
abline(v=best, lty=2, col=4)
min(err)


# 3 Boosting - smaller learning rate 

boost.songs=gbm(Popularity ~ ., data=dati.train, 
                 distribution="gaussian", n.trees=5000, interaction.depth=1, shrinkage=0.01)
plot(boost.songs$train.error, type="l")

par(mai=mai.new)

summary(boost.songs, las=1, cBar=10) 
par(mai=mai.old)

yhat.boost=predict(boost.songs ,newdata=dati.test,n.trees=1:5000)
err = apply(yhat.boost,2,function(pred) mean((dati.test$Popularity-pred)^2))
plot(err, type="l")


plot(boost.songs$train.error, type="l")
lines(err, type="l", col=2)
best=which.min(err)
best
abline(v=best, lty=2, col=4)
min(err)


# 4 Boosting - combination of previous models
boost.songs=gbm(Popularity ~ .,data=dati.train, 
                 distribution="gaussian",n.trees=5000, interaction.depth=4, shrinkage=0.01)

plot(boost.songs$train.error, type="l")
#

par(mai=mai.new)

summary(boost.songs, las=1, cBar=10) 

par(mai=mai.old)

yhat.boost=predict(boost.songs ,newdata=dati.test,n.trees=1:5000)
err = apply(yhat.boost, 2, function(pred) mean((dati.test$Popularity-pred)^2))
plot(err, type="l")


plot(boost.songs$train.error, type="l")
lines(err, type="l", col=2)
best=which.min(err)
best
abline(v=best, lty=2, col=4)
err.boost= min(err)


boost.songs
# partial dependence plots
plot(boost.songs, i.var=1, n.trees = best)
plot(boost.songs, i.var=2, n.trees = best)
plot(boost.songs, i.var=c(2,5), n.trees = best) #bivariate
#
plot(boost.songs, i.var=11, n.trees = best) # categorical

p.gbm <- predict(boost.songs,newdata=dati.test,n.trees = best)
dati.test$Popularity[1:15]
p.gbm[1:15]
dev.gbm <- sum((p.gbm-dati.test$Popularity)^2)
dev.gbm
dev.gam
dev.lm

## need to try different loss