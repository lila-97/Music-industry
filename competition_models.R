library(readxl)
library(DIMORA)

#MusicData <- read_excel("MusicData.xlsx")
#View(MusicData)
setwd("~/uni/business/Business_project-main")
MusicData_1_ <- read_excel("updated_MusicData.xlsx")
#View(MusicData_1_)
unique(MusicData_1_$formatType)

dati <- MusicData_1_

#### PREPROCESSING

# lp e vinili assieme
# cassette, cassette single, other tapes, 8-track
# cd, sacd, cd single, music video, dvd audio
# download album, download music video, ringtones & ringbacks, dowload single
# paid subscriptions, limitied tier paid subscription, paid subscription, on demand streaming, other ad supported streaming
#   other digital, soundexchange distributions, synchronization

# togliere: kiosk

dim(dati)
dati <- dati[!(dati$formatType == "Kiosk"),]
dim(dati)
unique(dati$formatType)

# 5 categorie

lp <- c("LP/EP","Vinyl Single")
cassette <- c("Cassette", "Cassette Single", "Other Tapes")
cd <- c("CD", "SACD", "CD Single", "Music Video (Physical)", "DVD Audio")
downloads <- c("Download Album", "Download Music Video", "Ringtones & Ringbacks", "Download Single")
streaming <- c("Paid Subscriptions", "Paid Subscription", "Limited Tier Paid Subscription", 
               "On-Demand Streaming (Ad-Supported)", "Other Ad-Supported Streaming",
               "SoundExchange Distributions",  "Other Digital")
# "Synchronization"

dim(dati)
tipo <- rep(0,2904)
dati$tipo = tipo

for(i in 1:2904){
  if(dati$formatType[i] %in% lp){
    dati$tipo[i] = "LP"}
}


for(i in 1:2904){
  if(dati$formatType[i] %in% cassette){
    dati$tipo[i] = "cassette"}
}

for(i in 1:2904){
  if(dati$formatType[i] %in% cd){
    dati$tipo[i] = "cd"}
}
  
for(i in 1:2904){
  if(dati$formatType[i] %in% downloads){
    dati$tipo[i] = "downloads"}
}

for(i in 1:2904){
  if(dati$formatType[i] %in% streaming){
    dati$tipo[i] = "streaming"}
}
  

sum(dati$tipo == 0)

# divisione per 5 tipologie

dati_lp <- dati[dati$tipo == "LP" & dati$metric == "Value (Adjusted)",]
dati_lp <- aggregate(dati_lp$actualValue, by=list(year=dati_lp$year), FUN=sum)
plot(dati_lp$year, dati_lp$x, type = "l")

dati_cassette <- dati[dati$tipo == "cassette" & dati$metric == "Value (Adjusted)",]
dati_cassette$actualValue[is.na(dati_cassette$actualValue)] = 0
dati_cassette <- aggregate(dati_cassette$actualValue, by=list(year=dati_cassette$year), FUN=sum)
plot(dati_cassette$year, dati_cassette$x, type="l")

dati_cd <- dati[dati$tipo == "cd" & dati$metric == "Value (Adjusted)",]
# dim(dati_cd)
# sum(is.na(dati_cd$actualValue))
dati_cd$actualValue[is.na(dati_cd$actualValue)] = 0
dati_cd <- aggregate(dati_cd$actualValue, by=list(year=dati_cd$year), FUN=sum)
plot(dati_cd$year, dati_cd$x, type="l")

dati_downloads <- dati[dati$tipo == "downloads" & dati$metric == "Value (Adjusted)",]
#dim(dati_downloads)
#sum(is.na(dati_downloads$actualValue))
dati_downloads$actualValue[is.na(dati_downloads$actualValue)] = 0
dati_downloads <- aggregate(dati_downloads$actualValue, by=list(year=dati_downloads$year), FUN=sum)
plot(dati_downloads$year, dati_downloads$x, type="l")

dati_streaming <- dati[dati$tipo == "streaming" & dati$metric == "Value (Adjusted)",]
# dim(dati_streaming)
# sum(is.na(dati_streaming$actualValue))
dati_streaming$actualValue[is.na(dati_streaming$actualValue)] = 0
dati_streaming <- aggregate(dati_streaming$actualValue, by=list(year=dati_streaming$year), FUN=sum)
plot(dati_streaming$year, dati_streaming$x, type="l")

# formats over time plot
plot(dati_lp$year, dati_lp$x, type="l",col=1, lwd=2,ylim=c(-500,25000),
     main="Formats over time",xlab="year",ylab="Value (Adjusted)")
lines(dati_cassette$year, dati_cassette$x, col=2, lwd=2)
lines(dati_cd$year, dati_cd$x, col=3, lwd=2)
lines(dati_downloads$year, dati_downloads$x, col=4, lwd=2)
lines(dati_streaming$year, dati_streaming$x, col=5, lwd=2)
legend(1973,24000,legend=c("LP","cassette","cd","downloads","streaming"),
       col=c(1,2,3,4,5),lwd=2,cex=0.6,text.font=20,bty="n")

##### Modelli

### STANDARD BASS MODELS
# standard Bass model LP (completo)
tsdisplay(dati_lp$x) 
lp_BMs <- BASS.standard(dati_lp$x, display=F, ous=100, prelimestimates = c(100000,0.01,0.1)) 
summary(lp_BMs) # all highly significant
predicted_lp_BMs <- predict(lp_BMs, newx=1:70)

par(mfrow=c(2,2))
plot(lp_BMs,oos="y",mode="i",xlim=c(1,70))
plot(residuals(lp_BMs),type="l")
Acf(residuals(lp_BMs))
Pacf(residuals(lp_BMs)) # acf and pacf show residuals are autocorrelated
par(mfrow=c(1,1))

BMs_sarimax <- SARMAX.refinement(lp_BMs,
                                 arima_order=c(3,2,4),
                                 seasonal_order=c(0,0,2)) # fit arima onto the BM?
summary(BMs_sarimax)
plot(residuals(BMs_sarimax))
Acf(residuals(BMs_sarimax)) # residuals aren't autocorrelated in this case
Pacf(residuals(BMs_sarimax))
sarimax_fit <- fitted(BMs_sarimax)

plot(dati_lp$x, type="l",xaxt="n",xlab = "Year",ylab = "Adjusted Value")
axis(1, at = seq(1,length(dati_lp$x),1),labels = dati_lp$year)
lines(predicted_lp_BMs, col=2)
lines(sarimax_fit, col=3) # Standard Bass model + arima
legend(x=27, y=6000,legend = c("BM","BM+ARIMA"), col = c(2,3),lty=1)

# standard Bass model cassette
tsdisplay(dati_cassette$x[1:36]) # last cassette sales seen at step 36
cassette_BMs <- BASS.standard(dati_cassette$x[1:36], display=F, ous=100, prelimestimates = c(100000,0.01,0.1)) 
summary(cassette_BMs) # all highly significant
predicted_cassette_BMs <- predict(cassette_BMs, newx=1:70)

par(mfrow=c(2,2))
plot(cassette_BMs,oos="y",mode="i",xlim=c(1,40))
plot(residuals(cassette_BMs),type="l")
Acf(residuals(cassette_BMs))
Pacf(residuals(cassette_BMs)) # acf and pacf show residuals are autocorrelated
par(mfrow=c(1,1))

BMs_sarimax <- SARMAX.refinement(cassette_BMs,
                                 arima_order=c(2,2,2),
                                 seasonal_order=c(1,1,1)) # fit arima onto the BM?
summary(BMs_sarimax)
plot(residuals(BMs_sarimax))
Acf(residuals(BMs_sarimax)) # residuals aren't autocorrelated in this case
Pacf(residuals(BMs_sarimax))
sarimax_fit <- fitted(BMs_sarimax)

plot(dati_cassette$x[1:36], type="l",xaxt="n",xlab = "Year",ylab = "Adjusted Value")
axis(1, at = seq(1,length(dati_cassette$x[1:36]),1),labels = dati_cassette$year[1:36])
lines(predicted_cassette_BMs, col=2)
lines(sarimax_fit, col=3) # Standard Bass model + arima
legend(x=27, y=6000,legend = c("BM","BM+ARIMA"), col = c(2,3),lty=1)

# standard Bass model cd
tsdisplay(dati_cd$x[11:48]) # the first non-zero observation is at time-step 11
cd_BMs <- BASS.standard(dati_cd$x[11:48], display=F, ous=100, prelimestimates = c(300000,0.01,0.1)) 
summary(cd_BMs) # all highly significant
predicted_BMs <- predict(cd_BMs, newx=1:70)

par(mfrow=c(2,2))
plot(cd_BMs,oos="y",mode="i",xlim=c(1,40))
plot(residuals(cd_BMs),type="l")
Acf(residuals(cd_BMs))
Pacf(residuals(cd_BMs)) # acf and pacf show residuals are autocorrelated
par(mfrow=c(1,1))


BMs_sarimax <- SARMAX.refinement(cd_BMs,
                                 arima_order=c(2,2,1),
                                 seasonal_order=c(1,1,1)) # fit arima onto the BM?
summary(BMs_sarimax)
plot(residuals(BMs_sarimax))
Acf(residuals(BMs_sarimax)) # residuals aren't autocorrelated in this case
Pacf(residuals(BMs_sarimax))
sarimax_fit <- fitted(BMs_sarimax)

plot(dati_cd$x[11:48], type="l",xaxt="n",xlab = "Year",ylab = "Adjusted Value")
axis(1, at = seq(1,length(dati_cd$x[11:48]),1),labels = dati_cd$year[11:48])
lines(predicted_BMs, col=2)
lines(sarimax_fit, col=3) # Standard Bass model + arima
legend(x=27, y=17500,legend = c("BM","BM+ARIMA"), col = c(2,3),lty=1)

# standard Bass model downloads
tsdisplay(dati_downloads$x[32:48]) # the first observation of (legal) downloads is at time-step 32
downloads_BMs <- BASS.standard(dati_downloads$x[32:48], display=F, ous=100, prelimestimates = c(50000,0.01,0.1)) 
summary(downloads_BMs) # all highly significant
predicted_downloads_BMs <- predict(downloads_BMs, newx=1:70)

par(mfrow=c(2,2))
plot(downloads_BMs,oos="y",mode="i",xlim=c(1,20),ylim=c(0,4000))
plot(residuals(downloads_BMs),type="l")
Acf(residuals(downloads_BMs))
Pacf(residuals(downloads_BMs)) # acf and pacf show residuals are autocorrelated
par(mfrow=c(1,1))

BMs_sarimax <- SARMAX.refinement(downloads_BMs,
                                 arima_order=c(2,2,2),
                                 seasonal_order=c(1,1,1)) # fit arima onto the BM?
summary(BMs_sarimax)
plot(residuals(BMs_sarimax))
Acf(residuals(BMs_sarimax)) # residuals aren't autocorrelated in this case
Pacf(residuals(BMs_sarimax))
sarimax_fit <- fitted(BMs_sarimax)

plot(dati_downloads$x[32:48], type="l",xaxt="n",xlab = "Year",ylab = "Adjusted Value",ylim=c(0,4000))
axis(1, at = seq(1,length(dati_downloads$x[32:48]),1),labels = dati_downloads$year[32:48])
lines(predicted_downloads_BMs, col=2)
lines(sarimax_fit, col=3) # Standard Bass model + arima
legend(x=13, y=3000,legend = c("BM","BM+ARIMA"), col = c(2,3),lty=1)


# standard Bass model streaming
tsdisplay(dati_streaming$x[32:48]) # the first observation for streaming is at time-step 32
streaming_BMs <- BASS.standard(dati_streaming$x[32:48], display=F, ous=100, prelimestimates = c(250000,0.01,0.1)) 
summary(streaming_BMs) # all highly significant
predicted_streaming_BMs <- predict(streaming_BMs, newx=1:70)

par(mfrow=c(2,2))
plot(streaming_BMs,oos="y",mode="i",xlim=c(1,35),ylim=c(0,15000))
plot(residuals(streaming_BMs),type="l")
Acf(residuals(streaming_BMs))
Pacf(residuals(streaming_BMs)) # acf and pacf show residuals are autocorrelated
par(mfrow=c(1,1))

BMs_sarimax <- SARMAX.refinement(streaming_BMs,
                                 arima_order=c(0,2,0),
                                 seasonal_order=c(0,1,1)) # fit arima onto the BM?
summary(BMs_sarimax)
plot(residuals(BMs_sarimax))
Acf(residuals(BMs_sarimax)) # residuals aren't autocorrelated in this case
Pacf(residuals(BMs_sarimax))
sarimax_fit <- fitted(BMs_sarimax)

plot(dati_streaming$x[32:48], type="l",xaxt="n",xlab = "Year",ylab = "Adjusted Value",ylim=c(0,15000))
axis(1, at = seq(1,length(dati_streaming$x[32:48]),1),labels = dati_streaming$year[32:48])
lines(predicted_streaming_BMs, col=2)
lines(sarimax_fit, col=3) # Standard Bass model + arima
legend(x=3, y=10000,legend = c("BM","BM+ARIMA"), col = c(2,3),lty=1)

### BASS MODEL VARIATIONS

# Generalized bass model cassette
cassette_GBM <- BASS.generalized(dati_cassette$x[1:36],shock = "exp",
                           nshock = 1,
                           display=F,
                           prelimestimates = c(1.08e+05,3.24e-03,2.75e-01,10,-01,-0.1)) 


summary(cassette_GBM) # all significant, negative shock, corresponding to entrance in the market of CDs
predicted_GBM <- predict(cassette_GBM, newx=1:50)
adj_R2 <- (cassette_GBM$Rsquared - cassette_BMs$Rsquared)/(1 - cassette_BMs$Rsquared)
adj_R2 # significant nested model compared to standard bass model
plot(cassette_GBM,oos="y",mode="i",xlim=c(1,40))

# GGM cassette

summary(cassette_BMs)
cassette_GGM <- GG.model(dati_cassette$x[1:36],prelimestimates = c(1.09e+05,0.01,0.1,0.01,0.1),display=F)
summary(cassette_GGM)
predicted_GGM <- predict(cassette_GGM, newx=1:50)
adj_R2 <- (cassette_GGM$Rsquared - cassette_BMs$Rsquared)/(1 - cassette_BMs$Rsquared)
adj_R2 # significant nested model compared to standard bass model
plot(cassette_GGM,oos="y",mode="i",xlim=c(1,40))

# Generalized bass model cd

cd_GBM <- BASS.generalized(dati_cd$x[11:48],shock = "exp",
                           nshock = 1,
                           display=F,
                           prelimestimates = c(3.2e+05,3.354e-03,2.515e-01,16,-0.1,-0.1)) 


summary(cd_GBM) # all significant, negative shock (possibly due to early release of mp3 format, pre-napster)
predicted_GBM <- predict(cd_GBM, newx=1:70)
adj_R2 <- (cd_GBM$Rsquared - cd_BMs$Rsquared)/(1 - cd_BMs$Rsquared)
adj_R2 # significant nested model compared to standard bass model

par(mfrow=c(2,2))
plot(cd_GBM,oos="y",mode="i",xlim=c(1,40))
plot(residuals(cd_GBM),type="l")
Acf(residuals(cd_GBM))
Pacf(residuals(cd_GBM)) # acf and pacf show residuals are autocorrelated
par(mfrow=c(1,1))


GBM_sarimax <- SARMAX.refinement(cd_GBM,
                                 arima_order=c(2,1,2),
                                 seasonal_order=c(0,2,1)) # fit arima onto the BM?
summary(GBM_sarimax)
par(mfrow=c(2,2))
plot(residuals(GBM_sarimax))
Acf(residuals(GBM_sarimax)) # residuals aren't autocorrelated in this case
Pacf(residuals(GBM_sarimax))
par(mfrow=c(1,1))
gbm_sarimax_fit <- fitted(GBM_sarimax)

plot(dati_cd$x[11:48], type="l",xaxt="n",xlab = "Year",ylab = "Adjusted Value")
axis(1, at = seq(1,length(dati_cd$x[11:48]),1),labels = dati_cd$year[11:48])
lines(predicted_GBM, col=2)
lines(gbm_sarimax_fit, col=3) # Generalized Bass model + arima
legend(x=27, y=17500,legend = c("GBM","GBM+ARIMA"), col = c(2,3),lty=1)

# GGM cd
summary(cd_BMs)
cd_GGM <- GG.model(dati_cd$x[11:48],prelimestimates = c(3.21e+05,0.01,0.1,0.01,0.1),display=F)
summary(cd_GGM)
predicted_GGM <- predict(cd_GGM, newx=1:50)
adj_R2 <- (cd_GGM$Rsquared - cd_BMs$Rsquared)/(1 - cd_BMs$Rsquared)
adj_R2 # significant nested model compared to standard bass model
plot(cd_GGM,oos="y",mode="i",xlim=c(1,40))

# Generalized bass model download
download_GBM <- BASS.generalized(dati_downloads$x[32:48],shock = "exp",
                                 nshock = 1,
                                 display=F,
                                 prelimestimates = c(3.85e+04,2.33e-02,3.25e-01,10,-01,-0.1)) 

summary(download_GBM) # all significant, negative shock 
predicted_GBM <- predict(download_GBM, newx=1:50)
adj_R2 <- (download_GBM$Rsquared - downloads_BMs$Rsquared)/(1 - downloads_BMs$Rsquared)
adj_R2 # significant nested model compared to standard bass model
plot(download_GBM,oos="y",mode="i",xlim=c(1,20),ylim=c(0,4500))

# GGM and GBM of download and streaming have plently of non-significant params

## MODELLI DI COMPETIZIONE

# modello di competizione fra cassette e cd

cd <- dati_cd$x[11:48]
cassette <- dati_cassette$x
plot(dati_cassette$year, cassette, type="b",pch=16,lty=3,cex=0.7,col=4,xlab="Year",ylab="Adjusted value",main="Death of Cassette",ylim=c(-500,22000),xlim=c(1973,2020))
points(dati_cd$year[11:48], cd, type="b",pch=16,lty=3,cex=0.7,col=3)
  length(cassette) - length(cd)
cassette_cd <- UCRCD(cassette,cd,c2=10,par="unique",display = F)
summary(cassette_cd)
coefficients(cassette_cd)

plot(dati_cassette$year[1:48], cassette, type="b",pch=16,lty=3,cex=0.7,col=4,xlab="Year",ylab="Adjusted value",main="Death of Cassette",ylim=c(-500,22000))
points(dati_cd$year[11:48], cd, type="b",pch=16,lty=3,cex=0.7,col=3)
cassette_fit <- cassette_cd$fitted[[1]]
cassette_fit[cassette_fit < 0] = 0
cassette_fit
lines(dati_cassette$year, cassette_fit, col=2)
lines(dati_cd$year[11:48], cassette_cd$fitted[[2]], col=6)

# modello di competizione fra cd e prodotti digitali

digital <- (dati_downloads$x + dati_streaming$x)[32:48]
dati_downloads$x + dati_streaming$x
dati_streaming$year
downloads <- dati_downloads$x[32:48]
stream_services <- dati_streaming$x[32:48]



# UCRCD cd vs digital
ill.down.rate <- c(0.15,0.2,0.23,0.25,0.26,0.26,0.25,0.25,0.25,0.2,0.15,rep(0.05,10))
ill.down.rate <- c(0.15,0.2,0.23,0.25,0.26,0.26,0.25,0.25,0.25,0.2,0.15,0.12,0.1,0.08,rep(0.05,7))
plot(ill.down.rate)
ill.down.estimate <- dati_cd$x[28:48] *ill.down.rate / 0.8
ill.down.estimate <- c(dati_cd$x[27]*0.1/0.9,ill.down.estimate)
ill.down.estimate
digital
digital <- c(0,0,0,0,0,digital)
digital
length(ill.down.estimate)
length(digital)
digital <- digital + ill.down.estimate
digital

# Graphical inspection of data
plot(dati_cd$year[11:48], cd, type="b",pch=16,lty=3,cex=0.7,col=4,xlab="Year",ylab="Adjusted value",main="Death of CDs")
points(dati_cd$year[27:48], digital, type="b",pch=16,lty=3,cex=0.7,col=3)

length(cd)
length(cd) - length(digital)
cd_digital<-UCRCD(cd,digital,c2=16,par="unique",
                  m1 = 150000,m2 = 200000,p1c = 0.01,p2c = 0.01,q1c = 0.1,q2c = 0.1)
summary(cd_digital) # non significativo ?
coefficients(cd_digital)

cd
downloads
# downloads <- c(2900,3500,4000,4400,downloads) esempio stime download illegali
downloads <- c(0,0,0,0,0,downloads) # aggiungendo stime
downloads <- downloads + ill.down.estimate # aggiunta stime
length(cd) - length(downloads)
ucrcd_cd_downloads <- UCRCD(cd, downloads, 16, par="unique") 
summary(ucrcd_cd_downloads)
coef(ucrcd_cd_downloads)

#stream_services
#length(cd) - length(stream_services)
#ucrcd_cd_streaming <- UCRCD(cd,stream_services,c2=21,par="unique")
#summary(ucrcd_cd_streaming)
#coef(ucrcd_cd_streaming)

length(downloads) - length(stream_services)
stream_services <- stream_services[-1]
ucrcd_downloads_streaming <- UCRCD(downloads, stream_services, c2=6,par="double")
plot(ucrcd_downloads_streaming,oos="y",mode="i",ylim=c(0,10000))
summary(ucrcd_downloads_streaming)
coefficients(ucrcd_downloads_streaming)
