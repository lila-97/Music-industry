############# IMMAGINE TUTTI GLI STANDARD BASS MODEL


par(mfrow=c(3,2))

# lp
BMs_sarimax <- SARMAX.refinement(lp_BMs,
                                 arima_order=c(3,2,4),
                                 seasonal_order=c(0,0,2)) # fit arima onto the BM?
sarimax_fit <- fitted(BMs_sarimax)

plot(dati_lp$x, type="l",xaxt="n",xlab = "Year",ylab = "Adjusted Value",main="LP")
axis(1, at = seq(1,length(dati_lp$x),1),labels = dati_lp$year)
lines(predicted_lp_BMs, col=2)
lines(sarimax_fit, col=3) # Standard Bass model + arima
legend(x=27, y=8000,legend = c("BM","BM+ARIMA"), col = c(2,3),lty=1)
text()

# cassette
BMs_sarimax <- SARMAX.refinement(cassette_BMs,
                                 arima_order=c(2,2,2),
                                 seasonal_order=c(1,1,1)) # fit arima onto the BM?
sarimax_fit <- fitted(BMs_sarimax)

plot(dati_cassette$x[1:36], type="l",xaxt="n",xlab = "Year",ylab = "Adjusted Value",main="cassette")
axis(1, at = seq(1,length(dati_cassette$x[1:36]),1),labels = dati_cassette$year[1:36])
lines(predicted_cassette_BMs, col=2)
lines(sarimax_fit, col=3) # Standard Bass model + arima
#legend(x=27, y=6000,legend = c("BM","BM+ARIMA"), col = c(2,3),lty=1)

# cd
BMs_sarimax <- SARMAX.refinement(cd_BMs,
                                 arima_order=c(2,2,1),
                                 seasonal_order=c(1,1,1)) # fit arima onto the BM?
sarimax_fit <- fitted(BMs_sarimax)

plot(dati_cd$x[11:48], type="l",xaxt="n",xlab = "Year",ylab = "Adjusted Value",main="cd")
axis(1, at = seq(1,length(dati_cd$x[11:48]),1),labels = dati_cd$year[11:48])
lines(predicted_BMs, col=2)
lines(sarimax_fit, col=3) # Standard Bass model + arima
#legend(x=27, y=17500,legend = c("BM","BM+ARIMA"), col = c(2,3),lty=1)

# download (legali)
BMs_sarimax <- SARMAX.refinement(downloads_BMs,
                                 arima_order=c(2,2,2),
                                 seasonal_order=c(1,1,1)) # fit arima onto the BM?
sarimax_fit <- fitted(BMs_sarimax)

plot(dati_downloads$x[32:48], type="l",xaxt="n",xlab = "Year",ylab = "Adjusted Value",ylim=c(0,4000),main="(legal) downloads")
axis(1, at = seq(1,length(dati_downloads$x[32:48]),1),labels = dati_downloads$year[32:48])
lines(predicted_downloads_BMs, col=2)
lines(sarimax_fit, col=3) # Standard Bass model + arima
#legend(x=13, y=4200,legend = c("BM","BM+ARIMA"), col = c(2,3),lty=1)

# streaming
BMs_sarimax <- SARMAX.refinement(streaming_BMs,
                                 arima_order=c(0,2,0),
                                 seasonal_order=c(0,1,1)) # fit arima onto the BM?
sarimax_fit <- fitted(BMs_sarimax)

plot(dati_streaming$x[32:48], type="l",xaxt="n",xlab = "Year",ylab = "Adjusted Value",ylim=c(0,15000), main="streaming")
axis(1, at = seq(1,length(dati_streaming$x[32:48]),1),labels = dati_streaming$year[32:48])
lines(predicted_streaming_BMs, col=2)
lines(sarimax_fit, col=3) # Standard Bass model + arima
#legend(x=3, y=10000,legend = c("BM","BM+ARIMA"), col = c(2,3),lty=1)


par(mfrow=c(1,1))




#### UCRCD modelli di competizione

par(mfrow=c(2,2))
plot(dati_cassette$year[1:48], cassette, type="b",pch=16,lty=3,cex=0.7,col=4,xlab="Year",ylab="Adjusted value",main="Competition cassette/cd",ylim=c(-500,22000))
points(dati_cd$year[11:48], cd, type="b",pch=16,lty=3,cex=0.7,col=3)
cassette_fit <- cassette_cd$fitted[[1]]
cassette_fit[cassette_fit < 0] = 0
cassette_fit
lines(dati_cassette$year, cassette_fit, col=2)
lines(dati_cd$year[11:48], cassette_cd$fitted[[2]], col=6)

plot(dati_cd$year[11:48], cd, type="b",pch=16,lty=3,cex=0.7,col=4,xlab="Year",ylab="Adjusted value",main="Competition cd/downloads (estimated)",ylim=c(-500,22000))
points(dati_cd$year[27:48], downloads, type="b",pch=16,lty=3,cex=0.7,col=3)
lines(dati_cd$year[11:48], ucrcd_cd_downloads$fitted[[1]], col=2)
lines(dati_cd$year[27:48], ucrcd_cd_downloads$fitted[[2]], col=6)


plot(dati_downloads$year[27:48], downloads, type="b",pch=16,lty=3,cex=0.7,col=4,xlab="Year",ylab="Adjusted value",main="Competition downloads/streaming (estimated)",ylim=c(-500,22000))
points(dati_streaming$year[33:48], stream_services, type="b",pch=16,lty=3,cex=0.7,col=3)
lines(dati_downloads$year[27:48], ucrcd_downloads_streaming$fitted[[1]], col=2)
lines(dati_streaming$year[33:48], ucrcd_downloads_streaming$fitted[[2]], col=6)

par(mfrow=c(1,1))

##### 

plot(dati_cd$year[11:48], cd, type="b",pch=16,lty=3,cex=0.7,col=4,xlab="Year",ylab="Adjusted value",main="Competition cd/digital",ylim=c(-500,22000))
points(dati_cd$year[32:48], digital, type="b",pch=16,lty=3,cex=0.7,col=3)
cd_fit <- cd_digital$fitted[[1]]
lines(dati_cd$year[11:48], cd_fit, col=2)
lines(dati_cd$year[32:48], cd_digital$fitted[[2]], col=6)
 