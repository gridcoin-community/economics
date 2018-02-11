source("./lib.R")

coinSupply <- 400000000
RRfrac <- seq(from=0.65,to=0.85,by=0.05)
inflationRate <- 30000*365/(coinSupply*RRfrac)

years <- 50

modelSplit <- function(years,coinSupply,RRfrac,inflationRate){
    res <- list()
    res[["supplyRes"]] = rep(0,years)
    res$supplyRes[1] <- coinSupply

    reward <- rewardStruct(coinSupply,RRfrac,inflationRate,1)

    res[["totalInf"]] <- rep(0,years-1)

    for (year in 2:years){
        grcCreatedFix <- reward$RRmint + reward$CBRmint
        res$supplyRes[year] <- res$supplyRes[year-1] + grcCreatedFix 
        res$totalInf[year-1] <- grcCreatedFix/res$supplyRes[year-1]*100
    }
    return(res)
}

#Coin Supply
mOut <- modelSplit(years,coinSupply,RRfrac[1],inflationRate[1])

par(mfrow=c(2,1)) 
plot(1:years,mOut$supplyRes,type='l',xlab="year",ylab="Total Supply")

for (s in 2:5){
    mOut <- modelSplit(years,coinSupply,RRfrac[s],inflationRate[s])
    lines(1:years,mOut$supplyRes,col=s)
}

legend("topleft",legend=RRfrac,col=1:5,lty=c(1,1),lwd=c(2,2))

#Total Monetary Inflation Rate
mOut <- modelSplit(years,coinSupply,RRfrac[1],inflationRate[1])

plot(1:(years-1),mOut$totalInf,type='l',ylim=c(0,4.5),xlab="year",ylab="Monetary Inflation Rate")

for (s in 2:5){
    mOut <- modelSplit(years,coinSupply,RRfrac[s],inflationRate[s])
    lines(1:(years-1),mOut$totalInf,col=s)
}

legend("topright",legend=RRfrac,col=1:5,lty=c(1,1),lwd=c(2,2))
