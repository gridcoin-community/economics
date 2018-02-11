

rewardStruct <- function(grcSup,RRfrac,APR,timeInterval,blockTime=90,doPrint=TRUE){
# blockTime in seconds
# timeInterval in years
    res <- list()
    res[["RRmint"]] <- grcSup * (RRfrac * APR) * timeInterval
    res[["CBRmint"]] <- grcSup * ((1-RRfrac) * APR) * timeInterval
    blocksInInterval <- timeInterval*365*24*60*60/blockTime
    res[["CBRvalue"]] <- res$CBRmint/blocksInInterval
    res[["dailyRRmint"]] <- res$RRmint/timeInterval*(1/365)
    if(doPrint){
        print("")
        print("Reward Structure")
        print(paste("Parameter: Coin Supply=",grcSup," RRfrac=",RRfrac," APR=",APR," timeInterval(in years)=",timeInterval," block time=",blockTime,sep=""))
        print(paste("Results: RRmint=",res$RRmint," CBRmint=",res$CBRmint," CBRvalue=",res$CBRvalue," dailyRRmint=",res$dailyRRmint,sep=""))
        print("")
    }
    return(res)
}
