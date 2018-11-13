library(Rcpp)
library(parallel)

########### reward ###########

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

########### stake ###########

wTarget <-function(diff,nCoins,COIN=100000000){
    return( (2^224/diff) * (nCoins*COIN/1250000) )
}

pb <- function(diff,nCoins){
    return( wTarget(diff=diff,nCoins=nCoins)/2^256 )
}

pbB <- function(diff,nCoins,blockSpacing=90,timeMask=16){
    return( 1-(1-pb(diff=diff,nCoins=nCoins))^(blockSpacing/timeMask) )
}

tBlock <- function(diff,nCoins,timeMask=16){
    return( timeMask/pb(diff=diff,nCoins=nCoins) )
}

coinsStaking <- function(diff,blockSpacing=90,timeMask=16,COIN=100000000){
    return( (timeMask*2^32*1250000*diff)/(blockSpacing*COIN) )
}

########### stake reward simulation ###########

cppFunction('NumericVector simStakeCBRutxoC(NumericVector utxosIn,double diff,double tEnd,double CBR,double coinMaturity) {
    NumericVector utxos = clone(utxosIn);
    int nutxos = utxos.size();
    NumericVector lockTime(nutxos);
    for(int i = 0; i < nutxos; ++i) {
        lockTime[i] = 0;
    }
    double pFactor = 100000000 / (1250000 * pow(2,32) * diff);
    double pb;
    double draw;
    double t = 0;
    while(t<=tEnd){
        for(int i = 0; i < nutxos; ++i) {
            if (lockTime[i]<=0){
                pb = utxos[i] * pFactor;
                draw = R::runif(0,1);
                if (draw <= pb){
                    utxos[i] += CBR;
                    lockTime[i] = coinMaturity; // cool down in seconds
                }
            }else{
                lockTime[i] -= 16; // 16 seconds passed
            }
        }
        t += 16; // 16 seconds stake interval
    }
    return utxos;
}')

simFinalBalance <- function(utxos,diff,tEnd,CBR,coinMaturity){
    utxoRes <- simStakeCBRutxoC(utxos=utxos,diff=diff,tEnd=tEnd,CBR=CBR,coinMaturity=coinMaturity)
    return(sum(utxoRes))
}

simFinalBalanceNt <- function(utxos,diff,tEnd,CBR,coinMaturity,tries=1){
    triesRes <- sapply(1:tries,FUN=function(x,...){return(simFinalBalance(utxos,diff,tEnd,CBR,coinMaturity))},utxos,diff,tEnd,CBR,coinMaturity)
    return(triesRes)
}

simFinalBalanceNtPar <- function(utxos,diff,tEnd,CBR,coinMaturity,tries=1,nCores=1){
    clust <- makeForkCluster(nCores)
    triesRes <- parSapply(clust,1:tries,FUN=function(x,...){return(simFinalBalance(utxos,diff,tEnd,CBR,coinMaturity))},utxos,diff,tEnd,CBR,coinMaturity)
    stopCluster(clust)
    return(triesRes)
}

simMultipleSplits <- function(balance,nutxos,diff,simYears,CBR,coinMaturity,tries,nCores=1){
    tEnd <- simYears*365*24*60*60
    uLen <- length(nutxos)
    nData <- tries * uLen
    res <- data.frame(balanceStart=rep(balance,nData),balanceEnd=rep(NA,nData),nutxos=rep(NA,nData),diff=rep(diff,nData),simYears=rep(simYears,nData),CBR=rep(CBR,nData),coinMaturity=rep(diff,nData))
    for(ii in 1:uLen){
        nu <- nutxos[ii]
        print(sprintf("Simulating utxos split %d of %d. Splitting to %d utxos.",ii,uLen,nu))
        utxos <- rep(balance/nu,nu)
        resTmp <- simFinalBalanceNtPar(utxos=utxos,diff=diff,tEnd=tEnd,CBR=CBR,coinMaturity=coinMaturity,tries=tries,nCores=nCores)
        res$balanceEnd[((ii-1)*tries+1):(tries*ii)] <- resTmp
        res$nutxos[((ii-1)*tries+1):(tries*ii)] <- rep(nu,tries)
    }
    res$reward <- res$balanceEnd - res$balanceStart
    return(res)
}




