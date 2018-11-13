source("./lib.R")

nCores <- 2

CBR <- 10
diff <- 10
coinMaturity <- 16*60*60

nYears <- 5 # number of years to simulate
tEnd <- nYears*365*24*60*60
nutxos <- c(1,10,100,1000)
tries <- 1000
balance <- 100000

res <- simMultipleSplits(balance=balance,nutxos=nutxos,diff=diff,simYears=nYears,CBR=CBR,coinMaturity=coinMaturity,tries=tries,nCores=nCores)

boxplot(reward~nutxos,data=res, main=sprintf("Best UTXO split of %.2f GRC",balance),
   xlab="Number of UTXOs", ylab=sprintf("Rewards over %.2f years",nYears)) 
means <- aggregate(res$reward, by=list(res$nutxos), FUN=mean)
points(1:length(nutxos), means[,2],col=2,pch=3)

