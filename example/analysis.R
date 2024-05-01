library(rsem)
library(lavaan)
library(mice)
library(missForest)
library(VIM)
library(psych)

data <- read.table('robustsem.ex2.txt')
colnames(data) <- c('V1','V2','V3','V4','income','grade')

data1 <- data[-1, 1:4]
data1[] <- lapply(data1, as.numeric)

#missing rate
proportion <- round(100*apply(data[-1,1:4], 2, function(x) mean(is.na(x))),3)

# descriptive statistics
describe(data1)

# normality
hist(data1$V1, main="Histogram of Data", xlab="Data$V1", breaks=30, col="blue")
hist(data1$V2, main="Histogram of Data", xlab="Data$V2", breaks=30, col="blue")
hist(data1$V3, main="Histogram of Data", xlab="Data$V3", breaks=30, col="blue")
hist(data1$V4, main="Histogram of Data", xlab="Data$V4", breaks=30, col="blue")

shapiro.test(data1$V1)
shapiro.test(data1$V2)
shapiro.test(data1$V3)
shapiro.test(data1$V4)


#FIML
gcmodel<-'i =~ 1*V1 + 1*V2 + 1*V3  + 1*V4
	s =~ 0*V1 + 1*V2 + 2*V3 + 3*V4'

res.FIML <- growth(gcmodel, data1, missing = "fiml")
parameterEstimates(res.FIML)

#varphi=0

#TSRE
pat <- rsem.pattern(data1)
musig <- rsem.emmusig(pat, varphi=.1, max.it = 100000)
res2 <- growth(gcmodel, sample.cov=musig$sigma, sample.mean=musig$mu, sample.nobs=399,mimic='EQS')
ascov <- rsem.Ascov(pat, musig)
robust.se <- rsem.se(res2, ascov$Gamma)
 res.est <- cbind(coef(res2), robust.se$se[[1]])[5:9,]
 round(res.est,3)


# micecart
data.imp2.L1 <- mice(data1, m= 20, meth = "cart", minbucket = 5,seed = 20230304,print= FALSE)
complete.data2.L1 <- complete(data.imp2.L1,action = 'all')
sum5.L1 <- 0
for(i in 1:20){
  res5.L1 <- growth(gcmodel, complete.data2.L1[i][[1]])
  est5.L1 <- parameterEstimates(res5.L1)
  sum5.L1 <- sum5.L1 + est5.L1[c(13:15,20:21),4:9]
}

est.55.L1 <- sum5.L1/20
round(est.55.L1,3)

#miceforest
data.imp1.L1 <- mice(data1, m= 20, meth = "rf", ntree = 10,seed = 20230304,print= FALSE)
complete.data1.L1 <- complete(data.imp1.L1,action = 'all')
sum4.L1 <- 0
for(i in 1:20){
  res4.L1 <- growth(gcmodel, complete.data1.L1[i][[1]])
  est4.L1 <- parameterEstimates(res4.L1)
  sum4.L1 <- sum4.L1 + est4.L1[c(13:15,20:21),4:9]
}
est.44.L1 <- sum4.L1/20
round(est.44.L1,3)

#missforest
data.imp.L1 <- missForest(data1, maxiter = 10, ntree = 10, variablewise = TRUE,
                          mtry = 2)
res3.L1 <- growth(gcmodel, data=data.imp.L1$ximp)

parameterEstimates(res3.L1)

#knn
data.imp6 <- kNN(data1, k=5)
res6 <- growth(gcmodel, data.imp6)
summary(res6)




