library("Quandl")
library("xts")
library("PerformanceAnalytics")
source("D:/FF data/Read_FF_Data_From_CSV.r") # breakpoints

# Load returns/factors
# monthly value wtd returns of six portfolios formed on S and BTM (2x3)
ff.6_S_BTM_AVWR_M<-Quandl("KFRENCH/6_S_BTM_AVWR_M", authcode='UrhC6e98rWr8wGppq2LF',type="xts")/100
colnames(ff.6_S_BTM_AVWR_M)<-c("SG","SN","SV","BG","BN","BV")
ff.factors_m <- Quandl("KFRENCH/FACTORS_M", authcode='UrhC6e98rWr8wGppq2LF',type='xts')/100
smb<-(apply(ff.6_S_BTM_AVWR_M[,1:3],1,sum)-apply(ff.6_S_BTM_AVWR_M[,4:6],1,sum))/3
hml<-(apply(ff.6_S_BTM_AVWR_M[,c(3,6)],1,sum)-apply(ff.6_S_BTM_AVWR_M[,c(1,4)],1,sum))/2
bp<-getBreakpoints()
charts.PerformanceSummary(ff.6_S_BTM_AVWR_M,colorset=rich6equal,ylog=TRUE)            
t(table.CalendarReturns(ff.6_S_BTM_AVWR_M))
table.Stats(ff.6_S_BTM_AVWR_M)
#chart.RollingPerformance(ff.6_S_BTM_AVWR_M[,1:3],120)
ff.returns<-getFactorReturns()
bp.beme<-bp[["BEME"]]
data <- as.xts(bp.beme[,"BP14"]-bp.beme[,"BP6"],order.by=as.Date(row.names(bp.beme),"%Y-%m-%d"))
colnames(data)<-"BP_14m6"
data$BP_6d14 <- bp.beme[,"BP6"]/bp.beme[,"BP14"]
data<-merge(data,lag(ff.returns[,"HML.1y"],-12),join = "left")
data<-merge(data,lag(ff.returns[,"HML.3y"],-36),join = "left")
data<-merge(data,lag(ff.returns[,"HML.5y"],-60),join = "left")
data<-merge(data,lag(ff.returns[,"HML.10y"],-120),join = "left")
par()
plot(as.matrix(data$BP_6d14),as.matrix(data$HML.10y))
plot(data$HML.10y)
abline(h=0,col="gray")
points(data$HML.5y, col="green",type="l")
par(new=TRUE)
plot(-data$BP_6d14,col="red",pch=18,type="p",xaxt="n",yaxt="n",xlab="",ylab=0)

lmfit<-lm(HML.10y~BP_6d14,data=data)
lmfit<-lm(HML.10y~BP_14m6,data=data)
