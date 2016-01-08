readBreakpointFileInfo<-function(fn="filelayouts.csv",dn="d:/FF data/"){
    out<-read.csv(paste0(dn,fn),header=TRUE,stringsAsFactors = FALSE)
}
readBreakpointFile<-function(csvfn,Col1Code,Col2Name,Col3Name,dn="d:/FF data/"){
    out<-read.csv(paste0(dn,csvfn),header=FALSE,stringsAsFactors = FALSE)
    v1<-out$V1
    v1<-suppressWarnings(as.numeric(v1))
    NonNAidx<-which(!is.na(v1))
    firstNonNA<-min(NonNAidx)
    out<-read.csv(paste0(dn,csvfn),header=FALSE,stringsAsFactors = FALSE,skip=(firstNonNA-1))
    rownames(out)<-out$V1
    if (is.na(Col3Name)){
        cnames<-c("date",Col2Name,paste0(rep("BP",20),seq(1:20)))
    } else {
        cnames<-c("date",Col2Name,Col3Name,paste0(rep("BP",20),seq(1:20)))
    }
    colnames(out)<-cnames
    out$date<-NULL
    out<-out[complete.cases(out),]
    if (Col1Code=="%Y"){
        temp<-paste0(rownames(out),"1201")
    } else { # code is %Y%m
        temp<-paste0(rownames(out),"01")
        
    }
    temp<-yearmontoeom(as.Date(temp,"%Y%m%d"))    
    rownames(out)<-temp
    return(out)
}

getBreakpoints<-function(fn="filelayouts.csv",dn="d:/FF data/"){
    bp_fileinfo<-readBreakpointFileInfo(fn,dn)
    out<-list()
    for (i in 1:nrow(bp_fileinfo)){
        csvfn<-bp_fileinfo[i,"FileName"]
        Col1Code<-bp_fileinfo[i,"Col1Code"]
        Col2Name<-bp_fileinfo[i,"Col2Name"]
        Col3Name<-bp_fileinfo[i,"Col3Name"]
        df<-readBreakpointFile(csvfn,Col1Code,Col2Name,Col3Name)        
        out[[bp_fileinfo[i,"Varname"]]]<-df
    }
    return(out)  
}

printAllBreakpoints<-function(){
    bp<-getBreakpoints()
    for (i in 1:length(bp)){
        print(names(bp)[i])
        print(head(bp[[i]]))
        print(tail(bp[[i]]))
        print(" ")
    }
}

getFactorReturns<-function(){
    library("Quandl")
    library("xts")
    library("PerformanceAnalytics")
    out <- Quandl("KFRENCH/FACTORS_M", authcode='UrhC6e98rWr8wGppq2LF',type='xts')/100
    out$HML.1y<-rollapply(1+out$HML,12,prod)-1
    out$HML.3y<-rollapply(1+out$HML,36,prod)^(1/3)-1
    out$HML.5y<-rollapply(1+out$HML,60,prod)^(1/5)-1
    out$HML.10y<-rollapply(1+out$HML,120,prod)^(1/10)-1
    out$SMB.1y<-rollapply(1+out$SMB,12,prod)-1
    out$SMB.3y<-rollapply(1+out$SMB,36,prod)^(1/3)-1
    out$SMB.5y<-rollapply(1+out$SMB,60,prod)^(1/5)-1
    out$SMB.10y<-rollapply(1+out$SMB,120,prod)^(1/10)-1
    out$Mkt.RF.1y<-rollapply(1+out$Mkt.RF,12,prod)-1
    out$Mkt.RF.3y<-rollapply(1+out$Mkt.RF,36,prod)^(1/3)-1
    out$Mkt.RF.5y<-rollapply(1+out$Mkt.RF,60,prod)^(1/5)-1
    out$Mkt.RF.10y<-rollapply(1+out$Mkt.RF,120,prod)^(1/10)-1
    out$RF.1y<-rollapply(1+out$RF,12,prod)-1
    out$RF.3y<-rollapply(1+out$RF,36,prod)^(1/3)-1
    out$RF.5y<-rollapply(1+out$RF,60,prod)^(1/5)-1
    out$RF.10y<-rollapply(1+out$RF,120,prod)^(1/10)-1
    index(out)<-yearmontoeom(index(out))
    return(out)
}

yearmontoeom<-function(dt){
    library(lubridate)
    mos<-as.numeric(format(dt,"%m")) 
    yrs<-as.numeric(format(dt,"%Y")) 
    yrs<-ifelse(mos==12,yrs+1,yrs)
    mos<-ifelse(mos==12,1,mos+1)
    dt1<-as.Date(as.character(yrs*10000+mos*100+1),"%Y%m%d")
    out<-dt1-days(1)
    return(out)
}

yeartoeom<-function(dt){
    library(lubridate)
    yrs<-as.numeric(format(dt,"%Y")) 
    dt1<-as.Date(as.character(yrs*10000+1200+1),"%Y%m%d")
    out<-dt1-days(1)
    return(out)
}