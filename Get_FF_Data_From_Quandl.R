# Quandl(code, type = c("raw", "ts", "zoo", "xts"),
#	start_date, end_date,
#	transformation = c("", "diff", "rdiff", "normalize", "cumul", "rdiff_from"),
#	collapse = c("", "weekly", "monthly", "quarterly", "annual"),
#	rows, sort = c("desc", "asc"), meta = FALSE,
#	authcode = Quandl.auth())

#Quandl.auth(UrhC6e98rWr8wGppq2LF)
library("Quandl")
library("xts")
ff.factors_m <- Quandl("KFRENCH/FACTORS_M", authcode='UrhC6e98rWr8wGppq2LF',type='xts')
ff.6bgl_vm <- Quandl("KFRENCH/6BGL_VM", authcode='UrhC6e98rWr8wGppq2LF',type="xts")
ff.6_S_BTM_AVWR_M<-Quandl("KFRENCH/6_S_BTM_AVWR_M", authcode='UrhC6e98rWr8wGppq2LF',type="xts")
mydata

q<-Quandl.search("Fama French",authcode='UrhC6e98rWr8wGppq2LF')

printMonthly<-function(){
    q<-Quandl.search("Fama French",authcode='UrhC6e98rWr8wGppq2LF')
    for (i in 1:length(q)){
        if (q[[i]]$frequency=="monthly"){
            print(q[[i]]$name)
            print(q[[i]]$description)
            print(" ")
        }
    }
}
    
