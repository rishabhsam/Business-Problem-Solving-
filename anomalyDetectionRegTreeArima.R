#-----------------------Setting path for access of libraries--------------------------------#
# .libPaths("C:\\ProgramData\\Rservices\\library")
#======================================================================================================#
# Loading required libraries
#======================================================================================================#
library(lubridate)
library(dplyr)
library(ggplot2)
library(tseries)
library(forecast)
library(party)

#======================================================================================================#
# Function definitions
#======================================================================================================#
na.ma                            <-function (x, k = 4, weighting = "exponential") 
{
  data <- x
  if (!anyNA(data)) {
    return(data)
  }
  if (k < 1) {
    stop("Parameter k has  to be larger than 0")
  }
  if (!is.null(dim(data)[2]) && !dim(data)[2] == 1) {
    stop("Wrong input type for parameter x")
  }
  if (!is.null(dim(data)[2])) {
    data <- data[, 1]
  }
  if (!is.numeric(data)) {
    stop("Input x is not numeric")
  }
  tempdata <- data
  for (i in 1:length(tempdata)) {
    if (is.na(tempdata[i])) {
      ktemp <- k
      usedIndices <- (i - ktemp):(i + ktemp)
      usedIndices <- subset(usedIndices, usedIndices >= 
                              1)
      t <- tempdata[usedIndices]
      while (length(t[!is.na(t)]) < 2) {
        ktemp <- ktemp + 1
        usedIndices <- (i - ktemp):(i + ktemp)
        usedIndices <- subset(usedIndices, usedIndices >= 
                                1)
        t <- tempdata[usedIndices]
      }
      if (weighting == "simple") {
        data[i] <- mean(tempdata[usedIndices], na.rm = TRUE)
      }
      else if (weighting == "linear") {
        weightsData <- 1/(abs(usedIndices - i) + 1)
        naCheck <- ifelse(is.na(tempdata[usedIndices]), 
                          0, 1)
        weightsData <- weightsData * naCheck
        sumWeights <- sum(weightsData)
        weightedData <- (tempdata[usedIndices] * weightsData)/sumWeights
        data[i] <- sum(weightedData, na.rm = TRUE)
      }
      else if (weighting == "exponential") {
        weightsData <- 1/(2^abs(usedIndices - i))
        naCheck <- ifelse(is.na(tempdata[usedIndices]), 
                          0, 1)
        weightsData <- weightsData * naCheck
        sumWeights <- sum(weightsData)
        weightedData <- (tempdata[usedIndices] * weightsData)/sumWeights
        data[i] <- sum(weightedData, na.rm = TRUE)
      }
      else {
        stop("Wrong input for parameter weighting. Has to be \"simple\",\"linear\" or \"exponential\".")
      }
    }
  }
  return(data)
}

create_yearflg                   <-function(x){
  if(x == "FY2015"){
    return(1)
  }else if(x == "FY2016"){
    return(2)
  }else if(x == "FY2017"){
    return(3)
  }else if(x == "FY2014"){
    return(0)
  }
}

create_quarflg                   <-function(x){
  temp<-strsplit(as.character(x),split = "-")
  x<-unlist(temp)[ c(FALSE,TRUE) ]
  if(x == "Q1"){
    return(1)
  }else if(x == "Q2"){
    return(2)
  }else if(x == "Q3"){
    return(3)
  }else if(x == "Q4"){
    return(4)
  }
}



#======================================================================================================#
# Preparing the data
#======================================================================================================#
ADS                              <-read.csv("C:\\Users\\James.C\\Documents\\backup\\backup\\ADS_V2.csv")
temp                             <-strsplit(as.character(ADS$Due.Date),split = " ")
ADS$Due.Date                     <-unlist(temp)[ c(TRUE,FALSE) ]
rm(temp)
ADS$Due.Date                     <- as.Date(ADS$Due.Date,format = "%Y-%m-%d")
ADS$week_name                    <-as.Date(ADS$week_name,format = "%Y/%m/%d")
#-----------------------Upcoming week--------------------------------#
upcomingWeek                     <- "2017-03-29"

#-----------------------Subsetting fiscal years from 2014 till 2017 for ARIMAX--------------------------------#
highVariancecountries            <- c("Greater China","MEA","Western Europe","Germany","United States")
consideredYearsforARIMAX         <- c("FY2015","FY2016","FY2017")
ADS                              <- ADS[which(ADS$MS.Fiscal.Year.Name %in% consideredYearsforARIMAX),]

#-----------------------Subsetting fiscal years from 2015 till 2017 for REG TREE--------------------------------#
consideredYearsforRGtree         <- c("FY2016","FY2017")
ADS_RGtree                       <- ADS[which(ADS$MS.Fiscal.Year.Name %in% consideredYearsforRGtree),]

#-----------------------Creating flags for years and quarters--------------------------------#
ADS$yearflag                     <-sapply(ADS$MS.Fiscal.Year.Name,create_yearflg)
ADS$quarterflag                  <-sapply(ADS$MS.Fiscal.Qtr.Name,create_quarflg)

#-----------------------Looking at NWS Gross--------------------------------#
ADS                              <-ADS[which(ADS$NWS.Scenario.Key == 2),]
weeks                            <-(unique(ADS[,c("yearflag","quarterflag","week_name")]))
weeks$week_name                  <-as.Date(weeks$week_name,format = "%Y/%m/%d")

#--------------------Taking useful columns and rolling up data at weekly level--------------------#
ADS_weeklevel                    <-ADS
ARIMAXtable                      <-NULL
initialREGtreetable              <- NULL
#--------------------Creating quantilies on low variances--------------------#
ADS_weeklevel                    <-ADS_weeklevel %>% group_by(week_name,WW.Area.Name)%>%
  summarise(weekly_CUS=sum(NWS.MS.Amt.Current.CUS))
ADS_weeklevel                    <-ADS_weeklevel %>% 
  filter(!(WW.Area.Name %in% highVariancecountries))
quantileTable<-aggregate(ADS_weeklevel$weekly_CUS,
                         by=list(ADS_weeklevel$WW.Area.Name),FUN=quantile)
temp                             <- as.data.frame(quantileTable$x)
Lower_cutoffIQR                  <-temp["25%"]-((temp["75%"]-temp["25%"])*1.5)
Upper_cutoffIQR                  <-temp["75%"]+((temp["75%"]-temp["25%"])*1.5)
tableofIQR                       <-cbind.data.frame(quantileTable[1],Lower_cutoffIQR,Upper_cutoffIQR)
colnames(tableofIQR)             <- c("Area","Lower_cutoffIQR","Upper_cutoffIQR")                                                 

#--------------------Considering countries only--------------------#
for(consideredCountry in highVariancecountries)
{
  ADS_weeklevel_country          <-ADS[which(ADS$WW.Area.Name == consideredCountry),]
  ADS_RGtree_country             <-ADS_RGtree[which(ADS_RGtree$WW.Area.Name == consideredCountry),]
  ADS_weeklevel_country          <-ADS_weeklevel_country %>% 
    group_by(yearflag,quarterflag,week_name) %>% 
    summarise(weekly_CUS = sum(NWS.MS.Amt.Current.CUS))
  ADS_RGtree_country             <-ADS_RGtree_country  %>% group_by(week_name,WW.Area.Name,PPG) %>% 
    summarize(weekly_CUS=sum(NWS.MS.Amt.Current.CUS))
  
  #--------------------Min Criterian according to countries for RG tree --------------------#
  minCriteria                    <- ifelse(consideredCountry == "Germany",.5,
                                             (ifelse(consideredCountry == "Greater China",.6,.7)))
  
  #--------------------Adding missing weeks--------------------#
  ADS_weeklevel_country          <-bind_rows(ADS_weeklevel_country,
                                               (setdiff(weeks,ADS_weeklevel_country
                                                        [,c("yearflag","quarterflag","week_name")])))
  
  #--------------------Quarter change flag--------------------#
  ADS_weeklevel_country               <-ADS_weeklevel_country[order(ADS_weeklevel_country$week_name),]
  x                                   <-ADS_weeklevel_country$quarterflag
  ADS_weeklevel_country$quarchangeflag<-as.numeric(!(x == c(x[-1],4)))
  ADS_weeklevel_country$quarchangeflag[nrow(ADS_weeklevel_country)] <- 1
  
  #--------------------Month end flag other than quarter ends--------------------#
  ADS_weeklevel_country$monthendpattern <- rep(x = 0,nrow(ADS_weeklevel_country))
  ADS_weeklevel_country$monthendpattern <- as.numeric(format(as.Date(ADS_weeklevel_country$week_name,
                                                                     format = "%Y/%m/%d"), "%d") %in% c("22","29") &
                                                        !(format(as.Date(ADS_weeklevel_country$week_name,
                                                                         format = "%Y/%m/%d"), "%m") %in% c("09","12","03","06")))
  
  
  #======================================================================================================#
  #Regression Tree
  #======================================================================================================#
  RGtreemodel                    <- ctree(formula = weekly_CUS ~ PPG ,data= ADS_RGtree_country,
                                            control = ctree_control(mincriterion = minCriteria))
  ADS_RGtree_country$leaf        <- predict(RGtreemodel,type="node")
  UniquefinalTable               <-unique(ADS_RGtree_country[,c("leaf","PPG","WW.Area.Name")])
  quantileTable                  <- aggregate(ADS_RGtree_country["weekly_CUS"],
                                                by = list(leaf=ADS_RGtree_country$leaf),quantile,
                                                prob=c(0.25,0.75))
  temp                           <- as.data.frame(quantileTable$weekly_CUS)
  Lower_cutoffRGtree             <-temp["25%"]-((temp["75%"]-temp["25%"])*1.5)
  Upper_cutoffRGtree             <-temp["75%"]+((temp["75%"]-temp["25%"])*1.5)
  tabletobemerged                <-cbind.data.frame(quantileTable[1],Lower_cutoffRGtree,
                                                      Upper_cutoffRGtree)
  UniquefinalTable               <-merge(x=UniquefinalTable,y=tabletobemerged,by.x="leaf",by.y="leaf",x.all=TRUE) 
  UniquefinalTable[1]            <-NULL
  colnames(UniquefinalTable)     <- c("PPG","Area","Lower_cutoffRGtree","Upper_cutoffRGtree")
  initialREGtreetable            <- rbind.data.frame(initialREGtreetable,UniquefinalTable)
  
  #======================================================================================================#
  #Tests before ARIMAX
  #======================================================================================================#
  
  #-----------------------Moving average on missing values--------------------------------#
  ADS_weeklevel_country$weekly_CUS        <-na.ma(ADS_weeklevel_country$weekly_CUS, weighting = "simple") 
  
  #-----------------------Creating timeseries--------------------------------#
  timeSeriescountry_weekly_CUS            <- ts(na.omit(ADS_weeklevel_country$weekly_CUS), 
                                                frequency=59.25)
  #--------------------Stationarity test--------------------#
  Stationarity_test                <- adf.test(timeSeriescountry_weekly_CUS, alternative = "stationary")
  Stationarity_test
  if(Stationarity_test$p.value > .05)
  {
    next;
    ARIMAXtable                     <-rbind.data.frame(ARIMAXtable,cbind(Area = consideredCountry,
                                                                         predicted_Week = upcomingWeek,
                                                                         Upper_cutoffARIMAX = NA,
                                                                         Lower_cutoffARIMAX = NA))
    
  }
  
  #--------------------Box L-Jung Test--------------------#
  Whitenoise_test                  <-Box.test(timeSeriescountry_weekly_CUS,lag = 20,type ="Ljung")
  Whitenoise_test
  if(Whitenoise_test$p.value > .05)
  {
    ARIMAXtable                     <-rbind.data.frame(ARIMAXtable,cbind(Area = consideredCountry,
                                                                         predicted_Week = upcomingWeek,
                                                                         Upper_cutoffARIMAX = NA,
                                                                         Lower_cutoffARIMAX = NA))
    next;
  }
  #======================================================================================================#
  #ARIMAX Modelling
  #======================================================================================================#
  
  fit_nonseasonal_original         <-auto.arima(timeSeriescountry_weekly_CUS[1:162], 
                                                seasonal=TRUE,
                                                xreg = cbind(ADS_weeklevel_country$quarchangeflag[1:162],
                                                             ADS_weeklevel_country$monthendpattern[1:162]))
  
  #======================================================================================================#
  #Forecasting
  #======================================================================================================#
  fcast_nonseasonal_original       <-forecast(fit_nonseasonal_original,
                                              xreg = cbind(ADS_weeklevel_country$quarchangeflag[163],
                                                           ADS_weeklevel_country$monthendpattern[163]))
  ARIMAXtable                      <-rbind.data.frame(ARIMAXtable,cbind(Area = consideredCountry,
                                                                        predicted_Week = upcomingWeek,
                                                                        Lower_cutoffARIMAX = fcast_nonseasonal_original$lower[1,2],
                                                                        Upper_cutoffARIMAX = fcast_nonseasonal_original$upper[1,2]))
  
  
}
ARIMAXtable_REGtreetable         <-merge(x=ARIMAXtable,y=initialREGtreetable,by.x="Area",by.y="Area",y.all = TRUE)

Outputdata                       <-bind_rows(tableofIQR,ARIMAXtable_REGtreetable)
Outputdata$predicted_Week        <-upcomingWeek
plot(fcast_nonseasonal_original)
ts.plot(as.ts(c(fcast_nonseasonal_original$fitted,fcast_nonseasonal_original$upper[,2])),timeSeriescountry_weekly_CUS[1:163],
        gpars = list(col = c("Red", "Black")),xlab='Week Number',ylab='Number of Leakages')
legend("topleft", legend = c("Actuals","Predicted"), col = 1:10, lty = 1)