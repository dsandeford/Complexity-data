### ProcDat processes data from a Seshat Wiki scrape file.csv into the table form (TSDat)
### ***NUMERIC VERSION*** Averages ranges, uncertainty, and disagreements
####    Run 0UpdatePolsVars.R to create PolsVar.Rdata
setwd("/")

{ ### 1. Construct CanonDat.csv
  ### Define the set of variables to process
  load("PolsVars.Rdata")
   
  polities <- rbind(polities, polities_all[polities_all$PolID == "JpNara*",]) ### Polity not crossing century mark
  Vars <- variables
  Vars <- Vars[Vars$Variable != "Other",]   ### Results in duplication
  dat <- read.table("full_stripped_oct2020.csv", sep=",", header=TRUE, quote = "", colClasses = "character")
  
  # Add NGAs
  dat <- dat[dat$Polity != "Code book",]
  dat <- subset(dat, select = -Error.Note)
  for(i in 1:nrow(dat)){
    NGA <- polities$NGA[dat$Polity[i] == polities$PolID]
    if(length(NGA) == 1) {dat$NGA[i] <- NGA}
  }
  dat <- dat[dat$NGA != "NGA",]   ####  Drop non-NGA macrostates
  write.csv(dat, file="Equinox2020_CanonDat.csv",  row.names=FALSE)
  
  CanonDat <- data.frame()
  for(i in 1:nrow(Vars)){
    dt <- dat[dat$Variable==Vars$Variable[i],]
    CanonDat <- rbind(CanonDat,dt)
  }

  # Eliminate data for polities not in the list
  CanonDat$Eliminate <- "n"
  for(i in 1:nrow(CanonDat)){
    if(is.element(CanonDat$Polity[i], polities$PolID) == FALSE){CanonDat$Eliminate[i] <- "y"}
  }
  CanonDat <- CanonDat[CanonDat$Eliminate == "n",]
  CanonDat <- subset(CanonDat, select = -Eliminate)
  
  # Eliminate duplicate variables in other sections
  CanonDat <- CanonDat[CanonDat$Section != "Economy variables",]
  CanonDat <- CanonDat[CanonDat$Section != "Economy and Technology variables (NGA-Level)",]

  ### Manually correct population values for CnEHan* where Value.From == 480000-53869588, 49150220-49730550
  CanonDat[12,]$Value.From = 480000
  CanonDat[12,]$Value.To = 53869588
  CanonDat[13,]$Value.From = 49150220
  CanonDat[13,]$Value.To = 49730550
  
  ### Manually correct population, territory, and largest settlement sizes for IrSusa1
  CanonDat[196,]$Value.From = 8500
  CanonDat[196,]$Value.To = 25000 #(Hole 1987:91)
  CanonDat[197,]$Value.From = 8500/2
  CanonDat[197,]$Value.To = 25000/2 # N occupied sites decreased from 82 to 53 so est. 50% decline, not 90%, The coder invokes a population collapse for IrSusa1 @ -3900 but this is a misreading of the source material. It collapsed in the adjacent Deh Luran Plain but not in Susiana. See Hole 1987 and pp. 42-3, tables 8-9 on p. 63, and figures 9-10 on p. 73 in chapter preceding Hole 1987. I don't have time now to do the computations but the number of occupied sites in Susiana decreased from 82 to 53 -- so the population did not decline by 90% as now recorded. 
  CanonDat[626,]$Value.From = 1500
  CanonDat[626,]$Value.To = 2100 # (Hole 1987:73, 90)
  CanonDat[1006,]$Value.From = 750
  CanonDat[1006,]$Value.To = 3600
  
  write.csv(CanonDat, file="CanonDat.csv",  row.names=FALSE)
}


{  ### 2. Process values and dates 
  # Convert categorical values to numbers
  load("PolsVars.Rdata")
  polities <- polities_all
  SelDat <- CanonDat
  SelDat$error <- "y"
  
  for(i in 1:nrow(SelDat)){
    for(j in 6:7){
      if(SelDat[i,j] == "present"){SelDat[i,j] <- "1"; SelDat$error[i] <- "n"}
      if(SelDat[i,j] == "inferred present"){SelDat[i,j] <- "1"; SelDat$error[i] <- "n"}
      if(SelDat[i,j] == "inferred absent"){SelDat[i,j] <- "0"; SelDat$error[i] <- "n"}
      if(SelDat[i,j] == "absent"){SelDat[i,j] <- "0"; SelDat$error[i] <- "n"}
      if(SelDat[i,j] == "suspected unknown"){SelDat[i,j] <- "0"; SelDat$error[i] <- "n"}
      if(SelDat[i,j] == "unknown"){SelDat[i,j] <- ""; SelDat$error[i] <- "n"}
      if(SelDat[i,j] == "uncoded"){SelDat[i,j] <- ""; SelDat$error[i] <- "n"}
      if(SelDat[i,j] == "not applicable"){SelDat[i,j] <- ""; SelDat$error[i] <- "n"}
    }}
  
  # Change BCE to negative years and remove CE.
  SelDat1 <- SelDat
  for(i in 1:nrow(SelDat)){
    if(substr(SelDat[i,8], (nchar(SelDat[i,8]) - 2) , nchar(SelDat[i,8]) ) =="BCE" )
    {a <- -as.numeric(substr(SelDat[i,8], 1, (nchar(SelDat[i,8]) - 3)))
    SelDat[i,8] <- a}
    if(substr(SelDat[i,9], (nchar(SelDat[i,9]) - 2) , nchar(SelDat[i,9]) ) =="BCE" )
    {a <- -as.numeric(substr(SelDat[i,9], 1, (nchar(SelDat[i,9]) - 3)))
    SelDat[i,9] <- a}
  }
  for(i in 1:nrow(SelDat)){
    if(substr(SelDat[i,8], (nchar(SelDat[i,8]) - 1) , nchar(SelDat[i,8]) ) =="CE" )
    {a <- as.numeric(substr(SelDat[i,8], 1, (nchar(SelDat[i,8]) - 2)))
    SelDat[i,8] <- a}
    if(substr(SelDat[i,9], (nchar(SelDat[i,9]) - 1) , nchar(SelDat[i,9]) ) =="CE" )
    {a <- as.numeric(substr(SelDat[i,9], 1, (nchar(SelDat[i,9]) - 2)))
    SelDat[i,9] <- a}
  }
  
#  a=SelDat1[is.na(SelDat$Date.From),]; write.csv(a, file="errors.csv", row.names=FALSE)
  SelDat <- SelDat[is.na(SelDat$Date.From) == FALSE,]
  
  ##### Process uncertain by moving the value to Value.To
  SelDat1 <- SelDat
  for(i in 1:nrow(SelDat)){
    if(SelDat$Value.Note[i] == "uncertain"){
      dt <- SelDat[c(i,i+1),]
      if(all(dt[1,c(1:5,8:12)] == dt[2,c(1:5,8:12)])){
        SelDat1$Value.To[i] <- SelDat1$Value.From[i+1]
        SelDat1$Value.Note[i+1] <- "eliminate"
      }
    }
  }
  SelDat <- SelDat1 <- SelDat1[SelDat1$Value.Note != "eliminate",]
  
  ##### Process disagreements
  for(i in 1:nrow(SelDat1)){
    if(SelDat1$Value.Note[i] == "disputed"){
      dt <- SelDat1[c(i,i+1),]
      if(all(dt[1,c(1:5,8:12)] == dt[2,c(1:5,8:12)])){
        SelDat1$Value.To[i] <- SelDat1$Value.From[i+1] 
        SelDat1$Value.Note[i+1] <- "eliminate"
      }
    }
  }
  SelDat <- SelDat1 <- SelDat1[SelDat1$Value.Note != "eliminate",]
  
  ##### Reduce ranges, uncertain, and disputed to averages
  for(i in 1:nrow(SelDat1)){
    if(SelDat1$Value.To[i] != ""){
      SelDat1$Value.From[i] <- mean(c(as.numeric(SelDat$Value.From[i]),as.numeric(SelDat$Value.To[i])))}
  }
  SelDat <- SelDat1
}

{  ###### 3. Reshape SelDat into TSDat
  TSDat <- matrix(nrow=0,ncol=3+nrow(Vars))
  colnames(TSDat) <- c("NGA","PolID","Time",Vars$ShortName)

  ### First, process data that are not time-stamped
  SelDatND <- SelDat[SelDat$Date.From == "",]
  errors <- data.frame()
  for(i in 1:nrow(polities)){
    tmin <- ceiling(0.01*min(polities$Start[i]))
    tmax <- floor(0.01*max(polities$End[i]))
    tsdt <- matrix(nrow=c(length(100*tmin:tmax)),ncol=(3+nrow(Vars)))
    tsdt[,1] <- polities$NGA[i]
    tsdt[,2] <- polities$PolID[i]
    tsdt[,3] <- 100*tmin:tmax
    for(j in 1:nrow(Vars)){
      dt <- SelDatND[SelDatND$Polity==polities$PolID[i] & SelDatND$Variable==Vars$Variable[j],]
      if(nrow(dt) == 1){ tsdt[,3+j] <- dt$Value.From[1]     }
      else{if(nrow(dt)>1){
        print(dt[,c(2,5,6,8)])  ####### If more than 1 row, print warning, and add to the errors table
        errors <- rbind(errors, dt)
        tsdt[,3+j] <- dt$Value.From[1]} }  
    }
    TSDat <- rbind(TSDat, tsdt)
  }
  
  ##### Next, add time-stamped data
  SelDat <- SelDat[SelDat$Date.From != "",]   ### Eliminate data rows that have been processed
  for(i in 1:nrow(SelDat)){ ### Extend simple dates to (nearly) the whole century
    if(SelDat$Date.To[i] == ""){time <- as.numeric(SelDat$Date.From[i])
    SelDat$Date.From[i] <- as.character(time-49)
    SelDat$Date.To[i] <- as.character(time+49)}
  }   
  
  TSDat <- as.data.frame(TSDat, stringsAsFactors = FALSE) # Convert to data frame, as character
  TSDat1 <- matrix(nrow=0, ncol=ncol(TSDat))
  SelDat$error <- 1:nrow(SelDat)
  Used <- matrix(nrow=0, ncol=ncol(SelDat))
  for(i in 1:nrow(polities)){
    tsdt <- TSDat[TSDat$NGA == polities$NGA[i] & TSDat$PolID == polities$PolID[i],]
    for(j in 1:nrow(Vars)){
      for(t in 1:nrow(tsdt)){
        dat <- SelDat[SelDat$NGA == tsdt$NGA[t] & SelDat$Polity == tsdt$PolID[t] 
                      & SelDat$Variable == Vars$Variable[j] 
                      & as.numeric(SelDat$Date.From) <= as.numeric(tsdt$Time[t]) 
                      & as.numeric(SelDat$Date.To) >= as.numeric(tsdt$Time[t]),]
        if(nrow(dat) >= 1){
          tsdt[t,3+j] <- dat$Value.From[1]
          Used <- rbind(Used,dat[1,])
        }
      }
    }
    TSDat1 <- rbind(TSDat1,tsdt)
  }
  TSDat <- TSDat1
  
  ### Replace missing values for a particular century with data for the polity, if available
  out <- data.frame()
  for(i in 1:nrow(TSDat)){
    for(j in 1:nrow(Vars)){
      if(is.na(TSDat[i,3+j])) {
        dt <- SelDat[SelDat$Polity == TSDat$PolID[i] & SelDat$Variable == Vars$Variable[j],]
        if(nrow(dt)>0){ TSDat1[i,3+j] <- dt$Value.From[1]; out <- rbind(out,dt) }
      }
    }
  }

  ##### Substitute NA with ""
  for(i in 1:nrow(TSDat)){
    for(j in 1:ncol(TSDat)){
      if(is.na(TSDat[i,j])) {TSDat[i,j] <- ""}
    }
  }
  ##### Flag redundant rows
  TSDat$uniq <- "y"
  for(i in 2:nrow(TSDat)){
    if(all( TSDat[i,c(1:2,4:(ncol(TSDat)-1))] == TSDat[(i-1),c(1:2,4:(ncol(TSDat)-1))] ) == TRUE) 
    {TSDat$uniq[i] <- "n"}
  }
  TSDat1 <- TSDat[TSDat$uniq == "y",]
  
  ##### Calculate proportion coded
  PropCoded <- matrix(nrow=nrow(TSDat), ncol=1)
  for(i in 1:nrow(TSDat)){
    PropCoded[i,1] <- sum(TSDat[i,4:length(TSDat)] != "")
  }
  PropCoded <- round((100*PropCoded/(length(TSDat) - 3)), digits=0)
  TSDat <- cbind(TSDat[,1:3],PropCoded,TSDat[,4:length(TSDat)])
  hist(TSDat$PropCoded)
  
  ###### Add Dupl column
  TSDat$Dupl <- NA
  for(i in 1:nrow(TSDat)){
    TSDat$Dupl[i] <- polities_all$Dupl[polities_all$NGA == TSDat$NGA[i] & polities_all$PolID == TSDat$PolID[i]]
  }
  
  write.csv(TSDat, file="TSDat1.csv",  row.names=FALSE)
}


{ ##### 4. Additional checks
  ##### Check for data that were not used
  for(i in 1:nrow(SelDat)){
    if(is.element(row.names(SelDat[i,]), row.names(Used))){SelDat$Comment[i] <- "eliminate"}
  }
  Remainder <- SelDat[SelDat$Comment != "eliminate",]
  Remainder$Date.From <- as.numeric(Remainder$Date.From)
  Remainder$Date.To <- as.numeric(Remainder$Date.To)
  
}


#########################################################################################
###  Replace missing data with -999
TSDat1 <- TSDat <- read.table('TSDat1.csv', sep=",", header=TRUE, colClasses = "character")

for(j in 5:(ncol(TSDat1)-2)){
  for(i in 1:nrow(TSDat1)){
    if(is.na(TSDat1[i,j])){TSDat1[i,j] <- -999} 
    if(TSDat1[i,j] == "" | TSDat1[i,j] == "U" | TSDat1[i,j] == "U*"){TSDat1[i,j] <- -999} 
  }
  TSDat1[,j] <- as.numeric(TSDat1[,j])
}

### Find problems
a=data.frame()
for(j in 5:(ncol(TSDat1)-2)){
  for(i in 1:nrow(TSDat1)){
    if(is.na(TSDat1[i,j])){a<- rbind(a,c(i,j)); print(c(i,j))}
  }
}

for(i in 1:nrow(a)){print( c(TSDat$PolID[a[i,1]], colnames(TSDat)[a[i,2]], TSDat[a[i,1], a[i,2]]) )}

TSDat <- TSDat1
write.csv(TSDat, file="TSDat123.csv",  row.names=FALSE)





#####################################
#### Fix values by hand (needs to be also fixed on the Wiki)
TSDat$LargestArmy[TSDat$PolID == "FrBurbL"] <- mean(c(130000,150000))
write.csv(TSDat, file="TSDat123.csv",  row.names=FALSE)
