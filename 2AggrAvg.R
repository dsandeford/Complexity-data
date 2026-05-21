############   2AggrAvg.R aggregates TSDat into AggrDat, uses averages.
###                     takes TSDat123.csv from 1ProcDat.R as input
#
setwd("/")
{############################## Get data ###############################################
  load("PolsVars.Rdata")
  TSDat <- read.table('TSDat123.csv', sep=",", header=TRUE, stringsAsFactors = FALSE)
  TSDat$Llama <- 0;   TSDat$Llama[TSDat$NGA == "Cuzco"] <- 1   ### "Other" animal = Llama
  rm(variables) #keep polities_all in memory so PolName and World.Region can be added 
}

{#################################### Aggregate variables into CCs ###########################
  AggrSCDat <- data.frame(NGA=TSDat$NGA, PolID=TSDat$PolID, Time=TSDat$Time,
                          Pop=NA, Terr=NA,                                                                # Social scale and area 
                          Hier=NA, Gov=NA, Infra=NA, Info=NA, Idea=NA, MilTech=NA, Complexity=NA, Agri=NA,    # CCs
                          Cap=NA )                   # CCs/language data used for imputation/Region used for k-fold testing
  for(i in 1:nrow(TSDat)){
    if(TSDat$PolPop[i] != 0 & TSDat$PolPop[i] != -999){AggrSCDat$Pop[i] <- log10(TSDat$PolPop[i])}       ### Pop
    if(TSDat$PolTerr[i] != 0 & TSDat$PolTerr[i] != -999){AggrSCDat$Terr[i] <- log10(TSDat$PolTerr[i])}    ### Terr 
    if(TSDat$CapPop[i] != 0 & TSDat$CapPop[i] != -999){AggrSCDat$Cap[i] <- log10(TSDat$CapPop[i])}       ### Cap
    
   
    dt <- subset(TSDat, select = c(SettlHier,AdmLev))[i,]                           ### Hier = mean(Settl, Adm)
    dt <- dt[dt != -999]
    if(length(dt) > 0){AggrSCDat$Hier[i] <- mean(dt)}                             
    
    
    dt <- subset(TSDat, select = c(ProfOfficer, ProfSoldier, ProfPriest,FullTBur,ExamSyst,MeritProm,GovtBldg,
                                   Court,LegCode,Judge,Lawyer,ConstrGov,ConstrNonGov,Impeach))[i,] #
    dt <- dt[dt != -999]
    if(length(dt) > 0){AggrSCDat$Gov[i] <- sum(dt)}            
  
    
    
    dt <- subset(TSDat, select = c(Irrigation,WaterSuppl,Market,FoodStor,Road,Bridge,Canal,Port,Mine,
                                   Courier,PostStation,PostService))[i,]
    dt <- dt[dt != -999]
    if(length(dt) > 0){AggrSCDat$Infra[i] <- sum(dt)}             
    
    
    dt <- subset(TSDat, select = c(Mnemonic, NonWRecord, Script, NonPhWrit, PhAlph, WRecord, Lists, Calendar, 
                                   SacrTxt, ReligLit, PractLit, History, Philosophy, SciLit, Fiction,
                                   Article, Token, PrecMetal, ForCoin, IndigCoin, PaperCurr))[i,]
    dt[dt == -999] <- 0
    AggrSCDat$Info[i] <- sum(dt)                                        
    
    
    ##### Add PolName to AggrSCDat
    AggrSCDat$PolName[i] <- polities_all$PolName[which(polities_all$PolID == TSDat[i,]$PolID)][1]
    

    dt <- subset(TSDat, select = c(PrimaryMSP,CertainMSP,BroadMSP,TargetedMSP,RulerMSP,
                                   ElitesMSP,CommonersMSP,RuleLegGods,SelectStatus,IdProsocial,
                                   IdEqual,IdElitCom,IdRuleCom,PublGoods))[i,]
    dt[dt == -999] <- 0
    AggrSCDat$Idea[i] <- sum(dt)
    
    dt <- subset(AggrSCDat, select = c(Gov, Infra, Idea, Info, Hier))[i,] 
    AggrSCDat$Complexity[i] <- sum(dt, na.rm=TRUE)
    }
}

{##################### Aggregate variables into WCs ##################################################
  WarDat <- data.frame(NGA=TSDat$NGA, PolID=TSDat$PolID, Time=TSDat$Time,
                       Metal=0, Project=0, Weapon=0, Animal=0, Armor=0, Defense=0)
  for(i in 1:nrow(TSDat)){
    dt <- subset(TSDat, select = c(Copper,Bronze,Iron,Steel))[i,]                        ### Metal
    for(j in 1:length(dt)){if(dt[j]==-999){dt[j] <- 0}}  ### Missing == absent
    WarDat$Metal[i] <- max(dt*1:4)
    dt <- subset(TSDat, select = c(Javelin, Atlatl, Sling, SelfBow, CompBow, Crossbow, TensSiege,
                                   SlingSiege, Artillery, HandGun))[i,]                  ### Projectiles
    for(j in 1:length(dt)){if(dt[j]==-999){dt[j] <- 0}}  
    WarDat$Project[i] <- max(dt*1:10)
    dt <- subset(TSDat, select = c(WarClub, BattleAxe, Dagger,Sword,Spear,Polearm))[i,]  ### Weapon
    for(j in 1:length(dt)){if(dt[j]==-999){dt[j] <- 0}}  
    WarDat$Weapon[i] <- sum(dt)
    dt <- subset(TSDat, select = c(Donkey, Llama, Camel, Elephant, Horse))[i,]           ### Animal 
    for(j in 1:length(dt)){if(dt[j]==-999){dt[j] <- 0}}  
    WarDat$Animal[i] <- max(dt*c(1,1,2,2,3))
    dt <- subset(TSDat, select = c(WoodArmor, LeathArmor, Chainmail, ScaleArmor, LaminArmor, PlateArmor,
                                   Shield, Helmet, Breastplate, LimbProt))[i,]           ### Armor
    for(j in 1:length(dt)){if(dt[j]==-999){dt[j] <- 0}}  
    WarDat$Armor[i] <- max(dt[1:6]*c(1,2,3,3,4,4)) + sum(dt[7:10]) #51
    dt <- subset(TSDat, select = c(DefPosition, Rampart, NonMStone, Palisade, StoneWall, Ditch, Moat, 
                                   FortCamp, ComplxFort, ModernFort, LongWall))[i,]      ### Defense
    for(j in 1:length(dt)){if(dt[j]==-999){dt[j] <- 0}}  
    WarDat$Defense[i] <- max(dt[1:5]*1:5) + max(dt[6:7]*1:2) + sum(dt[8:10]) + (dt[11]>0)
  }
}


{################################ Add MilTech and Complexity ####################################
  AggrDat <- cbind(TSDat[,1:3], subset(AggrSCDat, select = -c(NGA,PolID,Time)))
  AggrDat$MilTech <- apply(subset(WarDat, select = c(Metal, Project, Weapon, Animal, Armor, Defense)),1,sum)
  AggrDat$Complexity <- log10(AggrDat$Complexity + AggrDat$MilTech)
  AggrDat[which(AggrDat$Complexity=="-Inf")]$Complexity <- NA
}

{#########################################################################
  ### Agri 
  HistYield <- read.table('HistYield+.csv', sep=",", header=TRUE, stringsAsFactors = 	FALSE)
  AggrDat$Agri <- NA
  for(i in 1:nrow(AggrDat)){
    NGA <- AggrDat$NGA[i]
    Time <- AggrDat$Time[i]
    if(Time >= -10000){
      HY <- HistYield[, HistYield[1,] == Time]
      HY <- HY[HistYield[,1] == NGA]
      if(length(HY) == 1) {AggrDat$Agri[i] <- HY}
    }
  }
  
 #for(i in 1:nrow(AggrDat)){AggrDat$PropCoded[i] <- 100*sum(!is.na(AggrDat[i,4:13]))/10}
  AggrDat$uniq <- TSDat$uniq
  AggrDat$Dupl <- TSDat$Dupl
  

  AggrDat <- AggrDat[AggrDat$Dupl == "n",]
  AggrDat <- AggrDat[AggrDat$uniq == "y",]
  
  
  scalingData <- AggrDat
  
  
  # Remove erroneous territory data entries
  scalingData[which(scalingData$PolID=='IrSusa1'),]$Terr = NA # Village area, not polity territory given
  scalingData[which(scalingData$PolID=='MlJeJe1'),]$Terr = NA # Village does not have known territory, no source given in database, see seshat-db.com
  scalingData[which(scalingData$PolID=='EcJivaE'),]$Terr = NA # Village does not have known territory, no source given in database, see seshat-db.com
  scalingData[which(scalingData$PolID=='USMisSp'),]$Terr = NA # Village does not have known territory, no source given in database, see seshat-db.com
  
  
  
  save(scalingData, file = "TableData.Rdata")
}

