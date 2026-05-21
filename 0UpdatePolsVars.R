#####   Update PolsVar.Rdata
setwd("/")
 polities_all <- read.csv("polities_all.csv", header=TRUE, stringsAsFactors = FALSE)
 polities <- polities_all[polities_all$Dupl == "n",]
 variables <- read.csv("variables.csv", header=TRUE, stringsAsFactors = FALSE)
 save(polities,polities_all,variables, file="PolsVars.Rdata")



