rm(list=ls())

library(dplyr)
library(sfdSAR)
library(tidyr)
library(icesVMS)
library(vmstools)
## load sample vms data
#data(test_vms)

gear_widths <- get_benthis_parameters()

data(metier_lookup)

L6_metiers <- read.csv("L6_lut.csv")

rcgs <- unique(L6_metiers$RCG)

i<-1

cname <- paste(rcgs[i], "_LE_MET_Level6", sep="")

temp <- L6_metiers %>%
            filter(RCG == rcgs[i]) %>%
           select(c(OLD_metier_level_6, NEW_metier_level_6)) %>%
           rename("LE_MET_level6" = "OLD_metier_level_6", 
           cname = "NEW_metier_level_6")

colnames(temp)[2] <- cname

mlup <- left_join(metier_lookup, temp, by = "LE_MET_level6")  

for (i in 2:5){
  cname <- paste(rcgs[i], "_LE_MET_Level6", sep="")
  
  temp <- L6_metiers %>%
    filter(RCG == rcgs[i]) %>%
    select(c(OLD_metier_level_6, NEW_metier_level_6)) %>%
    rename("LE_MET_level6" = "OLD_metier_level_6", 
           cname = "NEW_metier_level_6")
  
  colnames(temp)[2] <- cname
  
  mlup <- left_join(mlup, temp, by = "LE_MET_level6")  
  
}

mlup$unique_values <- rowSums(!is.na(mlup[9:13]))

mlup <- mlup[rev(order(mlup$unique_values)),]

write.csv(mlup, "New_L6_lookup.csv", row.names = F)
  
mlup$no_benthis <- mlup$Benthis_metiers

mlup$no_benthis[is.na(mlup$no_benthis)] <- "NO_BENTHIS"

dump <- NULL

for(i in 1:5){
  dump <- rbind(dump, data.frame(L6 = mlup$LE_MET_level6, identifier = paste(mlup[,8+1], mlup$no_benthis, sep = "_")), rcgs[i])
}

  # join widths and lookup
aux_lookup <-
  gear_widths %>%
  right_join(metier_lookup, by = c("benthis_met" = "Benthis_metiers"))

# add aux data to vms
vms <-
  aux_lookup %>%
  right_join(temp, by = c("LE_MET_level6" ="leMetLevel6"))

# calculate the gear width model
vms$gearWidth_model <-
  predict_gear_width(vms$gear_model, vms$gear_coefficient, vms)

predict_gear_width(vms$gear_model, vms$gear_coefficient, vms)


predict_gear_width

unique(vms$gear_coefficient)
head(vms
)
vms[is.na(vms$gear_coefficient),]
unique(vms$LE_MET_level6)
