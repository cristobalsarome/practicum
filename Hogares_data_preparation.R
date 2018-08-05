##Loading packages


#install.packages("ggplot2")
library(foreign)
library(ggplot2)
library(plyr)
library(bit)
library(magrittr)
library(reshape2) #melt
library(xlsx)
#install.packages("xlsx")
#install.packages("klaR")
library(klaR)#clustering for categorical variables
#install.packages("cba")
library(cba) #RockCluster


##Data preprocessing

#hogares (homes)
#Reading the .sav file (SPSS)
hogares.filename <- "Base EDSA hogares 2010.Difusión (V.BETA).sav"
hogares <- read.spss(hogares.filename)
hogares.attr<-attributes(hogares)
hogares.df <- read.spss(hogares.filename,to.data.frame = T)
#str(hogares.df)
#Writing data into a CSV
#write.csv(hogares.df,file="homes_data.csv")
#Removing 29 observations with missing values (5682 to 5653)
#LOST FIELD ATTRIBUTES SUBSETING
hogares.clean <- hogares.df[!is.na(hogares.df$PON_HOG),]

metadata <- as.data.frame(hogares.attr[c(2,3)],stringsAsFactors=F)
metadata$field_nr <- c(1:length(metadata[[1]]))
metadata$type <- as.vector(sapply(hogares.df,class))
metadata$levels <- sapply(hogares.df,levels) %>% sapply(paste,collapse="; ") %>% as.vector()
#We export metadata (only once)
#write.xlsx(metadata,"metadata.xlsx")


#We add some information to metadata
metadata2 <- as.data.frame(hogares.attr[c(2,3)],stringsAsFactors=F)
metadata2$field_nr <- c(1:length(metadata2[[1]]))
metadata2$type <- as.vector(sapply(hogares.clean,class))
metadata2$levels <- sapply(hogares.clean,levels) %>% sapply(paste,collapse="; ") %>% as.vector()
#write only once
#write.xlsx(cbind.data.frame(metadata,metadata2),"metadata2.xlsx")
#We import the english labels from the excel file
english_labels <- read.xlsx("metadata_modified.xlsx",1,stringsAsFactors=F)[[3]]
#write only once #write.csv(english_labels,file = "english_labels.csv")
metadata2 <- cbind.data.frame( english_labels,metadata2,stringsAsFactors=F)
metadata2 <- metadata2[c(4,1:3,5,6)]
#We change the field names of the data frame for the english ones
colnames(hogares.clean) <- english_labels

#We separate all fields that contain dichotomous (binary) information 
#relatedwith poverty
binary.fields<-sapply(hogares.df,nlevels)==2
#paste(unname(which(binary.fields)),collapse=", ") #used as reference in the process
poverty.binary <- c( 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 46, 45, 47, 48, 58, 59, 60, 61, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79)
names(poverty.binary) <-  names(hogares.df)[poverty.binary]
poverty.binary.df <- hogares.clean[poverty.binary]
pov.bin.meta <- metadata[poverty.binary,]
poverty.binary.df <- as.data.frame(sapply(poverty.binary.df,as.factor))
level.byn <- as.data.frame(sapply(poverty.binary.df,levels))

#We stadardize the levels of the values so all are expresed in terms
#of "deficitary" (living conditions are deficitary in observed aspect) adn
#"not_deficitary"

#The folowing fields need to change the name and invert the order
#We have chosen the first state to be deficitary
swap.levels<-c(4,20,21,26,42,43,45)
inv.lev.names <- c("not_deficitary","deficitary")
lev.names <-  c("deficitary","not_deficitary")
sapply(poverty.binary.df[swap.levels],setattr,"levels",inv.lev.names) %>%  invisible
poverty.binary.df[swap.levels] <- sapply(poverty.binary.df[swap.levels],factor,
                                         levels = lev.names) %>%  invisible

#The rest of the field only need to change the names,
#we rename all of them for convinience (due to the sapply behaviour)
#knowing that the fields that have already been swapped and renamed won't
#suffer anny change
sapply(poverty.binary.df,setattr,"levels",lev.names) %>%  invisible

#some fields are stil not factors
poverty.binary.df <- as.data.frame(sapply(poverty.binary.df,as.factor))

#finally we replace the columns in hogares.clean by the standardized columns
hogares.clean[poverty.binary] <- poverty.binary.df

#we generate a list of fields that are factors
hogares.factor <- hogares.clean[unlist(sapply(hogares.clean,class))=="factor"]
hogares.factor <- hogares.factor[-1] #remove the survey year


#levels translation food_safety_category
levels(hogares.clean$food_safety_category) <- c("safety","some_unsafety","unsafety")
hogares.clean$food_safety_category <- factor(hogares.clean$food_safety_category,ordered=T)
levels(hogares.clean$head_house_educa)<- c("sec_school_compl","sec_school_incompl")
levels(hogares.clean$head_house_empl) <- c("full_employment","precarious_employm","unemployed","inactive")
levels(hogares.clean$work_class) <- c("proff_middle_class","non_proffesional_middle_class","working_class","marginal")
hogares.clean$work_class <- factor(hogares.clean$work_class,ordered=T)