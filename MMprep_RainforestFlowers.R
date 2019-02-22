#Preparing batch upload of Rainforest Flowers data to Multimedia module (Subject/Keywords field in Detail tab)
list.files()
flr<-read.csv('./SimplifiedRawData/Verified_RI19_26_27_28_24_aggregated_simple.csv' ,na.strings = c("", "NA"))

library("dplyr")
library("tidyr")
#=============================================================================================================
#PART I: Preparing columns for plant parts ####
#=============================================================================================================
#1.1 Prepare the dataframe####
#select only necesary columns
names(flr)
flr2<-flr[,c(3,4,7,10)]
str(flr2)

#how many rows have NA for colums Flower,Fruit and Anything.else?
flr2NA<-flr2%>%
  filter(is.na(Flower)) %>%
  filter(is.na(Fruit)) %>%
  filter(is.na(Anything.else)) 
nrow(flr2NA)#166
#should expect 4420-166 =4254 for Verified_RI19_26_27_28_24_aggregated_simple

#Split "Anything.else" column by ", "
flr2$Anything.else<-as.character(flr2$Anything.else)
flr3<-flr2%>% 
  separate(Anything.else,c('Anything.else1','Anything.else2','Anything.else3',
                           'Anything.else4','Anything.else5','Anything.else6','Anything.else7',
                           'Anything.else8','Anything.else9'), ", ")

#1.2. gather####
flr.gather<-gather(flr3, key="category", value= "parts", "Flower", "Fruit", "Anything.else1",
                   "Anything.else2","Anything.else3","Anything.else4","Anything.else5",
                   "Anything.else6","Anything.else7","Anything.else8","Anything.else9")
head(flr.gather)
#delete blank rows
flr.gather2<-flr.gather %>% na.omit()
head(flr.gather2)
#remove the "category" column
flr.gather3<-flr.gather2[,-2]
head(flr.gather3)

#order by Catalog.IRN
flr.gather3<-flr.gather3[order(flr.gather3$MM_IRN),]

#find the unique "parts" for each Catalog.IRN 
flr.unique<-unique(flr.gather3)
head(flr.unique)

#1.3. spread#####
#createa new column "id.row" based on length within Catalog.IRN
flr.unique$id.row<- sequence(rle(flr.unique$MM_IRN)$length)
head(flr.unique)
flr.spread<-spread(flr.unique,
                   key=id.row,
                   value=parts,
                   fill="")
head(flr.unique)
#1.4.format intos a EMu spread format####

#turning all the data to lower case
flr.spread<-flr.spread %>%
  mutate_all(tolower)

#rename column names
library(data.table)
setnames(flr.spread, old=c("1", "2", "3", "4", "5"), 
         new=c("DetSubject_tab(1)", "DetSubject_tab(2)", 
               "DetSubject_tab(3)", "DetSubject_tab(4)","DetSubject_tab(5)"))
head(flr.spread)

#=============================================================================================================
#PART II: Preparing columns for plant color columns ####
#=============================================================================================================
#2.1. Prepare the dataframe####
#2.2.1 Dominant Flower Color 
#add column title for each row
DomFl<-flr[,c(3,5)]
#delete blank rows
DomFl2<-DomFl %>% na.omit()
#add column title in each row and add a pipe (|) at the end 
DomFl2$DomFlColor<-paste("dominantflower: ", DomFl2$Dominant.Flower)
DomFl3<-DomFl2[,c(1,3)]
head(DomFl3)

#2.2.2 Other Flower Color 
#add column title for each row
OtherFl<-flr[,c(3,6)]
#delete blank rows
OtherFl2<-OtherFl %>% na.omit()
#add column title in each row and add a pipe (|) at the end 
OtherFl2$OtherFlColor<-paste("otherflower: ", OtherFl2$Other.Flower )
OtherFl3<-OtherFl2[,c(1,3)]
head(OtherFl3)

#2.2.3 Dominant Fruit Color 
#add column title for each row
DomFr<-flr[,c(3,8)]
#delete blank rows
DomFr2<-DomFr %>% na.omit()
#add column title in each row and add a pipe (|) at the end 
DomFr2$DomFrColor<-paste("dominantfruit: ", DomFr2$Dominant.Fruit)
DomFr3<-DomFr2[,c(1,3)]
head(DomFr3)

#2.2.4 Other Flower Color 
#add column title for each row
OtherFr<-flr[,c(3,9)]
#delete blank rows
OtherFr2<-OtherFr %>% na.omit()
#add column title in each row and add a pipe (|) at the end 
OtherFr2$OtherFrColor<-paste("otherfruit: ", OtherFr2$Other.Fruit)
OtherFr3<-OtherFr2[,c(1,3)]
head(OtherFr3)

#=============================================================================================================
#Part III: 
#=============================================================================================================
#full_join all with the color columns and parts column from Part I
#3.1 Join all the dataframes 
DomFl3$MM_IRN<-as.character((DomFl3$MM_IRN))
OtherFl3$MM_IRN<-as.character((OtherFl3$MM_IRN))
DomFr3$MM_IRN<-as.character((DomFr3$MM_IRN))
OtherFr3$MM_IRN<-as.character((OtherFr3$MM_IRN))

all<-flr.spread%>%full_join(DomFl3, by="MM_IRN") %>%
  full_join(DomFr3, by="MM_IRN")%>%
  full_join(OtherFr3, by="MM_IRN")
names(all)
head(all)

#3.2 Gather and delete  all the blank cells 
#Now we need to delete all the blank cells.
#First, we need to gather
#1.2. gather###
all.gather<-gather(all, key="categories", value= "parts","DetSubject_tab(1)", "DetSubject_tab(2)", "DetSubject_tab(3)",
                   "DetSubject_tab(4)", "DetSubject_tab(5)", "DomFlColor", "DomFrColor","OtherFrColor")
#where blank cell, add NA
all.gather[all.gather==""]<-NA
#Delete all the NAs
all.gather2<-all.gather%>% na.omit()
#3.3 Spread
#sort by MM_IRN
#order by Catalog.IRN
all.gather2<-all.gather2[order(all.gather2$MM_IRN),]

#createa new column "id.row" based on length within Catalog.IRN
all.gather2$id.row<- sequence(rle(all.gather2$MM_IRN)$length)
head(all.gather2)
#delete "category 
all.gather2<-all.gather2[,-2]
all.spread<-spread(all.gather2,
                   key=id.row,
                   value=parts,
                   fill="")

#rename column names
library(data.table)
setnames(all.spread, old=c("MM_IRN","1", "2", "3", "4", "5", "6", "7"), 
         new=c("irn", "DetSubject_tab(1)", "DetSubject_tab(2)", 
               "DetSubject_tab(3)", "DetSubject_tab(4)","DetSubject_tab(5)", "DetSubject_tab(6)","DetSubject_tab(7)"))
head(all.spread)
#write.csv(all.spread, "MM_keywords_RI19_26_27_28_24.csv", row.names=FALSE)
##=============================================================================================================
#Part V: Checking the row number of discrapancies beween flr and all.spread. 
#=============================================================================================================
#flr adn all.spread should have the same numbers but the difference is 166 for 
#"Verified_RI19_26_27_28_24_aggregated_simple.csv" - that's because these rows have all blank cells.
#i.e. no classifications from Rainforest Flowers
setdiff(flr$MM_IRN,all.spread$MM_IRN)
