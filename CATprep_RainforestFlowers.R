#Preparing batch upload of Rainforest Flowers data to Catalogue module (Description tab)
list.files()
flr<-read.csv('./SimplifiedRawData/Verified_All_Old_no_ferns_section1_simple.csv' ,na.strings = c("", "NA"))

library("dplyr")
library("tidyr")
#=============================================================================================================
#PART I: Preparing Material Field####
#=============================================================================================================
#1.1 Prepare the dataframe####
#Note: row for 3824414, "Insect" "Spider"	and "Fungus or Animal"
#were merged into one row "Insect	Spider	Fungus or Animal"
#de-select unnecesary columns
head(flr)
flr2<-flr[,c(2,4,7,10)]
str(flr2)

#how many rows have NA for colums 4,7 and 10? Answer:181
flr2NA<-flr2%>%
            filter(is.na(Flower)) %>%
            filter(is.na(Fruit)) %>%
            filter(is.na(Anything.else)) 
  
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
flr.gather3<-flr.gather2[,-8]
head(flr.gather3)

#order by Catalog.IRN
flr.gather3<-flr.gather3[order(flr.gather3$Catalog.IRN),]

#find the unique "parts" for each Catalog.IRN 
flr.unique<-unique(flr.gather3)
head(flr.unique)

#1.3. spread#####
#createa new column "id.row" based on length within Catalog.IRN
flr.unique$id.row<- sequence(rle(flr.unique$Catalog.IRN)$length)
head(flr.unique)
flr.spread<-spread(flr.unique,
                 key=id.row,
                 value=parts,
                 fill="")
head(flr.unique)
head(flr.spread)
#1.4.format intos a EMu spread format####

#turning all the dat to lower case
flr.spread<-flr.spread %>%
  mutate_all(tolower)

#rename column names
library(data.table)
setnames(flr.spread, old=c("Catalog.IRN","1", "2", "3", "4", "5", "6"), 
         new=c("irn", "DesMaterials_tab(+)", "DesMaterials_tab(+)", 
               "DesMaterials_tab(+)", "DesMaterials_tab(+)","DesMaterials_tab(+)", "DesMaterials_tab(+)"))
head(flr.spread)

#write.csv(flr.spread, "Cat.Materials_All_Old_no_ferns_section1.csv",row.names=FALSE)
#Note: two Catalogue records were not found in EMu (3825035, 3835174)

#1.5. Preparing the dataframe for Part III####
#add NA
flr.spread2<-flr.spread[flr.spread==""]<-NA
#concatinate all the columns
flr.spread3<-unite(flr.spread, parts, "DesMaterials_tab(1)":"DesMaterials_tab(6)" , sep = " | ")
#remove NA - regular expression not working. 
#Basically, need to remove anything after the last letter
 #flr.spread3$parts <- gsub("|", "", flr.spread3$parts)
#gsub("|| ", "", flr.spread3$parts)

#write.csv(flr.spread3, "Measures_for_Descriptio_All_Old_no_ferns_section1.csv" )
#=============================================================================================================
#PART II: Preparing Description Field ####
#=============================================================================================================
#2.1. Prepare the dataframe####
#Note: row for 3824414, "Insect" "Spider"	and "Fungus or Animal"
#were merged into one row "Insect	Spider	Fungus or Animal"
#de-select unnecesary columns
head(flr)
col<-flr[,c(2,5,6,8,9)]
str(col)


#split "Other.Flower" and "Other.Fruit by ", "
col$Other.Flower<-as.character(col$Other.Flower)
col$Other.Fruit<-as.character(col$Other.Fruit)

col2<-col%>% 
  separate(Other.Flower,c('Other.Flower1','Other.Flower2','Other.Flower3',
                           'Other.Flower4','Other.Flower5','Other.Flower6','Other.Flower7',
                           'Other.Flower8','Other.Flower9'), ", ")%>%
  separate(Other.Fruit,c('Other.Fruit1','Other.Fruit2','Other.Fruit3',
                          'Other.Fruit4','Other.Fruit5','Other.Fruit6','Other.Fruit7',
                          'Other.Fruit8','Other.Fruit9'), ", ")%>%
  separate(Dominant.Flower,c('Dominant.Flower1','Dominant.Flower2'), ", ")
head(col2)  

#2.2. gather####
col.gather<-gather(col2, key="category", value= "color", "Other.Flower1", "Other.Flower2", "Other.Flower3", 
                   "Other.Flower4", "Other.Flower5", "Other.Flower6", "Other.Flower7", "Other.Flower8", "Other.Flower9",
                   "Dominant.Fruit","Other.Fruit1", "Other.Fruit2", "Other.Fruit3", "Other.Fruit4", "Other.Fruit5", 
                    "Other.Fruit6", "Other.Fruit7", "Other.Fruit8", "Other.Fruit9","Dominant.Flower1","Dominant.Flower2")
head(col.gather)

#delete blank rows
col.gather2<-col.gather %>% na.omit()
head(col.gather2)

#2.3. spread####
#2.3.1. Dominent Flower####
#find the unique "category"="DomiantFlower" for each Catalog.IRN 
col.unique.DFl<-col.gather2%>%filter(category %in%  c("Dominant.Flower1","Dominant.Flower2"))%>%
                        select(-category)%>%
                        arrange(Catalog.IRN) %>%
                        distinct()
#spread Dominant Flowers
col.DFl.spread<-col.unique.DFl%>%group_by(Catalog.IRN)%>%
                              mutate(x_count=1:n())%>%
                              spread(key=x_count,
                                     value=color,
                                     fill="")
#concatinate all the colors                          
##note: note make sure to adjust the "1","2" part. they're the name of columns on "col.DFl.spread"

col.DFl.con<-col.DFl.spread%>%
  unite("DomFlColor","1","2",remove = FALSE, sep =" ")
#show the white space
paste(col.DFl.con$DomFlColor)
# trimming leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
col.DFl.con$DomFlColor <- trim(col.DFl.con$DomFlColor)

#select columns of interest
names(col.DFl.con)
DominantFlColor<-col.DFl.con[,c(1,2)]
head(DominantFlColor)

#formatting 
DominantFlColor$DomFlColor<-paste("dominantflower: ", DominantFlColor$DomFlColor, " | ")
#turning all the dat to lower case
DominantFlColor<-DominantFlColor %>%
  mutate_all(tolower)

#b)2.3.2. Other Flower####
#find the unique "category"="OtherFlower" for each Catalog.IRN 
col.unique.OFl<-col.gather2%>%filter(category %in%  c("Other.Flower1","Other.Flower2","Other.Flower3","Other.Flower4", "Other.Flower5","Other.Flower6", "Other.Flower7","Other.Flower8","Other.Flower9"))%>%
                            select(-category)%>%
                            arrange(Catalog.IRN) %>%
                            distinct()
#spread oTHER Flowers
col.OFl.spread<-col.unique.OFl%>%group_by(Catalog.IRN)%>%
  mutate(x_count=1:n())%>%
  spread(key=x_count,
         value=color,
         fill="")
#concatinate all the colors                          
#note: note make sure to adjust the "1","2","3","4","5" part. they're the name of columns on "col.OFl.spread"
col.OFl.con<-col.OFl.spread%>%
  unite("OtherFlColor","1","2","3","4","5",remove = FALSE, sep =" ")
#show the white space
paste(col.OFl.con$OtherFlColor)
# trimming leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
col.OFl.con$OtherFlColor <- trim(col.OFl.con$OtherFlColor)

#select columns of interest
names(col.OFl.con)
OtherFlColor<-col.OFl.con[,c(1,2)]
head(OtherFlColor)

#formatting 
OtherFlColor$OtherFlColor<-paste("otherflower: ", OtherFlColor$OtherFlColor, " | ")
#turning all the dat to lower case
OtherFlColor<-OtherFlColor %>%
  mutate_all(tolower)

#c)2.3.3. Dominent Fruit####
#find the unique "category"="DomiantFruit" for each Catalog.IRN 
#Note: there's only one dominant color for each Catalog.IRN, if there was a fruit
col.unique.DFr<-col.gather2%>%filter(category %in%  "Dominant.Fruit")%>%
  select(-category)%>%
  arrange(Catalog.IRN) %>%
  distinct()
#spread Dominant Flowers
col.DFr.spread<-col.unique.DFr%>%group_by(Catalog.IRN)%>%
  mutate(x_count=1:n())%>%
  spread(key=x_count,
         value=color,
         fill="")
#concatinate all the colors   
#note:#note make sure to adjust the "1" part. they're the name of columns on "col.DFr.spread"
col.DFr.con<-col.DFr.spread%>%
  unite("DomFrColor","1",remove = FALSE, sep =" ")
#show the white space
paste(col.DFr.con$DomFrColor)
# trimming leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
col.DFr.con$DomFrColor <- trim(col.DFr.con$DomFrColor)

#select columns of interest
names(col.DFr.con)
DominantFrColor<-col.DFr.con[,c(1,2)]
head(DominantFrColor)

#formatting 
DominantFrColor$DomFrColor<-paste("dominantfruit: ", DominantFrColor$DomFrColor, " | ")
#turning all the dat to lower case
DominantFrColor<-DominantFrColor %>%
  mutate_all(tolower)

#d)2.3.4. Other Fruits####
#find the unique "category"="OtherFruit for each Catalog.IRN 
col.unique.OFr<-col.gather2%>%filter(category %in%  c("Other.Fruit1","Other.Fruit2","Other.Fruit3","Other.Fruit4"))%>%
  select(-category)%>%
  arrange(Catalog.IRN) %>%
  distinct()
#spread oTHER Fruits
col.OFr.spread<-col.unique.OFr%>%group_by(Catalog.IRN)%>%
  mutate(x_count=1:n())%>%
  spread(key=x_count,
         value=color,
         fill="")
#concatinate all the colors           
#note make sure to adjust the "1","2","3","4" part. they're the name of columns on "col.OFr.spread"
col.OFr.con<-col.OFr.spread%>%
  unite("OtherFrColor","1","2","3","4",remove = FALSE, sep =" ")
#show the white space
paste(col.OFr.con$OtherFrColor)
# trimming leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
col.OFr.con$OtherFrColor <- trim(col.OFr.con$OtherFrColor)

#select columns of interest
names(col.OFr.con)
OtherFrColor<-col.OFr.con[,c(1,2)]
head(OtherFrColor)

#formatting 
OtherFrColor$OtherFrColor<-paste("otherfruit: ", OtherFrColor$OtherFrColor, " | ")
#turning all the dat to lower case
OtherFrColor<-OtherFrColor %>%
  mutate_all(tolower)

#=============================================================================================================
#Part III: full_join all with the part I:Preparing Material Field #####
#=============================================================================================================
#Join al lthe dataframes 
colors<-DominantFlColor%>%full_join(OtherFlColor, by="Catalog.IRN") %>%
                          full_join(DominantFrColor, by="Catalog.IRN")%>%
                          full_join(OtherFrColor, by="Catalog.IRN")
names(colors)
#concatinate all the color colums
colors2<-unite(colors,flfrcolors, "DomFlColor", "OtherFlColor", "DomFrColor", "OtherFrColor")

#Combine all the colors amd Part I (Measures_for_Descriptio_All_Old_no_ferns_section1_formatted.csv)
parts<-read.csv("Measures_for_Descriptio_All_Old_no_ferns_section1_formatted.csv")
colors2<-data.frame(colors2)
all<-parts%>%
            left_join(colors2, by="Catalog.IRN")

#concatinating all the columns
all$all.values<-paste(all$parts, " | ", all$flfrcolors)
all<-all[,c(1,4)]

#write.csv(all, "Descriptio_All_Old_no_ferns_section1.csv", row.names=FALSE )
#Note: couldn't figure out how to delete all hte "|NA" so still need to detail in Excel
#NOte: two records don't exist in EMu 3825035 and 3835174, 
