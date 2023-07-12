


# Setting -----------------------------------------------------------------
rm(list = ls())
library(dplyr)
Sys.setlocale(locale = "Persian")


# Read Data ---------------------------------------------------------------
States <- readxl::read_xlsx(path = "D:/Dourvash1401/Projects/Sarv/SarvNavasani/DataAndIntro/States.xlsx", sheet = 1)


# Update every time by quering from DB
Sarv_Trades <- read.csv(file = "D:/Dourvash1401/Projects/Sarv/SarvNavasani/DataAndIntro/Sarv_Trades.csv",
                        fileEncoding = "UTF-8")



# PreCalculation ----------------------------------------------------------

# add states abbr to buyers states
Sarv_Trades <- Sarv_Trades %>% left_join(States, by = c("buyer_mahal_sodoor" = "City"))

# add states abbr to sellers states
Sarv_Trades <- Sarv_Trades %>% left_join(States, by = c("seller_mahal_sodoor" = "City"))

# clear data
DF1 <- Sarv_Trades %>% dplyr::select(buyer_code_sahamdari, buyer_mahal_sodoor, State.x)
colnames(DF1) = c("Code_Melli", "States")
DF2 <- Sarv_Trades %>% dplyr::select(seller_code_sahamdari, seller_mahal_sodoor, State.y)
colnames(DF2) = c("Code_Melli", "States")

# union buyers and sellers
DF12 = union(DF1, DF2)

# remove total data duplicates
DF123 <- DF12[!duplicated(DF12), ]


# add months
Sarv_Trades$Month <- substr(Sarv_Trades$date_key, 1, 6)



# sort date
UnitDates <- unique(Sarv_Trades$date_key)  %>% sort(decreasing = F)



# empty DF
Grouped_Navasan <- list()


# loop to label oscillators and their measures
for(i in seq(length(UnitDates) - 1)) {
Buyers <- Sarv_Trades %>% dplyr::filter(date_key == UnitDates[i])  %>% as.data.frame()
Buyers$Value = Buyers$price * Buyers$number_of_units
Sellers <- Sarv_Trades %>% dplyr::filter(date_key == UnitDates[i])  %>% as.data.frame()
Sellers$Value = Sellers$price * Sellers$number_of_units

Buyers_GP <- Buyers %>% dplyr::group_by(buyer_code_melli) %>% 
  dplyr::summarise(BuyMeanPrice = mean(price), 
  BuyVolume = sum(number_of_units), 
  BuyValue = sum(Value), BuyCount = n())

Buyers_GP$buyer_code_melli <- as.numeric(Buyers_GP$buyer_code_melli)  

Sellers_GP <- Buyers %>% dplyr::group_by(seller_code_melli) %>%
  dplyr::summarise(SellMeanPrice = mean(price),
  SellVolume = sum(number_of_units), 
  SellValue = sum(Value), SellCount = n())

Sellers_GP$seller_code_melli <- as.numeric(Sellers_GP$seller_code_melli)

Buyers_Sellers_GP <- Buyers_GP %>% dplyr::left_join(Sellers_GP,
              by = c("buyer_code_melli" = "seller_code_melli"))




Buyers_Sellers_GP$SellMeanPrice[is.na(Buyers_Sellers_GP$SellMeanPrice)] = Buyers_Sellers_GP$BuyMeanPrice[is.na(Buyers_Sellers_GP$SellMeanPrice)]

Buyers_Sellers_GP$SellVolume[is.na(Buyers_Sellers_GP$SellVolume)] = 0

Buyers_Sellers_GP$SellValue[is.na(Buyers_Sellers_GP$SellValue)] = 0

Buyers_Sellers_GP$SellCount[is.na(Buyers_Sellers_GP$SellCount)] = 0



Buyers_Sellers_GP$NetVolume <- Buyers_Sellers_GP$BuyVolume - Buyers_Sellers_GP$SellVolume

Buyers_Sellers_GP$NetValue <-  Buyers_Sellers_GP$BuyValue - Buyers_Sellers_GP$SellValue

Buyers_Sellers_GP$Return <- ((Buyers_Sellers_GP$SellMeanPrice / Buyers_Sellers_GP$BuyMeanPrice) - 1)*100 
Buyers_Sellers_GP$Return <- round(Buyers_Sellers_GP$Return, digits = 3)

Buyers_Sellers_GP$Navasangir <- ifelse(Buyers_Sellers_GP$NetVolume < 0, 1, 
                                       ifelse(Buyers_Sellers_GP$NetVolume == 0 ,2, 0))

Buyers_Sellers_GP$Navasangir[is.na(Buyers_Sellers_GP$Navasangir)] = 0

Buyers_Sellers_GP$SellPercent <- ifelse(Buyers_Sellers_GP$NetVolume < 0, 100,
                                        ((Buyers_Sellers_GP$SellVolume / Buyers_Sellers_GP$BuyVolume)) * 100)

Buyers_Sellers_GP$date_key = UnitDates[i]

Grouped_Navasan[[i]] <- Buyers_Sellers_GP

print(i)

}

# fill grouped oscillators data
Grouped_Navasan_DF <- do.call(rbind, Grouped_Navasan)

# write.csv(Grouped_Navasan_DF, file = "D:/Dourvash1401/Projects/Sarv/SarvNavasani/DataAndIntro/SarvGroupedNavasn.csv",  
#           fileEncoding = "UCS-2LE")



# final oscillators data
Result <- Grouped_Navasan_DF %>% dplyr::group_by(Navasangir, date_key) %>% 
  dplyr::summarise(SellPercent = mean(SellPercent, na.rm = T), MeanReturn = mean(Return, na.rm = T), 
                   MeanNetValue = mean(NetValue, na.rm = T), MeanBuyCount = mean(BuyCount, na.rm = T), 
                   MeanSellCount = mean(SellCount, na.rm = T), numberofholders = n(),
                   MeanBuyVolume = mean(BuyVolume, na.rm = T), MeanSellVolume = mean(SellVolume, na.rm = T), 
                   MeanBuyValue = mean(BuyValue, na.rm = T), MeanSellValue = mean(SellValue, na.rm = T)
                   )

InitNavasangiri <- Result

## save oscillators in R format
save(InitNavasangiri, file = "D:/Dourvash1401/Projects/Sarv/SarvNavasani/DataAndIntro/InitSarvNavasan.RData")

## save oscillators in csv format
write.csv(Result,file = "D:/Dourvash1401/Projects/Sarv/SarvNavasani/DataAndIntro/InitSarvNavasan2.csv")

