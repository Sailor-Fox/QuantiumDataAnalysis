library(tidyverse)
library(data.table)

filePath <- "C:/Users/sailo/Documents/University/Quantium job sim/QuantiumDataAnalysis/"
transactionData <- fread(paste0(filePath,"QVI_transaction_data.csv"))
customerData <- fread(paste0(filePath,"QVI_purchase_behaviour.csv"))
str(customerData)
str(transactionData)

# convert date to DATE data type
transactionData$DATE <- as.Date(transactionData$DATE, origin = "1899-12-30")

# summary of raw PROD_NAME column
view(unique(transactionData$PROD_NAME))

# looking at the frequency of words in the product names
productWords <- data.table(unlist(strsplit(unique(transactionData[, PROD_NAME]), " "))) %>% 
  setnames("words")
productWords <- productWords[, USELESS := grepl("^$", words) | 
                               grepl("&", words) | 
                               grepl("and", tolower(words))][USELESS == FALSE, ][, USELESS := NULL]
productWords <- productWords[, freq := .N, by = words][order(-freq, words)] %>% 
  unique()
view(productWords)

# removing non-chip items (that shouldn't have been in the data)
transactionData[, SALSA := grepl("salsa", tolower(PROD_NAME))] # create new column 'SALSA' containing TRUE or FALSE depending on the grepl
transactionData <- transactionData[SALSA == FALSE, ][, SALSA := NULL] # select rows with where SALSA is false, then remove column SALSA
summary(transactionData)
# we realise there is transaction/s with 200 of the chips bought (outlier)
idToRemove <- transactionData[PROD_QTY == 200, ]$LYLTY_CARD_NBR[1]
# this customer likely for commercial purpose so will remove for analysis
transactionData <- transactionData[LYLTY_CARD_NBR != idToRemove]
summary(transactionData)




# MY OWN CODE (not based off sample template)
# some initial visualisation of the purchasers
customerData %>%
  ggplot(aes(x = LIFESTAGE, fill = PREMIUM_CUSTOMER)) +
  geom_bar() +
  theme_bw() +
  labs(title = "Number of customers in each lifestage")

customerData %>% 
  ggplot(aes(x = PREMIUM_CUSTOMER, fill = LIFESTAGE)) +
  geom_bar() +
  theme_bw() +
  labs(title = "Number of type of customer")

customerData %>% 
  ggplot(aes(x=PREMIUM_CUSTOMER)) +
  geom_bar() +
  facet_wrap(~LIFESTAGE) +
  theme_bw() +
  labs(title = "Type of customer by lifestage")
