# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html
# useful data.table resource ^^^
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

# count the number of transactions by date
transactions_by_day <- transactionData[, N := .N, by = DATE][, list(DATE, N)] %>% 
  unique()
# make a sequence of 365 dates and join that to transactions_by_day (which is only 364 days)
allDates <- as.data.table(list(0:364)) %>% 
  setnames("V1", "DATE")
allDates$DATE <- as.Date(allDates$DATE, origin = "2018-07-01")
transactions_by_day <- merge(allDates, transactions_by_day, all = TRUE) 
transactions_by_day[is.na(N), N := 0]

# graphing with all days of year now  
ggplot(transactions_by_day, aes(x = DATE, y = N)) +
  geom_line() +
  labs(x = "Day", y = "Number of transactions", title = "Transactions over time") +
  scale_x_date(breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  theme_bw()

ggplot(transactions_by_day[months(DATE) == "December"], aes(x = DATE, y = N)) +
  geom_line() +
  labs(x = "Day", y = "Number of transactions", title = "Transactions over time") +
  scale_x_date(breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  theme_bw()

transactionData[, PACK_SIZE := parse_number(PROD_NAME)]
#### Always check your output
#### Let's check if the pack sizes look sensible
transactionData[, .N, PACK_SIZE][order(PACK_SIZE)]

ggplot(transactionData) +
  geom_histogram(aes(x=PACK_SIZE), bins=10) +
  theme_bw() +
  labs(title = "Transactions by chip pack size",
       y = "No. of transactions",
       x = "Chip pack size (g)")


# UP TO LINE 205 OF SAMPLE TEMPLATE
transactionData[, BRAND := parse_vector(PROD_NAME, col_character())]
transactionData[, BRAND := strsplit(PROD_NAME, " ")]
# ^^^^ NOT WORKING CURRENTLY

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
