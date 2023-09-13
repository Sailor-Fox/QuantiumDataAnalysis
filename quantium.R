library(tidyverse)
customerData <- read_csv("QVI_purchase_behaviour.csv")
transactionData <- read_csv("QVI_transaction_data.csv")
view(customerData)
view(transactionData)
str(customerData)
str(transactionData)

# convert date to DATE data type
transactionData$DATE <- as.Date(transactionData$DATE, origin = "1899-12-30")

# summary of PROD_NAME column
view(transactionData$PROD_NAME)

productWords <- tibble(unlist(strsplit(unique(transactionData[, PROD_NAME]), "")))


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
