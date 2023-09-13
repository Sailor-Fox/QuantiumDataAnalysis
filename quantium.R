library(tidyverse)
purchase <- read_csv("QVI_purchase_behaviour.csv")
transac <- read_csv("QVI_transaction_data.csv")
view(purchase)
view(transac)

# some initial visualisation of the purchasers
purchase %>%
  ggplot(aes(x = LIFESTAGE, fill = PREMIUM_CUSTOMER)) +
  geom_bar() +
  theme_bw() +
  labs(title = "Number of customers in each lifestage")

purchase %>% 
  ggplot(aes(x = PREMIUM_CUSTOMER, fill = LIFESTAGE)) +
  geom_bar() +
  theme_bw() +
  labs(title = "Number of type of customer")

purchase %>% 
  ggplot(aes(x=PREMIUM_CUSTOMER)) +
  geom_bar() +
  facet_wrap(~LIFESTAGE) +
  theme_bw() +
  labs(title = "Type of customer by lifestage")
