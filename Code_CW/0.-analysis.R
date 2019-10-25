
rm(list=ls())
require(data.table)
require(dplyr)
require(recipes)
require(lubridate)
require(arules)
library(FSelector)
require(ggplot2)
require(tidyquant)
require(corrr)

source('src/prepare_data.R')
source('src/visualize_data.R')

dt.accounts.train <- fread('input/accounts_train.csv') #info of the customers
dt.quotes.train   <- fread('input/quotes_train.csv')  #insurance prices offered to the clients

#dt.accounts.train <. dt.accounts.train[]
# dt.accounts.test <- fread('input/accounts_test.csv') #info of the customers
# dt.quotes.test   <- fread('input/quotes_test.csv')  #insurance prices offered to the clients

dt.accounts <- prepare_account_data(dt.accounts.train)
dt.quotes   <- prepare_quotes_data(dt.quotes.train)

setkey(dt.quotes,   account_uuid)
setkey(dt.accounts, account_uuid)

#fast joint
data <- dt.quotes[dt.accounts] 

# for now put -1
data <- eliminate_numeric_NAs(data)

#eliminate id's variables
data <- data[, -c('account_uuid', 'carrier_id')] #, 'product', 'Region', 'business_structure', 'sector'

# DUMMIES -----------------------------------------------------------------------------------------------
#Transform character to dummies
rec_obj <- recipe(convert ~ ., data = data) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  #step_discretize(premium, options = list(cuts = 6)) %>%
  prep(data = data)

data    <- bake(rec_obj, newdata = data)
data    <- as.data.table(data)
#-------------------------------------------------------------------------------------------------------
#Information gain
dt.values.no.interactions <- information.gain(convert ~., data)
g <- gain_info_graph(dt.values.no.interactions, top = 20)
g$d
list_var <- g$lis_var

# Interactions between variables --------------------------------------------------------------------------------------
#Make iteractions between all variables gives a  dimension of 2557 variables (originally 72)
#Iteractions between variables with entropy different of zero.
DT <- copy(data)
#I do not want interactions between premiums 
data <- data[, c('convert', list_var), with = F]
data <- data[, -c('premium_2', 'premium_3', 'premium_4', 'premium_6', 'premium_10', 'premium'), with = F]

form <- convert ~ .^2
data <- as.data.table(cbind(convert = DT$convert, model.matrix(form, data = data)))
dim(data)
data <- data[, -c("(Intercept)")]

#Information gain interactions
#data_sam <- data[, 1:100] # use all the data produce an error in the library :(
dt.values <- information.gain(convert ~., data)
g <- gain_info_graph(dt.values, top = 20)
g$d

#----------------------------------------------------------------------------------------------------------------------------------------------------
#Iteractions between all variables
DT <- DT[, -c('premium_2', 'premium_3', 'premium_4', 'premium_6', 'premium_10', 'premium'), with = F]
iteractions.info <- gain_info_all_iteractions(DT)
gain_info_all_iteractions_2(DT)
iteractions.info <- iteractions.info[value > 0, ]
iteractions.info <- iteractions.info[order(-value)]
plot_info_gain(iteractions.info[1:20, ])
#-----------------------------------------------------------------------------------------------------------------------------------------

# #treat quotes
# rec_obj <- recipe(convert ~ ., data = dt.quotes.train) %>%
#   #step_dummy(all_nominal(), -all_outcomes()) %>%
#   step_discretize(premium, options = list(cuts = 6)) %>% 
#   prep(data = data)
# 
# data    <- bake(rec_obj, newdata = dt.quotes.train)


head(dt.quotes.train)
head(dt.accounts.train)

dim(dt.quotes.train)
dim(dt.accounts.train)


# 5709 uniques id's
dt.quotes.train$account_uuid   %>%  unique() %>%  length()
dt.accounts.train$account_uuid %>%  unique() %>%  length()


# account value ---------------------------------------------------------------------------------------------------------------------------------------
dt.premium <- fread('input/example_account_value.csv')

# verify the account value
val <- dt.quotes.train %>%  mutate(new_account_value = premium*convert) %>% group_by(account_uuid) %>% summarise(new_account_value = sum(new_account_value)) %>%  
                ungroup() %>% filter(!duplicated(account_uuid))
val <- merge(val, dt.premium, by = 'account_uuid')
#------------------------------------------------------------------------------------------------------------------------------------------------------

dt.quotes.train %>%  filter(convert == 1) %>%  dim()
dt.quotes.train %>%  dim()

#-------------------------------------------------------------------------------------------------------------------------------------------------------

# Treat the account value
dt.quotes.train   <- fread('input/quotes_train.csv')  #insurance prices offered to the clients
dt.quotes.train   <- as.data.table(dt.quotes.train)
# dt.quotes.train   <- dt.quotes.train[order(account_uuid), ][, id_product := paste0(account_uuid, '-', product) ]
# dt.quotes.train   <- dt.quotes.train[,  ]
#count
#table(dt.quotes.train$id_product) # we can see that there is id-product combination more than one time
#table(dt.quotes.train$product)
table(dt.quotes.train$carrier_id)

rec_obj <- recipe(convert ~ ., data = dt.quotes.train) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_discretize(premium, options = list(cuts = 6)) %>% 
  prep(data = data)

data    <- bake(rec_obj, newdata = dt.quotes.train)













