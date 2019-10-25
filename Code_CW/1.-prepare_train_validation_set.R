

rm(list=ls())

figures.dir <- 'fig/'
source('utils/Rlibraries.R')
source('src/prepare_data.R')
source('src/visualize_data.R')

dt.accounts.train <- fread('input/accounts_train.csv') #info of the customers
dt.quotes.train   <- fread('input/quotes_train.csv')  #insurance prices offered to the clients

#almost balanced data 50%-50%
length(dt.quotes.train$conver[dt.quotes.train$conver == 0])/nrow(dt.quotes.train) 
length(dt.quotes.train$conver[dt.quotes.train$conver == 1])/nrow(dt.quotes.train) 

dt.accounts <- prepare_account_data(dt.accounts.train)
number_bins <- 10
dt.quotes   <- prepare_quotes_data_train(dt.quotes.train, bins = number_bins)

#fast joint
setkey(dt.quotes,   account_uuid)
setkey(dt.accounts, account_uuid)
data <- dt.quotes[dt.accounts]



#eliminate id's variables
data.id <- data[,  c('account_uuid', 'carrier_id', 'product')] 
data    <- data[, -c('account_uuid', 'carrier_id', 'product')] #, 'product', 'Region', 'business_structure', 'sector'
saveRDS(data, 'output/data_train_aux.rds')

# eliminate_numeric_NAs: for now put -1
data <- eliminate_numeric_NAs(data)
# DUMMIES -----------------------------------------------------------------------------------------------
#Transform character to dummies
rec_obj <- recipe(convert ~ ., data = data) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  prep(data = data)
file <- 'output/recipe.rds'
saveRDS(rec_obj, file)

file <- 'output/recipe.rds'
rec_obj <- readRDS(file)

data    <- bake(rec_obj, newdata = data)
data    <- as.data.table(data)

#-------------------------------------------------------------------------------------------------------
# correlations between variables 


#-----------------------------------------------------------------------------------------------------
# iteractions between variables
# iteractions.info <- gain_info_all_iteractions(data)
# iteractions.info <- iteractions.info[value > 0, ]
# iteractions.info <- iteractions.info[order(-value)] %>% unique()
# plot_info_gain(iteractions.info[1:20, ])
# 
# list_var <- c('convert', 'premium', as.character(iteractions.info$Var1[1:40])) %>%  unique()
# saveRDS(list_var, file =  'output/lista_variable.rds')

# selection of variables
#Information gain
# dt.values.no.interactions <- information.gain(convert ~., data)
# g <- gain_info_graph(dt.values.no.interactions, top = 20)
# g$d
# list_var <- g$lis_var

data <- all_interactions(data)
#data <- data %>%  select(list_var)

#id
# data    <- cbind(data.id, data)

#-----------------------------------------------------------------------------------------------------
# premium id's

# dt.premium <- fread('input/example_account_value.csv')
# premium_id <- dt.premium %>% select(account_uuid)
# setkey(premium_id, account_uuid)
# setkey(data, account_uuid)
# data[premium_id] %>%  dim()
# 
# dim(premium_id[data])
# dim(premium_id)
# dim(data)

#-----------------------------------------------------------------------------------------------------

data <- data[, id := 1:nrow(data)]
# train and validation set
dt.data.1 <- data[convert == 1, ]
dt.data.0 <- data[convert == 0, ]

set.seed(1)
# test set 10%
dt.set.0 <- dt.data.0[sample(1:nrow(dt.data.0), size = .10*nrow(dt.data.0), replace = FALSE), ]
dt.set.1 <- dt.data.1[sample(1:nrow(dt.data.1), size = .10*nrow(dt.data.1), replace = FALSE), ]
dt.test <- rbind(dt.set.0, dt.set.1)

# train
setkey(dt.set.0, id)
setkey(dt.set.1, id)
setkey(dt.data.1, id)
setkey(dt.data.0, id)

dt.train.1  <- dt.data.1[!dt.set.1]
dt.train.0  <- dt.data.0[!dt.set.0]

dt.train <- rbind(dt.train.0 , dt.train.1)
dt.train <- dt.train[, -c('id')]
dt.test  <- dt.test[, -c('id')]
#increase sample in quotes files...
dt.train <- mirroring_data(dt.train, min_mark = 1, max_mark = max(dt.train$premium))

save(dt.train, file =  paste0('output/', 'dt.train_with_iteractions.RData'))
save(dt.test, file =  paste0('output/', 'dt.test_with_iteractions.RData'))
# save(dt.train, file =  paste0('output/', 'dt.train.RData'))
# save(dt.test, file =  paste0('output/', 'dt.test.RData'))

#dt <- rbind(dt.train, dt.test)
#write.table(dt, file =  paste0('output/', 'dt.train.csv'), sep = '|', quote = F)


