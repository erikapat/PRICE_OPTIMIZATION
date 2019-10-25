
rm(list=ls())

figures.dir <- 'fig/'
source('utils/Rlibraries.R')
source('src/prepare_data.R')
source('src/visualize_data.R')



dt.accounts.test <- fread('input/accounts_test.csv') #info of the customers
dt.quotes.test   <- fread('input/quotes_test.csv')  #insurance prices offered to the clients

# train: quotes
dt.quotes.original <- fread('input/quotes_train.csv')
dt.quotes.train   <- copy(dt.quotes.original)
dt.quotes.train   <- dt.quotes.train  %>% select(-account_uuid)
dt.quotes.train   <- prepare_quotes_data_train(train = dt.quotes.train, test = dt.quotes.train)
dt.quotes.train.carriers <- dt.quotes.train %>% select(grep("arrier", names(dt.quotes.train), value=TRUE))  %>% unique()
dt.quotes.train.product <- dt.quotes.train %>% select(grep("product", names(dt.quotes.train), value=TRUE)) %>%  unique()

# test
data <- prepare_test_set(dt.quotes.original, dt.quotes.test, dt.accounts.test, dt.quotes.train.carriers, dt.quotes.train.product)
data <- data[, convert := 0]

# data_train_aux <- readRDS('output/data_train_aux.rds')
# data_train_aux <- data_train_aux %>% mutate(dx := 0)
#eliminate id's variables
data.id <- data[,  c('account_uuid', 'carrier_id', 'product', 'premium', 'premium_real')] #, 'product', 'Region', 'business_structure', 'sector'
data    <- data[, -c('account_uuid', 'carrier_id', 'product', 'premium_real')] #, 'product', 'Region', 'business_structure', 'sector'
# data <- data %>% mutate(dx := 1)

# data <- rbind(data, data_train_aux) %>%  as.data.table()
# eliminate_numeric_NAs: for now put -1
data <- eliminate_numeric_NAs(data)
# DUMMIES -----------------------------------------------------------------------------------------------
#Transform character to dummiespremium_real
rec_obj <- recipe(convert ~ ., data = data) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  prep(data = data)

data    <- bake(rec_obj, newdata = data)
data    <- as.data.table(data) 

data[is.na(data)] <- 0
# iteractions between variables
data <- all_interactions(data)

# data <- data %>% filter(dx ==1)
# dir         <- 'output/'
# load(paste0(dir, "dt.train_with_iteractions.RData"))
# var <- names(dt.train) %>% as.character()
# data <- data %>% select(var)
#---------------------------------------------------------------------------------------------------------------------------------
# open random forest object

xg.train <- readRDS("~/Dropbox/PRUEBAS_DATA_SCIENCE/CoverWallet/CW_DS_challenge/challenge/Code_CW/output/model_xgboost.rds")


data <- data %>%  select(convert, premium, annual_revenue ,ratio_carrier_winning, total_payroll,
ratio_wins_products, Number_year_established,
avg_carriers_winning_premium, Carriers_winning_premium_per_sell,
Number_wins_products, num_employees, Carriers_winning_bids, Number_bids_products,
Carriers_bids, sector_Food.and.Accommodation,
sector_Professional..Scientific.and.Technical.Services, sector_Contractors,
sector_Consultants, max_carriers_winning_premium,
sector_Other.Services, Region_SOUTH,
business_structure_Limited.Liability.Company, sector_other,
sd_carriers_winning_premium, Region_NORTHEAST,
sector_Retail.Trade, business_structure_Individual,
Region_WEST, rng_carriers_winning_premium,
sector_Manufacturing, sector_Healthcare,
# sector_UNKNOWN, sector_Transportation.and.Warehousing,
# business_structure_Non.Profit, business_structure_Partnership,
# business_structure_Other, business_structure_Not.sure.yet,
# business_structure_Limited.Partnership, Region_UNKNOWN,
# min_carriers_winning_premium, business_structure_Trust,
# business_structure_UNKNOWN
# iteractions
`ratio_wins_products:ratio_carrier_winning`,
`ratio_wins_products:avg_carriers_winning_premium`,
`Number_wins_products:avg_carriers_winning_premium`,
`Number_wins_products:Carriers_winning_premium_per_sell`,
`ratio_wins_products:Carriers_winning_premium_per_sell`,
`Number_bids_products:ratio_carrier_winning`
# ratio_wins_products:Carriers_winning_bids,
# Number_bids_products:avg_carriers_winning_premium,
# Number_bids_products:Carriers_bids,
# ratio_wins_products:Carriers_bids
)
dt.xg  <- data %>% select(-convert)
dt.xgclass  <- data  %>% select(convert)

dt.xg        <- xgb.DMatrix(data = as.matrix(dt.xg), label = dt.xgclass$convert,  missing = NA)
pred.xg.train <- predict(xg.train, dt.xg, type = "prob")

pred <- pred.xg.train
threshold <-  0.52631579 #0.5
pred[pred >= threshold] = 1
pred[pred < threshold] = 0
length(pred)
nrow(data.id)

data.def <- data.frame(data.id, convert = pred)
write.table(data.def, file = 'solution/XG_SOLUTION.csv', sep = '|', quote = F, row.names = F)

#---------------------------------------------------------------------------------------------------------------------------------


#aacount value
val <- copy(data.def)
val <- val  %>%  select(account_uuid, carrier_id, product, premium_real, convert)

val.1 <- val %>% filter(convert == 1)
val.0 <- val %>% filter(convert == 0)
val.1 <- val.1 %>% group_by(account_uuid, carrier_id, product) %>% filter(premium_real == max(premium_real)) %>% ungroup
val   <- rbind(val.0, val.1)

val <- val %>%  mutate(account_value = premium_real*convert) %>% group_by(account_uuid) %>% summarise(account_value = sum(account_value)) %>%  
  ungroup() %>% filter(!duplicated(account_uuid))

sum(val$account_value)
write.table(val, file = 'solution/XG_account_value.csv', sep = '|', quote = F, row.names = F)
#--------------------------------------------------------------------------------------------------------------------------------
