
rm(list=ls())

figures.dir <- 'fig/'
source('utils/Rlibraries.R')
source('src/prepare_data.R')
source('src/visualize_data.R')

dt.accounts.train <- fread('input/accounts_train.csv') #info of the customers
dt.quotes.train   <- fread('input/quotes_train.csv')  #insurance prices offered to the clients

#dt.accounts.train <. dt.accounts.train[]
# dt.accounts.test <- fread('input/accounts_test.csv') #info of the customers
# dt.quotes.test   <- fread('input/quotes_test.csv')  #insurance prices offered to the clients

dt.accounts <- prepare_account_data(dt.accounts.train)
dt.quotes   <- prepare_quotes_data_def(dt.quotes.train)

#increase sample in quotes files...
dt.quotes <- mirroring_data(dt.quotes)

#fast joint
setkey(dt.quotes,   account_uuid)
setkey(dt.accounts, account_uuid)
data <- dt.quotes[dt.accounts] 

#eliminate id's variables
data <- data[, -c('account_uuid', 'carrier_id')] #, 'product', 'Region', 'business_structure', 'sector'



#correlations with numeric values
dt <- only_numeric_variables(data) # take only numeric data for now
CorrelationGraph(dt, tam= 1.5)
# boxplot_graph(dt %>% select(annual_revenue, total_payroll))
#boxplot_graph(dt %>% select(premium, num_employees))
# boxplot_graph(dt %>% select(-c(annual_revenue, total_payroll, premium, num_employees)))
d <- scatter_plot(  dt %>%  select(-c(annual_revenue, total_payroll, premium, num_employees, Number_year_established))  )
d
rm(dt); gc()

# eliminate_numeric_NAs: for now put -1
data <- eliminate_numeric_NAs(data)


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
#gain_info_all_iteractions_2(DT)
iteractions.info <- iteractions.info[value > 0, ]
iteractions.info <- iteractions.info[order(-value)]
plot_info_gain(iteractions.info[1:20, ])
#-----------------------------------------------------------------------------------------------------------------------------------------
