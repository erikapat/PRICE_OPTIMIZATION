

# prepare set quotes:
# see what is better premium as a continous variable or has a discrete variable
# create new variables like:
#Number_bids_id: number of offers for a client
#Number_winning_bids_id: number of winning offers for a client
#Number_products_offered_id: number of ooffer made to a client
#Carriers_winning_bids: number of time tha a carrier win a bid
#Carriers_bids: number of time thaT a carrier make offers to the client
prepare_quotes_data <- function(dt.quotes.train){
  dt.quotes.train <- dt.quotes.train[, premium_2 := as.integer(discretize(premium, method = "frequency", breaks = 2, 
                                                                          labels = c(1, 2)))]
  dt.quotes.train <- dt.quotes.train[, premium_3 := as.integer(discretize(premium, method = "frequency", breaks = 3, 
                                                                          labels = c(1, 2, 3)))]
  dt.quotes.train <- dt.quotes.train[, premium_4 := as.integer(discretize(premium, method = "frequency", breaks = 4, 
                                                                          labels = c(1:4)))]
  dt.quotes.train <- dt.quotes.train[, premium_6 := as.integer(discretize(premium, method = "frequency", breaks = 6, 
                                                                          labels = c(1:6)))]
  dt.quotes.train <- dt.quotes.train[, premium_8 := as.integer(discretize(premium, method = "frequency", breaks = 8, 
                                                                           labels = c(1:8)))]
  dt.quotes.train <- dt.quotes.train[, premium_10 := as.integer(discretize(premium, method = "frequency", breaks = 10, 
                                                                          labels = c(1:10)))]
  dt.quotes.train <- dt.quotes.train[, premium_11 := as.integer(discretize(premium, method = "frequency", breaks = 11, 
                                                                           labels = c(1:11)))]
  
  dt.quotes.train <- dt.quotes.train[, key := paste0(product)][, Number_bids_products := .N, by = 'key']
  dt.quotes.train <- dt.quotes.train[, key := paste0(product)][, Number_wins_products := sum(convert), by = 'key']
  dt.quotes.train <- dt.quotes.train[, ratio_wins_products := Number_wins_products/Number_bids_products]
  dt.quotes.train <- dt.quotes.train[, key := paste0(carrier_id)][, Carriers_winning_bids :=  sum(convert), by = 'key'][, key := NULL]
  dt.quotes.train <- dt.quotes.train[, Carriers_bids :=  .N, by = 'carrier_id']
  dt.quotes.train <- dt.quotes.train[, ratio_carrier_winning := Carriers_winning_bids/Carriers_bids][, key := NULL]  
  dt.quotes.train <- dt.quotes.train[, Carriers_winning_premium_per_sell :=  sum(convert*premium)/sum(convert), by = 'carrier_id']
  dt.quotes.train <- dt.quotes.train[, max_carriers_winning_premium :=  max(convert*premium), by = 'carrier_id']
  dt.quotes.train <- dt.quotes.train[, min_carriers_winning_premium :=  min(convert*premium), by = 'carrier_id']
  dt.quotes.train <- dt.quotes.train[, avg_carriers_winning_premium :=  mean(convert*premium), by = 'carrier_id']
  dt.quotes.train <- dt.quotes.train[, sd_carriers_winning_premium :=  max(convert*premium), by = 'carrier_id']
  dt.quotes.train <- dt.quotes.train[, rng_carriers_winning_premium :=  max_carriers_winning_premium - min_carriers_winning_premium]
  dt.quotes.train <- dt.quotes.train[, premium := premium_6][, premium_6 := NULL]
  return(dt.quotes.train)
}

#with the definitive división
prepare_premiums_cuts <- function(dt.quotes.train, bins = 6){

  mastercuts <- arules::discretize(dt.quotes.train$premium, method = "frequency", breaks = bins, labels = c(1:bins), onlycuts = T)
  mastercuts[1] = 0

  return(mastercuts)
}

prepare_premiums <- function(dt.quotes.train, dt.quotes.test, bins = 6, real = F){
  mastercuts = prepare_premiums_cuts(dt.quotes.train,  bins)
  dt.quotes.test$premium_6 <- as.numeric(cut(dt.quotes.test$premium, breaks = mastercuts))
  if (real == F){
    dt.quotes.test <- dt.quotes.test[, premium := premium_6][, premium_6 := NULL]
  }else{
    dt.quotes.test <- dt.quotes.test[, premium_real := premium][, premium := premium_6][, premium_6 := NULL]
  }
  
  return(dt.quotes.test)
}

prepare_quotes_data_train <- function(train = dt.quotes.train, test = dt.quotes.test, bins = 6){

  dt.quotes <- copy(dt.quotes.train)
  dt.quotes <- dt.quotes[, key := paste0(product)][, Number_bids_products := .N, by = 'key']
  dt.quotes <- dt.quotes[, key := paste0(product)][, Number_wins_products := sum(convert), by = 'key']
  dt.quotes <- dt.quotes[, ratio_wins_products := Number_wins_products/Number_bids_products]
  dt.quotes <- dt.quotes[, key := paste0(carrier_id)][, Carriers_winning_bids :=  sum(convert), by = 'key'][, key := NULL]
  dt.quotes <- dt.quotes[, Carriers_bids :=  .N, by = 'carrier_id']
  dt.quotes <- dt.quotes[, ratio_carrier_winning := Carriers_winning_bids/Carriers_bids]
  dt.quotes <- dt.quotes[, Carriers_winning_premium_per_sell :=  sum(convert*premium)/sum(convert), by = 'carrier_id']
  dt.quotes <- dt.quotes[, max_carriers_winning_premium :=  max(convert*premium), by = 'carrier_id']
  dt.quotes <- dt.quotes[, min_carriers_winning_premium :=  min(convert*premium), by = 'carrier_id']
  dt.quotes <- dt.quotes[, avg_carriers_winning_premium :=  mean(convert*premium), by = 'carrier_id']
  dt.quotes <- dt.quotes[, sd_carriers_winning_premium :=  max(convert*premium), by = 'carrier_id']
  dt.quotes <- dt.quotes[, rng_carriers_winning_premium :=  max_carriers_winning_premium - min_carriers_winning_premium]
  dt.quotes <- prepare_premiums(dt.quotes, dt.quotes, bins)
  
  return(dt.quotes)
}

# change spaces for NA's for graphics
prepare_NAs <- function(dt.accounts){
  dt.accounts <- dt.accounts[state == '',       state       := NA_character_]
  dt.accounts <- dt.accounts[industry == '',    industry    := NA_character_]
  dt.accounts <- dt.accounts[subindustry == '', subindustry := NA_character_]
  dt.accounts <- dt.accounts[business_structure == '', business_structure := NA_character_]
  
  return(dt.accounts)
}

# prepare set account:
prepare_account_data <- function(dt.accounts){
  
  # dt.accounts.train <- fread('input/accounts_train.csv') #info of the customers
  # dt.accounts <- dt.accounts.train
  ####
  dt.accounts <- prepare_NAs(dt.accounts)
  dt.accounts <- dt.accounts[is.na(business_structure),  business_structure := 'UNKNOWN']
  dt.accounts <- dt.accounts[is.na(industry),  industry := 'UNKNOWN']
  dt.accounts <- dt.accounts[is.na(subindustry),  subindustry := 'UNKNOWN']

  #xx <- dt.accounts.train %>%  select(industry, subindustry, annual_revenue, total_payroll, num_employees )
  # xx <- xx[is.na(annual_revenue), annual_revenue := 0.0]
  # xx <- xx[is.na(total_payroll), total_payroll := 0.0]
  # xx <- xx[is.na(num_employees), num_employees := 0.0]
  dt.accounts <- dt.accounts[annual_revenue == 0, annual_revenue := NA]
  dt.accounts <- dt.accounts[total_payroll == 0, total_payroll := NA]
  dt.accounts <- dt.accounts[num_employees == 0, num_employees := NA]
  dt.accounts <- dt.accounts[, annual_revenue := as.numeric(annual_revenue)]
  dt.accounts <- dt.accounts[, num_employees := as.numeric(num_employees)]
  dt.accounts <- dt.accounts[, total_payroll := as.numeric(total_payroll)]
  
  dt.accounts <- dt.accounts[, annual_revenue_sub := as.numeric(median(annual_revenue, na.rm = T)), by = subindustry]
  dt.accounts <- dt.accounts[, total_payroll_sub  := as.numeric(median(total_payroll, na.rm = T)), by  = subindustry]
  dt.accounts <- dt.accounts[, num_employees_sub  := as.numeric(median(num_employees, na.rm = T)), by  = subindustry]
  dt.accounts <- dt.accounts[, annual_revenue_ind := as.numeric(median(annual_revenue, na.rm = T)), by = industry]
  dt.accounts <- dt.accounts[, total_payroll_ind  := as.numeric(median(total_payroll, na.rm = T)), by  = industry]
  dt.accounts <- dt.accounts[, num_employees_ind  := as.numeric(median(num_employees, na.rm = T)), by  = industry]
  
  dt.accounts <- dt.accounts[is.na(annual_revenue), annual_revenue := annual_revenue_sub]
  dt.accounts <- dt.accounts[is.na(total_payroll),  total_payroll := total_payroll_sub]
  dt.accounts <- dt.accounts[is.na(num_employees),  num_employees := num_employees_sub]
  dt.accounts <- dt.accounts[is.na(annual_revenue), annual_revenue := annual_revenue_ind]
  dt.accounts <- dt.accounts[is.na(total_payroll),  total_payroll := total_payroll_ind]
  dt.accounts <- dt.accounts[is.na(num_employees),  num_employees := num_employees_ind]
  dt.accounts <- dt.accounts %>%  select(-c( annual_revenue_sub, total_payroll_sub, num_employees_sub, 
                                             annual_revenue_ind, total_payroll_ind, num_employees_ind) )
  # industry
  dt.accounts <- prepare_variable_industry(dt.accounts,  option = 'industry', quantil = 0) #option = industry/subindustry
  dt.accounts <- prepare_variable_states(dt.accounts, dir_division = 'input/us_regions_divisions.csv', option = 'Region')
  
  #year
  actual_year <- year(today())
  dt.accounts <- dt.accounts[, Number_year_established := actual_year - year_established][, year_established := NULL]
  
  
  return(dt.accounts)
}

#division of the states
prepare_variable_states <- function(dt.accounts, dir_division = 'input/us_regions_divisions.csv', option = 'Region'){ #option = 'Region'/state/Division
  
  # VARIABLE: STATE
  #load divisions 
  dt.states <- fread(dir_division)
  #treat variable
  dt.accounts <- dt.accounts[, state := toupper(state)][, state := substr(state, 1, 2)]
  #dt.accounts <- dt.accounts[state == 'California', state := 'CA']
  dt.accounts <- dt.accounts[is.na(state), state := 'UN']
  dt.accounts <- dt.accounts[state == 'New York', state := 'NY']
  #merge data.tables
  dt.states <- dt.states[, state :=  `State Code`][, c('state', 'Region', 'Division')][, Region := toupper(Region)][, Division := toupper(Division)]
  dt.unknown <- as.data.table(data.frame(state = 'UN', Region = 'UNKNOWN', Division = 'UNKNOWN'))
  dt.states <- rbind(dt.states, dt.unknown)
  dt.states <- dt.states[, c('state', option),  with = F]
  #dt.states <- merge(dt.accounts, dt.states, by = 'state', all = T)
  #fast join
  setkey(dt.accounts, state)
  setkey(dt.states, state)
  dt.states <- dt.states[dt.accounts] 
  #
  dt.states <- dt.states[, -c('state'),  with = F]
  if (length(table(dt.accounts$state)) != 52){ #51 states + unkowns
    cat('[Warning:] Please check the states columns ', '\n')
  }
  dt.states <- dt.states[, Region := as.character(Region)]
  return(dt.states)
}  

#use industry or subindustry
prepare_variable_industry <- function(dt.accounts,  option = 'industry', quantil = 0){ #option = industry/subindustry
  
  dt.accounts <- dt.accounts[is.na(industry),    industry := 'UNKNOWN']
  dt.accounts <- dt.accounts[is.na(subindustry), subindustry := 'UNKNOWN']
  if (option == 'industry'){
    dt.accounts <- dt.accounts[, sector := industry][, -c('industry', 'subindustry'),  with = F]
  }else{
    dt.accounts <- dt.accounts[, sector := subindustry][, -c('industry', 'subindustry'),  with = F]
  }
  setkey(dt.accounts, sector)
  dt.accounts <- dt.accounts[, count := .N, by = sector]
  threshold <- quantile(dt.accounts$count[!duplicated(dt.accounts$count)], .5)
  dt.accounts <- dt.accounts[count < threshold, sector := 'other'][, count := NULL]
  #length(table(dt.accounts$sector))
  
  return(dt.accounts)
}

# a simple solution
eliminate_numeric_NAs <- function(data){
  data <- data[is.na(num_employees), num_employees := -1][is.na(annual_revenue), annual_revenue := -1]
  data <- data[is.na(total_payroll), total_payroll := -1]
  data <- data[is.na(Number_year_established), Number_year_established := -1]
  data <- data[is.na(Carriers_winning_premium_per_sell), Carriers_winning_premium_per_sell := -1]
  return(data)
}

# a simple solution
return_numeric_NAs <- function(data){
  data <- data[num_employees == -1, num_employees := NA][annual_revenue == -1, annual_revenue := NA]
  data <- data[total_payroll == -1, total_payroll := NA]
  data <- data[Number_year_established == -1, Number_year_established := NA]
  data <- data[Carriers_winning_premium_per_sell == -1, Carriers_winning_premium_per_sell := NA]
  return(data)
}

# all iteractions

all_interactions <- function(DT){
  convert <- DT  %>%  select(convert)
  
  form <- convert ~ .^2
  dt.model <- model.matrix(form, data = DT)
  data <- as.data.table(cbind(convert = convert$convert, dt.model))
  data <- data[, -c("(Intercept)")]
  return(data)
}

#interactions - information gain 
gain_info_all_iteractions <- function(DT){
  data <- all_interactions(DT)
    
  se <- as.integer(seq(2, ncol(data), length = 32))
  
  dt <- NULL
  for (i in 2:length(se)){
    print(i)
    dt.aux    <- data[, c(1, se[i - 1]:se[i]), with = F]
    dt.values <- information.gain(convert ~., dt.aux)
    dt.values <- melt(as.matrix(dt.values))
    dt.values <- as.data.table(dt.values)
    dt <- rbind(dt, dt.values)
  }
  
  dt <- dt %>% select( Var1, value)

return(dt)
}


#interactions - information gain 
gain_info_all_iteractions_2 <- function(DT){
  convert <- DT$convert
  
  form <- convert ~ .^2
  data <- as.data.table(cbind(convert = convert, model.matrix(form, data = DT)))
  data <- data[, -c("(Intercept)")]
  se <- as.integer(seq(2, ncol(data), length = 32))
  
  library(doParallel)
  registerDoParallel(cores=2)
  dt <- NULL
   dt <- foreach(i = 2:length(se), .combine = rbind) %dopar% {
    print(i)
    dt.aux    <- data[, c(1, se[i - 1]:se[i]), with = F]
    dt.values <- information.gain(convert ~., dt.aux)
    dt.values <- melt(as.matrix(dt.values))
    dt.values <- as.data.table(dt.values)
    return(dt.values)
  }
  
  dt <- dt %>% select( Var1, value)
  
  return(dt)
}

#increase the sample with the rule:
# if you lose a bid with the lowest premium you will lose with all the higher premiums
# if you win a bid with  the highest premium you will win with all the lower premiums
mirroring_data <- function(dt.data, min_mark = 1, max_mark = 6){
  
  for (i in 1:max(dt.data$premium)){
    dt.data <- functWin_mark(dt.data, i, min_mark) 
    dt.data <- functLoss_mark(dt.data, i, max_mark)
  }
 return(dt.data)
}


functWin_mark <- function(dt.data, mark, min_mark = 1){
  
  if (mark!= 1){
    for (i in min_mark:(mark - 1)){
      dt.data.new <- copy(dt.data[premium == mark & convert == 1])
      dt.data.new <- dt.data.new[, premium := i]
      dt.data <- rbind(dt.data, dt.data.new)
    }
  } 
  dt.data <- dt.data %>% unique()
  return(dt.data)
}

functLoss_mark <- function(dt.data, mark, max_mark = 6){
  
  if (mark!= 6){
    for (i in (mark + 1):max_mark){
      dt.data.new <- copy(dt.data[premium == mark & convert == 0])
      dt.data.new <- dt.data.new[, premium := i]
      dt.data <- rbind(dt.data, dt.data.new)
    }
  } 
  dt.data <- dt.data %>% unique()
  return(dt.data)
}

prepare_test_set <- function(dt.quotes.original, dt.quotes.test, dt.accounts.test, dt.quotes.train.carriers, dt.quotes.train.product){
  
  xx <- prepare_premiums(dt.quotes.original, dt.quotes.test, real = T)
  setkey(xx, carrier_id)
  setkey(dt.quotes.train.carriers, carrier_id)
  xx <- dt.quotes.train.carriers[xx]
  setkey(dt.quotes.train.product, product)
  setkey(xx,  product)
  xx <- dt.quotes.train.product[xx]
  
  dt.accounts.test <- prepare_account_data(dt.accounts.test)
  setkey(dt.accounts.test, account_uuid)
  setkey(xx,  account_uuid)
  xx <- dt.accounts.test[xx]
  
  return(xx)
  
}
