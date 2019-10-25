
rm(list=ls())

dt.accounts.train <- fread('input/accounts_train.csv') #info of the customers
dt.quotes.train   <- fread('input/quotes_train.csv')  #insurance prices offered to the clients

library(FSelector)
dt.values <- dt.data[, -c('Obs', 'Email', 'Socieconomic_Status',  'Right_Address'), with = F]

dt.values <- information.gain(Sales~., dt.values)
dt.values <- melt(as.matrix(dt.values))
dt.values <- as.data.table(dt.values)
dt.values <- dt.values[order(-value)]

d <- ggplot(dt.values, aes(y = value, x = reorder(Var1, value))) +  geom_bar(stat = "identity", color = 'blue') + coord_flip()
d <- d + ylab('Entropy') + xlab('Features') 
d <- d + theme(axis.title.y = element_text(size = rel(1.5)))
d <- d + theme(axis.text.y = element_text(size = rel(1.8)))
d <- d + theme(axis.text.x = element_text(size = rel(1.8)))
d <- d + theme(axis.title.x = element_text(size = rel(1.5)))
d <- d + theme(legend.text = element_text(size = 15))
d <- d + theme(legend.title = element_text(size = 15))
d <- d +  theme(
  panel.background = element_rect(fill="white") ,
  panel.grid.minor.y = element_line(size=3),
  panel.grid.major = element_line(colour = "lightgray"),
  plot.background = element_rect(fill="white")
)
d

plot.file <- paste0(figures.dir,"TOP.Entropy", ".png")
ggsave(plot.file, width=200, height=200, units="mm")

source(paste0(base.dir, '/src/correlation_functions.R'))
dt.selected_var <- dt.data[, -c('Obs', 'CodeCategory', 'ADSL', 'Price_Sensitivity', 'AGE', 'Numb_Mobile_Phones',
                                'Numb_Fixed_Lines', 'Devices_3G', 'Number_of_Semesters_Paid', 
                                'Phone_Call_Day', 'Product_Type_A', 'Product_Type_B', 'Right_Address'), with = F]
dt.selected_var <- dt.selected_var[!is.na(Income) &  !is.na(Estimated_Savings) & !is.na(ProdBought_Price_Sensitivity),] #14860 
d <- CorrelationGraph_3(dt.selected_var)


m1 <- dt.data[, c('Sales', 'Premium_Offered', 'ProdBought', 'ProdActive', 'NumberofCampaigns')] #, 'Income')] 
# 'ProdBought_Price_Sensitivity', 'ProdBought_Income')]
require(tree)
control.values <- tree.control(nrow(m1), mincut = 0, minsize = 2, mindev = 0.01)
#method = "recursive.partition",
#split = c("deviance", "gini"),
m1.tr <- tree(m1$Sales ~.,m1[, 2:6, with = F], split = "deviance", wts="TRUE") #usamos el estad?stico de Gini...
m1.tr
summary(m1.tr)
plot(m1.tr); text(m1.tr)