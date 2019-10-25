
#rLIBARIES
# Required packages list
packages <- c(
                'data.table',
              'recipes',
              'lubridate',
              'arules',
              'FSelector',
                'ggplot2',
                'foreach',
                'dplyr',
                'tidyquant',
              'highcharter',
              'plotly',
              'GGally',
              'randomForest',
              'rfUtilities',
              'xgboost'
)

# sudo apt-get install -y libxml2-dev libcurl4-openssl-dev libssl-dev


# Install packages not found in local machine
if(any(!(packages %in% installed.packages()))){
  install.packages(packages[!(packages %in% installed.packages())], dependencies = T)
}

# Require all packages
result <- lapply(packages, suppressMessages(require), character.only = TRUE)


