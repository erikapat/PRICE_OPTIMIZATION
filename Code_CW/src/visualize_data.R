
# calculate information gain
gain_info_graph <- function(dt.values, top = 20){
  
  dt.values <- melt(as.matrix(dt.values))
  dt.values <- as.data.table(dt.values)
  dt.values.0 <- dt.values[value > 0, ]
  dt.values <- copy(dt.values.0)[order(-value)]
  dt.values <- dt.values[1:top, ]
  dt.values <- dt.values[!is.na(Var1)]
  
  d <- plot_info_gain(dt.values)
  return(list(d = d, lis_var = as.character(dt.values.0$Var1)))
}

# plot information gain values
plot_info_gain <- function(dt.values){
  
  d <- ggplot(dt.values, aes(y = value, x = reorder(Var1, value))) +  geom_bar(stat = "identity", color = 'blue') + coord_flip()
  d <- d + ylab('Entropy') + xlab('Features') 
  #d <- d +  theme_tq() 
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
  return(d)
}

#correlation ---


correlation_graph <- function(data){
  # Feature correlations to convert
  corrr_analysis <- data %>%
    mutate(convert = convert) %>%
    correlate() %>%
    focus(convert) %>%
    rename(feature = rowname) %>%
    arrange(abs(convert)) %>%
    mutate(feature = as_factor(feature)) 
  corrr_analysis
  
  
  
  # Correlation visualization
  corrr_analysis %>%
    ggplot(aes(x = convert, y = fct_reorder(feature, desc(convert)))) +
    geom_point() +
    # Positive Correlations - Contribute to convert
    geom_segment(aes(xend = 0, yend = feature), 
                 color = palette_light()[[2]], 
                 data = corrr_analysis %>% filter(convert > 0)) +
    geom_point(color = palette_light()[[2]], 
               data = corrr_analysis %>% filter(convert > 0)) +
    # Negative Correlations - Prevent convert
    geom_segment(aes(xend = 0, yend = feature), 
                 color = palette_light()[[1]], 
                 data = corrr_analysis %>% filter(convert < 0)) +
    geom_point(color = palette_light()[[1]], 
               data = corrr_analysis %>% filter(convert < 0)) +
    # Vertical lines
    geom_vline(xintercept = 0, color = palette_light()[[5]], size = 1, linetype = 2) +
    geom_vline(xintercept = -0.25, color = palette_light()[[5]], size = 1, linetype = 2) +
    geom_vline(xintercept = 0.25, color = palette_light()[[5]], size = 1, linetype = 2) +
    # Aesthetics
    theme_tq() +
    labs(title = "convert Correlation Analysis",
         subtitle = "Positive Correlations (contribute to convert), Negative Correlations (prevent convert)",
         y = "Feature Importance")
}

#-------------------------------------------------------------------------------------------------------------------------


HeatMaps.Nas <- function(dt.data, figures.dir){
  # HEAT MAP OF MISSING VALUES
  dt.nas <- as.matrix(colSums(is.na(dt.data))/nrow(dt.data))
  dt.nas <- melt(dt.nas)
  dt.nas <- dt.nas[-1, ]
  
  
  d <- ggplot(data = dt.nas, aes(y=reorder(Var1, abs(value)), x=Var2, fill=value)) 
  d <- d + geom_tile() + xlab('') + ylab('')
  d <- d + scale_fill_gradient2(low = "gray", high = "blue", mid = "gray", 
                                midpoint = 0, limit = c(0,1), space = "Lab" 
  ) 
  #d <- d + geom_tile(aes(fill = value), color='white') 
  d <- d + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  d <- d + theme(axis.title.y = element_text(size = rel(1.5)))
  d <- d + theme(axis.text.y = element_text(size = rel(1.8)))
  d <- d + theme(axis.text.x = element_text(size = rel(1.8)))
  d <- d + theme(axis.title.x = element_text(size = rel(1.5)))
  d <- d +  theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank())
  d <- d + theme(legend.text = element_text(size = 15))
  d <- d + theme(legend.title = element_text(size = 15))
  d
  
  plot.file <- paste0(figures.dir,'heat_map', ".png")
  ggsave(plot.file, width=150, height=200, units="mm")
  
  return()
}

#------------------------------------------------------------------------------------------------------------

only_numeric_variables <- function(data){
  
  tokeep <- which(sapply(data, is.numeric))
  data = data[ , tokeep, with=FALSE]
  data <- data[, -c('premium_2', 'premium_3', 'premium_4', 'premium_6', 'premium_8', 'premium_10', 'premium_11'), with = F]
  
  return(data)
}


CorrelationGraph <- function(dt.train, tam= 1.5){
  
  library(dplyr)
  library(reshape2)
  d_cor <- as.matrix(cor(dt.train, use = "pairwise.complete.obs"))
  d_cor_melt         <- arrange(melt(d_cor), -abs(value))
  d_cor_melt         <- as.data.table(d_cor_melt)
  
  d <- ggplot(data = d_cor_melt, aes(x=reorder(Var1, abs(value)), y=reorder(Var2, abs(value)), fill=value)) 
  d <- d + geom_tile() + xlab('') + ylab('')
  d <- d + scale_fill_gradient2(low = "orange", high = "blue", mid = "white", 
                                midpoint = 0, limit = c(-1,1), space = "Lab", 
                                name="Pearson\nCorrelation") 
  d <- d + geom_text(aes(Var1, Var2, label = round(value, 2)), color = "black", size = 4) 
  #d <- d + geom_tile(aes(fill = value), color='white') 
  d <- d + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  d <- d + theme(axis.title.y = element_text(size = rel(tam)))
  d <- d + theme(axis.text.y = element_text(size = rel(tam)))
  d <- d + theme(axis.text.x = element_text(size = rel(tam)))
  d <- d + theme(axis.title.x = element_text(size = rel(tam)))
  d <- d + theme(legend.text = element_text(size = tam*10))
  d <- d + theme(legend.title = element_text(size = tam*10))
  
  plot.file <- paste0(figures.dir,'correlation_3', ".png")
  ggsave(plot.file, width=200, height=200, units="mm")
  
  return(d)
}

boxplot_graph <- function(dt.data){
  
  dt.melt.aux <- copy(dt.data)
  dt.melt.aux <- melt(dt.melt.aux)
  g <- hcboxplot(x = dt.melt.aux$value, var = dt.melt.aux$variable,
            name = "value", color = "#2980b9") 
  
  return(g)
  
}


scatter_plot <- function(dt.data){
  
  dt.data <- dt.data[,  convert := as.factor(convert)]
  gg <- ggpairs(dt.data, 
                # columns = c("antiguedad", "canciones_por_dia", "generos_por_dia", 'dispositivos', 'proporcion_horas_de_trabajo', 'busquedas_por_dia',
                #                'proporcion_canciones_de_moda', 'USA'), 
                aes(color = convert), 
                upper  = list(continuous = "blank"), alpha = .9)
  return(gg)
}