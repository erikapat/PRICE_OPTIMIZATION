

#Par?metros de Entrada
#result: #agrupaci?n obtenida mediante un discriminador
#real: agrupaci?n original
#imprimir: variable que indica si se imprime o no la matriz de confusi?n junto con resultados relacionados
#por defecto es igual a cero, esto significa que solo se devolver? el error de la clasificaci?n sin la matriz de confusi?n.



mc <- function(result, real, imprimir = 0)
{
  
  
  #para hacer una matriz de confusi?n
  conf<-vector(mode="numeric")
  s=0
  for (i in 1:length(real))
  {
    if (real[i]!=result[i])
    {
      s=s+1
      conf[s]=real[i] #mal clasificados # 1=> bueno clasificado como malo, y 0 viceversa          
    }                                                       
  }
  
  #total de Buenos en la realidad
  ng<-length(real[real==1])
  #total de malos en la realidad
  nb<-length(real[real==0])
  #total de buenos clasificados como malos
  bg<-length(conf[conf==1])
  #total de malos clasificados como buenos
  gb<-length(conf[conf==0])
  #total de buenos bien clasificados
  gg<-ng-bg
  #total de malos bien clasificados
  bb<-nb-gb
  
  #TASA DE EXACTITUD
  tasaexact  <- (gg+bb)/(gg+bb+gb+bg) #que tan buena fue la clasificaci?n, ACCURACITY
  precision  <- gg/(gb+gg)
  TP         <- gg/(gg+bg)
  FP         <- gb/(gb+bb)
  TN         <- bb/(bb+gb)
  FN         <- bg/(bg+gg)
  recall     <- gg/(gg + bg) #TP/(TP + FN)
  f1         <- 2*precision*recall/(precision + recall) #2*precision2*recall/(precision2 + recall)
  
  
  if (imprimir==1){
    #ORIGINAL
    #l=c("G","B","TOTAL")
    #confusi<-matrix(c(gg,gb,gg+gb,bg,bb,bg+bb,ng,nb,length(real)),ncol=3,nrow=3,byrow = TRUE,
    #                dimnames = list(c("Class", "Predicted",""),c("Class", "Original","")))
    #confusi<-rbind(l,confusi)
    #l=c("","G","B","TOTAL")
    #confusi<-as.data.frame(cbind(l,confusi))
    #print(confusi)
    # TRANSPOSE
    l=c("B","G","TOTAL")
    confusi<-matrix(c(bb,gb,gb+bb, bg,gg,gg+bg,bg+bb,gg+gb,length(real)),ncol=3,nrow=3,byrow = TRUE,
                    dimnames = list(c("Class", "Original",""),c("Class", "Predicted","")))
    confusi <- rbind(l,confusi)
    l=c("","B","G","TOTAL")
    confusi<-as.data.frame(cbind(l,confusi))
    print(confusi)
    
    print("########################################################") 
    
    relacionado<-matrix(c(tasaexact,TP, FP, TN, FN, precision, recall, f1),nrow= 8,ncol=1, 
                        dimnames = list(c("E", "TP","FP","TN","FN","P", "recall", "f1")))
    print(relacionado)
  }
  
  #error de clasificaci?n
  error <- 1 - tasaexact
  TP <- gg/(gg+bg)
  FP <- gb/(gb+bb)
  
  return(list(Error = error, Precision = precision, TP = TP, FP = FP, recall = recall, f1 =f1, 
              pos_cases = gg))
  #return(72000*bg+20000*gb)
  #return(gb)
}

#------------------------------------------------------------------------------------------------------------------------


density_plot <- function(pred, output.file, pos = NULL, legend = c('negative', 'positive'), 
                         colors = c("red", "green")) 
{
  stopifnot(require('ROCR'))
  stopifnot(length(pred@predictions) == 1) #Multiple runs not supported
  lev <- levels(pred@labels[[1]])
  stopifnot(length(lev)==2) #Only binary classification supported for now
  
  if (is.null(pos)){
    pos <- lev[2]
  }
  neg <- setdiff(lev, pos)
  
  neg_col = colors[1]
  pos_col = colors[2]
  
  neg_dens <- density(pred@predictions[[1]][pred@labels[[1]]==neg])
  pos_dens <- density(pred@predictions[[1]][pred@labels[[1]]==pos])
  top <- ceiling(max(neg_dens$y, pos_dens$y))
  
  png(output.file, width = 1124, height = 768)
  
  plot(0,0,type="n", xlim= c(0,1), ylim=c(0,top),
       xlab="Cutoff", ylab="Density",
       main="How well do the predictions separate the classes?",
       cex.lab=2.5, cex.axis=2.5, cex.main=1.5, cex.sub=2.5)
  lines(neg_dens, col=neg_col, lwd = 5)
  lines(pos_dens, col=pos_col, lwd = 5)
  legend(.8, top, legend = legend, col=c(neg_col, pos_col), lwd = 5, cex = 2)
  
  dev.off()
}

hist_plot_prediction <- function(dt.predicted, type = 'train', model = 'xgboost', extra_test = '_', eti = c("NO FAULT","FAULT")){
  
  
  d <- ggplot(dt.predicted, aes(pred, fill = factor(CLASS, labels= eti))) + geom_histogram(alpha = 0.8, aes(y = ..density..), position = 'identity')
  d <- d + xlab('PREDICTION') + ylab('DENSITY') 
  d <- d + guides(fill=guide_legend(title=""))
  d <- d + scale_fill_manual(values = alpha(c("cyan4", 'coral1'), .8))
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
  
  plot.file <- paste0(figures.dir,"hist_", model, "_", type, extra_test, ".png")
  ggsave(plot.file, width = 200, height = 200, units="mm")
  
  
}

BIP_GRAPH <- function(dt.predicted, type = 'train', model = 'xgboost', extra_test = '_'){
  
  #load("/home/erika/Dropbox/0 BIP/Vodafone/Rmaintenaince/output/predicted_training.xgboost.RData")
  #dt.predicted <- dt.predicted
  
  
  real <- as.vector(as.matrix(dt.predicted$CLASS))
  g <- dt.predicted$pred
  
  th <- seq(0, 1, length = 20)
  
  dt.results <- NULL
  for (i in 1:length(th)){
    result <- rep(0, length(g))
    result[g >= th[i]] = 1
    dt.val <- mc(result, real, imprimir = 0)
    val <- NULL
    val <- cbind(th[i], (length(result[result == 1])/length(result))*100, dt.val$recall*100, dt.val$P*100, dt.val$f1*100)
    dt.results <- rbind(dt.results, val)
  }
  
  dt.results <- as.data.table(dt.results)
  setnames(dt.results, names(dt.results), c('th', 'TRUE_PRED','recall', 'precision', 'f1'))
  #dt.results <- dt.results[th == max(th), c('TP','recall', 'precision', 'f1') := 0]
  #dt.results <- dt.results[th == 1, c('TP','recall', 'f1') := 0][th == 1, precision := 0]
  dt.results <- dt.results[!is.na(f1), ]
  write.table(dt.results, file = paste0(dir, 'threshold_results',  extra_test,  '.csv'), quote = F, sep = ";" ,row.names = F)
  d <- NULL
  #d <- ggplot(dat, aes(y=..density..,x= S)) + geom_histogram(bins = 15, colour = "black", size = .6, fill = "white", breaks=seq(2.1, 3.5,0.1)) 
  d <- ggplot(dt.results, aes(y= TRUE_PRED,x= th)) + geom_bar(stat="identity", alpha = .6) 
  d <- d + geom_line(aes(x = th, y = recall, colour = 'Recall'), size = 1)  #+   stat_smooth(se = FALSE) 
  d <- d + geom_line(aes(x = th, y = precision, colour = 'Precision'), size = 1)  #+   stat_smooth(se = FALSE) 
  d <- d + geom_line(aes(x = th, y = f1, colour = 'F1'), size = 1) 
  d <- d + scale_colour_manual("", 
                               breaks = c("Recall", "Precision", 'F1'),
                               values = c("red", "blue", "black")) 
  d <- d + xlab('Threshold') + ylab('%') + theme_bw() + theme(axis.text=element_text(size = 20),axis.title=element_text(size= 20,face="bold"),
                                                              legend.title=element_text(size=30) , legend.text=element_text(size=30))
  d
  
  plot.file <- paste0(figures.dir,"bip_", model, "_", type, extra_test, ".png")
  ggsave(plot.file, width = 400, height = 200, units="mm")
  
  return(dt.results)
  
}


mk_stats <- function(preds, labels) {
  dt.data <- data.table(PREDS=preds, CLASS=labels)
  
  cat("  [INFO]  AUC = ")
  pred <- prediction(dt.data$PREDS, dt.data$CLASS)
  auc.tmp <- performance(pred,"auc")
  auc <- as.numeric(auc.tmp@y.values)
  cat(auc, "\n")
  
  n.preds <- nrow(dt.data)
  setkey(dt.data, PREDS)
  dt.data[, Q := .I/n.preds ]
  tp.5 <- nrow(dt.data[ Q >= 0.95 & CLASS == 1 ])/nrow(dt.data[ Q >= 0.95 ])
  cat("  [INFO]  TP(5%) =", tp.5, "\n")
  
  dev.null <- dt.data[, NORM_PRED := PREDS ][ PREDS < 1e-15, NORM_PRED := 1e-15 ][ PREDS > 1-1e-15, NORM_PRED := 1 - 1e-15 ]
  dt.data[, LL := CLASS*log(NORM_PRED) + (1-CLASS)*log(1-NORM_PRED) ]
  log.loss <- -mean(dt.data$LL)
  cat("  [INFO]  Logarithmic Loss =", log.loss, "\n")
  
  dt.data[, BS := (CLASS-PREDS)^2 ]
  brier.score <- mean(dt.data$BS)
  cat("  [INFO] Brier Score =", brier.score, "\n")
  
  #-------------------
  # 3.1 Precision plot
  #-------------------
  
  step <- 0.02
  dt.data[, cutoff := round(PREDS/step)*step ]
  dt.data[, N_per_cutoff  := .N,         by=cutoff ]
  dt.data[, TP_per_cutoff := sum(CLASS), by=cutoff ]
  dt.data[, Precision_per_cutoff := TP_per_cutoff/N_per_cutoff ]
  
  pp <- ggplot(dt.data[ ! duplicated(cutoff) ])
  pp <- pp + geom_abline(colour = "black", linetype = "dashed", size = 0.4, alpha = 0.5)
  pp <- pp + geom_smooth(aes(cutoff, Precision_per_cutoff), method = "loess", color="#6495ed", alpha=0.1, se=F, na.rm=T)
  pp <- pp + geom_line(aes(cutoff, Precision_per_cutoff), color="orange", size = 1.05)
  pp <- pp + coord_fixed()
  pp <- pp + theme_minimal() +  xlab("cutoff") + ylab("Precision")
  pp <- pp + xlim(c(0,1)) + ylim(c(0,1))
  pp <- pp + ggtitle("Calibration")
  
  plot.file <- paste0(figures.dir,"calibration.png")
  cat("  [SAVE] ", plot.file)
  ggsave(plot.file, width=200, height=200, units="mm")
  cat("\n")
  
  #-------------
  # 3.2 ROC plot
  #-------------
  
  dt.roc <- mk_ROC(dt.data, step)
  dt.roc.miss <- data.table(FPR=1,TPR=1,cutoff=0)
  dt.roc[, cutoff := MID ]
  dt.roc <- dt.roc[,.(FPR, TPR,cutoff)]
  dt.roc <- rbindlist(list(dt.roc.miss, dt.roc))
  
  pp <- ggplot(dt.roc)
  pp <- pp + geom_abline(colour = "black", linetype = "dashed", size = 0.4, alpha = 0.5)
  pp <- pp + geom_line(aes(FPR, TPR, color=cutoff), size = 1.05)
  pp <- pp + scale_color_gradientn(colours = rainbow(10))
  pp <- pp + coord_fixed()
  pp <- pp + theme_minimal() +  xlab("False positive rate") + ylab("True positive rate")
  pp <- pp + xlim(c(0,1)) + ylim(c(0,1))
  pp <- pp + ggtitle(paste0("AUC = ", round(10000*auc)/10000))
  #pp <- pp + theme(legend.position = "none")
  
  plot.file <- paste0(figures.dir,"roc.png")
  cat("  [SAVE] ", plot.file)
  ggsave(plot.file, width=200, height=200, units="mm")
  cat("\n")
  
  #------------------------
  # 3.3 Probability density
  #------------------------
  
  pp <- ggplot(dt.data)
  pp <- pp + geom_histogram(aes(PREDS), binwidth = step, color="khaki4", fill="wheat3")
  pp <- pp + theme_minimal() # +  xlab("cutoff") + ylab("Precision")
  pp <- pp + xlim(c(0,1))
  pp <- pp + ggtitle(paste0("Probability distribution - log loss = ", -round(10000*log.loss)/10000))
  
  plot.file <- paste0(figures.dir, "p_histogram.png")
  
  cat("  [SAVE] ", plot.file)
  ggsave(plot.file, width=200, height=200, units="mm")
  cat("\n")
}

mk_ROC <- function(dt.data, step = 0.01) {
  # Count population in each bucket
  .dt <- copy(dt.data)
  .dt[, COUNT := PREDS ]
  h.count <- hist(.dt$COUNT, breaks=seq(0,1,step), plot=F)
  
  # Count positive population in each bucket
  .dt <- copy(dt.data)
  .dt[, P := (2*as.integer(as.character(CLASS))-1)*PREDS]
  .dt <- .dt[ P > 0 ]
  h.p <- hist(.dt$P, breaks=seq(0,1,step), plot=F)
  
  # Assume that the cutoff is the upper limit of each break
  # so, each element is a negative
  .dt <- copy(dt.data)
  .dt[, FN := (2*as.integer(as.character(CLASS))-1)*PREDS ]
  .dt <- .dt[ FN > 0 ]
  h.fn <- hist(.dt$FN, breaks=seq(0,1,step), plot=F)
  
  f <- as.data.table(data.frame(MID=h.count$mids, POPULATION=h.count$count,
                                POPULATION_P=h.p$count))
  
  f[, CUTOFF_LO  := MID - step/2 ]
  f[, CUTOFF_MID := MID          ]
  f[, CUTOFF_HI  := MID + step/2 ]
  
  f[, POPULATION_N := POPULATION - POPULATION_P  ]
  # cumsums
  f[, TOTAL_POPULATION   := cumsum(POPULATION)   ]
  f[, TOTAL_POPULATION_P := cumsum(POPULATION_P) ]
  f[, TOTAL_POPULATION_N := cumsum(POPULATION_N) ]
  f[, MAX_POPULATION       := max(TOTAL_POPULATION) ]
  f[, MAX_POPULATION_P     := max(TOTAL_POPULATION_P) ]
  f[, MAX_POPULATION_N     := max(TOTAL_POPULATION_N) ]
  f[, NEG_TOTAL_POPULATION   := MAX_POPULATION - TOTAL_POPULATION ]
  f[, NEG_TOTAL_POPULATION_P := MAX_POPULATION_P - TOTAL_POPULATION_P ]
  f[, NEG_TOTAL_POPULATION_N := MAX_POPULATION_N - TOTAL_POPULATION_N ]
  
  # The good stuff
  f[, TPR := NEG_TOTAL_POPULATION_P/(NEG_TOTAL_POPULATION_P + TOTAL_POPULATION_P)]
  f[, FPR := NEG_TOTAL_POPULATION_N/(NEG_TOTAL_POPULATION_N + TOTAL_POPULATION_N)]
  f[, HIT_RATE := POPULATION_P/POPULATION ]
  
  # Calculate AUC
  dt.tmp <- f[, list(FPR, TPR)]
  setkey(dt.tmp, FPR)
  dt.auc <- dt.tmp
  dt.auc[, X := FPR ]
  dt.auc[, Y := TPR ]
  dt.auc[, NEXT_X := c(X[2:.N], NA) ]
  dt.auc[, PREV_X := c(NA, X[seq_len(.N-1)]) ]
  dt.auc[, DELTA_Xp := abs(NEXT_X - X) ]
  dt.auc[, DELTA_Xm := abs(PREV_X - X) ]
  dt.auc[, DX := (DELTA_Xp + DELTA_Xm)/2 ]
  dt.auc[ is.na(DELTA_Xp), DX := DELTA_Xm/2 ]
  dt.auc[ is.na(DELTA_Xm), DX := DELTA_Xp/2 ]
  dt.auc[, AUC := DX*Y ]
  auc <- sum(dt.auc$AUC)
  
  f[, AUC := auc ]
  
  return(f[ ! duplicated(FPR) ])
}


