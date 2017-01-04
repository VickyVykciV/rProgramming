ROCValues <- function(Dt){
  require(ROCR)
  pd <- prediction(Dt$Predict,Dt$Actual)
  return(list(rl=performance(pd,"tpr","fpr"),auc=formatC(round(unlist(slot(performance(pd,"auc"), "y.values")),3),width = 3, format = "f")))
}

# ROC plot
PlotROC <- function(Train,Validation=NULL,Test=NULL,label=NULL){
  TrainSmry <- ROCValues(Train)
  
  par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)
  # plotting the ROC curve
  plot(TrainSmry$rl,col="blue",lty=1, lwd=1,main=label)
  
  lgd <- paste0('Train: ',TrainSmry$auc)
  lgc <- 'blue'
  if(!is.null(Validation)){
    ValidationSmry <- ROCValues(Validation)
    plot(ValidationSmry$rl,col="green",lty=1, lwd=1,main=label,add=T)
    lgd <- c(lgd,paste0('Validation: ',ValidationSmry$auc))
    lgc <- c(lgc,'green')
  }
  if(!is.null(Test)){
    TestSmry <- ROCValues(Test)
    plot(TestSmry$rl,col="red",lty=1, lwd=1,main=label,add=T)
    lgd <- c(lgd,paste0('Test: ',TestSmry$auc))
    lgc <- c(lgc,'red')
  }
  abline(a=0, b= 1)
  legend('bottomright',legend=lgd,fill=lgc,col = lgc,cex=.7)
}
# PlotROC(Train=TrainPredict,Validation=ValidationPredict,Test=TestPredict)

KSValue <- function (Indata,g=10,targetValue=1){
  require(magrittr)
  require(dplyr)
  
  # Summarize the data in each band  
  Prob <- Indata %>% 
    mutate(nActual = ifelse(Actual==targetValue,"Target","Non-Target")) %>%
    arrange(-Predict) %>%
    mutate(Band=as.numeric(cut(1:length(Actual),g,include.lowest = T))) %>%
    group_by(Band) %>% 
    summarise(Min.Pridict=round(min(Predict),3),
              Max.Pridict=round(max(Predict),3),
              N=length(Predict),
              Target=sum(nActual=="Target"),
              Non_Target=sum(nActual=="Non-Target")) %>%
    mutate(CumTarget=cumsum(Target),
           CumNONTarget=cumsum(Non_Target),
           CumTargetPct = round((CumTarget/sum(Target))*100,2),
           CumNONTargetPct = round((CumNONTarget/sum(Non_Target))*100,2),
           KS = CumTargetPct-CumNONTargetPct,
           Lift = round((CumTargetPct/(Band*10)),2),
           CapturedTarget=round(Target*100/N,2))
  return(Prob)
}
# (KSTrain <- KSValue(TrainPredict,targetValue='1'))
# (KSValidation <- KSValue(ValidationPredict,targetValue='1'))
# (KSTest <- KSValue(TestPredict,targetValue='1'))

ProbDensityPlot <- function(Train,Validation=NULL,Test=NULL){
  
  require(magrittr)
  require(dplyr)
  require(ggplot2)
  
  PlotData <- Train %>% mutate(label='Train')
  
  if(!is.null(Validation))
    PlotData <- Validation %>% mutate(label='Validation') %>% bind_rows(PlotData)
  if(!is.null(Test))
    PlotData <- Test %>% mutate(label='Test') %>% bind_rows(PlotData)
  
  ggplot(PlotData, aes(Predict, fill = as.factor(Actual))) + 
    geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity',binwidth = .1) + 
    facet_grid(ordered(label,levels = c("Train", "Validation", "Test"))~.) + 
    theme_bw() + 
    xlim(0,1) +
    scale_fill_discrete(guide = guide_legend(title = "Lapsed")) +
    labs(title = "Probability Distributions")
}
# ProbDenaityPlot(TrainPredict,ValidationPredict,TestPredict)

ConfusionMatrixSummary <- function(dt,name=NULL,Thes){
  cfm <- caret::confusionMatrix((dt$Predict>=Thes)*1,dt$Actual)
  cfmtmp <- as.data.frame(prop.table(cfm$table))
  nms <- paste0('Act_',cfmtmp$Reference,'_','Pred_',cfmtmp$Prediction,'_Pct')
  cfmtmp <- data.frame(t(round(cfmtmp$Freq*100,2)))
  colnames(cfmtmp) <- nms
  tmp <- data.frame(t(data.frame(round(cfmtmp,3),t(round(cfm$overall,3)),t(round(cfm$byClass,3)))))
  colnames(tmp) <- 'Values'
  if(!is.null(name)) 
    colnames(tmp) <- name
  return(tmp)
}


ConfusionMatrixSummaryCompare <- function(Train,Validation=NULL,Test=NULL,ThesValue=.5){
  
  AllData <- ConfusionMatrixSummary(Train,'Train',ThesValue)
  nms <- row.names(AllData)
  if(!is.null(Validation))
    AllData <- AllData %>% bind_cols(ConfusionMatrixSummary(Validation,'Validation',ThesValue))
  if(!is.null(Test))
    AllData <- AllData %>% bind_cols(ConfusionMatrixSummary(Test,'Test',ThesValue))
  
  row.names(AllData) <- nms
  return(AllData)
}
# ConfusionMatrixSummaryCompare(TrainPredict,ValidationPredict,TestPredict,ThesValue=.324)

# Senititivity Analysis
SenitivityAnalysis <- function(dt,Problimits=NULL,h=0.1,Bins=10,ShowSenSmryPlot=T){
  
  if(is.null(Problimits)){
    limits <- round(seq(from=min(dt$Predict),to=max(dt$Predict),length.out=Bins),3)
  }else{
    limits <- seq(from=Problimits[1],to=Problimits[2],by=h);
  }
  SenSmry <- data.frame(Threshold=limits)
  colmap <- c("blue","red")
  
  for(i in 1:nrow(SenSmry)){
    thres <- SenSmry$Threshold[i]
    cfMd <- as.data.frame(table(dt$Actual,as.numeric(dt$Predict >= thres)));
    tn <- ifelse(length(cfMd$Freq[cfMd$Var1 == 0 & cfMd$Var2 == 0])==0,0,cfMd$Freq[cfMd$Var1 == 0 & cfMd$Var2 == 0])
    fp <- ifelse(length(cfMd$Freq[cfMd$Var1 == 0 & cfMd$Var2 == 1])==0,0,cfMd$Freq[cfMd$Var1 == 0 & cfMd$Var2 == 1])
    fn <- ifelse(length(cfMd$Freq[cfMd$Var1 == 1 & cfMd$Var2 == 0])==0,0,cfMd$Freq[cfMd$Var1 == 1 & cfMd$Var2 == 0])
    tp <- ifelse(length(cfMd$Freq[cfMd$Var1 == 1 & cfMd$Var2 == 1])==0,0,cfMd$Freq[cfMd$Var1 == 1 & cfMd$Var2 == 1])
    precision <- tp/(tp+fp);
    recall <- tp/(tp+fn);
    SenSmry$"Accuracy(%)"[i] <- round((tn+tp)*100/(tn+tp+fn+fp),digits=2)
    SenSmry$"True.Positive.Rate"[i] <- round(100*tp/(tp+tn),digits=2)
    SenSmry$"False.Positive.Rate"[i] <- round(fp*100/(fp+tn),digits=2)
    SenSmry$"False.Discovery.Rate"[i] <- round(fp*100/(fp+tp),digits=2)
    SenSmry$"TN,FP,FN,TP"[i] <- paste0(tn,',',fp,',',fn,',',tp)
    SenSmry$"FScore"[i] <- round(2*precision*recall/(precision+recall),digits=5)
  }
  
  if(ShowSenSmryPlot){
    dt <- dt[order(dt$Predict),]
    dt$Index=1:nrow(dt);
    plot(dt$Index,dt$Predict,pch=19,cex=0.7,col=colmap[as.numeric(dt$Actual)+1],ylab="Probablity",xlab="Index");
  }
  return(SenSmry)
}
# (Threshvalues <- SenitivityAnalysis(dt=TrainPredict,ShowSenSmryPlot=F))


# TrainPredict <- data.frame(Predict=predict(fit_xgb,dtrain),Actual=getinfo(dtrain,'label')) # Train
# ValidationPredict <- data.frame(Predict=predict(fit_xgb,dval),Actual=getinfo(dval,'label')) # randomized in sample
# TestPredict <- data.frame(Predict=predict(fit_xgb,dtest),Actual=getinfo(dtest,'label')) # Out of sample prediction

# ProbDenaityPlot(TrainPredict,ValidationPredict,TestPredict)
# PlotROC(Train=TrainPredict,Validation=ValidationPredict,Test=TestPredict)
# (Threshvalues <- SenitivityAnalysis(dt=TrainPredict,ShowSenSmryPlot=F))
# ConfusionMatrixSummaryCompare(TrainPredict,ValidationPredict,TestPredict,ThesValue=.324)
# (KSTrain <- KSValue(TrainPredict,targetValue='1'))
# (KSValidation <- KSValue(ValidationPredict,targetValue='1'))
# (KSTest <- KSValue(TestPredict,targetValue='1'))