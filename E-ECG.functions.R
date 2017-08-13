####################################################################################
############################## Install and load packages ###########################
####################################################################################
#"doMC","foreach"
list.of.packages = c("zoo", "TTR", "ggplot2", "plyr", "XML", "reshape2", "forecast", "tsoutliers", "gridExtra", "dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
for( lib in list.of.packages) {
  library(lib, character.only=TRUE);
}

############################################################################################
###### ts Clean 3; #########################################################################
############################################################################################
## tsclean + set to NA if less then 5 beats/phase

tscleanalt<-function(VctECGparam,VctGrouping, ReplaceMissing ){
  VctClean<-VctECGparam
  
  if(length( VctECGparam[ !is.na(VctECGparam)  ] ) <=3 ){return(0)}

  VctClean<- tsclean( VctECGparam, replace.missing = ReplaceMissing,lambda = BoxCox.lambda(VctECGparam, method = "loglik"))
  #print(VctClean)
  for (group in unique(VctGrouping) ){
    if( length( VctECGparam[VctGrouping==group][ !is.na(VctECGparam[VctGrouping==group])  ] ) <=3 ) {  ## need at least 2 values to interpolate
      VctClean[VctGrouping==group]<-NA
    }
  }
  #qplot(seq_along(VctECGparam),VctECGparam)
  #qplot(seq_along( VctECGparam[VctGrouping==group]), VctECGparam[VctGrouping==group])
  #qplot(seq_along(   VctClean[VctGrouping==group]),   VctClean[VctGrouping==group])
  return(VctClean)
} 
####################################################################################
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
####################################################################################
############################## NA compare  ##############################
####################################################################################

CompareSeq <- function(v1,v2) {
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}

####################################################################################
##############################  Replace NA With Nearest Non-NA  ####################
####################################################################################

ReplaceNAWithNearestNonNA <- function(Vct) {
  N <- length(Vct)
  na.pos <- which(is.na(Vct))
  if (length(na.pos) %in% c(0, N)) {
    return(Vct)
  }
  non.na.pos <- which(!is.na(Vct))
  intervals  <- findInterval(na.pos, non.na.pos,
                             all.inside = TRUE)
  left.pos   <- non.na.pos[pmax(1, intervals)]
  right.pos  <- non.na.pos[pmin(N, intervals+1)]
  left.dist  <- na.pos - left.pos
  right.dist <- right.pos - na.pos
  
  Vct[na.pos] <- ifelse(left.dist <= right.dist,
                        Vct[left.pos], Vct[right.pos])
  return(Vct)
}
####################################################################################
############################## Function TrendData  #################################
####################################################################################

extractTrendData<-function(xml_data,i){
  Seconds=as.numeric(xml_data$TrendData[i+1]$TrendEntry$EntryTime$Second)
  Minutes=as.numeric(xml_data$TrendData[i+1]$TrendEntry$EntryTime$Minute)
  TotalTime=Minutes*60+Seconds
  
  Heartrate=as.numeric(xml_data$TrendData[i+1]$TrendEntry$HeartRate)
  Mets=as.numeric(xml_data$TrendData[i+1]$TrendEntry$Mets)
  VECount=as.numeric(xml_data$TrendData[i+1]$TrendEntry$VECount)
  PaceCount=as.numeric(xml_data$TrendData[i+1]$TrendEntry$PaceCount)
  Artifact=xml_data$TrendData[i+1]$TrendEntry$Artifact
  Load=as.numeric(xml_data$TrendData[i+1]$TrendEntry$Load$text)
  Grade=as.numeric(xml_data$TrendData[i+1]$TrendEntry$Grade$text)
  PhaseTime=as.numeric(xml_data$TrendData[i+1]$TrendEntry$PhaseTime$Minute)*60+as.numeric(xml_data$TrendData[i+1]$TrendEntry$PhaseTime$Second)
  PhaseName=xml_data$TrendData[i+1]$TrendEntry$PhaseName
  map<-setNames(c("ex", "re", "pr"),c("Exercise", "Rest", "Pretest"))
  PhaseName=unname(map[PhaseName])
  
  StageTime=as.numeric(xml_data$TrendData[i+1]$TrendEntry$StageTime$Minute)*60+as.numeric(xml_data$TrendData[i+1]$TrendEntry$StageTime$Second)
  StageNumber=as.numeric(xml_data$TrendData[i+1]$TrendEntry$StageNumber)
  StageName=xml_data$TrendData[i+1]$TrendEntry$StageName
  
  #7=lead I, 8=II, 9=III
  I_JPointAmplitude=as.numeric(xml_data$TrendData[i+1]$TrendEntry[7]$LeadMeasurements$JPointAmplitude$text)
  I_STAmplitude20ms=as.numeric(xml_data$TrendData[i+1]$TrendEntry[7]$LeadMeasurements$STAmplitude20ms$text)
  I_STAmplitude=as.numeric(xml_data$TrendData[i+1]$TrendEntry[7]$LeadMeasurements$RAmplitude$text)
  I_RAmplitude=as.numeric(xml_data$TrendData[i+1]$TrendEntry[7]$LeadMeasurements$STAmplitude20ms$text)
  I_R1Amplitude=as.numeric(xml_data$TrendData[i+1]$TrendEntry[7]$LeadMeasurements$R1Amplitude$text)
  I_STSlope=as.numeric(xml_data$TrendData[i+1]$TrendEntry[7]$LeadMeasurements$STSlope$text)
  I_STIntegral=as.numeric(xml_data$TrendData[i+1]$TrendEntry[7]$LeadMeasurements$STIntegral)
  I_STIndex=xml_data$TrendData[i+1]$TrendEntry[7]$LeadMeasurements$STIndex
  
  
  II_JPointAmplitude=as.numeric(xml_data$TrendData[i+1]$TrendEntry[8]$LeadMeasurements$JPointAmplitude$text)
  II_STAmplitude20ms=as.numeric(xml_data$TrendData[i+1]$TrendEntry[8]$LeadMeasurements$STAmplitude20ms$text)
  II_STAmplitude=as.numeric(xml_data$TrendData[i+1]$TrendEntry[8]$LeadMeasurements$RAmplitude$text)
  II_RAmplitude=as.numeric(xml_data$TrendData[i+1]$TrendEntry[8]$LeadMeasurements$STAmplitude20ms$text)
  II_R1Amplitude=as.numeric(xml_data$TrendData[i+1]$TrendEntry[8]$LeadMeasurements$R1Amplitude$text)
  II_STSlope=as.numeric(xml_data$TrendData[i+1]$TrendEntry[8]$LeadMeasurements$STSlope$text)
  II_STIntegral=as.numeric(xml_data$TrendData[i+1]$TrendEntry[8]$LeadMeasurements$STIntegral)
  II_STIndex=xml_data$TrendData[i+1]$TrendEntry[8]$LeadMeasurements$STIndex
  
  
  III_JPointAmplitude=as.numeric(xml_data$TrendData[i+1]$TrendEntry[9]$LeadMeasurements$JPointAmplitude$text)
  III_STAmplitude20ms=as.numeric(xml_data$TrendData[i+1]$TrendEntry[9]$LeadMeasurements$STAmplitude20ms$text)
  III_STAmplitude=as.numeric(xml_data$TrendData[i+1]$TrendEntry[9]$LeadMeasurements$RAmplitude$text)
  III_RAmplitude=as.numeric(xml_data$TrendData[i+1]$TrendEntry[9]$LeadMeasurements$STAmplitude20ms$text)
  III_R1Amplitude=as.numeric(xml_data$TrendData[i+1]$TrendEntry[9]$LeadMeasurements$R1Amplitude$text)
  III_STSlope=as.numeric(xml_data$TrendData[i+1]$TrendEntry[9]$LeadMeasurements$STSlope$text)
  III_STIntegral=as.numeric(xml_data$TrendData[i+1]$TrendEntry[9]$LeadMeasurements$STIntegral)
  III_STIndex=xml_data$TrendData[i+1]$TrendEntry[9]$LeadMeasurements$STIndex
  vars=c("TotalTime","Heartrate","Mets","VECount","PaceCount","Artifact","Load","Grade","PhaseName","StageTime","StageNumber","StageName","I_JPointAmplitude","I_STAmplitude20ms","I_STAmplitude","I_RAmplitude","I_R1Amplitude","I_STSlope","I_STIntegral","I_STIndex","II_JPointAmplitude","II_STAmplitude20ms","II_STAmplitude","II_RAmplitude","II_R1Amplitude","II_STSlope","II_STIntegral","II_STIndex","III_JPointAmplitude","III_STAmplitude20ms","III_STAmplitude","III_RAmplitude","III_R1Amplitude","III_STSlope","III_STIntegral","III_STIndex")
  
  for (n in vars) {
    v = get(n)
    if (length(v)<1 ) {
      v=NA
      #print(paste(n,v,"NA..."))
    }
    assign(n, v)
  }
  
  # print(length(data.frame(TotalTime,Heartrate,Mets,VECount,PaceCount,Artifact,Load,Grade,PhaseName,StageTime,StageNumber,StageName,I_JPointAmplitude,I_STAmplitude20ms,I_STAmplitude,I_RAmplitude,I_R1Amplitude,I_STSlope,I_STIntegral,I_STIndex,II_JPointAmplitude,II_STAmplitude20ms,II_STAmplitude,II_RAmplitude,II_R1Amplitude,II_STSlope,II_STIntegral,II_STIndex,III_JPointAmplitude,III_STAmplitude20ms,III_STAmplitude,III_RAmplitude,III_R1Amplitude,III_STSlope,III_STIntegral,III_STIndex)))
  return(data.frame(TotalTime,Heartrate,Mets,VECount,PaceCount,Artifact,Load,Grade,PhaseName,StageTime,StageNumber,StageName,I_JPointAmplitude,I_STAmplitude20ms,I_STAmplitude,I_RAmplitude,I_R1Amplitude,I_STSlope,I_STIntegral,I_STIndex,II_JPointAmplitude,II_STAmplitude20ms,II_STAmplitude,II_RAmplitude,II_R1Amplitude,II_STSlope,II_STIntegral,II_STIndex,III_JPointAmplitude,III_STAmplitude20ms,III_STAmplitude,III_RAmplitude,III_R1Amplitude,III_STSlope,III_STIntegral,III_STIndex))
}
####################################################################################
##############################  definition of exercise phases according to Load  ###
####################################################################################
GeneratePhasesAccordingToLoad<-function(VctLoad,VctPhaseName){ #dfTrendData
  #VctPhaseNameOriginal<-dfTrendData[1:10,1:10]$PhaseName #test
  #VctLoad<-c(" 0", " 1", " 1", " 1", " 1", " 1", " 2", " 0", " 0", " 0") #test
  VctPhaseNameLoad <- VctLoad
  VctPhaseCorrect_numeric <- as.numeric(gsub("\\s+", "", VctPhaseNameLoad, perl=TRUE))
  VctPhaseNameLoad<-VctPhaseName # init


  if ( length(unique(VctPhaseCorrect_numeric)) >1 ) {
    ### divide into three parts, non-zero, before and after non-zero
    Vctnon_0 = which(VctPhaseCorrect_numeric >= 1) # Position[VctPhaseCorrect_numeric, _?(# < which.max(VctPhaseCorrect_numeric) &)] < which.max(VctPhaseCorrect_numeric) 
    min_1 = Vctnon_0[1]
    max_1 = Vctnon_0[length(Vctnon_0)]
    EndpointOfMaxExercise=tail(which(VctPhaseCorrect_numeric==max(na.omit(VctPhaseCorrect_numeric))),1) ## added na.omit in cause otherwise everyting will turn NA if NA present 
    ## before non-zero is pretest
    if ( (min_1-1) != 0 ){
      VctPhaseCorrect_numeric[1:(min_1-1)] = "pr"
    } 
    ## after non-zero till EndpointOfMaxExercise is exercise
    VctPhaseCorrect_numeric[min_1:EndpointOfMaxExercise] = "ex"
    if(EndpointOfMaxExercise <length(VctPhaseCorrect_numeric) ){ ## if exercise stops at the end, there is no rest and cannot be replaced
      VctPhaseCorrect_numeric[(EndpointOfMaxExercise+1):length(VctPhaseNameLoad)] = "re"
    }
    VctPhaseNameLoad = as.factor(VctPhaseCorrect_numeric)
  }
  VctPhaseNameLoad=paste(VctPhaseNameLoad,"L",sep="")

  return(VctPhaseNameLoad)
}


GeneratePhaseConcensus<-function(VctPhaseName,VctPhaseNameLoad){
  if(length(VctPhaseName) != length(VctPhaseNameLoad)){stop("error, phases not equally long")}
  
  VctPhaseName[VctPhaseNameLoad=="reL"]="re"
  
  length(VctPhaseName[VctPhaseName=="re"])
  VctPhaseNameConsensus
}

####################################################################################
##############################  READ IN  XML  #################
####################################################################################

xmlfile_to_df<-function(xmlfile){
  ## infer vars
  Visit= gsub("\\d{7}_\\d{4}_", "", basename(xmlfile), useBytes = TRUE, fixed = FALSE)
  Visit= gsub("_.*", "", Visit, useBytes = TRUE, fixed = FALSE)
  n_eid= gsub("_.*", "", basename(xmlfile), perl=TRUE)
  
  ## LOAD DATA: 
  print(paste("loading: ",xmlfile))
  ### xmlToList IS SLOW --> need to access nodes by name somehow to speed up but this does not work: doc[[123]][[1]][["geoLocation"]][["geogLocation"]][["latitude"]][["text"]] gets you the latitude. Or by number if you are confident the format is constant (eg: doc[[123]][[1]][[3]][[1]][[1]][["text"]]. Also, do conversions to numeric at the end on the whole data frame column (df$latitude = as.numeric(df$latitude)). – Spacedman Jun 11 '15 at 7:50
  xml_data<-xmlTreeParse(xmlfile, useInternalNodes=TRUE, options = HUGE)
  xml_data<-xmlToList(xml_data)
  
  NumberOfEntriesTrenddata<-as.numeric(xml_data$TrendData[1]$NumberOfEntries)
  dfTrendData=ldply(1:NumberOfEntriesTrenddata, function(x) extractTrendData(xml_data,x)) ### EXTRACT TREND DATA FROM XML
  dfTrendData<-dfTrendData[!duplicated(dfTrendData),] ## deduplicate -- sometimes rows are duplicated???
  dfTrendData[1,]$TotalTime<-0 ### fix to set totaltime to begin at 0 (sometimes it begins at 1...  
  dfTrendData<-data.frame(TotalTime=0:max(dfTrendData$TotalTime+100)) %>% left_join(dfTrendData) %>% na.locf ## EXPAND to 0,1,2,3,4,5 ... max(totaltime)
  dfTrendData$TotalTime<-as.integer(dfTrendData$TotalTime) ## fix totaltime as integer.
  
  if (any(is.na(dfTrendData$Load) ) )  { dfTrendData$Load<-ReplaceNAWithNearestNonNA(dfTrendData$Load) } ## fix if load =NA in the beginning. ### NOT TEST on ALL ECGS
  if (any(is.na(dfTrendData$PhaseName) ) )  { dfTrendData$PhaseName<-ReplaceNAWithNearestNonNA(dfTrendData$PhaseName) }
  ####################################################################################################
  
  dfTrendData$n_eid<-n_eid
  dfTrendData$Visit<-Visit
  return(dfTrendData)
}


gqrs_to_df<-function(gqrsfile){
  
  print(paste("loading: ",gqrsfile))
  dfConstrue= read.table(gqrsfile, header = TRUE, na.strings = c("nan","NA"))
  dfConstrue= dfConstrue[which(rowSums(is.na(dfConstrue))!=ncol(dfConstrue)),]
  dfConstrue[ c("QRS") ]= dfConstrue[ c("QRS") ] /frequency
  dfConstrue$TotalTimeExact<-dfConstrue$QRS#-dfConstrue$QRS[1] ### COMPARED TO THE TREND DATA QT-end = 432ms, trenddata-end = 435ms )
  dfConstrue$TotalTimeRounded<-round(dfConstrue$TotalTimeExact)
  ###################### Duration ECG     #############################  
  dfConstrue$DurationECG <- max(dfConstrue$TotalTimeRounded)
  
  return(dfConstrue)
}


####################################################################################
##############################  ExtractStatistcs  #################
####################################################################################

calcRMSSD<-function(VctECGparameter){
  seq1=VctECGparameter[1:length(VctECGparameter)-1]
  seq2=VctECGparameter[2:length(VctECGparameter)]
  SqDiff<-(seq1-seq2)^2
  RMSSD<-( sum(  SqDiff[!is.na(SqDiff)] )/(length(seq1) -1 ) )  ^(1/2)
  return(c(RMSSD))
}

calcpNN50<-function(VctECGparameter){
  #pNN50 = (NN50 count) / (total NN count)
  seq1=VctECGparameter[1:length(VctECGparameter)-1]
  seq2=VctECGparameter[2:length(VctECGparameter)]
  pNN50=sum(abs(seq1-seq2)>0.050)/length(VctECGparameter)
  return(pNN50)
}

tonumeric<-function(x){
  return(as.numeric(as.character(x)))
}
####

plotexerciseseq<-function(VctECGparameter,VctOutliers,VctGrouping,VctTime,pdffile){
  #print(VctECGparameter)

  VctECGparameterEXOutliers<-VctECGparameter
  VctECGparameterEXOutliers[VctOutliers]<-NA
  VctECGparameterExOutliersTsc<-tscleanalt( VctECGparameterEXOutliers ,VctGrouping ,ReplaceMissing=TRUE) ## tsclean on whole sequence + sets sequence to NA per phase if <=5 beats
  VctECGparameterExOutliersTscWithNA<-tscleanalt( VctECGparameterEXOutliers ,VctGrouping ,ReplaceMissing=FALSE) ## tsclean on whole sequence + sets sequence to NA per phase if <=5 beats

  PlotVctECGparameter               <-qplot(  VctTime,VctECGparameter, shape=VctGrouping, colour = VctGrouping)
  PlotVctECGparameterEXOutliers     <-qplot(  VctTime, VctECGparameterEXOutliers, shape=VctGrouping, colour =VctGrouping)
  PlotVctECGparameterExOutliersTsc  <-qplot(  VctTime, VctECGparameterExOutliersTsc, shape=VctGrouping, colour = VctGrouping)

  pdf(pdffile)
  grid.arrange(PlotVctECGparameter,PlotVctECGparameterEXOutliers, PlotVctECGparameterExOutliersTsc,  ncol=1, newpage = TRUE)
  
  dev.off()
}

###
ExtractLoadstats<-function(VctLoad,VctGrouping,VctTime){
  options(warn=-1)
  #VctLoad<-data$Load
  VctLoad<-tonumeric(VctLoad)
  maxload=max(VctLoad)
  LstOutputAll=c(maxload=maxload)
  for (group in unique(VctGrouping)) {
    VctLoadGroup<-VctLoad[VctGrouping == group]
    VctTimeGroup<- VctTime[VctGrouping == group] 
    VctTimerelative<- VctTimeGroup  - min(  VctTimeGroup  )
    
    if( sum(!is.na(VctLoadGroup)) <=3 ) {
      #print(paste("nothing in",group))
    } else{
      for ( t in c(10, 20, 30, 40, 50, 60,90,120,180)) { 
        twiggle=3
        MaxLoadAt = max(na.omit(  VctLoadGroup [VctTimerelative>(t-twiggle) & VctTimerelative<(t+twiggle) ]  ), na.rm=TRUE)
        names(MaxLoadAt)<-paste(group,"_","MaxLoadAt",t,"s",sep="")
        LstOutputAll<-c(LstOutputAll,MaxLoadAt)
      }
    }
  }
  return(LstOutputAll[!is.infinite(LstOutputAll)])
}
#### 
ExtractStatistcs<-function(VctECGparameter,VctOutliers,VctGrouping,VctTime,TscReplaceMissing=TRUE){
  #print("..summarizing results")
  VctECGparameterEXOutliers<-VctECGparameter
  VctECGparameterEXOutliers[VctOutliers]<-NA
  VctECGparameterExOutliersTscWithNA<-tscleanalt( VctECGparameterEXOutliers ,VctGrouping ,ReplaceMissing=FALSE) ## tsclean on whole sequence + sets sequence to NA per phase if <=5 beats
  VctECGparameterExOutliersTsc<-tscleanalt( VctECGparameterEXOutliers ,VctGrouping ,ReplaceMissing=TRUE) ## tsclean on whole sequence + sets sequence to NA per phase if <=5 beats
  
  if (length(VctECGparameter[!is.na(VctECGparameter)]) <=3){return (c(n_seq=0))}
  Lambda<-BoxCox.lambda(VctECGparameter, method = "loglik") 

  if (TscReplaceMissing==FALSE){VctECGparameterClean=VctECGparameterExOutliersTscWithNA}
  if (TscReplaceMissing==TRUE){VctECGparameterClean=VctECGparameterExOutliersTsc}

  LstOutputAll<-c(Lambda=Lambda) 

  for (group in unique(VctGrouping)) {
    LstOutputGroup<-c() # reset these lists
    LstBoxVars=c()
    LstMean3sAt=c()
    LstMean2sAt=c()
    LstMean1sAt=c()

    if( sum(!is.na(VctECGparameterEXOutliers[VctGrouping == group])) <=3 ) {
      #print(paste("nothing in",group))
    } else{
      VctECGparameterGroup<-VctECGparameterClean[VctGrouping == group] ## use VctECGparameterClean to calculate summary statistics
      VctTimeGroup<- VctTime[VctGrouping == group] 
      VctTimerelative<- VctTimeGroup  - min(  VctTimeGroup  )
      ### percentage of outliers that has been removed compared to total time
      n_seq<-length(VctECGparameterGroup)

      n_outliers<- CompareSeq(VctECGparameterGroup, VctECGparameterEXOutliers[VctGrouping == group])
      n_outliers<- sum(n_outliers== FALSE, na.rm=TRUE)
      pct_outliers<-(n_outliers/n_seq) * 100
      
      n_tscld<- CompareSeq(VctECGparameterGroup, VctECGparameterExOutliersTscWithNA[VctGrouping == group])
      n_tscld<- sum(n_tscld== FALSE, na.rm=TRUE)
      pct_tscld<-(n_tscld/n_seq) * 100
      n_naRaw<-sum( is.na(VctECGparameter [VctGrouping == group] )==TRUE, na.rm=TRUE)
      pct_na<-(n_naRaw/n_seq) * 100
      n_naTscldwNA<-sum( is.na(VctECGparameterExOutliersTscWithNA [VctGrouping == group] )==TRUE, na.rm=TRUE)
      pct_naTscldwNA<-(n_naTscldwNA/n_seq) * 100

      #print(VctECGparameterExOutliersTscWithNA)
      slidingSdSdTsc<-sd(rollapply(VctECGparameterExOutliersTsc[!is.na(VctECGparameterExOutliersTsc)] ,width = 3, by = 2, FUN = sd, align = "left"))
      slidingSdSdTscWithNA<-sd(rollapply(VctECGparameterExOutliersTscWithNA[!is.na(VctECGparameterExOutliersTscWithNA)] ,width = 3, by = 2, FUN = sd, align = "left"))
      mean<-mean(VctECGparameterGroup, na.rm=TRUE)
      min<- min(VctECGparameterGroup, na.rm=TRUE)
      max<- max(VctECGparameterGroup, na.rm=TRUE)
      
      ## boxplot:
      LstBoxVars=c()
      LstBoxVars<- as.vector(boxplot(VctECGparameterGroup,plot=FALSE )$stats)
      names(LstBoxVars)<-c("boxmin","boxq1","boxmed","boxq3","boxmax")
      
      SDNN <-sd( VctECGparameterGroup)
      RMSSD<-calcRMSSD(VctECGparameterGroup)
      pNN50<-calcpNN50(VctECGparameterGroup)
    
      duration<-max( VctTimeGroup ) - min( VctTimeGroup   )

      for ( t in c(10, 20, 30, 40, 50, 60,90,120,180)) { 
        twiggle=3
        Mean3sAt = mean(na.omit(  VctECGparameterGroup [VctTimerelative>(t-twiggle) & VctTimerelative<(t+twiggle) ]  ), na.rm=TRUE)
        names(Mean3sAt)<-paste("mean3s",t,"s",sep="")
        LstMean3sAt=c(LstMean3sAt,Mean3sAt )
      }
   
      ### CONCAT IN VECTOR:
      LstOutputGroup<-c(n_seq=n_seq,n_outliers=n_outliers,pct_outliers=pct_outliers, n_tscld=n_tscld,pct_tscld=pct_tscld,pct_na=pct_na,pct_naTscldwNA=pct_naTscldwNA,
                        slidingSdSdTsc=slidingSdSdTsc,slidingSdSdTscWithNA=slidingSdSdTscWithNA,mean=mean, min=min, max=max, 
                        LstBoxVars,
                        SDNN=SDNN,RMSSD=RMSSD,pNN50=pNN50,
                        duration=duration,
                        LstMean3sAt,LstMean2sAt,LstMean1sAt)
      names(LstOutputGroup)=paste(group,"_",names(LstOutputGroup),sep="")
      
    }
 
    LstOutputAll<-c(LstOutputAll,LstOutputGroup)
    
  }
  
  return(LstOutputAll)
}



