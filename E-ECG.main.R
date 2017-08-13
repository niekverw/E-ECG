### argument can be a directory with multiple .qrs.csv files or one file.
#3354094_6025_1_0
thisFile <- function() {
        cmdArgs <- commandArgs(trailingOnly = FALSE)
        needle <- "--file="
        match <- grep(needle, cmdArgs)
        if (length(match) > 0) {return(normalizePath(sub(needle, "", cmdArgs[match])))  # Rscript
        } else {return(normalizePath(sys.frames()[[1]]$ofile)) # 'source'd via R console
        }}
#############################
sourcedir=dirname(thisFile()) 
Renvironmentfile<-paste(sourcedir,"/E-ECG.functions.R",sep="")
source(Renvironmentfile)
## fixed parameters
frequency= 500 
##arg1 = directory (ending with/) with .qrs.csv$ files or 1  .qrs.csv$ file.
args = commandArgs(trailingOnly=TRUE)
if ( substrRight(args[1],1)=="/" ){
 qrsfiles=list.files(args[1],pattern="*.qrs.csv$",full.names=TRUE, recursive = TRUE)
  outfile=paste(basename(args[1]),".sumstats.tsv",sep="")
} else {qrsfiles = args[1] }
################
dfresults<-data.frame() 
for (gqrsfile in qrsfiles){
	#gqrsfileqrsfiles[1]
	xmlfile=sub(pattern = ".qrs.csv",replacement = ".xml",gqrsfile)


	dfTrendData<-xmlfile_to_df(xmlfile) ## this should be faster... don't know how; skip ToList.
	dfTrendData<-dfTrendData[!duplicated(dfTrendData$TotalTime),] # deduplicate on totaltime; some STvar's are at the same second; take the first. 
	#estimate exercise phases:
	dfTrendData$PhaseNameLoad <-GeneratePhasesAccordingToLoad(VctLoad=dfTrendData$Load,VctPhaseName=dfTrendData$PhaseName) #Phase according to load
	dfTrendData$PhaseNameConsensus<-dfTrendData$PhaseName
	dfTrendData$PhaseNameConsensus[dfTrendData$PhaseNameConsensus=="re" | dfTrendData$PhaseNameLoad=="reL" ]="reL"

	#load peak detected dataset
	dfConstrue<-gqrs_to_df(gqrsfile)
	data<-merge(dfConstrue, dfTrendData,by.x ="TotalTimeRounded" ,by.y = "TotalTime",all.x=TRUE) 
	#Basic variables ##################################################################
	n_eid=data$n_eid[1]
	Visit=data$Visit[1]
	#totalecgvars
	DurationECG=data$DurationECG[1]
	maxmets=max(tonumeric(data$Mets))
	maxVECount=max(tonumeric(data$VECount))

	LstLoads<-ExtractLoadstats(VctLoad=data$Load,VctGrouping=data[,"PhaseNameConsensus"],VctTime=data$TotalTimeExact  )
	nrow=nrow(data)
	totalecgvars<-c(DurationECG=DurationECG,as.list(LstLoads),maxmets=maxmets,maxVECount=maxVECount,nrow=nrow)
	names(totalecgvars)<-paste("ECG_",names(totalecgvars),"_",Visit,sep="")
	####################################################################################
	if(nrow(data)<=3){
		print("less than 3 datapoints, skipping ")
		Lstout<-c(totalecgvars,n_eid=n_eid)
		dfsumstatsfile<-data.frame(t(data.frame(Lstout)))
		print(dfsumstatsfile)
		write.table(dfsumstatsfile, paste(gqrsfile, ".sumstats.tsv", sep="") , na='NA', quote = FALSE , row.names=FALSE,col.names=TRUE, sep="\t" )
		dfresults<-rbind.fill(dfresults,dfsumstatsfile)
		next}
	####################################################################################
	### RR interval:
	data$RR<- c((data[-1,]$QRS-data[1:(nrow(data)-1),]$QRS),NA) ## , NA is needed otherwise it doesnt fit the df, last is by def NA.


	RRstatsPhaseNameTsc<-ExtractStatistcs(VctECGparameter=data$RR             [1:length(data$RR)-1], ## remove last because it's by def NA
	                        VctOutliers=(data$RR > 60/30 | data$RR < 60/210)  [1:length(data$RR)-1],
	                        VctGrouping=data[,"PhaseName"]                    [1:length(data$RR)-1],
	                        VctTime=data$TotalTimeExact                       [1:length(data$RR)-1],
	                        TscReplaceMissing=TRUE)

	plotexerciseseq(VctECGparameter=data$RR                                       [1:length(data$RR)-1], ## remove last because it's by def NA
	                            VctOutliers=(data$RR > 60/30 | data$RR < 60/210)  [1:length(data$RR)-1],
	                            VctGrouping=data[,"PhaseName"]           [1:length(data$RR)-1],
	                            VctTime=data$TotalTimeExact                       [1:length(data$RR)-1],
	                            pdffile=paste(gqrsfile,".pdf",sep=""))

	names(RRstatsPhaseNameTsc)<-paste("RR_",names(RRstatsPhaseNameTsc),"_",Visit,sep="")


	####################################################################################
	## concat
	####################################################################################
	Lstout<-c(as.list(RRstatsPhaseNameTsc),totalecgvars,n_eid=n_eid)
  
	dfsumstatsfile<-data.frame(Lstout)
	write.table(dfsumstatsfile, paste(gqrsfile, ".sumstats.tsv", sep="") , na='NA', quote = FALSE , row.names=FALSE,col.names=TRUE, sep="\t" )
	dfresults<-rbind.fill(dfresults,dfsumstatsfile)
}

#if directory, concatenate results to file:
if ( substrRight(args[1],1)=="/" ){
	### write all to file:
	write.table(dfresults, outfile , na='NA', quote = FALSE , row.names=FALSE,col.names=TRUE, sep="\t" )
	#dfresultsagg<-aggregate(x=dfresults[!names(dfresults) %in% c("n_eid")], by=list(n_eid=dfresults$n_eid),   FUN=function(x) 
	#  if (any(is.finite(z<-na.omit(x)))) mean(z) else NA)
	#write.table(dfresultsagg, paste(outfile,".aggregate",sep="") , na='NA', quote = FALSE , row.names=FALSE,col.names=TRUE, sep="\t" )
}