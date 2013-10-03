get_AE<-function() {
  #this function gets attendance and enrollment information
  read.table("./official/Attendance.txt",sep="\t",fill=TRUE,header=TRUE) -> att
  l <- list()
  for (yr in 2006:2012) {
    grep(yr,names(att)) -> index
    att[,c(1,index)] -> tmp
    strsplit(names(tmp),yr) -> nms
    lapply(nms,function(nms) nms[nms!="X"]) -> nms
    unlist(nms) -> names(tmp)
    apply(is.na(tmp[,-1]),1,all) -> index
    tmp[!index,] -> tmp
    yr -> tmp$OutcomeYear
    tmp -> l[[as.character(yr)]]
  }
  do.call("rbind",l) -> att
  ifelse(att$Avg.Attendance<0,NA,att$Avg.Attendance) -> att$Avg.Attendance
  subset(att,select=c("StudentNumber","OutcomeYear","Avg.Attendance")) -> att
  paste(att$OutcomeYear-1,att$OutcomeYear,sep="-")->att$SchoolYear
  #
  load(file="./cu_data/enrollment.Rdata") #see enrollment_munging.R
  names(enrollment)[1]<-"StudentNumber"
  #
  merge(att,enrollment,by=c("StudentNumber","SchoolYear"))
}
  
  
