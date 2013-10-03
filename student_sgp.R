
student_sgp<-function(stud, #student data
                      np, #number of prior years
                      outcome.grade,
                      outcome.year
                      ) {
  require(SGP)
  #now get data to compute sgps
  subset(stud,select=c("OutcomeYear","StudentID","GRADE","ScaleScore"))->y
  y[!duplicated(y),]->y #this gets rid of multiple student records
  #
  gr.dat<-list() #data by grade
  gp<-(outcome.grade-np):outcome.grade
  yp<-(outcome.year-np):outcome.year
  for (ii in 1:length(gp)) {
    y[y$GRADE==gp[ii] & y$OutcomeYear==yp[ii],]->gr.dat[[ii]]
  }
  do.call("rbind",gr.dat)->y
  paste(y$OutcomeYear,y$GRADE,sep=".")->y$time
  NULL->y$GRADE->y$OutcomeYear
  reshape(y,v.names="ScaleScore",timevar="time",idvar="StudentID",direction="wide")->y
  matrix(gp,nrow(y),length(gp),byrow=TRUE)->gp.cols
  cbind(y$StudentID,gp.cols,y[,-1])->y
  studentGrowthPercentiles(y,sgp.labels=list(my.year=outcome.year,my.subject="CSAP"))->sgp
  sgp$SGPercentiles[[1]]->sgp
  stud[stud$GRADE==outcome.grade & stud$OutcomeYear==outcome.year,]->tmp
  merge(tmp,sgp,by.x="StudentID",by.y=1)->tmp
  subset(tmp,select=c("StudentID","OutcomeYear","TeacherID","GRADE","SGP"))
}
