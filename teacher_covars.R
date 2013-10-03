
teacher_covars<-function(stud, #student level data
                         TeacherID="TeacherID"
                         ) {
  toupper(names(stud))->names(stud)
  toupper(TeacherID)->TeacherID
  #get rid of bad data
  stud[!is.na(stud[[TeacherID]]),]->stud
  #
  covars<-list()
  split(stud,stud[[TeacherID]])->classes
  names(classes)->tch.ids
  ## 1) average of students' prior grade achievement in same subject
  prior.score.mean<-function(x,prior) {
    x$SCALESCORE.PRIOR->prior
    mean(prior,na.rm=TRUE)
  }
  sapply(classes,prior.score.mean)->prior.scores
  data.frame(TeacherID=tch.ids,prior.mean=prior.scores)->covars$prior.mean
  prior.score.sd<-function(x,prior) {
    x$SCALESCORE.PRIOR->prior
    sd(prior,na.rm=TRUE)
  }
  sapply(classes,prior.score.sd)->prior.scores
  data.frame(TeacherID=tch.ids,prior.sd=prior.scores)->covars$prior.sd
  ## 2) FRL% (which I would really like to replace with a much better SES indicator as noted in my writeup)
  frl.mean<-function(x) {
    x$FRL->frl
    ifelse(frl==3,0,1)->frl
    mean(frl,na.rm=TRUE)
  }
  sapply(classes,frl.mean)->frl
  data.frame(TeacherID=tch.ids,frl=frl)->covars$frl
  ## 3) SPED% and/or SCD% [not the same thing]
  iep.mean<-function(x) {
    x$SPED==1 & x$GT==0->iep
    mean(iep,na.rm=TRUE)
  }
  sapply(classes,iep.mean)->iep
  data.frame(TeacherID=tch.ids,iep=iep)->covars$iep
  #4) ELL%
  ell.mean<-function(x) {
    x$ELL->ell
    mean(ell,na.rm=TRUE)
  }
  sapply(classes,ell.mean)->ell
  data.frame(TeacherID=tch.ids,ell=ell)->covars$ell
  ## 5) Gifted and Talented %
  gt.mean<-function(x) {
    x$GT->gt
    ifelse(gt %in% 1:3,1,0)->gt
    mean(gt,na.rm=TRUE)
  }
  sapply(classes,gt.mean)->gt
  data.frame(TeacherID=tch.ids,gt=gt)->covars$gt
  ## 6) class size
  class.size<-function(x) nrow(x)
  sapply(classes,class.size)->class.size
  data.frame(TeacherID=tch.ids,class.size=class.size)->covars$class.size
  ## ## ## 7) "churn rate" of students in class [defined as 1 - proportion of students associated with a teacher in a year that were in the class the entire year]
  ## churn<-function(x) mean(1-x$stable.student,na.rm=TRUE)
  ## sapply(classes,churn)->churn
  ## data.frame(TeacherID=tch.ids,churn=churn)->covars$churn
  #8.5
  attendance<-function(x) mean(x$AVG.ATTENDANCE,na.rm=TRUE)
  sapply(classes,attendance)->attendance
  data.frame(TeacherID=tch.ids,Avg.Attendance=attendance)->covars$Avg.Attendance
  enrollment<-function(x) mean(x$ENROLLMENT,na.rm=TRUE)
  sapply(classes,enrollment)->enrollment
  data.frame(TeacherID=tch.ids,Enrollment=enrollment)->covars$Enrollment
  ## 8) Novice teacher indicator (<4 years experience)
  novice<-function(x) {
    x$TOTAL_TEACHING_EXPERIENCE->tmp
    if (all(is.na(tmp))) NA else {
      max(tmp,na.rm=TRUE)->tmp
      ifelse(tmp>4,0,1)
    }
  }
  sapply(classes,novice)->novice
  novice[!is.finite(novice)]<-NA
  data.frame(TeacherID=tch.ids,novice=novice)->covars$novice
  #
  covars[[1]]->out
  for (i in 2:length(covars)) merge(out,covars[[i]],by="TeacherID",all=TRUE)->out
  grep("TeacherID",names(out))->index
  names(out)[index]<-TeacherID
  out
}
