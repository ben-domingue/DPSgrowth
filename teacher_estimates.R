teacher_estimates<-function(stud, #student level data
                            outcome.grade,
                            outcome.year,
                            model.return=TRUE,
                            churn) {
  teacher_covars(stud=stud,outcome.grade=outcome.grade,outcome.year=outcome.year,churn=churn)->covars
  teacher_mgp(stud=stud,outcome.grade=outcome.grade,outcome.year=outcome.year)->mgp
  #
  merge(mgp,covars,by="TeacherID",all=TRUE)->x
  #
  x[x$class.size>14,]->x
  x[rowSums(is.na(x))==0,]->all.x
  covars->covars.orig
  std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
  for (nm in names(covars)[-1]) std(all.x[[nm]])->all.x[[nm]]
  #model
  lm(mgp~prior.mean+frl+iep+ell+gt+class.size+churn+novice,data=all.x)->mod
  data.frame(TeacherID=all.x$TeacherID,adj.mgp=mod$residual)->df
  merge(x,df,by="TeacherID",all=TRUE)->x
  if (model.return) list(x,mod,covars.orig) else x
}
