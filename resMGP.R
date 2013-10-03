resMGP<-function(x,N.mgp,N.resMGP,fm) {
  inner.fun <- function(x,fm,
                        TeacherID='SECTION',#classes is a list containing teacher-level data in each and fm is a formula
                        #by.grade=FALSE, #to do things by grade, you need to send it as aprt of the teacher id, split with --
                        N) {
    #do.call("rbind",classes) -> x
    teacher_covars(x,TeacherID) -> tmp
    #
    split(x,x[[TeacherID]])->classes
    #sapply(classes,function(x) length(table(table(x$STUDENT_NUMBER))))->test
    #if (!all(test==1)) stop("student listed more than once in a section")
    #
    sapply(classes,teacher_MeanGP,N=N) -> mgp
    data.frame(names(mgp),mgp=unlist(mgp)) -> mgp
    names(mgp)[1]<-TeacherID
    merge(mgp,tmp,by=TeacherID) -> tmp
    ## if (by.grade) {
    ##   strsplit(tmp[[TeacherID]],"--") -> baz
    ##   matrix(unlist(baz),ncol=2,byrow=TRUE) -> baz
    ##   factor(baz[,2]) -> tmp$GRADE
    ## }
    #
    strsplit(tmp[[TeacherID]],"__")->foo
    sapply(foo,function(x) x[3])->tmp$GRADE_NAME
    subset(tmp,select=all.vars(fm)) -> foo
    tmp[rowSums(is.na(foo))==0,] -> tmp
    lm(fm,tmp) -> mod
    data.frame(tmp[[TeacherID]],resMGP=mod$residuals)->df
    names(df)[1]<-TeacherID
    merge(df,tmp)->df
    grep("SCHOOL_SECTION",names(tmp))->index.1
    grep("GRADE_NAME",names(tmp))->index.2
    list(summary(mod)$coef,colMeans(tmp[-c(index.1,index.2)]))->l
    list(df,l)
  }
  info<-mgp <- list()
  #now get section resMGP
  paste(x$SCHOOL_NUMBER,x$SECTION,x$GRADE_NAME,sep="__")->x$SCHOOL_SECTION
  inner.fun(x,fm=fm,TeacherID="SCHOOL_SECTION",N=N.mgp) -> out
  out[[2]]->info
  #now combine for teacher resmgp
  out[[1]]->y
  subset(x,select=c("TEACHER_DPSID","SCHOOL_SECTION"))->tmp
  tmp[!duplicated(tmp),]->tmp
  merge(y,tmp,by="SCHOOL_SECTION")->y
  split(y,y$TEACHER_DPSID)->z
  fun<-function(x) {
    weighted.mean(x$resMGP,x$class.size)->wm
    if (sum(x$class.size)>N.resMGP) wm else NA
  }
  lapply(z,fun)->z
  data.frame(TEACHER_DPSID=names(z),resMGP=unlist(z))->mgp
  list(info=info,mgp=mgp)
}
