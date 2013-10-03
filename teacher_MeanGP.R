teacher_MeanGP<-function(stud, #this is meant to be applied to a list where data is split by teacher.
                         N.stud=15,
                         bootstrap.N=500,
                         wt=NULL,
                         sgp.col.name="GrowthPercentile",
                         CI.type="other",
                         bootstrap.options=NULL #for confidence interval, this will be the quantiles; for effect sizes, it will just be a percentage.
                      ) {
  if (is.null(wt)) rep(1,nrow(stud))->wt
  stud[[sgp.col.name]]->stud$GrowthPercentile
  #mgp
  stud[!is.na(stud$GrowthPercentile),]->stud
  weighted.mean(stud$GrowthPercentile,wt)->mgp
  switch(CI.type,
         "confidence interval"={
           length(stud$GrowthPercentile)->N
           boot<-numeric()
           data.frame(GrowthPercentile=stud$GrowthPercentile,wt=wt)->tmp
           for (i in 1:bootstrap.N) {
             sample(1:N,N,replace=TRUE)->index
             tmp[index,]->tmp.boot
             if (all(wt==1)) mean(tmp.boot$GrowthPercentile)->boot[i] else weighted.mean(tmp.boot$GrowthPercentile,wt=wt)->boot[i]
           }
           if (is.null(bootstrap.options)) c(.025,.975,.16,.84)->bootstrap.options
           quantile(boot,bootstrap.options)->boot.ci
           c(mgp,boot.ci)->tr
         },
         "effect size"={
           NA->group
           if (mgp>40 & mgp<60) 3->group
           if (is.na(group)) {
             length(stud$GrowthPercentile)->N
             boot<-numeric()
             data.frame(GrowthPercentile=stud$GrowthPercentile,wt=wt)->tmp
             for (i in 1:bootstrap.N) {
               sample(1:N,N,replace=TRUE)->index
               tmp[index,]->tmp.boot
               if (all(wt==1)) mean(tmp.boot$GrowthPercentile)->boot[i] else weighted.mean(tmp.boot$GrowthPercentile,wt=wt)->boot[i]
             }
             if (is.null(bootstrap.options)) .6->bootstrap.options
             if (mgp<=40) {
               (sum(boot<=40))/length(boot)->per
               if (per>bootstrap.options) 1->group else 2->group
             }
             if (mgp>=60) {
               (sum(boot>=60))/length(boot)->per
               if (per>bootstrap.options) 5->group else 4->group
             }
           }
           c(mgp,group)->tr
         }, { #no CI
           mgp->tr
         }
         )
  if (nrow(stud)<N.stud) rep(NA,length(tr))->tr
  tr    
}


