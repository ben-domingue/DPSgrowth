.onLoad<-function(libname,pkgname) {
  #this function basically just sets the working directory to the DPS_VAM data directory and other things.
  Sys.getenv("USER")->user
  message("Switching working directory to data directory...")
  if (user!="") {
    switch(user,
           bd={
             setwd("~/Dropbox/DPSvam/int/data")
           },
           domingue={
             setwd("~/dps/data")
           },
           ## damian={
           ##   setwd("/home/damian/Documents/Research/SGP/2012/DPS_VAM")
           ## },
           derekbriggs={
             setwd("~/Dropbox/DPSvam/int/data")
           },
           briggsd={
             setwd("~/Dropbox/DPSvam/int/data")
           })
  } else { #let's just hope this is jessica's windows machine.
    setwd("C:/Users/jeal5954/Dropbox/Derek/DPSvam/int/data/")
  }
}
           
