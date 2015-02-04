### Verification-script for reading WRF-files and observations. Calculates and plots ME, SDE, RMSE and MAE for T2m for one month

#Load packages. You only need this for linux. For windows load the packages ncdf, maps, and mapdata from the "package" tab on the right.
require(ncdf, lib.loc="/vol/fou/atmos/R-packages/precise/")
library(maps, lib.loc="/vol/fou/atmos/R-packages/precise")
library(mapdata, lib.loc="/vol/fou/atmos/R-packages/precise")

##Time period. Not needed if manually set dates. Script can only handle one month or less for now
start.date = "2015/1/30"
end.date = "2015/2/3"

## Reading dates:
dates <- seq(as.Date(start.date),as.Date(end.date),"days")

###Missing dates:
### Which dates are missing?
### Example: Dates 13, 17, and 24 of June 2013 are missing:
### avoid.dates <- as.Date(c("2013-06-13", "2013-06-17", "2013-06-24"))
### If no missing dates: missing.dates <- NULL
missing.dates <- NULL
dates <- dates [! dates %in% missing.dates]


##OPTIONAL: Manually set dates.
##****************************************************
##Example: I want 18,20 and 24 of January 2014
#dates <- c("2014-01-18","2014-01-20","2014-01-24")
##****************************************************

# lead times in hours: start.time, end.time, interval
prg <-c(seq(27,72,24)) #This can be extended to 75h, when 75h forecasts are availiable

# colours
mod.col    <- c(WRF="red", ECMWF="green")

# line types (see ?par lty)
mod.lty    <- c(WRF="solid", ECMWF="solid")

## name of text file with observation sites
## need two for lat and lon in decimal coordinatesin this case
file.sites <- "STATIONG_MYM.csv" #change path
file.sites1 <- "stations_myanmar_latlon.csv" #change path

#name of file with observations
obs.file <- "Rainfall_june2013.csv"

#Thresholds in mm/day. You can vary this if you like
thresh.rr  <- c(0, 0.1, 1, 2,4,8,12,16,20,25,30,35,40,45,50,60,70,80,90)

# plot type ("o" or "b" lines and points, "l" lines only, "p" points only)
type       <- "o"

# type of points (0=square, 1=open circle, 2=triangle, 4=cross,
#                 15=filled square, 16=filled circle, see example(points))
pch        <- 16

# file format of figures. If ps is TRUE then eps, otherwise (FALSE) pdf
ps         <- F

# write data to text files? TRUE/FALSE (useful for input to other applications)
dump.data  <- F

##**********************************************************
##EXTRACT COORDINATES FROM WRF
##**********************************************************

source("merge2.R")

#Reading in sites
sites1  <- read.table(file.sites, header=TRUE, sep=";")
sites  <- read.table(file.sites1, header=TRUE, sep=";")

colnames(sites)[1] <- "name"

#Merge the two files together
sites.test <- merge2(sites1, sites, by=c("name"), all.x=TRUE, suffixes=c("",""))

#Remove stations without LAT and LON in decimals
tyu <-rowSums(is.na(sites.test[,c("LAT", "LON")]))==0
tyu <-sites.test[tyu,]

#station list
sites2 <- tyu

colnames(sites2)[14] <- "LON_DEC"
colnames(sites2)[15] <- "LAT_DEC"

######################################################################
######### G E T   C O O R D I N A T E S ##############################
######################################################################

for (i in models) {
  

if (i == "WRF") {
# Note that some systems have problems with ":". You might have to change the name of the file
ex.nc = open.ncdf("wrfout_d01_2013-06-01_00:00:00") 
lon_in = get.var.ncdf(ex.nc,"XLONG")  
lat_in = get.var.ncdf(ex.nc,"XLAT")

}

if (i == "ECMWF") {
  ex.nc = open.ncdf("/disk1/Myanmar/ECdata/myanmar_atmos_20150131_00.nc")
  lon_in = get.var.ncdf(ex.nc,"longitude")  
  lat_in = get.var.ncdf(ex.nc,"latitude")
  
}




lon <- lon_in[,,1]
lat <- lat_in[,,1]

source("obsdumpTrain_eksempel.R")

}
## Reading dates:
# dates <- seq(as.Date(start.date),as.Date(end.date),"days")


###****************************************************
###   G E T     E C M W F       F I L E S
###****************************************************
# 
# n=0
# ECMWF <- NULL
# 
# for (date in dates){
#   n=n+1
#   year=substr(dates[n],1,4)
#   month=substr(dates[n],6,7)
#   day=substr(dates[n],9,10)
#     #Remember to update the path to were the ec-files are
#     ECMWF_test <- paste("/vol/fou/nwparc2/ec/",year,"/ec_",year,month,day,"00.felt",sep="")
#     ECMWF <- rbind(ECMWF,ECMWF_test)  
#     
# }
# 
# ##Choose the models
# files      <- list(ECMWF)
# 
# names(files) <- c("ECMWF")


#
#    H E I G H T       C O R R E C T I O N
#

# # Model heights interpolated from felt files 
# prm.topo <- rbind(c(2,101,1000,0), c(2,101,1000,0))
# 
# for (i in 1:length(files)) {
#   if (names(files)[i] == "ECMWF") {
#     #read ECMWF topography
#     dat <- miReadFelt(files=ECorog.file, sites=sites2[,c("STNR","LAT_DEC","LON_DEC")], 
#                       prm=prm.topo, prg=cbind(4,0),df=FALSE, atime=FALSE,collapse.time=TRUE,grid=127,prod=98,
#                       interpolation=interpolation2)
#     tdat <- dat[1,"Zs",]
#     sites2$EC.HOH <- ifelse(tdat<100000,round(tdat,digits=1),NA)
#     sites2$hgtcorrEC <- (sites2$EC.HOH-sites2$AMSL)*0.0065
#     dat <- NULL
#     tdat <- NULL
#   }
#   
# }
#Height correction for the WRF-forecast

topo.nc <-open.ncdf("geo_em.d01.nc")

for (j in 1:length(sites2$elevation)) {
  sites2$WRF.HOH[j] <- get.var.ncdf(topo.nc, "HGT_M" ,start=c(sites2$NEAREST_X[j], sites2$NEAREST_Y[j],1), count=c(1,1,1))
  #sites2$ukal.HOH[j] <- get.var.ncdf(file.nc1, "altitude", start=c(lon_j[j],lat_k[j],1), count=c(1,1,1))
}
sites2$hgtcorrWRF <- (sites2$WRF.HOH-sites2$elevation)*0.0065
#sites2$hgtcorrEC_ukal <- (sites2$ukal.HOH-sites2$AMSL)*0.0065

#
#     R E A D    F O R E C A S T     D A T A
#


#
#     W R F
#

# Matrix for the WRF forecasts
wrf.fc <-matrix(data=NA,nrow=(length(dates)*length(prg)*length(sites2$name)), ncol=5)
n=0
m=0

# lead times
prg <-c(seq(3,72,24))

for (date in dates){ # Loop over forecasts
  
  m=m+1
  year=substr(dates[m],1,4)
  month=substr(dates[m],6,7)
  day=substr(dates[m],9,10)
  
  ##Open the wrf file for reading. 
  ##Note that some systems have problems with ":". You might have to change the name of the file
  tmp.nc <- open.ncdf(paste("wrfout_d01_",year,"-",month,"-", day,"_00:00:00", sep=""))
  
  #Get time
  time = get.var.ncdf(tmp.nc,"Times")
  
  time.new <- as.Date(time)
  year <- substr(time.new,1,4)
  month <- substr(time.new,6,7)
  day <- substr(time.new,9,10)
  hour <- substr(time,12,13)

  
  wrf.time <- paste(year,month,day,hour, sep="")
  
  
  for (j in 1:length(sites2$NEAREST_X)){ #loop over observation sites

    
    for (k in 1:length(prg)){ #loop over lead times
      n=n+1
      
      #get variables
      wrf.T2.daily <- get.var.ncdf(tmp.nc, "T2", start=c(sites2$NEAREST_X[j],sites2$NEAREST_Y[j],((k-1)*8+1)), count=c(1,1,8)) -273.15 +sites2$hgtcorrWRF[j]
   
      wrf.T2.max <- max(wrf.T2.daily)
      wrf.T2.min <- min(wrf.T2.daily)
      
   
      #Filter "unrealistic" data
      if (abs(wrf.T2.max) > 100) { 
        wrf.T2.max = NA
      } 
   if (abs(wrf.T2.min) > 100) { 
     wrf.T2.min = NA
   }      

      #Add data to the matrix
      site=sites2$id[j]
      prg_ec=prg[k]+9
      wrf.fc[n,1] <- as.numeric(site)
      wrf.fc[n,2] <- as.numeric(prg_ec)
      wrf.fc[n,3] <- as.numeric(wrf.time[(k)*8-3])
      wrf.fc[n,4] <- as.numeric(wrf.T2.max)
      wrf.fc[n,5] <- as.numeric(wrf.T2.min)

    }
  }
}

#Name the columns
colnames(wrf.fc) <- c("SITE","PRG","TIME","T2max.WRF", "T2min.WRF")


###*********************************************
##  R E A D    O B S E R V A T I O N S
###*********************************************

station.id <- sites2[,1:2]

#File with observations. Can be very sensitive to file format and small errors in the file
obs.all <- read.table("july_max_2014.csv", header=F, sep=",") #need to change path
obs.min <- read.table("july_min_2014.csv", header=F, sep=",") #need to change path

#Define period from wrf.fc
period <- c(min(wrf.fc[,3]), max(wrf.fc[,3]))

#Add column names
name.col <- "name"
number.col <- c(seq(1,length(dates),1))
test <- c(name.col, number.col)
colnames(obs.all) <- test
colnames(obs.min) <- test

#add a column with WMO number 
obs.merge <- merge2(station.id,obs.all,by=c("name"), all.x=TRUE, suffixes=c("","") )
obs.merge.min <- merge2(station.id,obs.min,by=c("name"), all.x=TRUE, suffixes=c("","") )

# Get the observations on right array structure
obs.name.id <- obs.merge[,1:2]
obs.fin <- NULL

for (i in 3:ncol(obs.merge)){
  n=i-2
  if (n < 10) {
    time <- paste("0", n, sep="")
  } else {
    time <- n
  }
  
  obstime <- paste(year[1],month[1], time, "12", sep="")
 
  obs.day <- obs.merge[,i]
  obs.time <-matrix(obstime, ncol=1,nrow=length(obs.day)) 
  obs.tmp <- cbind(obs.name.id,obstime,obs.day)
  
  obs.fin <- rbind(obs.fin,obs.tmp)  
}

colnames(obs.fin)[2] <- "SITE"
colnames(obs.fin)[3] <- "TIME"

# Get the observations on right array structure
obs.name.id.min <- obs.merge.min[,1:2]
obs.fin.min <- NULL

for (i in 3:ncol(obs.merge.min)){
  n=i-2
  if (n < 10) {
    time <- paste("0", n, sep="")
  } else {
    time <- n
  }
  
  obstime <- paste(year[1],month[1], time, "12", sep="")
  
  obs.day <- obs.merge.min[,i]
  obs.time <-matrix(obstime, ncol=1,nrow=length(obs.day)) 
  obs.tmp <- cbind(obs.name.id.min,obstime,obs.day)
  
  obs.fin.min <- rbind(obs.fin.min,obs.tmp)  
}

colnames(obs.fin.min)[2] <- "SITE"
colnames(obs.fin.min)[3] <- "TIME"

#
# M E R G E   O B S E R V A T I O N S
#

obs.fin.max.min <- merge2(obs.fin, obs.fin.min, by=c("name","SITE","TIME"), all.x=TRUE, suffixes=c(".MAX",".MIN"))

#
#     M E R G E     D A T A 
#

x <- merge2(wrf.fc, obs.fin.max.min, by=c("SITE","TIME"), all.x=TRUE, suffixes=c("",".OBS"))

colnames(x)[7] <-"T2max.OBS"
colnames(x)[8] <-"T2min.OBS"

#
#     C O M P U T E     S T A T I S T I C S 
#
prg <- c(seq(12,60,24))

# statistics for each site
stats.all <- array(NA, dim=c(1, 6, length(prg), nrow(sites2), 2),
                   dimnames=list("WRF", c("ME","SDE","RMSE","MAE","N","NFRAC"),
                                 prg, sites2$name, c("T2max","T2min")))
stats.all[,"N",,,] <- 0

ncases <- max(tapply(x$TIME, list(x$SITE,x$PRG), length), na.rm=TRUE)

cat("  compute statistics: ")
for (iprm in c("T2max", "T2min")) {
  cat(iprm, " ")
  mod.name <- paste(iprm, "WRF", sep=".")
  obs.name <- paste(iprm, "OBS", sep=".")
  for (ist in 1:nrow(sites2)) {
    for (iprg in 1:length(prg)) {
      k  <- x$SITE==sites2$id[ist] & x$PRG==prg[iprg]
      xx <- x[k,c(mod.name,obs.name)]
      k  <- rowSums(is.na(xx)) == 0
      xx <- xx[k,]
      if (nrow(xx) > 0) {
        for (im in 1:1) {
          i <- mod.name[im]
          stats.all[im,"ME",iprg,ist,iprm]   <- mean(xx[,i]-xx[,obs.name], na.rm=TRUE)
          stats.all[im,"SDE",iprg,ist,iprm]  <- sd(xx[,i]-xx[,obs.name], na.rm=TRUE)
          stats.all[im,"RMSE",iprg,ist,iprm] <- sqrt(mean((xx[,i]-xx[,obs.name])^2,
                                                          na.rm=TRUE))
          stats.all[im,"MAE",iprg,ist,iprm]  <- mean(abs(xx[,i]-xx[,obs.name]),
                                                     na.rm=TRUE)
          stats.all[im,"N",iprg,ist,iprm]    <- sum(!is.na(xx[,i]-xx[,obs.name]))
        }
      }
      stats.all[,"NFRAC",iprg,ist,iprm] <- stats.all[,"N",iprg,ist,iprm] / ncases
      #           if (stats.all[1,"NFRAC",iprg,ist,iprm] < min.frac)
      #             stats.all[,1:4,iprg,ist,iprm] <- NA
    }
  }
}
cat("\n")
#rm(iprm, mod.name, obs.name, ist, iprg, k, xx, im, i, ncases) 


# statistics summarized over sites
tmp           <- stats.all
#    for (iprm in c("T2")) {
#      for (ist in 1:nrow(sites2)) {
#         if (any(stats.all[,"NFRAC",,ist,iprm] < min.frac)) {
#           tmp[,1:4,,ist,iprm] <- NA
#         }
#      }
#    }
stats         <- apply(tmp[,1:5,,,,drop=FALSE], c(1:3,5), mean, na.rm=TRUE)
stats[,"N",,] <- apply(tmp[,1,,,,drop=FALSE], c(1:3,5), function(u) sum(!is.na(u)))
dimnames(stats)[[2]][5] <- "NSITES"
rm(tmp)


#
#     P L O T S
#
period <- paste(range(x$TIME), collapse="-")

for (iprm in c("T2max", "T2min")) {
  #     if (ps)
  #       postscript(paste(iprm,"_", period1, "_", kk, ".eps", sep=""), width=11.69, height=8.27,
  #                  horizontal=FALSE, onefile=FALSE, paper="special")
  #     else
  #       pdf(paste(iprm, "_", period1,"_", kk, ".pdf", sep=""), width=11.69, height=8.27)
  #     par(mfrow=c(2,2), mar=c(4,4,3,2), oma=c(0,0,3,0), cex=0.8)
  
  for (i in c("ME","SDE","RMSE","MAE")) {
    if (ps)
      postscript(paste(iprm,"_", period,"_", i,  ".eps", sep=""), width=11.69, height=8.27,
                 horizontal=FALSE, onefile=FALSE, paper="special")
    else
      pdf(paste(iprm, "_", period,"_",i, ".pdf", sep=""), width=11.69, height=8.27)
    par(mar=c(5.1,6.1,4.1,2.1))
    #par(mfrow=c(2,2), mar=c(4,4,3,2), oma=c(0,0,3,0), cex=0.8)
    
   # if (iprm == "T2") {
      if (i == "ME") {
        ylim=c(min(stats[,"ME",,][!is.na(stats[,"ME",,])])-0.5,c(max(stats[,"ME",,][!is.na(stats[,"ME",,])]))+0.5)
        # ylim=c(-1.5,2.0)
        ylab="ME [C]"
      }
      if (i == "SDE") {
        ylim=c(min(stats[,"SDE",,][!is.na(stats[,"SDE",,])]),c(max(stats[,"SDE",,][!is.na(stats[,"SDE",,])])))
        #ylim=c(1.5,4.0)
        ylab="SDE [C]"
      }
      if (i == "RMSE") {
        ylim=c(min(stats[,"RMSE",,][!is.na(stats[,"RMSE",,])]),c(max(stats[,"RMSE",,][!is.na(stats[,"RMSE",,])])))
        #  ylim=c(1.0,5.0)
        ylab="RMSE [C]"
      }
      if (i == "MAE") {         
        ylim=c(min(stats[,"MAE",,][!is.na(stats[,"MAE",,])]),c(max(stats[,"MAE",,][!is.na(stats[,"MAE",,])])))
        # ylim=c(1.0,3.0)
        ylab="MAE [C]"
      }
    #}
    
    if (any(is.finite(stats[,i,,iprm]))) {
     # if (iprm == "T2") {
        matplot(prg, (stats[,i,,iprm]), type=type, pch=pch, xlab="", ylab="",
                ylim=ylim,
                col=mod.col[dimnames(stats)[[1]]], lty=mod.lty[dimnames(stats)[[1]]],
                axes=FALSE, cex=par("cex"))
      #}
      mtext(i, side=3, adj=0.5, line=0, cex=2)
      mtext(ylab, side=2, adj=0.5, line=4, cex=2)
      if (iprm=="T2min")
        axis(1, at=prg, labels=c("24", "48", "72"), lwd=0.25, cex.axis=2)
      else
        axis(1, at=prg, labels=prg, lwd=0.25, cex.axis=2)
      axis(2, lwd=0.25, las=1, cex.axis=2)
      box(lwd=0.25)
      #         if (i == "ME") {
      abline(0, 0, lty="dashed", lwd=0.25)
      #           mtext(switch(iprm, MSL="Mean sea level pressure", T2="Temperature 2m",
      #                        FF="Wind speed 10m", RR="Daily precipitation"),
      #                 side=3, adj=0.5, outer=TRUE, line=1, cex=1.5*par("cex"))
      #           mtext(paste(period, collapse=" - "), side=3, adj=0.5, outer=TRUE,
      #                 line=-0.5, cex=par("cex"))
      nsites <- length(unique(x$SITE))
      mtext(paste(nsites, "stations"), side=3, adj=1,
            line=0, cex=2)
      mtext("Lead Time", side=1, line=3, cex=2)
      #        }
      #if (i == "MAE")
      legend("topleft", legend=dimnames(stats)[[1]], col=mod.col[dimnames(stats)[[1]]],
             pch=if (type!="l") pch else NULL, lty=mod.lty[dimnames(stats)[[1]]], bty="n", cex=2.0)
      # legend("topleft", legend=models, col=mod.col[models],
      #        pch=if (type!="l") pch else NULL, lty=mod.lty[models], bty="n")
      
      
    }
    dev.off()
  }
  #dev.off()
}
rm(nsites, iprm, i)

#
#  M A P S
#

stats.spat <- matrix(data=NA,nrow=length(sites2$id),ncol=3)
n=0

for (op in sites2$id) {
  n=n+1
  me.tmp <- (x$SITE == op)
  me.tmp <- x[me.tmp,]
  
  me.sites <- mean(me.tmp$T2max.WRF-me.tmp$T2max.OBS, na.rm=TRUE)
  mae.sites <- mean(abs(me.tmp$T2max.WRF-me.tmp$T2max.OBS), na.rm=TRUE)
  stats.spat[n,1] <- op
  stats.spat[n,2] <- me.sites
  stats.spat[n,3] <- mae.sites
}
colnames(stats.spat) <- c("id","ME","MAE")

sites3 <- merge(sites2,stats.spat,by="id")

#Map of Myanmar
lon_min =90
lon_max =101
lat_min =9
lat_max =30

pdf(paste("T2max_map_ME_",period, ".pdf", sep=""))
map(database="worldHires", xlim=c(lon_min,lon_max), ylim=c(lat_min,lat_max), resolution=1, col="lightgray", fill=TRUE)

#title("ME AROME.KF")
purple <- (sites3$ME < -1.5) 
purple <- sites3[purple,]
points(purple$LON_DEC, purple$LAT_DEC, pch=16, cex=0.5, col="purple")

blue <- (sites3$ME < -0.5) & (sites3$ME >= -1.5) 
blue <- sites3[blue,]
points(blue$LON_DEC, blue$LAT_DEC, pch=16, cex=0.5, col="blue")

green <- (sites3$ME <= 0.5) & (sites3$ME >= -0.5) 
green <- sites3[green,]
points(green$LON_DEC, green$LAT_DEC, pch=16, cex=0.5, col="green")

yellow <- (sites3$ME > 0.5) & (sites3$ME <= 1.5)
yellow <- sites3[yellow,]
points(yellow$LON_DEC, yellow$LAT_DEC, pch=16, cex=0.5, col="orange")

red <- (sites3$ME > 1.5) 
red <- sites3[red,]
points(red$LON_DEC, red$LAT_DEC, pch=16, cex=0.5, col="red")
legend("topleft", legend=c("< -1.5", "=> -1.5, < -0.5", "=> -0.5, <= 0.5",  "> 0.5, <= 1.5", "> 1.5"), text.col=c("purple", "blue", "green", "orange","red"))
dev.off()


stats.spat <- matrix(data=NA,nrow=length(sites2$id),ncol=3)
n=0

for (op in sites2$id) {
  n=n+1
  me.tmp <- (x$SITE == op)
  me.tmp <- x[me.tmp,]
  
  me.sites <- mean(me.tmp$T2min.WRF-me.tmp$T2min.OBS, na.rm=TRUE)
  mae.sites <- mean(abs(me.tmp$T2min.WRF-me.tmp$T2min.OBS), na.rm=TRUE)
  stats.spat[n,1] <- op
  stats.spat[n,2] <- me.sites
  stats.spat[n,3] <- mae.sites
}
colnames(stats.spat) <- c("id","ME","MAE")

sites3 <- merge(sites2,stats.spat,by="id")

#Map of Myanmar
lon_min =90
lon_max =101
lat_min =9
lat_max =30

pdf(paste("T2min_map_ME_",period, ".pdf", sep=""))
map(database="worldHires", xlim=c(lon_min,lon_max), ylim=c(lat_min,lat_max), resolution=1, col="lightgray", fill=TRUE)

#title("ME AROME.KF")
purple <- (sites3$ME < -1.5) 
purple <- sites3[purple,]
points(purple$LON_DEC, purple$LAT_DEC, pch=16, cex=0.5, col="purple")

blue <- (sites3$ME < -0.5) & (sites3$ME >= -1.5) 
blue <- sites3[blue,]
points(blue$LON_DEC, blue$LAT_DEC, pch=16, cex=0.5, col="blue")

green <- (sites3$ME <= 0.5) & (sites3$ME >= -0.5) 
green <- sites3[green,]
points(green$LON_DEC, green$LAT_DEC, pch=16, cex=0.5, col="green")

yellow <- (sites3$ME > 0.5) & (sites3$ME <= 1.5)
yellow <- sites3[yellow,]
points(yellow$LON_DEC, yellow$LAT_DEC, pch=16, cex=0.5, col="orange")

red <- (sites3$ME > 1.5) 
red <- sites3[red,]
points(red$LON_DEC, red$LAT_DEC, pch=16, cex=0.5, col="red")
legend("topleft", legend=c("< -1.5", "=> -1.5, < -0.5", "=> -0.5, <= 0.5",  "> 0.5, <= 1.5", "> 1.5"), text.col=c("purple", "blue", "green", "orange","red"))
dev.off()

#Read results into file
write.table(sites2, file="sites3.txt", sep=" ", row.names=FALSE)
