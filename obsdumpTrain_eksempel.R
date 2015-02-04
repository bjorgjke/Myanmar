##
## DUMP AIR TEMP STATION DATA 
## 
## 
##  
## Ivar May 2013 
##

#require(miIO, lib.loc = "/vol/fou/atmos/R-packages/precise/") 

#file.sites <- "/home/bjorgjke/R/stations_TA.txt"
# obs <- NULL 

# 
# for (i in 1:nrow(sites)) {
# obs.ta    <- miDVH(stnr=sites$STNR[i], period=c(20030607,20130607), prm="TA",
#                 month=seq(1,12,1), hour=c(0,6,12,18))
# obs <- rbind(obs,obs.ta) 
# } 
# obs$TA <- obs$TA + 273.15
# 
# obs.ta    <- miDVH(stnr=13150, period=c(20030607,20130607), prm="TA",
#                 month=seq(1,12,1), hour=c(0,6,12,18))
# 
# 
# ##  sites with priority (in case of ties)
# sites_pr <- c("MELSOM", "NOTODDEN FLYPLASS", "STAVANGER - VÅLAND",
#               "HAMAR II", "OSLO - BLINDERN", "FAGERNES", "VÆRNES",
#               "SVOLVÆR LUFTHAVN", "TROMSØ")

## get lat lon coordinates of modelgrid 
# ex.nc = open.ncdf("/home/bjorgjke/nwparc/eps_lqqt/2013/eps25_lqqt_2013062600Z.nc")
# lon = get.var.ncdf(ex.nc,"lon")   # variable
# lat = get.var.ncdf(ex.nc,"lat")   # variable
# #nc_close(ex.nc)

##  read stations and identify closest grid points

  cosd<-function(degrees) {return(cos(degrees*pi/180));};
  sind<-function(degrees) {return(sin(degrees*pi/180));};
  acosd<-function(rad) {   return(180/pi*acos(rad));};
  asind<-function(rad) {   return(180/pi*asin(rad));};
  atan2d<-function(y,x) {   return(180/pi*atan2(y,x));};

  getangle <- function(lata,lona,latb,lonb) {
  y = sqrt((cosd(latb)*sind(lonb-lona))^2 
	   + (cosd(lata)*sind(latb) - sind(lata)*cosd(latb)*cosd(lonb-lona))^2);
  x = sind(lata)*sind(latb)+cosd(lata)*cosd(latb)*cosd(lonb-lona);
  return(atan2d(y,x));
}

  sites2$NEAREST_X=0;
  sites2$NEAREST_Y=0;
# starting search position
  ci=1;
  cj=1;

   # loop over sites
     for (i in 1:dim(sites2)[1]) {
       if (sites2$NEAREST_X[i]==0 || sites2$NEAREST_Y[i]==0) {
         # iterate to closest location
         changed=1;
         while (changed!=0) {
	   a1 <- getangle(lat[ci,cj],lon[ci,cj],sites2$LAT_DEC[i],sites2$LON_DEC[i]);
	   changed=0;
	   if (changed==0 & ci < dim(lat)[[1]]) {
	     a2 <- getangle(lat[ci+1,cj],lon[ci+1,cj],sites2$LAT_DEC[i],sites2$LON_DEC[i]);
	     if (a2 < a1) {
	       ci=ci+1;
	       changed=changed+10;
	     };
	   };
	   if (changed==0 & ci > 1) {
	     a2=getangle(lat[ci-1,cj],lon[ci-1,cj],sites2$LAT_DEC[i],sites2$LON_DEC[i]);
	     if (a2 < a1) {
	       ci=ci-1;
	       changed=changed-10;
	     };
	   };
	   if (changed==0 & cj < dim(lat)[[2]]) {
	     a2=getangle(lat[ci,cj+1],lon[ci,cj+1],sites2$LAT_DEC[i],sites2$LON_DEC[i]);
	     if (a2 < a1) {
	       cj=cj+1;
	       changed=changed+1;
	     };
	   };
	   if (changed==0 & cj > 1) {
	     a2=getangle(lat[ci,cj-1],lon[ci,cj-1],sites2$LAT_DEC[i],sites2$LON_DEC[i]);
	     if (a2 < a1) {
	       cj=cj-1;
	       changed=changed-1;
	     };
	   };
	 };
# store closest position in grid
	 sites2$NEAREST_X[i] <- ci;
	 sites2$NEAREST_Y[i] <- cj;
       };
     }; # site loop



