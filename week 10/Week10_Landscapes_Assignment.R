# Load the packages from this week's tutorial, aka vignette
#We looked at brook trout population demographics in relationship to water quality and stream flashiness.

pkgs <- installed.packages()
if (!('devtools' %in% pkgs)) { install.packages('devtools') }
if ('dbfishR' %in% pkgs) { remove.packages('dbfishR') }

devtools::install_github(repo = 'Team-FRI/dbfishR', upgrade = 'never')

library (dbfishR)

sites <- get_sites()
events <- get_events()
events_meta <- merge(sites, events[,c("SiteCode","EventCode","WaterTemp","pH","SpecCond","Alk","DO")])
events_meta$year <-substring(as.character(events_meta$EventCode),1,4)

fish_rec <- get_fish_records()

brookie_count <- aggregate(ID~EventCode, data = subset(fish_rec, Species == "Brook Trout" & Pass == "Pass 1"), FUN = length)
colnames(brookie_count)[2] <- "TotalCount"
small_brookie_count <- aggregate(ID~EventCode, data = subset(fish_rec, Length_mm < 100 & Species == "Brook Trout" & Pass == "Pass 1"), FUN = length)
colnames(small_brookie_count)[2] <- "SmallCount"
big_brookie_count <- aggregate(ID~EventCode, data = subset(fish_rec, Length_mm > 99 & Species == "Brook Trout" & Pass == "Pass 1"), FUN = length)
colnames(big_brookie_count)[2] <- "BigCount"


df_list <- list(brookie_count,small_brookie_count, big_brookie_count)
all_brookies <- Reduce(function(x, y) merge(x,y, all= TRUE), df_list)

all_brookies$SmallCount[is.na(all_brookies$SmallCount)] <- 0 #this allows the replace NA below to only take care of 100% YOY NAs
all_brookies$YOYRatio <- all_brookies$SmallCount/(all_brookies$BigCount+all_brookies$SmallCount)
all_brookies$YOYRatio[is.na(all_brookies$YOYRatio)] <- 1 #NAs are 100% YOY.

brookie_events <- merge(all_brookies, events_meta)


library(dataRetrieval)

HUC6 <- "020501"#North Branch Susquehanna
HUC_list <-paste(rep(HUC6,10), seq(0, 9, length.out = 10), sep="0")#To do a full HUC6 at once, just pick your HUC6 and auto-populate the subwatersheds (only works up to 9 HUC8 in a HUC6)

gage_df <- readNWISdata(huc = HUC_list, parameterCd = "00060", startDate = "2010-01-01", endDate = "2020-12-31")

devtools::install_github(repo = 'leppott/ContDataQC', force = TRUE)

library(ContDataQC)

gage_df$year <- sapply(strsplit(as.character(gage_df$dateTime), "-"),"[[",1)#Create year to get annual R-B index

R_B_HUC <- aggregate(X_00060_00003~year+site_no, data = gage_df, FUN = RBIcalc)#Aggregate by year and site w/in the HUC
colnames(R_B_HUC)[3] <- "RBI" #rename column


stations_meta <- readNWISsite(unique(R_B_HUC$site_no))

medium_stations <- subset(stations_meta, drain_area_va > 10 & drain_area_va < 100)


install.packages("sf")
library(sf)
medium_stations_so <- st_as_sf(medium_stations,coords = c("dec_lat_va", "dec_long_va"))

events_so <- st_as_sf(brookie_events[!is.na(brookie_events$SiteLon),], coords = c("SiteLat","SiteLon"))#remove NAs to create spatial object

fish_flow_tmp <- st_join(events_so, medium_stations_so, join = st_nearest_feature)


#Spatial join
install.packages("nngeo")
library(nngeo)
#distances are in degrees
fish_flow_tmp$dist <- unlist(st_nn(events_so, medium_stations_so, returnDist = T)$dist)
fish_flow_tmp <- subset(fish_flow_tmp, dist < 0.5)

#because of year and spatial join needed to change order of operations. Space first, then time
fish_flow <- merge(fish_flow_tmp, R_B_HUC, by = c("year", "site_no"))

mod <- lm(TotalCount~RBI, data = fish_flow)

summary(mod)


mod2 <- lm(BigCount~RBI, data = fish_flow)
summary(mod2)

mod3 <- lm(SmallCount~RBI, data = fish_flow)
summary(mod3)


mod4 <- lm(YOYRatio~RBI, data = fish_flow)
summary(mod4) #winner winner chicken dinner



install.packages("itsadug")
library(itsadug)
plot(mod4$residuals)#residuals from the YOYRatio lm() above


gam.mod <- gam(YOYRatio~RBI, data = fish_flow, na.action = na.omit, method = "REML")#RBI only
summary(gam.mod)

gam.mod <- gam(YOYRatio~RBI+Alk+SpecCond, data = fish_flow, na.action = na.omit, method = "REML")#RBI, alk, and specific conductivity
summary(gam.mod)

plot_smooth(gam.mod, view="RBI", rm.ranef=FALSE)

par(mfrow=c(1,2)) 
plot_smooth(gam.mod, view="RBI", rm.ranef=FALSE)
plot_smooth(gam.mod, view="Alk", rm.ranef=FALSE, ylab = "", xlab = "Specific Conductivity")



#1: Give two specific conclusions you can make from these patterns. (4 pts)
#The YOY is significantly effected by the year and sites the ata were collected form, a combined RBI
#as a stream gets more flashy we can expect less young fish to be present

#2: Rerun this analysis with either (a) a different metric of brook trout populations or a different species from the database. (6 pts)

brookie_count <- aggregate(ID~EventCode, data = subset(fish_rec, Species == "Brook Trout" & Pass == "Pass 1"), FUN = length)
colnames(brookie_count)[2] <- "TotalCount"
small_brookie_count <- aggregate(ID~EventCode, data = subset(fish_rec, Length_mm < 50 & Species == "Brook Trout" & Pass == "Pass 1"), FUN = length)
colnames(small_brookie_count)[2] <- "SmallCount"
big_brookie_count <- aggregate(ID~EventCode, data = subset(fish_rec, Length_mm > 49 & Species == "Brook Trout" & Pass == "Pass 1"), FUN = length)
colnames(big_brookie_count)[2] <- "BigCount"

df_list <- list(brookie_count,small_brookie_count, big_brookie_count)
all_brookies <- Reduce(function(x, y) merge(x,y, all= TRUE), df_list)

all_brookies$SmallCount[is.na(all_brookies$SmallCount)] <- 0 #this allows the replace NA below to only take care of 100% YOY NAs
all_brookies$YOYRatio <- all_brookies$SmallCount/(all_brookies$BigCount+all_brookies$SmallCount)
all_brookies$YOYRatio[is.na(all_brookies$YOYRatio)] <- 1 #NAs are 100% YOY.

brookie_events <- merge(all_brookies, events_meta)


library(dataRetrieval)

HUC6 <- "020501"#North Branch Susquehanna
HUC_list <-paste(rep(HUC6,10), seq(0, 9, length.out = 10), sep="0")#To do a full HUC6 at once, just pick your HUC6 and auto-populate the subwatersheds (only works up to 9 HUC8 in a HUC6)

gage_df <- readNWISdata(huc = HUC_list, parameterCd = "00060", startDate = "2010-01-01", endDate = "2020-12-31")

devtools::install_github(repo = 'leppott/ContDataQC', force = TRUE)

library(ContDataQC)

gage_df$year <- sapply(strsplit(as.character(gage_df$dateTime), "-"),"[[",1)#Create year to get annual R-B index

R_B_HUC <- aggregate(X_00060_00003~year+site_no, data = gage_df, FUN = RBIcalc)#Aggregate by year and site w/in the HUC
colnames(R_B_HUC)[3] <- "RBI" #rename column


stations_meta <- readNWISsite(unique(R_B_HUC$site_no))

medium_stations <- subset(stations_meta, drain_area_va > 10 & drain_area_va < 100)


install.packages("sf")
library(sf)
medium_stations_so <- st_as_sf(medium_stations,coords = c("dec_lat_va", "dec_long_va"))

events_so <- st_as_sf(brookie_events[!is.na(brookie_events$SiteLon),], coords = c("SiteLat","SiteLon"))#remove NAs to create spatial object

fish_flow_tmp <- st_join(events_so, medium_stations_so, join = st_nearest_feature)


#Spatial join
install.packages("nngeo")
library(nngeo)
#distances are in degrees
fish_flow_tmp$dist <- unlist(st_nn(events_so, medium_stations_so, returnDist = T)$dist)
fish_flow_tmp <- subset(fish_flow_tmp, dist < 0.5)

#because of year and spatial join needed to change order of operations. Space first, then time
fish_flow <- merge(fish_flow_tmp, R_B_HUC, by = c("year", "site_no"))

mod <- lm(TotalCount~RBI, data = fish_flow)

summary(mod)


mod2 <- lm(BigCount~RBI, data = fish_flow)
summary(mod2)

mod3 <- lm(SmallCount~RBI, data = fish_flow)
summary(mod3)


mod4 <- lm(YOYRatio~RBI, data = fish_flow)
summary(mod4) #winner winner chicken dinner



install.packages("itsadug")
library(itsadug)
plot(mod4$residuals)#residuals from the YOYRatio lm() above


gam.mod <- gam(YOYRatio~RBI, data = fish_flow, na.action = na.omit, method = "REML")#RBI only
summary(gam.mod)

gam.mod <- gam(YOYRatio~RBI+Alk+SpecCond, data = fish_flow, na.action = na.omit, method = "REML")#RBI, alk, and specific conductivity
summary(gam.mod)

plot_smooth(gam.mod, view="RBI", rm.ranef=FALSE)

par(mfrow=c(1,2)) 
plot_smooth(gam.mod, view="RBI", rm.ranef=FALSE)
plot_smooth(gam.mod, view="Alk", rm.ranef=FALSE, ylab = "", xlab = "Specific Conductivity")


#3: How do the results of your analysis compare to the vignette? (5 pts)
#the intercept of RBI and YOY is significant but when testing its individual RBI, Alk or conductivity, nothing can be significantly correlated.
#furthermore, there is a definite bottleneck  of data points when comparing species conductivity to YOY, but the RBI is signifcantly stairghter than it was before in the original data set. 


#4: For your final project you'll need to find two separate data sources to combine similar to the process here.
  #In prep for that, find one data source to compare with either the data in dbfishR OR DataRetrieval. (5 pts)
  #Read data from that source into your script. (5 pts)
  #Create any analysis of your choice that combines the two data sources, this can be as simple as a linear model. (5 pts)