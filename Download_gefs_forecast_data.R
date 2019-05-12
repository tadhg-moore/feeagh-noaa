if (!"rNOMADS" %in% installed.packages()) install.packages("rNOMADS")
if (!"RCurl" %in% installed.packages()) install.packages("RCurl")
if (!"stringr" %in% installed.packages()) install.packages("stringr")

library(rNOMADS)
library(RCurl)
library(stringr)

#User defined location of interest and directory

lake_lat_n = 53.9
lake_lon_w = 9.5

directory = "C:\\Users\\mooret\\OneDrive - Dundalk Institute of Technology\\PROGNOS_offline\\CareyVT\\feeagh-NOAA/"

#####

time <- c(0,64) #6 hour prediction for 16 days
lon.dom <- seq(0, 360, by = 1) #domain of longitudes in model
lat.dom <- seq(-90, 90, by = 1) #domain of latitudes in model
lon <- which.min(abs(lon.dom  - (360 - lake_lon_w))) - 1 #NOMADS indexes start at 0
lat <- which.min(abs(lat.dom - lake_lat_n)) - 1 #NOMADS indexes start at 0 


#Get yesterdays 6 am GMT, 12 pm GMT, 6 pm GMT, and todays 12 an GMT runs

for(k in 1:10000){
  message('Querying NOAA server for up to date data...')
  urls.new <- GetDODSDates(abbrev = "gens_bc")
  if(k != 1){
    ind = (urls.old$date %in% urls.new$date)
    
    urls = urls.new$url[!ind]
    dates = urls.new$date[!ind]
    if(length(urls) == 0){
      message(paste('No new forecasts at', Sys.time()))
      Sys.sleep(21600)
      next
    }
  }else{
    urls = urls.new$url
    dates = urls.new$date
    
  }
  
  
  for(i in 1:length(urls)){
    
    model.url <- urls[i]
    model.url <- str_replace(model.url,"gens_bc","gens")
    run_date <- dates[i]
    model_list <- c('gep_all_00z', 'gep_all_06z', 'gep_all_12z', 'gep_all_18z')
    
    for(m in 1:length(model_list)){
      
      file_present_local <- file.exists(paste(directory,run_date,model_list[m],'.csv',sep=''))
      print(paste0(directory,run_date,model_list[m],'.csv is already downloaded: ',file_present_local))
      
      #Check if already downloaded
      if(!file_present_local){
        
        model.runs <- GetDODSModelRuns(model.url)
        #check if avialable at NOAA
        if(model_list[m] %in% model.runs$model.run){
          
          model.run <- model.runs$model.run[which(model.runs$model.run == model_list[m])]
          #Get variables of interest for GLM
          
          #tmp2m #temp at 2 m
          
          #dlwrfsfc #surface downward long-wave rad. flux [w/m^2]
          
          #dswrfsfc #surface downward short-wave rad. flux [w/m^2]
          
          #pratesfc #surface precipitation rate [kg/m^2/s]
          
          #rh2m #2 m above ground relative humidity [%] 
          
          #vgrd10m  #10 m above ground v-component of wind [m/s] 
          
          #ugrd10m #10 m above ground u-component of wind [m/s] 
          
          #spfh2m #2 m above specific humidity  [kg/kg] 
          
          #pressfc #Surface pressure [pa]
          
          tmp2m <- DODSGrab(model.url, model.run, "tmp2m", time = time, lon = c(lon,lon), 
                            lat = c(lat,lat),ensembles=c(0,20))
          
          dlwrfsfc <- DODSGrab(model.url, model.run, "dlwrfsfc", time = time, lon = c(lon,lon), 
                               lat = c(lat,lat),ensembles=c(0,20))
          
          dswrfsfc <- DODSGrab(model.url, model.run, "dswrfsfc", time = time, lon = c(lon,lon), 
                               lat = c(lat,lat),ensembles=c(0,20))
          
          pratesfc <- DODSGrab(model.url, model.run, "pratesfc", time = time, lon = c(lon,lon), 
                               lat = c(lat,lat),ensembles=c(0,20))
          
          rh2m <- DODSGrab(model.url, model.run, "rh2m", time = time, lon = c(lon,lon), 
                           lat = c(lat,lat),ensembles=c(0,20))
          
          vgrd10m <- DODSGrab(model.url, model.run, "vgrd10m", time = time, lon = c(lon,lon), 
                              lat = c(lat,lat),ensembles=c(0,20))
          
          ugrd10m <- DODSGrab(model.url, model.run, "ugrd10m", time = time, lon = c(lon,lon), 
                              lat = c(lat,lat),ensembles=c(0,20))
          
          spfh2m <- DODSGrab(model.url, model.run, "spfh2m", time = time, lon = c(lon,lon), 
                             lat = c(lat,lat),ensembles=c(0,20))
          
          pressfc <- DODSGrab(model.url, model.run, "pressfc", time = time, lon = c(lon,lon), 
                              lat = c(lat,lat),ensembles=c(0,20))
          tcdcclm <- DODSGrab(model.url, model.run, "tcdcclm", time = time, lon = c(lon,lon), 
                              lat = c(lat,lat),ensembles=c(0,20))
          
          forecast.time <- strftime(tmp2m$forecast.date, format="%Y-%m-%d %H:%M:%S",tz = 'GMT')
          
          forecast_noaa <- data.frame(forecast.date = forecast.time, 
                                      ensembles = tmp2m$ensembles, 
                                      tmp2m = tmp2m$value, 
                                      dlwrfsfc= dlwrfsfc$value, 
                                      dswrfsfc = dswrfsfc$value, 
                                      pratesfc = pratesfc$value, 
                                      rh2m = rh2m$value, 
                                      vgrd10m = vgrd10m$value, 
                                      ugrd10m = ugrd10m$value, 
                                      spfh2m = spfh2m$value, 
                                      pressfc = pressfc$value,
                                      tcdcclm =tcdcclm$value)
          
          write.csv(forecast_noaa,paste(directory,run_date,model_list[m],'.csv',sep=''),row.names = FALSE)
          message(paste('Finished',model_list[m],'for',tmp2m$model.run.date[1]))
        }
      }
    }
  }
  urls.old <- urls.new
  message(paste('Finished downloading forecasts, will check again at '),(Sys.time() + 3*60*60))
  Sys.sleep(10800)
}


