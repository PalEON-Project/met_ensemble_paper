#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

## R Code to convert from NACP intercomparison NETCDF met files into ED2 ascii met files

## It requires the rhdf5 library, which is not available on CRAN, but by can be installed locally:
## >source('http://bioconductor.org/biocLite.R')
## >biocLite('rhdf5')

## If files already exist in 'Outfolder', the default function is NOT to overwrite them and only
## gives user the notice that file already exists. If user wants to overwrite the existing files,
## just change overwrite statement below to TRUE.


##' met2model wrapper for ED2
##'
##' @title met2model for ED2
##' @export
##' @param in.path location on disk where inputs are stored
##' @param in.prefix prefix of input and output files
##' @param outfolder location on disk where outputs will be stored
##' @param start_date the start date of the data to be downloaded (will only use the year part of the date)
##' @param end_date the end date of the data to be downloaded (will only use the year part of the date)
##' @param lst timezone offset to GMT in hours
##' @param overwrite should existing files be overwritten
##' @param verbose should the function be very verbose
##' @param path.co2 file path to where the CO2 driver is housed
##' @param force.sanity (logical) do we force the met to pass ED sanity checks?
##' @param freq.agg the number of observations per day we want; if NULL data will remain in native resolution
##' @importFrom ncdf4 ncvar_get ncdim_def ncatt_get ncvar_add
met2model.ED2 <- function(in.path, in.prefix, outfolder, start_date, end_date, lst = 0, lat = NA,
                          lon = NA, overwrite = FALSE, verbose = FALSE, 
                          path.co2, force.sanity=FALSE,
                          freq.agg=NULL, ...) {
  
  # Additional funcitons from solar_angle.R
  equation_of_time <- function(doy) {
    stopifnot(doy <= 366)
    f      <- pi / 180 * (279.5 + 0.9856 * doy)
    et     <- (-104.7 * sin(f) + 596.2 * sin(2 * f) + 4.3 *
                 sin(4 * f) - 429.3 * cos(f) - 2 *
                 cos(2 * f) + 19.3 * cos(3 * f)) / 3600  # equation of time -> eccentricity and obliquity
    return(et)
  }
  
  cos_solar_zenith_angle <- function(doy, lat, lon, dt, hr) {
    et <- equation_of_time(doy)
    merid  <- floor(lon / 15) * 15
    merid[merid < 0] <- merid[merid < 0] + 15
    lc     <- (lon - merid) * -4/60  ## longitude correction
    tz     <- merid / 360 * 24  ## time zone
    midbin <- 0.5 * dt / 86400 * 24  ## shift calc to middle of bin
    t0   <- 12 + lc - et - tz - midbin  ## solar time
    h    <- pi/12 * (hr - t0)  ## solar hour
    dec  <- -23.45 * pi / 180 * cos(2 * pi * (doy + 10) / 365)  ## declination
    cosz <- sin(lat * pi / 180) * sin(dec) + cos(lat * pi / 180) * cos(dec) * cos(h)
    cosz[cosz < 0] <- 0
    return(cosz)
  }
  
  
  overwrite <- as.logical(overwrite)

  # results are stored in folder prefix.start.end
  start_date <- as.POSIXlt(start_date, tz = "UTC")
  end_date   <- as.POSIXlt(end_date, tz = "UTC")
  met_folder <- outfolder
  met_header <- file.path(met_folder, "ED_MET_DRIVER_HEADER")

  # results <- data.frame(file = c(met_header),
  #                       host = c(PEcAn.remote::fqdn()),
  #                       mimetype = c("text/plain"),
  #                       formatname = c("ed.met_driver_header files format"),
  #                       startdate = c(start_date),
  #                       enddate = c(end_date),
  #                       dbfile.name = "ED_MET_DRIVER_HEADER",
  #                       stringsAsFactors = FALSE)

  ## check to see if the outfolder is defined, if not create directory for output
  dir.create(met_folder, recursive = TRUE, showWarnings = FALSE)

  ### FUNCTIONS
  dm <- c(0, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366)
  dl <- c(0, 32, 61, 92, 122, 153, 183, 214, 245, 275, 306, 336, 367)
  month <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
  mon_num <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
  day2mo <- function(year, day) {
    leap      <- lubridate::leap_year(year)
    mo        <- rep(NA, length(day))
    mo[leap]  <- findInterval(day[leap], dl)
    mo[!leap] <- findInterval(day[!leap], dm)
    return(mo)
  }

  # get start/end year since inputs are specified on year basis
  start_year <- lubridate::year(start_date)
  end_year <- lubridate::year(end_date)
  day_secs <- udunits2::ud.convert(1, "day", "seconds")

  # CO2 is available as a single netcdf file elsewhere
  nc.co2 <- ncdf4::nc_open(path.co2)
  co2.all <- ncdf4::ncvar_get(nc.co2)
  ncdf4::nc_close(nc.co2)
  
  # Brute force adding CO2 for 2011-2015 from Scripps; accessed at https://www.co2.earth/monthly-co2 27 Nov, 2017
  co2.all <- c(co2.all, 
               391.30, 391.92, 392.45, 393.37, 394.28, 393.69, 392.60, 390.21, 389.00, 388.93, 390.24, 391.80, #2011
               393.07, 393.35, 394.36, 396.43, 396.87, 395.88, 394.52, 392.54, 391.13, 391.01, 392.95, 394.34, #2012
               395.61, 396.85, 397.26, 398.35, 399.98, 398.87, 397.37, 395.41, 393.39, 393.70, 395.19, 396.82, #2013
               397.93, 398.10, 399.47, 401.33, 401.88, 401.31, 399.07, 397.21, 395.40, 395.65, 397.22, 398.79, #2014
               399.85, 400.31, 401.51, 403.45, 404.11, 402.88, 401.60, 399.00, 397.50, 398.28, 400.24, 401.89 #2015
               )
  yrs.co2 <- rep(850:2015, each=12)
  mos.co2 <- rep(1:12, length.out=length(co2.all))
  
  ## loop over files
  for (year in start_year:end_year) {
    ncfile <- file.path(in.path, paste(in.prefix, stringr::str_pad(year, 4, pad=0), "nc", sep = "."))

    nday <- ifelse(lubridate::leap_year(year), 366, 365)
    ## extract file root name froot <- substr(files[i],1,28) print(c(i,froot))

    ## open netcdf
    nc <- ncdf4::nc_open(ncfile)

    # check lat/lon
    flat <- try(ncdf4::ncvar_get(nc, "latitude"), silent = TRUE)
    if (!is.numeric(flat)) {
      flat <- nc$dim[[1]]$vals[1]
    }
    if (is.na(lat)) {
      lat <- flat
    } else if (lat != flat) {
      warning("Latitude does not match that of file", lat, "!=", flat)
    }

    flon <- try(ncdf4::ncvar_get(nc, "longitude"), silent = TRUE)
    if (!is.numeric(flon)) {
      flat <- nc$dim[[2]]$vals[1]
    }
    if (is.na(lon)) {
      lon <- flon
    } else if (lon != flon) {
      warning("Longitude does not match that of file", lon, "!=", flon)
    }

    ## determine GMT adjustment lst <- site$LST_shift[which(site$acro == froot)]

    ## extract variables
    lat  <- eval(parse(text = lat))
    lon  <- eval(parse(text = lon))
    sec  <- nc$dim$time$vals
    obs.day <- length(sec)/nday
    ## convert time to seconds
    sec <- udunits2::ud.convert(sec, unlist(strsplit(nc$dim$time$units, " "))[1], "seconds")
    
    dt <- udunits2::ud.convert(nday, 'days', 'seconds') / length(sec)
    toff <- -as.numeric(lst) * 3600 / dt
    
    # dt <- PEcAn.utils::seconds_in_year(year) / length(sec)
    # dt <- 1/obs.day * 24/1 * 60/1 * 60/1 # 1/(obs/day) * hrs/day * min/hr * sec/hr) Observation time step in seconds
    
    Tair <- ncdf4::ncvar_get(nc, "air_temperature")
    Qair <- ncdf4::ncvar_get(nc, "specific_humidity")  #humidity (kg/kg)
    Wind    <- ncdf4::ncvar_get(nc, "wind_speed")
    Rain <- ncdf4::ncvar_get(nc, "precipitation_flux")
    pres <- ncdf4::ncvar_get(nc, "air_pressure")
    SW   <- ncdf4::ncvar_get(nc, "surface_downwelling_shortwave_flux_in_air")
    LW   <- ncdf4::ncvar_get(nc, "surface_downwelling_longwave_flux_in_air")

    ncdf4::nc_close(nc)
    
    # Dupe monthly CO2 into hourly CO2
    CO2 <- vector()
    for(i in 1:12){
      co2.now <- rep(co2.all[which(yrs.co2==year & mos.co2==i)], lubridate::days_in_month(i)*obs.day)
      CO2 <- c(CO2, co2.now)
    }
    
    # if we need to add in leap year, add it to the end
    if(length(CO2) < length(Tair)){
      CO2 <- c(CO2, CO2[(length(CO2) - (length(Tair) - length(CO2)) +1 ):length(CO2)])
    }

    ## buffer to get to GMT
    slen <- length(SW)
    Tair <- c(rep(Tair[1], toff), Tair)[1:slen]
    Qair <- c(rep(Qair[1], toff), Qair)[1:slen]
    Wind <- c(rep(Wind[1], toff), Wind)[1:slen]
    Rain <- c(rep(Rain[1], toff), Rain)[1:slen]
    pres <- c(rep(pres[1], toff), pres)[1:slen]
    SW   <- c(rep(SW[1], toff), SW)[1:slen]
    LW   <- c(rep(LW[1], toff), LW)[1:slen]
    # if (useCO2) {
      CO2 <- c(rep(CO2[1], toff), CO2)[1:slen]
    # }
    
    if(!is.null(freq.agg)){
      fact.agg <- obs.day/freq.agg

      df.tmp <- data.frame(sec, Tair, Qair, Wind, Rain, pres, SW, LW, CO2)
      time.ind <- rep(1:(slen/fact.agg), each=fact.agg)

      df.tmp <- aggregate(df.tmp, by=list(time.ind), FUN=mean)

      sec  <- df.tmp$sec
      Tair <- df.tmp$Tair
      Qair <- df.tmp$Qair
      Wind <- df.tmp$Wind
      Rain <- df.tmp$Rain
      pres <- df.tmp$pres
      SW   <- df.tmp$SW
      LW   <- df.tmp$LW
      CO2  <- df.tmp$CO2

      # Update our time indices
      dt <- udunits2::ud.convert(nday, 'days', 'seconds') / length(sec)
      toff <- -as.numeric(lst) * 3600 / dt
    }
      
    if(force.sanity){
      SW[SW<0] <- 0
      SW[SW>1500] <- 1500-1
      LW[LW<40] <- 40+1
      LW[LW>600] <- 600-1
      Tair[Tair<184] <- 184+1
      Tair[Tair>331] <- 331-1
      Qair[Qair<1e-6] <- 1.1e-6
      Qair[Qair>3.2e-2] <- 3.19e-2
      CO2[CO2<100] <- 100.1
      CO2[CO2>1100] <- 1100-1
      pres[pres<45000] <- 450001
      pres[pres>110000] <- 110000-1
      Rain[Rain<0] <- 0
      Rain[Rain>0.1111] <- 0.1110
      Wind[Wind<0] <- 0.01
      Wind[Wind>85] <- 85-1
    }

    ## build time variables (year, month, day of year)
    skip <- FALSE
    nyr <- floor(udunits2::ud.convert(length(sec) * dt, "seconds", "days")/nday)
    # nyr  <- 0
    yr   <- NULL
    doy  <- NULL
    hr   <- NULL
    asec <- sec
    for (y in seq(year, year + nyr - 1)) {
      diy <- nday
      ytmp <- rep(y, udunits2::ud.convert(diy / dt, "days", "seconds"))
      dtmp <- rep(seq_len(diy), each = day_secs / dt)
      if (is.null(yr)) {
        yr  <- ytmp
        doy <- dtmp
        hr  <- rep(NA, length(dtmp))
      } else {
        yr  <- c(yr, ytmp)
        doy <- c(doy, dtmp)
        hr  <- c(hr, rep(NA, length(dtmp)))
      }
      rng <- length(doy) - length(ytmp):1 + 1
      if (!all(rng >= 0)) {
        skip <- TRUE
        warning(paste(year, "is not a complete year and will not be included"))
        break
      }
      asec[rng] <- asec[rng] - asec[rng[1]]
      hr[rng]   <- (asec[rng] - (dtmp - 1) * day_secs) / day_secs * 24
    }
    mo <- day2mo(yr, doy)
    if (length(yr) < length(sec)) {
      rng <- (length(yr) + 1):length(sec)
      if (!all(rng >= 0)) {
        skip <- TRUE
        warning(paste(year, "is not a complete year and will not be included"))
        break
      }
      yr[rng]  <- rep(y + 1, length(rng))
      doy[rng] <- rep(1:366, each = day_secs / dt)[1:length(rng)]
      hr[rng]  <- rep(seq(0, length = day_secs / dt, by = dt / day_secs * 24), 366)[1:length(rng)]
    }
    if (skip) {
      print("Skipping to next year")
      next
    }


    ## calculate potential radiation in order to estimate diffuse/direct
    cosz <- cos_solar_zenith_angle(doy, lat, lon, dt, hr)

    rpot <- 1366 * cosz
    rpot <- rpot[1:length(SW)]

    # SW[rpot < SW] <- rpot[rpot < SW]  ## ensure radiation < max
    ### this causes trouble at twilight bc of missmatch btw bin avergage and bin midpoint
    frac <- SW/rpot
    frac[frac > 0.9] <- 0.9  ## ensure some diffuse
    frac[frac < 0] <- 0
    frac[is.na(frac)] <- 0
    frac[is.nan(frac)] <- 0
    SWd <- SW * (1 - frac)  ## Diffuse portion of total short wave rad

    ### convert to ED2.1 hdf met variables
    n      <- length(Tair)
    # nbdsfA <- SW  # near IR beam downward solar radiation [W/m2]
    # nddsfA <- rep(0, n)  # near IR diffuse downward solar radiation [W/m2]
    # vbdsfA <- rep(0, n)  # visible beam downward solar radiation [W/m2]
    # vddsfA <- rep(0, n)  # visible diffuse downward solar radiation [W/m2]
    nbdsfA <- (SW - SWd) * 0.57  # near IR beam downward solar radiation [W/m2]
    nddsfA <- SWd * 0.48  # near IR diffuse downward solar radiation [W/m2]
    vbdsfA <- (SW - SWd) * 0.43  # visible beam downward solar radiation [W/m2]
    vddsfA <- SWd * 0.52  # visible diffuse downward solar radiation [W/m2]
    prateA <- Rain  # precipitation rate [kg_H2O/m2/s]
    dlwrfA <- LW  # downward long wave radiation [W/m2]
    presA  <- pres  # pressure [Pa]
    hgtA   <- rep(50, n)  # geopotential height [m]
    ugrdA  <- Wind  # zonal wind [m/s]
    # vgrdA  <- V  # meridional wind [m/s]
    shA    <- Qair  # specific humidity [kg_H2O/kg_air]
    tmpA   <- Tair  # temperature [K]
    
    co2A <- CO2  # surface co2 concentration [ppm] from drivers

    ## create directory if(system(paste('ls',froot),ignore.stderr=TRUE)>0)
    ## system(paste('mkdir',froot))

    ## write by year and month
    for (y in year + 1:nyr - 1) {
      sely <- which(yr == y)
      for (m in unique(mo[sely])) {
        selm <- sely[which(mo[sely] == m)]
        mout <- paste(met_folder, "/", y+1000, month[m], ".h5", sep = "")
        if (file.exists(mout)) {
          if (overwrite == TRUE) {
            file.remove(mout)
            rhdf5::h5createFile(mout)
          }
          if (overwrite == FALSE) {
            warning("The file already exists! Moving to next month!")
            next
          }
        } else {
          rhdf5::h5createFile(mout)
        }
        dims  <- c(length(selm), 1, 1)
        nbdsf <- array(nbdsfA[selm], dim = dims)
        nddsf <- array(nddsfA[selm], dim = dims)
        vbdsf <- array(vbdsfA[selm], dim = dims)
        vddsf <- array(vddsfA[selm], dim = dims)
        prate <- array(prateA[selm], dim = dims)
        dlwrf <- array(dlwrfA[selm], dim = dims)
        pres  <- array(presA[selm], dim = dims)
        hgt   <- array(hgtA[selm], dim = dims)
        ugrd  <- array(ugrdA[selm], dim = dims)
        sh    <- array(shA[selm], dim = dims)
        tmp   <- array(tmpA[selm], dim = dims)
        # if (useCO2) {
          co2 <- array(co2A[selm], dim = dims)
        # }
        rhdf5::h5write.default(nbdsf, mout, "nbdsf")
        rhdf5::h5write.default(nddsf, mout, "nddsf")
        rhdf5::h5write.default(vbdsf, mout, "vbdsf")
        rhdf5::h5write.default(vddsf, mout, "vddsf")
        rhdf5::h5write.default(prate, mout, "prate")
        rhdf5::h5write.default(dlwrf, mout, "dlwrf")
        rhdf5::h5write.default(pres, mout, "pres")
        rhdf5::h5write.default(hgt, mout, "hgt")
        rhdf5::h5write.default(ugrd, mout, "ugrd")
        rhdf5::h5write.default(sh, mout, "sh")
        rhdf5::h5write.default(tmp, mout, "tmp")
        # if (useCO2) {
          rhdf5::h5write.default(co2, mout, "co2")
        # }
          
        # Add our time dims
        rhdf5::h5write.default(sec, mout, "time")
        rhdf5::h5write.default(lat, mout, "lat" )
        rhdf5::h5write.default(lon, mout, "lon")
        rhdf5::h5write.default(sec, mout, "time")
      }
    }

    ## write DRIVER file
    sites <- 1
    metgrid <- c(1, 1, 1, 1, lon, lat)
    metvar <- c("nbdsf", "nddsf", "vbdsf", "vddsf", "prate", "dlwrf",
                "pres", "hgt", "ugrd", "sh", "tmp", "co2")
    nmet <- length(metvar)
    metfrq <- rep(dt, nmet)
    metflag <- rep(1, nmet)
    # if (!useCO2) {
      # metflag[metvar == "co2"] <- 4
      # metfrq[metvar == "co2"] <- 380
    # }
    write.table("header", met_header, row.names = FALSE, col.names = FALSE)
    write.table(sites, met_header, row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table(met_folder, met_header, row.names = FALSE, col.names = FALSE, append = TRUE,
                quote = FALSE)
    write.table(matrix(metgrid, nrow = 1), met_header, row.names = FALSE, col.names = FALSE,
                append = TRUE, quote = FALSE)
    write.table(nmet, met_header, row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE)
    write.table(matrix(metvar, nrow = 1), met_header, row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table(matrix(metfrq, nrow = 1), met_header, row.names = FALSE, col.names = FALSE, append = TRUE,
                quote = FALSE)
    write.table(matrix(metflag, nrow = 1), met_header, row.names = FALSE, col.names = FALSE,
                append = TRUE, quote = FALSE)
  }  ### end loop over met files

  print("Done with met2model.ED2")
  # return(invisible(results))
} # met2model.ED2
