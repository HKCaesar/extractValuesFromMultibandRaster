### Functions for the extract script "extract_from_raster_based_on_polygon_zones.R"


#############################################################
#### Functions to select specific format files
#### Libraries used:
#install.packages("raster") #Install if necessary (Just the first time)
require(raster) # For importing rasters and shapefile
#install.packages("velox") #Install if necessary (Just the first time)
require(velox) # For fast raster processing
#############################################################

#### Function to choose a CSV file interactively, halts if the input files or folders dont exist.
##   accepts to set the default location to open in the search window.
askCSV <- function (defaultP=""){
  #Interactive ask CSV file
  f <- choose.files(default = defaultP, multi = FALSE, caption = "Select the input  *.CSV file", filters = matrix(c("CSV File","*.csv"),1,2, byrow= TRUE))
  #Open data table CSV
  inputList <- read.csv(f,stringsAsFactors=FALSE)
  #check valid filenames 
  if ((invalidInputFiles(inputList))) {
    stop("The input list has some invalid input file name") #Stop script if there are invalid filenames in the list.
  }
  return(inputList)
}

#### Function to choose a shapefile file interactively.
##   accepts to set the default location to open in the search window.
#zones <- readOGR(dsn = fgdb, layer = roi_name) # This can read also GDB features
askSHP <- function (defaultP = ""){
  f <- choose.files(default = defaultP, multi = FALSE, caption = "Select the input  *.shp file", filters = matrix(c("Shapefile","*.shp"),1,2, byrow= TRUE))
  print(paste("selected shp: ",f)) #Say which shp was selected
  return(shapefile(f)) # Return shapefile filename
}

#### Function to choose a raster file interactively, Allow Tif, Bsq and All
##   accepts to set the default location to open in the search window.}
askRaster <- function (defaultP = ""){
  f <- choose.files(default = defaultP, multi = FALSE, caption = "Select the input raster", filters = matrix(c("BSQ","*.bsq","All","*","Tiff","*.tif"),3,2, byrow= TRUE))
  print(paste("selected raster: ",f)) #Print the selected raster name
  return(f) # Return raster name 
}

# Check the first 3 columns of the input files list, i.e. the raster filenames, the zones shapefile names and output folder locations for extraction, in_mode ==1
invalidInputFiles <- function(inputList){
  unexistingFiles <- !apply(inputList[,c(1:3)],2,file.exists)
  if (any(unexistingFiles)) {
    print("The following files in list don't exist")
    print(inputList[,1:3][unexistingFiles]) 
    return(TRUE)
  } else (return(FALSE)) # No invalid filenames
}

#######################################################################
####
#### Statistics Functions removing NA values
####
#######################################################################
## Median removing NA
median_na <- function(x) median(x, na.rm = TRUE)

## Mean removing NA
mean_na <- function(x) mean(x, na.rm = TRUE)

## sd removing NA
sd_na <- function(x) sd(x, na.rm = TRUE)

## 1st quartile removing NA
Q1_na <- function(x) quantile(x, prob=c(0.25), na.rm = TRUE)

## 3rd quartile removing NA
Q3_na <- function(x) quantile(x, prob=c(0.75), na.rm = TRUE)


#######################################################################
#### Functions to perform the zonal statistics on multiband rasters
#### Libraries used:
#require(raster) #is already imported in this file
#######################################################################

#### Extract vales from images (velox package)
extractThis <- function(r_file,zones, outFolder, ID_field ="Name",func=median_na,buf=0,small=TRUE,band_names=NULL){
  ##   Imports raster as stack to allow multiple bands, uses "velox" object to make raster operations faster
  b <- brick(r_file) #Brick raster object
  r <- velox(b) #Velox raster object
  #Set output name like the raster (remove extension and path from), save in the output folder
  outName <- tools::file_path_sans_ext(basename(r_file))
  OutTableName <- file.path(outFolder,paste0(outName,".csv"),fsep=.Platform$file.sep)
  ## Zonal statistics with velox/raster package 
  #Calculate buffer if necessary, buffer = 0 means no buffer
  if (buf != 0 ) {
    zones <- buffer(zones, width = buf, dissolve= FALSE)
  }
  #Check if they have the same Coordinate system, unify if necessary
  if(proj4string(b) != proj4string(zones)){
    message("Raster and zones dont have the same CRS, zones CRS will be transformed to the raster one (not permanently)")
    zones <- spTransform(zones,CRS(proj4string(b)))
  }
  ## With this we get a table with the values for all the plots and bands
  table <- r$extract(zones, df=TRUE, fun= func, small=small) #Extract and summarize the function, remove NA values. Returns a dataframe
  ## Set ID names from the shapefile ID field
  table$ID_sp <- zones@data[,ID_field] 
  ##Set band names if they are given and if they are not, assign them
  ic <- substring(outName,1,1) #Get the first letter of the raster name to know the camera used
  # Assign band names depending on the camera used
  if(!is.null(band_names)){ #If the band names were passed (not NULL), use them
    names(table) <- band_names
  } else{#Or try to assign them based on the camera used (THis may not be valid for imagery subproducts)
    band_names <- switch(ic,
                       "q" = c("ID","Green","Red","Red Edge","NIR"),
                       "e" = c("ID","Blue","Green","Red","Red Edge","NIR"),
                       "h" = c("ID",paste0("B",1:62)),
                       "t" = c("ID","celsius"))
    if(!is.null(band_names)) names(table) <- band_names #Assign the band names if we have a matching pattern
  }
  #Save to disk
  write.csv(table,OutTableName, row.names = FALSE)
  ##plot histograms. the column list to use will be calculated
  hist_extr(table, outName,outFolder)
}

#######################################################################
#### Functions to perform graphics and checks
#### Libraries used: R-base code
#######################################################################

## Function to plot the histogram of selected extracted bands
## t : is the extracted output table from "extractThis" [data frame]
## outName : is a name to identify the output
## column_list  : is the band # list of which bands to use for the histograms [integer vector]
## par : is to divide the grid of graphs per page, default 3x3
##  Consider that the first column is always the ID
hist_extr <- function(t, outName, outFolder, par1=3){
  tryCatch({
      pdf(file.path(outFolder,sprintf("Histogram_%s.pdf",outName),fsep=.Platform$file.sep)) ## Start PDF
      par <- par(mfrow=c(par1, par1))
      #Select the bands to use based on the raster (to avoid too many plots for hyper)
      if (!ncol(t) > 6) { #If no more than 6 bands in the table, use them 
        column_list <- seq_len(ncol(t)-1) #plot one histogram per band/column. But not he first (ID field)
      } else{ #But if more, make a short sequence skiping bands
        column_list <-seq(1,ncol(t)-1,by=ncol(t)/10)
      }
      #Print the histogram for each selected band
      for (c in column_list) {
        # the first column of the table would be the ID, so we skip it
        hist(t[,c+1], nclass=20, main=sprintf("Histogram band: %i",c),xlab="Band value")
      }
      dev.off() ## Stop PDF
      print(paste("saved PDF in outFolder")) #Say that it was saved the PDF
    }#,
    ##In case of halting error:
    #error = function(e){print(c("An error ocurred: ",e$message))}#,
  )
}

#### Function to do some statistics for the yield components tables
## Ongoing
QA_yield <- function(t){
  boxplot(t)
  
}

#### Function to do some statistics for the extracted bands tables
QA_bands <- function(t){
  boxplot(t[,c("yield12","biomass","spikesm2","grainsm2")])
  boxplot(t[,c("w1000","maturity", "pb","pn","kb","count","height")])
  
  
}