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
askCSV <- function (default){
  #Interactive ask CSV file
  f <- choose.files(default = "", multi = FALSE, caption = "Select the input  *.CSV file", filters = matrix(c("CSV File","*.csv"),1,2, byrow= TRUE))
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
askSHP <- function (default){
  f <- choose.files(default = "", multi = FALSE, caption = "Select the input  *.shp file", filters = matrix(c("Shapefile","*.shp"),1,2, byrow= TRUE))
  print(paste("selected shp: ",f)) #Say which shp was selected
  return(shapefile(f)) # Return shapefile filename
}

#### Function to choose a raster file interactively, Allow Tif, Bsq and All
##   accepts to set the default location to open in the search window.}
askRaster <- function (default){
  f <- choose.files(default = "", multi = FALSE, caption = "Select the input raster", filters = matrix(c("BSQ","*.bsq","All","*","Tiff","*.tif"),3,2, byrow= TRUE))
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
#### Statistics Functions removing NA values
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

#### Extract vales from images
extractThis <- function(r_file,zones, outFolder, ID_field ="Name",func=median_na,buf=0,small=FALSE,band_names=NULL){
  ##   Imports raster as stack to allow multiple bands, uses "velox" object to make raster operations faster
  r <- velox(stack(r_file))
  #Set output name like the raster (remove extension and path from), save in the output folder
  outName <- tools::file_path_sans_ext(basename(r_file))
  OutTableName <- file.path(outFolder,paste0(outName,".csv"),fsep=.Platform$file.sep)
  ## Zonal statistics with velox/raster package 
  #Calculate buffer if necessary, buffer = 0 means no buffer
  if (buf != 0 ) {
    zones <- buffer(zones, width = buf, dissolve= FALSE)
  }
  ## With this we get a table with the values for all the plots and bands
  table <- r$extract(zones, df=TRUE, fun= func, small=small) #Extract and summarize the function, remove NA values. Returns a dataframe
  ## Set ID names from the shapefile IF field
  table$ID_sp <- zones@data[,ID_field] 
  ##Set band names if they are given and if they are not, assign them
  i <- substring(outName,1,1) #Get the first letter of the raster name to know the camera used
  # Assign band names depending on the camera used
  band_names <- switch(i,
                       "q" = c("ID","Red","Green","Red Edge","NIR"),
                       "e" = c("ID","Blue","Red","Green","Red Edge","NIR"),
                       "h" = c("ID",paste0("B",1:62)),
                       "t" = c("ID","celsius"))
  if(!is.null(band_names)) names(table) <- band_names #Assign the band names if we have them
  #Save to disk
  write.csv(table,OutTableName, row.names = FALSE)
  ##plot histograms. the column list to use will be calculated
  hist_extr(table, outName,outFolder)
}

#######################################################################
#### Functions to perform graphics
#### Libraries used: R-base code
#######################################################################

## Function to plot the histogram of selected extracted bands
## t : is the extracted output table from "extractThis" [data frame]
## outName : is a name to identify the output
## column_list  : is the band # list of which bands to use for the histograms [integer vector]
## par : is to divide the grid of graphs per page, default 3x3
##  Consider that the first column is always the ID
hist_extr <- function(t, outName, outFolder, column_list=NULL, par1=3){
  tryCatch({
      pdf(file.path(outFolder,sprintf("Histogram_%s.pdf",outName),fsep=.Platform$file.sep)) ## Start PDF
      par <- par(mfrow=c(par1, par1))
      #Select the bands to use based on the raster if they are not given
      if (is.null(column_list)) {
        i <- substring(outName, 1, 1) #Get the first letter of the raster name to know the camera used
        # Indicate which band # to use, 
        column_list <- switch(i, 
                              "q" = c(1,2,3,4), 
                              "e" = c(1,2,3,4,5), 
                              "h" = c(6,22,37,43,60), 
                              "t" = c(1,1))
      }
      if (is.null(column_list)) {column_list <- 2:ncol(t)-1}
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