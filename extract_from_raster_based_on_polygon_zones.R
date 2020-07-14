###################################################################################
#### Script to extract the values from multiband rasters (Tif, Tiff and BSQ) based on vector zones 
#### Summarize the extracted pixels to give one value per zone (Like "zonal statistics")
#### Recommnedations:
#### Have all the layers in the same CRS to avoid trouble
#### Check the parameter "small" in the definition of func extractThis in "functions_extract.R", related to extracting only if the polygon covers the center of the pixel or not.
#### Recopilado por Lorena Gonzalez __________________________________________####

#### (Set working directory to the location of this script and the functions file) <--- IMPORTANT
## Session > Set working directory > To source file Location

#### There are two options for the Inputs: 
####  > A.
####    + A CSV file list of the raster, vector, output location and selected statistics. Headers:
####      * raster, vector_zone, output_Folder, zone_ID_field, if_buffer_size, band_name (only working for 1 band: for more, see the "Functions_extract.R" file for atomatic naming)
####
####  > B.
####    + A folder that contains all rasters to process
####    + The vector to use as extraction zones
####  > C.
####    + raster
####    + vector
####
####    - Indicate:
####    - the selected statistics
####    - ID Field name
####    - And outputs location
####
#### For  the outputs:
####  + CSV file per raster with all bands plot-values

## !!!!! IMPORTANT: To first run check this settings !!!! <<< ------
# {
#### Get functions and libraries
## First set the working directory to the location of this script 
#setwd("C:\\")
## Youcan go in the menu: Session -> Set Working Directory -> To Source File Location
source("functions_extract.R") #Check that the function file is in the same Working directory
# It needs this packages to be installed
#require(raster)
#require(velox)

#### summarize using the selected function ("na" stands for: remove NA values)
# See the functions_extract.R
#func <- median_na ## Summarize by median, remove NA values
func <- mean_na ## Summarize by mean, remove NA values
#func <- sd_na ## Summarize by standard deviation, remove NA values
#func <- Q1_na ## Summarize by first quartile, remove NA values
#func <- Q3_na ## Summarize by third quartile, remove NA values

####Field in the shp table to use as identifier of the extracting features
#ID_field <- "cluster"
#ID_field <- "Name"
ID_field <- "plot"

#### Default folder location
defaultP = "F:\\Dropbox\\RS_SIP\\COMPASSOficial"
# }

#### Start of script, will display the menu of options
tryCatch({ ## Put it all inside a handle error function
  
    ## Ask user What "input mode" to use
    in_mode <- menu(c("CSV list of inputs", "Raster Folder and vector file","Raster File and Vector File"), title="How do you want to give the inputs?",graphics = TRUE);
    
    ##Proceed to get input as preferred
    if (in_mode == 1){ #### CSV list of inputs ####
      #Select the CSV file with the list of inputs
      inputList <- askCSV(defaultP) # read list from file
      ####EXTRACT####
      # run for every item in te list
      for (i in 1:nrow(inputList)) {
        print(paste("Processing list... ",inputList[i,1]))
        #extractThis(inputList[i,1],shapefile(inputList[i,2]), inputList[i,3], inputList[i,4],func,as.double(inputList[i,5]), band_names = c("cluster",inputList[i,6])) ## r_file,zones, outFolder, ID_field, func, buf, band_names
        extractThis(inputList[i,1],shapefile(inputList[i,2]), inputList[i,3], inputList[i,4],func,as.double(inputList[i,5])) ## r_file,zones, outFolder, ID_field, func, buf
      }
    } else if (in_mode == 2 || in_mode == 3){ ## Enter inputs
      #### Ask user where to put the output tables
      #outFolder <- ("C:\\temp\\r") # Set path fix
      outFolder <- choose.dir(default = defaultP, caption = "Select folder to save output tables")
      print(paste("outFolder is:",outFolder)) #Say which is the output folder
      ##Ask for the vector with the zones
      zones <- askSHP(defaultP)
      ## Ask if Buffer is necessary
      buf <- 0 ## The default is no buffer
      if_buf <- menu(c("No", "Yes"), title="Do you want to buffer the input vectors to extract? (Hint: Yes for points)",graphics = TRUE)
      if  (if_buf == 2){
        #Ask for the buffer size
        ##TODO: Add filters to deal with entering other than numbers for the buffer
        buf <- as.double(readline(prompt = "ACTION: Type the buffer size in meters (+Positive to grow, -negative to shrink the feature)"));
      }

      if (in_mode == 2){ #### Process all rasters in a folder ####
        ## Ask user for Raster folder
        rFolder <- choose.dir(default = defaultP, caption = "Select folder that contains the rasters to extract")
        #list the rasters inside the folder. Return full path name
        r_list <- list.files(path = rFolder, full.names= TRUE, pattern = "\\.tif$|\\.bsq$|\\.tiff$|\\.dat$") # Select Tif or BSQ for example
        ## Extract each raster
        for (r_file in r_list) {
          ####EXTRACT#### 
          print(paste("Processing raster in folder... ",r_file))
          extractThis(r_file,zones, outFolder,ID_field,func,buf)
        }
      }
      if (in_mode == 3){ #### Ask for Raster file ####
        ####EXTRACT####
        r_file <- askRaster(defaultP) #Get th raster name
        ## Run extraction and saves output, indicate statistic
        extractThis(r_file,zones, outFolder,ID_field,func,buf)
      }
    }
    print("Finish Script")
  },
  #In case of halting error:
  error = function(e){print(c("An error ocurred: ",e$message))}#,
  # In case of warnings:
  #warning = function(e){print(paste("Hay advertencias: ", e$message))}
)