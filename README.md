# extractValuesFromMultibandRaster
R script to extract the values from multiband rasters (Tif, Tiff and BSQ) based on vector zones. It can use all the rasters in a folder, use a list of inputs or just work 1-1 raster-vector.
###################################################################################
#### Summarize the extracted pixels to give one value per zone
#### Recopilado por Lorena Gonzalez,  Septiembre 2019                          ####
####___________________________________________________________________________####
#### There are three options for the Inputs: 
####  > A.
####    + A CSV file list of the raster, vector, output location and selected statistics. Headers:
####      * raster, vector_zone, output_Folder, zone_ID_field, if_buffer_size
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
