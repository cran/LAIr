#' @name NDVI2LAI
#' @title Derive LAI from NDVI using a set of conversion functions
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#' @importFrom terra values rast
#' @importFrom purrr map
#' @importFrom utils globalVariables
#' @description
#' The function calculates LAI from NDVI values given as a raster or a numeric vector input.
#' The conversion uses the formulas compiled by  Bajocco et al. 2022 \doi{10.3390/rs14153554}.
#' The choice of the equation depends on arguments related to vegetation (category, type, name), or sensor (name, platform, resolution).
#' If no filtering arguments are provided, the function calculate all 199 equations.
#' The function returns a Raster* or a dataframe depending on the input, with the LAI values computed from the available selected conversion equations.
#' @param input Raster* or numeric vector. For multi-layer Raster images, the computation is performed for each layer.
#' @param ID Character. Optional parameter to select the function based on its code. For available options, type 'NDVI2LAIeq' (field 'F.ID')
#' @param category Character. Optional parameter to select the Plant Category among: "Crop" "Forest" "Mixed".
#' @param type Character. Optional parameter to select the Plant Type. For available options, type 'NDVI2LAIeq' (field 'Plant.Type')
#' @param name Character. Optional parameter to select the Plant Name. For available options, type 'NDVI2LAIeq' (field 'Plant.Name')
#' @param sensor  Character. Optional parameter to select the Sensor Name. For available options, type 'NDVI2LAIeq' (field 'Sensor.Name')
#' @param platform Character. Optional parameter to select the Sensor Platform among: "Satellite" "Field" "Airborne".
#' @param resolution Character. Optional parameter to select the Sensor Resolution Class. For available options, type 'NDVI2LAIeq' (field 'Sensor.ResolutionClass')
#'
#' @return A Raster* or a dataframe depending on the input.
#' @examples
#' # generate a raster
#' set.seed(123)
#' input <- terra::rast(ncol = 9, nrows = 9, vals = runif(81))
#' res <- NDVI2LAI(input, category = 'Crop', name = c('Vineyard', 'Barley'), sensor = 'MODIS')
#' terra::plot(res)
#'
#' #using a vector
#' NDVI2LAI(seq(0.5,1,.2), category = 'Mixed', sensor = c('PROBA-V', 'SPOT'))
#'
#' @seealso Bajocco et al. (2022). On the Use of NDVI to Estimate LAI in Field Crops: Implementing a Conversion Equation Library. Remote Sens. , 3554, doi: \doi{10.3390/rs14153554}\cr
#' @usage NDVI2LAI(input,
#'  ID=NULL,
#'  category=NULL,
#'  type=NULL,
#'  name=NULL,
#'  sensor=NULL,
#'  platform=NULL,
#'  resolution=NULL)
#' @export

utils::globalVariables(c("F.ID","Sensor.ResolutionClass", "Equation.R2","NDVI2LAIeq", "Plant.Category", "Plant.Name", "Plant.Type", "Sensor.Name", "Sensor.Platform", "str_pad"))

NDVI2LAI<-function(input,
                   ID=NULL,
                   category=NULL,
                   type=NULL,
                   name=NULL,
                   sensor=NULL,
                   platform=NULL,
                   resolution=NULL){


  #Shortcuts for math operators:
  Math.Log10=function(x) {return(log10(x))}
  Math.Log=function(x) {return(log(x))}
  Math.Exp=function(x) {return(exp(x))}
  Math.Pow=function(x,a) {return(x^a)}
  Math.Sqrt=function(x) {return(x^0.5)}

  #Check if the input file is Raster* or numeric vector:
  if(!stringr::str_detect(class(input),'Raster|numeric')){
    stop("Wrong input. Allowed classes are Raster*, numeric vector.")
  }


  #evaluate filtering arguments;
  #filtering on F.ID
  if(is.null(ID)){
    flt1<-NDVI2LAIeq
  }
  else {
    if(length(setdiff(F.ID,unique(NDVI2LAIeq$F.ID)))>0){
      stop(paste0('Wrong input Function ID.\n It varies from ', (paste0('F',str_pad(1,3,pad='0'))), ' to ', paste0('F',str_pad(nrow(NDVI2LAIeq),3,pad='0'))) )
    }
    flt1<-NDVI2LAIeq |>
      dplyr::filter(F.ID  %in%  ID)
  }

  #second filtering:
  if(is.null(category)){
    flt2<-flt1
  }
  else {
    if(length(setdiff(category,unique(NDVI2LAIeq$Plant.Category)))>0){
      stop(paste0('Wrong input Plant Category. Available options are: ',cat(sort(unique(NDVI2LAIeq$Plant.Category)),sep='\n')))
    }
    flt2 <- flt1 |>
      dplyr::filter(Plant.Category %in% category)
  }

  #third filtering:
  if(is.null(type)){
    flt3<-flt2
  }
  else {
    if(length(setdiff(type,unique(NDVI2LAIeq$Plant.Type)))>0){
      stop(paste0('Wrong input Plant Type. Available options are: ',cat(sort(unique(NDVI2LAIeq$Plant.Type)),sep='\n')))
    }
    flt3 <- flt2 |>
      dplyr::filter(Plant.Type %in% type)
  }

  #fourth filtering:
  if(is.null(name)){
    flt4<-flt3
  }
  else {
    if(length(setdiff(name,unique(NDVI2LAIeq$Plant.Name)))>0){
      stop(paste0('Wrong input Plant Name. Available options are: ',cat(sort(unique(NDVI2LAIeq$Plant.Name)),sep='\n')))
    }
    flt4 <- flt3 |>
      dplyr::filter(Plant.Name %in% name)
  }

  #fifth filtering:
  if(is.null(sensor)){
    flt5<-flt4
  }
  else {
    if(length(setdiff(sensor,unique(NDVI2LAIeq$Sensor.Name)))>0){
      stop(paste0('Wrong input Sensor Name. Available options are: ',cat(sort(unique(NDVI2LAIeq$Sensor.Name)),sep='\n')))
    }
    flt5 <- flt4 |>
      dplyr::filter(Sensor.Name %in% sensor)
  }

  #sixth filtering:
  if(is.null(platform)){
    flt6<-flt5
  }
  else {
    if(length(setdiff(platform,unique(NDVI2LAIeq$Sensor.Platform)))>0){
      stop(paste0('Wrong input Platform Name. Available options are: ',cat(sort(unique(NDVI2LAIeq$Sensor.Platform)),sep='\n')))
    }
    flt6 <- flt5 |>
      dplyr::filter(Sensor.Platform %in% platform)
  }

  #seventh filtering:
  if(is.null(resolution)){
    flt7<-flt6
  }
  else {
    if(length(setdiff(resolution,unique(NDVI2LAIeq$Sensor.ResolutionClass)))>0){
      stop(paste0('Wrong input Resolution Available options are: ',cat(sort(unique(NDVI2LAIeq$Sensor.ResolutionClass)),sep='\n')))
    }
    flt7 <- flt6 |>
      dplyr::filter(Sensor.ResolutionClass %in% resolution)
  }


 #Final flt table is sequentially created from the previous arguments:
  flt <- flt7

#If filtering does not select nothing:
  if(nrow(flt)==0){
    stop('The selection did not find available equations.\n Try to refine your query.\n Type "NDVI2LAIeq" to see the available options and equations.')
  }


#Based on the input, it get different output:
  #case Raster*
  if(stringr::str_detect(class(input),'RasterLayer|RasterBrick|RasterStack')){
    input <- terra::rast(input)
  }
  if(stringr::str_detect(class(input),'Raster')){
    fnl.rst=input
    # names(fnl.rst) <- "input"
    for (i in 1: nrow(flt)){
      if(abs(max(terra::values(input)))>1){
        warning('values exceed the range of NDVI [-1,1]. These values will be omitted in the calculation.')
      }
      out.rst <- input
      # names(fnl.rst) <- "input"
      out<-purrr::map(terra::values(input),flt$Function.call[i])
      terra::values(out.rst)<-as.numeric(out)
      names(out.rst)<-paste0(names(out.rst),'_',gsub('NDVItoLAI_','',flt$Function.ID[i]))
      fnl.rst<-terra::rast(list(fnl.rst, out.rst))
    }
  }

  #case vector
  if(stringr::str_detect(class(input),'numeric')){
    fnl.rst = data.frame(input=input)
    for (i in 1: nrow(flt)){
      if(abs(max(input))>1){
        warning('values exceed the range of NDVI [-1,1]. They will be omitted in the calculation.')
      }
      out <- purrr::map(input,flt$Function.call[i])
      outdf <- data.frame(out=as.numeric(out))
      names(outdf) <- gsub('NDVItoLAI_','',flt$Function.ID[i])
      fnl.rst=cbind(fnl.rst,outdf)
    }
  }
  return(fnl.rst)
}

