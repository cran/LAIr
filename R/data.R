#' Dataset of empirical functions to convert NDVI to LAI in crops, forests and mixed vegetation.
#' @description Dataset of empirical functions to convert NDVI to LAI, compiled by [Bajocco et al. 2022](https://www.mdpi.com/2072-4292/14/15/3554).
#' It contains some arguments used by the [NDVI2LAI()] function to subset the conversion equations.
#' @format
#' A data frame with 199 rows and 19 columns:
#' \describe{
#'    \item{Function.ID}{Equation name.}
#'    \item{F.ID}{Function ID code varying between F001 and F199.}
#'    \item{Plant.Category}{Plant categories: "Crop" "Forest" "Mixed".}
#'    \item{Plant.Type}{Plant types: "Permanent" "Summer" "Winter" "Forest" "Perennial" "Mixed".}
#'    \item{Plant.Name}{Plant names: "Vineyard" "Maize" "Wheat" "Barley" "Forest" "Poplar" "Soybean" "Rice" "Sunflower" "Sugarcane" "Pasture" "Eucalypt" "Mixed".}
#'    \item{Location.Biome.Code}{The Biome code of the experimental location, following Olson & Dinerstein (1998) \doi{10.1046/j.1523-1739.1998.012003502.x}}
#'    \item{Location.Biome}{The Biome name of the experimental location: "Mediterranean Forests, Woodlands & Scrub" "Temperate Grasslands, Savannas & Shrublands"
#'                                           "Temperate Broadleaf & Mixed Forests""Deserts & Xeric Shrublands" "Tropical & Subtropical Dry Broadleaf Forests"
#'                                           "Montane Grasslands & Shrublands" "Tropical & Subtropical Grasslands, Savannas & Shrublands" "Tropical & Subtropical Moist Broadleaf Forests".}
#'    \item{Location.Country}{Country.}
#'    \item{Sensor.Name}{Sensor Names: "IKONOS" "MODIS" "Spectroradiometer" "Landsat" "AVHRR" "SPOT" "UAV" "Hyperion" "RapidEye" "Sentinel-2" "MultispectralCamera"
#'                                    "Quickbird" "WorldView2" "WorldView3" "Pleiades-1A" "GeoEye1" "AISA" "GF1" "HJ" "PROBA-V" "SPOT-VGT" "AWiFS" "BJ1"}
#'    \item{Sensor.Platform}{Sensor Platform: "Satellite" "Field" "Airborne".}
#'    \item{Sensor.Resolution}{Spatial resolution of the sensor.}
#'    \item{Sensor.ResolutionClass}{Sensor spatial resolution class: "High" "Low" "Very-High" "Moderate".}
#'    \item{Sensor.PixelSize}{Pixel size.}
#'    \item{Equation.R2}{Coefficient of determination of the equation (when available).}
#'    \item{Equation.Codomain}{The set of the function's possible outputs.}
#'    \item{Equation.Domain}{The set of inputs accepted by the function.}
#'    \item{Equation.MathematicalForm}{Mathematical form of the function: "Linear" "Logarithmic" "Exponential" "Polynomial" "Power".}
#'    \item{Reference.DOI}{DOI of the publication.}
#'    \item{Function.call}{Conversion formula used by the [NDVI2LAI()]}
#'                                     }
#' @source \doi{10.3390/rs14153554}
"NDVI2LAIeq"
