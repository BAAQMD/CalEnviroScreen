#' @title CalEnviroScreen 2.0 data
#' 
#' @description A dataset containing tract-level indicators used as inputs for CalEnviroScreen 2.0. 
#' The variables are as follows:
#' 
#' \itemize{
#'   \item FIPS. The FIPS code for the census tract.
#'   \item Age. Percent of population under age 10 or over age 65.
#'   \item Asthma. Spatially modeled, age-adjusted rate of emergency department (ED) visits for asthma per 10,000 (averaged over 2007-2009).  
#'   \item LBW. Percent low birth weight (< 2500 g), spatially modeled (averaged over 2006-2009).  
#'   \item Edu. Percent of the population over age 25 with less than a high school education (5-year estimate, 2008-2012).  
#'   \item LingIso. Percentage of households in which no one age 14 and over speaks English "very well" or speaks English only.  
#'   \item Poverty. Percent of the population living below two times the federal poverty level (5-year estimate, 2008-2012).  
#'   \item Unemp. Percent of the population over age 16 that is unemployed and eligible for the labor force (5-year estimate, 2008-2012).  
#'   \item Ozone. Portion of the daily maximum 8-hour ozone concentration over the California 8-hour standard (0.070 ppm), averaged over three years (2009 to 2011).
#'   \item PM25. Annual mean concentration of PM2.5 (average of quarterly means), over three years (2009-2011).
#'   \item DieselPM. Spatial distribution of gridded diesel PM emissions from on-road and non-road sources for a 2010 summer day in July (kg/day).  
#'   \item DrinkWat. Toxicity-weighted drinking water quality index for selected contaminants.
#'   \item PestUse. Total pounds of selected active pesticide ingredients (filtered for hazard and volatility) used in production-agriculture per square mile.
#'   \item ToxRel. Toxicity-weighted concentrations of modeled chemical releases to air from facility emissions and off-site incineration.
#'   \item Traffic. Sum of traffic volumes adjusted by road segment length (vehicle-kilometers per hour) divided by total road length (kilometers) within 150 meters of the census tract boundary.
#'   \item Cleanup. Sum of weighted cleanup sites within each census tract.  
#'   \item GndWat. Sum of weighted GeoTracker scores for sites within each census tract.  
#'   \item HazWaste. Sum of weighted permitted hazardous waste facilities and hazardous waste generators within each census tract.  
#'   \item ImpWat. Summed number of pollutants across all water bodies designated as impaired within the area.  
#'   \item SolWaste. Sum of weighted solid waste sites and facilities.  
#' }
#' 
#' @docType data
#' @name CES2_data
#' @usage data(CES2); CES2_data
#' @format A data frame with 8035 rows and 20 variables
#' @source \url{http://oehha.ca.gov/ej/ces2.html}
#' @keywords datasets
NULL

#' @title CalEnviroScreen 2.0 data, after conversion to percentiles
#' 
#' @description A dataset containing tract-level indicators used as inputs for CalEnviroScreen 2.0,
#' after conversion to percentiles. Used for QA/QC. See ?pctls for a proper function.
#'  
#' The variables are as follows:
#' 
#' \itemize{
#'   \item FIPS. The FIPS code for the census tract.
#'   \item Age. Percent of population under age 10 or over age 65.
#'   \item Asthma. Spatially modeled, age-adjusted rate of emergency department (ED) visits for asthma per 10,000 (averaged over 2007-2009).  
#'   \item LBW. Percent low birth weight (< 2500 g), spatially modeled (averaged over 2006-2009).  
#'   \item Edu. Percent of the population over age 25 with less than a high school education (5-year estimate, 2008-2012).  
#'   \item LingIso. Percentage of households in which no one age 14 and over speaks English "very well" or speaks English only.  
#'   \item Poverty. Percent of the population living below two times the federal poverty level (5-year estimate, 2008-2012).  
#'   \item Unemp. Percent of the population over age 16 that is unemployed and eligible for the labor force (5-year estimate, 2008-2012).  
#'   \item Ozone. Portion of the daily maximum 8-hour ozone concentration over the California 8-hour standard (0.070 ppm), averaged over three years (2009 to 2011).
#'   \item PM25. Annual mean concentration of PM2.5 (average of quarterly means), over three years (2009-2011).
#'   \item DieselPM. Spatial distribution of gridded diesel PM emissions from on-road and non-road sources for a 2010 summer day in July (kg/day).  
#'   \item DrinkWat. Toxicity-weighted drinking water quality index for selected contaminants.
#'   \item PestUse. Total pounds of selected active pesticide ingredients (filtered for hazard and volatility) used in production-agriculture per square mile.
#'   \item ToxRel. Toxicity-weighted concentrations of modeled chemical releases to air from facility emissions and off-site incineration.
#'   \item Traffic. Sum of traffic volumes adjusted by road segment length (vehicle-kilometers per hour) divided by total road length (kilometers) within 150 meters of the census tract boundary.
#'   \item Cleanup. Sum of weighted cleanup sites within each census tract.  
#'   \item GndWat. Sum of weighted GeoTracker scores for sites within each census tract.  
#'   \item HazWaste. Sum of weighted permitted hazardous waste facilities and hazardous waste generators within each census tract.  
#'   \item ImpWat. Summed number of pollutants across all water bodies designated as impaired within the area.  
#'   \item SolWaste. Sum of weighted solid waste sites and facilities.  
#' }
#' 
#' @docType data
#' @name CES2_pctls
#' @usage data(CES2); CES2_pctls
#' @format A data frame with 8035 rows and 20 variables
#' @source \url{http://oehha.ca.gov/ej/ces2.html}
#' @keywords datasets
NULL

#' @title CalEnviroScreen 2.0 scores 
#' @description CalEnviroScreen 2.0 scores (as published)
#' @docType data
#' @name CES2_scores
#' @usage data(CES2); CES2_scores
#' @format A data.frame with 8035 rows and 5 columns
#' 
#' @source \url{http://oehha.ca.gov/ej/ces2.html}
NULL

#' @name CES2_POLLUTION_VARS
#' @title Variables associated with pollution burden
#' @description Names of variables used to calculate pollution burden score
#' @docType data
#' @usage data(CES2_metadata); CES2_POLLUTION_VARS
#' @format named character vector, with names corresponding to column names in the published Excel file
NULL

#' @name CES2_POPCHAR_VARS
#' @title Variables associated with population characteristics
#' @description Names of variables used to calculate population characteristics score
#' @docType data
#' @usage data(CES2_metadata); CES2_POPCHAR_VARS
#' @format named character vector, with names corresponding to column names in the published Excel file
NULL

#' @name CES2_PERCENTILE_BREAKS
#' @title Percentile breaks (inferred from published CES 2.0 data)
#' @description Percentile breaks (inferred from published CES 2.0 data)
#' @docType data
#' @usage data(CES2_metadata); CES2_PERCENTILE_BREAKS
#' @format named numeric vector
NULL

#' @name CES2_WEIGHTS
#' @title Relative weights used to compute CES 2.0 scores
#' @description Relative weights used to compute CES 2.0 scores
#' @docType data
#' @usage data(CES2_metadata); CES2_WEIGHTS
#' @format named numeric vector
NULL

#' @name CA_tracts
#' @title California census tracts
#' @description California census tracts, with areas computed from an Albers projection
#' @docType data
#' @usage CA_tracts
#' @format A SpatialPolygonsDataFrame
#' @source \url{http://oehha.ca.gov/ej/ces2.html}
NULL

#' @name CA_regions
#' @title Geographical boundaries and tract identifiers for major CA regions
#' @description Based on data provided by the Bay Area AQMD
#' @docType data
#' @usage tract_regions
#' @format A list with four items: "San Joaquin", "South Coast", "Bay Area", and "Other". 
#'   Each element is a list with two items:
#'   \itemize{
#'     \item FIPS. A character vector of FIPS codes for tracts belonging to the region.
#'     \item boundary. A SpatialPolygons object, created by merging tract geometries.
#'   }
NULL