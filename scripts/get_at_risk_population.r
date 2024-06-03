## R script to calculate parity and determine the cow population at risk
## to be used for epidemiological claculations
## input: .RData objects with cows, visits and milk records
# run as Rscript --vanilla get_at_risk_population.r <config_file>

###################################
## read arguments from config file
###################################
# INPUT CONFIGURATION MANAGEMENT ------------------------------------------
args = commandArgs(trailingOnly=TRUE)
if (length(args) == 1) {
  #loading the parameters
  source(args[1])
} else {
  #this is the default configuration, used for development and debug
  writeLines('Using default config')
  
  #this dataframe should be always present in config files, and declared
  #as follows
  config = NULL
  config = rbind(config, data.frame(
    base_folder = '/home/filippo/Documents/moroni/podologia',
    data_folder = "data",
    cow_data = 'extracted_bovine.RData',
    vet_data = 'extracted_visite.RData',
    milk_data = 'extracted_status_bovine.RData',
    year = 2022,
    force_overwrite = FALSE
  ))
}

# FUNCTIONS -------------------------------------------------------------------
## convert date from timestamp to date (yyyy-mm-dd)
convert_date = function(x) {
  x = as.numeric(x)
  fecha = as.Date(as.POSIXct(x, origin="1970-01-01"))
  return(fecha)
}

date_to_timestamp = function(x) {
  tmstmp = as.numeric(as.POSIXct(x))
  return(tmstmp)
}

## extract cows for a specific year
extract_non_visited_cows = function(year, df) {
  
  begin_date = paste(year, "01", "01", sep="-")
  end_date = paste(year, "12", "31", sep="-")
  
  start = as.numeric(as.POSIXct(as.Date(begin_date), format="%a %b %d %H:%M:%S %Y"))
  end = as.numeric(as.POSIXct(as.Date(end_date), format="%a %b %d %H:%M:%S %Y"))
  temp = filter(df, (data_ingresso < end & data_riforma > start) | (data_ingresso < end & is.na(data_riforma)))
  
  print(paste("n. of cows present in the herd in the year", year, "(at least one day) is:", nrow(temp)))
  return(temp)
} 

extract_parity = function(year, df, cow) {
  
  temp <- filter(df, id_bovina == cow)
  begin_date = paste(year, "01", "01", sep="-")
  end_date = paste(year, "12", "31", sep="-")
  
  start = as.numeric(as.POSIXct(as.Date(begin_date), format="%a %b %d %H:%M:%S %Y"))
  end = as.numeric(as.POSIXct(as.Date(end_date), format="%a %b %d %H:%M:%S %Y"))
  temp = filter(temp, (data_ultimo_controllo <= end & data_ultimo_controllo >= start) )
  if(nrow(temp) > 0) {
    
    temp$testday <- as.Date(as.POSIXct(temp$data_ultimo_controllo, origin="1970-01-01"))
    temp <- filter(temp, data_ultimo_controllo == max(data_ultimo_controllo))
    parity = temp$numero_parti
    calving_date = temp$data_ultimo_parto
  } else {parity = 0; calving_date = NA}
  
  # print(paste("parity of cow", cow, "in year", year, "is", parity))
  return(c(parity, calving_date))
} 


# SETUP -------------------------------------------------------------------
library("knitr")
library("scales") 
library("ggplot2")
library("lubridate")
library("tidyverse")
library("data.table")

print(paste("data folder:",config$data_folder))
print(paste("input cow data:",config$cow_data))
print(paste("input vet visit data:",config$vet_data))
print(paste("input milk record data:",config$milk_data))
print(paste("selected YEAR:",config$year))

# LOAD DATA -------------------------------------------------------------------
## JSONLITE
writeLines(" - loading RData objects")

## extracting cow data
fname = file.path(config$base_folder, config$data_folder, config$cow_data)
load(fname)
print(paste("N. of animals loaded:", nrow(df_bovine)))

## extracting vet visit data
fname = file.path(config$base_folder, config$data_folder, config$vet_data)
load(fname)
print(paste("N. of vet visits loaded:", nrow(df_bovine)))

## extracting cow status data
fname = file.path(config$base_folder, config$data_folder, config$milk_data)
load(fname)
print(paste("N. of milk records loaded:", nrow(df_status_bovine)))

# CHECK DATA -------------------------------------------------------------------
## get time range (min and max year of animals present in the herd)
writeLines(" - basic check on cow enter/exit dates")

cows = unique(df_bovine$matricola)
temp <- convert_date(df_bovine$data_ingresso)
miny = min(format(temp, "%Y"))
temp <- convert_date(df_bovine$data_riforma)
maxy = max(format(temp, "%Y"), na.rm = TRUE)

print(paste("The first animals in the herd entered in year", miny, "and exited in year", maxy))

## extract cow data
writeLines(" - extract data from selected year")
res = extract_non_visited_cows(config$year, df_bovine)

res$data_ingresso = convert_date(res$data_ingresso)
res$data_riforma = convert_date(res$data_riforma)

print(paste("For reference year", config$year, nrow(res), "cows were extracted (including heifers that did not have any calves yet"))

extract_parity(config$year, df_status_bovine, res$id[1])[2]

## calculating parity
writeLines(" - calculating parity for each cow")
res <- res |>
  rowwise() |>
  mutate(parity = extract_parity(config$year, df_status_bovine, id)[1],
         calving_date = extract_parity(config$year, df_status_bovine, id)[2],
         birth_year = year(as.Date(as.POSIXct(data_nascita, origin="1970-01-01"))))

print("distribution of parity")
print(table(res$parity))

print(paste("N. of cows with parity > 0:", sum(res$parity > 0)))

writeLines(" - removing records where parity is 0")
res <- filter(res, parity > 0)

## convert back from date to timestamp
res$data_ingresso = date_to_timestamp(res$data_ingresso)
res$data_riforma = date_to_timestamp(res$data_riforma)

print(paste("N. of cows parity > 0 saved to file:", nrow(res)))

fname = paste("cow_filtered_", config$year, ".RData", sep="")
fname = file.path(config$base_folder, config$data_folder, fname)

writeLines(" - saving R data object with filtered cow data")
save(x = res, file = fname)

print("DONE!")

