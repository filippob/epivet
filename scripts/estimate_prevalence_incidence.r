## R script to estimate prevalence
## input: .RData objects with filtered cows (from get_at_risk_population.r) and visits
# run as Rscript --vanilla estimate_prevalence.r <config_file>

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
    vet_data = 'extracted_visite.RData',
    year = 2023,
    target_date = "09-01", ## format mm-dd
    interval = 10, ##n. of days around target date
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

#################################################################

library("epiR")
library("scales") 
library("ggplot2")
library("lubridate")
library("tidyverse")
library("data.table")

## 1. LOAD DATA
writeLines(" - loading data")
## extracting vet visit data
fname = file.path(config$base_folder, config$data_folder, config$vet_data)
print(paste("load global visit data from", fname))
load(fname)

## extracting cow data
print(paste("selected year is:", config$year))
fname = paste("cow_filtered_", config$year, ".RData", sep="")
fname = file.path(config$base_folder, config$data_folder, fname)
print(paste("loading fitlered cow data from", fname))
load(fname)

## all cows entered the herd before the end of 2019
if(sum(data.table::year(as.POSIXlt(as.numeric(res$data_ingresso))) > config$year) != 0) {
  
  stop("Error: animals not belonging to the selected year range have been found")
}

## all cows were reformed after the beginning of 2019 (or not yet reformed: missing culling date)
if(sum(year(as.POSIXlt(as.numeric(res$data_riforma))) < config$year, na.rm=TRUE) != 0) {
  
  stop("Error: animals not belonging to the selected year range have been found")
}

## 2. SELECT VISITS
writeLines(" - subsetting visits belonging to the selected year range")

subset <- df_visite |>
  mutate(data_visita = convert_date(data_effettuata), 
         year_visit = format(data_visita, "%Y"), month_visit = format(data_visita, "%m")) |>
  select(-c(locomotion_score_vet, locomotion_score_all, bcs)) |>
  filter(year_visit == config$year) |>
  rename(id_rec = id)

print(paste("N. of visit records selected for year", config$year, ":", nrow(subset)))

## combine visits and cow data
res <- res |>
  left_join(subset, by = c("id" = "id_bovina")) |>
  mutate(esito = ifelse(is.na(esito), "Sana", esito))

print(paste("There are", nrow(res), "records from", length(unique(res$matricola)),"cows in year", config$year))
  
print(paste(nrow(subset), "vet visit records (feet disorders)"))
print(paste(nrow(filter(res, is.na(data_visita))), "records from non-visited cows:
            cows that were not visited for feet disorders but that were present in
            the herd and at risk (adult cows with at least one calving)"))

#if(!(nrow(res)-nrow(subset) == nrow(filter(res, is.na(data_visita))))) {
  
#  stop("Error: n. of visited cows and non-visited cows do not add up")
#}

####################
## POINT PREVALENCE
####################
print("##----------------------")
print("estimating: POINT PREVALENCE")
print("##----------------------")
## 3. date and interval
## we use the interval to define a date and subset the dataset accordingly
## in this way, all visits in the interval (e.g. +/- 10 days around the chosen dates) are considered in the calculations

writeLines(" - pick date and interval for prevalence estimation")
data = paste(config$year, config$target_date, sep="-")
data = as.Date(data)
interval_days = config$interval
dtx = as.POSIXct(data, format="%a %b %d %H:%M:%S %Y")
window = interval_days*24*60*60
dtx <- as.numeric(dtx)

results = data.frame("type"=NULL, "denominator_ncows"=NULL, "denominator_cow_years"=NULL, "numerator"=NULL,
                 "estimate"=NULL)

## 4. denominator
## - calculate the time present in the herd as the difference, in number of days, 
##    between the arrival in the herd and the culling date
## - if the cow was already in the herd at the beginning of the period considered, 
##    or still present at the end of it, then the entire solar year was used

writeLines(" - calculate the denominator")
start_interval = dtx - (interval_days-1)*(60*60*24)
end_interval = dtx + interval_days*(60*60*24)

temp <- res |>
  filter(calving_date < end_interval) |>
  filter((data_ingresso < end_interval & data_riforma > start_interval) | (data_ingresso < end_interval & is.na(data_riforma)))

df0 <- temp %>%
  select(data_effettuata,data_riforma,data_ingresso,stato_epid,stato_epid_rev,esito,matricola,parity,calving_date) |>
  group_by(matricola) |>
  mutate(data_riforma = as.numeric(data_riforma),
         data_ingresso = as.numeric(data_ingresso),
         data_parto = ifelse(parity == 1, calving_date, data_ingresso), ## PER CALCOLARE IL TEMPO A RISCHIO DALLA DATA DI PARTO SE PRIMIPARA
         # start = max(data_ingresso, data_parto, as.POSIXct(as.Date("2020-01-01"), format="%a %b %d %H:%M:%S %Y")),
         start = max(data_ingresso, data_parto, start_interval),
         end = ifelse(is.na(data_riforma), end_interval, 
                      min(data_riforma, end_interval)
         ),
         start_date = convert_date(start),
         end_date = convert_date(end),
         time_present_days = ((end - start)+(24*60*60))/(24*60*60) ##add one day to count correctly the n. of days ((2-1)+1) = 2 days (day 1 and day 2)
  ) |>
  arrange(matricola, data_effettuata)

## calculate denominator only with cows actually present at the chosen date to calculate prevalence
denominator = df0 |>
  filter(data_ingresso <= dtx, data_riforma > dtx) |>
  group_by(matricola) |>
  summarise(N= n(), time = mean(time_present_days)) |>
  summarise(n_cows = length(unique(matricola)), tot = sum(time), `cow-years` = tot/365)

print(denominator) ## tot is time in days

print(paste("The calculated denominator is: i)", denominator$n_cows, "cows, or: ii)", denominator$`cow-years`, "cow-years"))

rtemp <- data.frame("type" = "point_prevalence", "denominator_ncows" = denominator$n_cows, "denominator_cow_years" = denominator$`cow-years`)

## 5. calculate the numerator
## subset records around the chose date
## then take the column `esito` to count the cases (this variable is either `Sana` or `Malata`)
numerator <- df0 |>
  filter(data_effettuata < dtx+window, data_effettuata > dtx-window, data_ingresso < dtx, data_riforma > dtx) |>
  ungroup() |>
  summarise(cases = sum(esito != "Sana"))

print(paste("The numerator is", numerator$cases))
rtemp$numerator = numerator$cases

## 5. point prevalence
## ratio between n. of cases at the chosen data and number of cows at chosen date
## numerator is numerator$cases in the interval considered
## denominator is denominator$n_cows present in the considered interval

point_prevalence = round((numerator$cases/denominator$n_cows),3)
print(paste("Point prevalence is", point_prevalence))

rtemp$estimate = point_prevalence

## 6. confidence interval
ncas <- numerator$cases
npop <- denominator$n_cows
tmp <- as.matrix(cbind(ncas, npop))

print("95% confidence interval")
print(epi.conf(tmp, ctype = "prevalence", method = "exact", design = 1, 
         conf.level = 0.95))

results = rbind.data.frame(results, rtemp)

####################
## 1-YEAR PREVALENCE
####################
print("##----------------------")
print("estimating: 1-YEAR PREVALENCE")
print("##----------------------")

fecha = paste(config$year, "01", "01", sep="-")
start_interval = as.POSIXct(as.Date(fecha), format="%a %b %d %H:%M:%S %Y") 
fecha = paste(config$year, "12", "31", sep="-")
end_interval = as.POSIXct(as.Date(fecha), format="%a %b %d %H:%M:%S %Y") 

## 2. calculate the numerator:
writeLines(" - calculate the denominator")
df0 <- res %>%
  select(data_effettuata,data_riforma,data_ingresso,stato_epid,stato_epid_rev,esito,matricola,parity,calving_date) |>
  group_by(matricola) |>
  mutate(data_riforma = as.numeric(data_riforma),
         data_ingresso = as.numeric(data_ingresso),
         data_parto = ifelse(parity == 1, calving_date, data_ingresso), ## PER CALCOLARE IL TEMPO A RISCHIO DALLA DATA DI PARTO SE PRIMIPARA
         # start = max(data_ingresso, data_parto, as.POSIXct(as.Date("2020-01-01"), format="%a %b %d %H:%M:%S %Y")),
         start = max(data_ingresso, data_parto, start_interval),
         end = ifelse(is.na(data_riforma), end_interval, 
                      min(data_riforma, end_interval)
         ),
         start_date = convert_date(start),
         end_date = convert_date(end),
         time_present_days = ((end - start)+(24*60*60))/(24*60*60) ##add one day to count correctly the n. of days ((2-1)+1) = 2 days (day 1 and day 2)
  ) |>
  arrange(matricola, data_effettuata)

denominator = df0 |>
  group_by(matricola) |>
  summarise(N= n(), time = mean(time_present_days)) |>
  summarise(n_cows = length(unique(matricola)), tot = sum(time), `cow-years` = tot/365)

print(denominator)

print(paste("The calculated denominator is: i)", denominator$n_cows, "cows, or: ii)", denominator$`cow-years`, "cow-years"))

rtemp <- data.frame("type" = "one_year_prevalence", "denominator_ncows" = denominator$n_cows, "denominator_cow_years" = denominator$`cow-years`)

## 2. calculate the numerator:
writeLines(" - calculate the numerator")

numerator <- df0 |>
  ungroup() |>
  summarise(cases = sum(esito != "Sana"))

print(paste("The numerator is", numerator$cases))
rtemp$numerator = numerator$cases

## 3. calculate 1-year prevalence
## ratio between n. of cases and number of cow-years in the selected year

print(paste("The numerator is:", numerator$cases, "cases in year", config$year))
print(paste("The denominator is:", denominator$`cow-years`, "cow-years in year", config$year))

year_prevalence = round((numerator$cases/denominator$`cow-years`),3)

print(paste("One-year prevalence is:", year_prevalence))

ncas <- numerator$cases
npop <- denominator$`cow-years`
tmp <- as.matrix(cbind(ncas, npop))
epi.conf(tmp, ctype = "prevalence", method = "exact", design = 1, 
         conf.level = 0.95)

print(epi.conf(tmp, ctype = "prevalence", method = "exact", design = 1, 
               conf.level = 0.95))

rtemp$estimate = year_prevalence
results = rbind.data.frame(results, rtemp)

########################
########################
## !! INCIDENCE !!
########################
########################

####################
## AVERAGE INCIDENCE
####################
print("##----------------------")
print("estimating: AVERAGE INCIDENCE")
print("##----------------------")

writeLines(" - calculating the denominator for incidence")
## calculate the denominator in the same way, based on the number of days each cow 
## stayed in the herd (the variable `id_bovina` is included here -- compared to the code above)
df0 <- res %>%
  select(data_effettuata,data_riforma,data_ingresso,stato_epid,stato_epid_rev,esito,matricola,bovina,parity,calving_date) |>
  unnest(bovina) |>
  group_by(matricola) |>
  mutate(data_riforma = as.numeric(data_riforma),
         data_ingresso = as.numeric(data_ingresso),
         data_parto = ifelse(parity == 1, calving_date, data_ingresso), ## PER CALCOLARE IL TEMPO A RISCHIO DALLA DATA DI PARTO SE PRIMIPARA
         start = max(data_ingresso, data_parto, as.POSIXct(as.Date("2020-01-01"), format="%a %b %d %H:%M:%S %Y")),
         end = ifelse(is.na(data_riforma), as.POSIXct(as.Date("2020-12-31"), format="%a %b %d %H:%M:%S %Y"), 
                      min(data_riforma, as.POSIXct(as.Date("2020-12-31"), format="%a %b %d %H:%M:%S %Y"))
         ),
         time_present_days = (end - start)/(24*60*60)
  ) |>
  arrange(matricola, data_effettuata)

nrecords = nrow(df0)
ncows = length(unique(df0$matricola))
cow_years = df0 |>
  group_by(matricola) |>
  summarise(days = mean(time_present_days)) |>
  summarise(cow_years = sum(days)/365) |>
  pull(cow_years)


print(paste("there are", nrecords, "records from", ncows, "cows which correspond 
-since not all cows remain in the herd for the entire year- to", cow_years, "cow-years (person-years). 
            This is the denominator."))

rtemp <- data.frame("type" = "avg_incidence_standard", "denominator_ncows" = ncows, "denominator_cow_years" = cow_years)

## 2. The numerator: standard classification (`stato_epid`)
new_cases = sum(df0$stato_epid == "Nuovo caso di bovina malata", na.rm = TRUE)
rtemp$numerator = new_cases

## 3. Incidence = new_cases / cow_years
incidence = round((new_cases/cow_years),3)
print(paste("The incidence with the standard classification is", incidence))
rtemp$estimate = incidence

results = rbind.data.frame(results, rtemp)

## 3. The numerator: revised classification (`stato_epid_rev`) (without relapses)
new_cases = sum(df0$stato_epid_rev == "Nuovo caso reale di bovina malata", na.rm = TRUE)
incidence = round((new_cases/cow_years),3)
print(paste("The incidence with the revised classification is", incidence))

rtemp <- data.frame("type" = "avg_incidence_revised", "denominator_ncows" = ncows, "denominator_cow_years" = cow_years,
                    "numerator" = new_cases, "estimate" = incidence)
results = rbind.data.frame(results, rtemp)

## 4. The numerator: revised classification (`stato_epid_rev`) (with relapses)
new_cases = sum(df0$stato_epid_rev %in% c("Nuovo caso reale di bovina malata", "Bovina recidiva"), na.rm = TRUE)
incidence = round((new_cases/cow_years),3)
print(paste("The incidence with the revised classification (including relapses) is", incidence))

rtemp <- data.frame("type" = "avg_incidence_relapses", "denominator_ncows" = ncows, "denominator_cow_years" = cow_years,
                    "numerator" = new_cases, "estimate" = incidence)
results = rbind.data.frame(results, rtemp)

fname = paste("prevalence_incidence_", config$year, ".csv", sep="")
outdir = file.path(config$base_folder, "results")
dir.create(outdir, showWarnings = FALSE)
fname = file.path(outdir,fname)

fwrite(x = results, file = fname)

print("DONE!")
