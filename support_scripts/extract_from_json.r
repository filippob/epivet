## R script to extract data from the input json file
## input: json file provided by the company (herd data)
# run as Rscript --vanilla extract_from_json.r <config_file>

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
    input_file = 'export-21-1546300800-1706832000.json',
    force_overwrite = FALSE
  ))
}

# SETUP -------------------------------------------------------------------
library("knitr")
library("broom")
library("jsonlite")
library("tidyjson")
library("tidyverse")
library("data.table")

print(paste("input file name:",config$input_file))
print(paste("data folder:",config$data_folder))


# READ DATA -------------------------------------------------------------------
## JSONLITE
writeLines(" - reading json file")
fname = file.path(config$base_folder, config$data_folder, config$input_file)
data <- jsonlite::fromJSON(txt = fname)

compl = data |> json_complexity()
compl$complexity
print(paste("json data complexity is:", compl$complexity))

# EXTRACT RELEVANT DATA --------------------------------------------------------
writeLines(" - extracting relevant data")
df_visite <- data$aziende$mandrie[[1]]$visite[[1]] %>% as_tibble()
df_bovine <- data$aziende$mandrie[[1]]$bovine[[1]] %>% as_tibble()
df_status_bovine <- data$aziende$mandrie[[1]]$status_bovina[[1]] %>% as_tibble()

print(paste("N. of unique animals extracted:", nrow(df_bovine)))
print(paste("N. of podological veterinary visits extracted:", nrow(df_visite)))
print(paste("N. of cow records (fertility, milk production) extracted:", nrow(df_status_bovine)))

# CLEAN DATA --------------------------------------------------------
## removing milk records where numero_parti is zero
df_status_bovine <- filter(df_status_bovine, numero_parti != 0)
print(paste("N. of cow records (fertility, milk production) after cleaning:", nrow(df_status_bovine)))

# SAVE EXTRACTED DATA --------------------------------------------------------
writeLines(" - saving extracted data to R data objects (one per type: cows, vet visits, milk records)")
save(df_visite, file = file.path(config$base_folder, config$data_folder, "extracted_visite.RData"))
save(df_bovine, file = file.path(config$base_folder, config$data_folder, "extracted_bovine.RData"))
save(df_status_bovine, file = file.path(config$base_folder, config$data_folder, "extracted_status_bovine.RData"))


print("Done!!")

print("#########")
print("## END ##")
print("#########")

