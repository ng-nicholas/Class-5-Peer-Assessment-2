## Data Processing
# Load required packages for data processing
if (!require("dplyr")){
    install.packages("dplyr")
    require("dplyr")
}

if (!require("data.table")){
    install.packages("data.table")
    require("data.table")
}

if (!require("reshape2")){
    install.packages("data.table")
    require("data.table")
}

# Checks if the data has been read into the environment, if not it checks if the
# file is present for reading. If not present, it downloads the file.
if (!exists("data.raw")) {
    # Finds directory of this script. Works only when script is saved.
    script.dir <- paste0(dirname(sys.frame(1)$ofile), "/")

    # Set working directory for an organised working environment.
    setwd(script.dir)

    # Checks if the data file has been downloaded before. Else, downloads the file.
    data.path <- "./repdata-data-StormData.csv.bz2"
    if (length(list.files(, pattern = "repdata-data-StormData.csv.bz2",
                          recursive = T)) == 0) {
        data.url <- "http://d396qusza40orc.cloudfront.net/
                repdata%2Fdata%2FStormData.csv.bz2"
        download.file(data.url, data.path)
    }

    # Reads data into environment.
    data.raw <- data.table(read.csv(bzfile(data.path), stringsAsFactors = F))
}

# Creates a copy of the data to keep original raw data as a backup
data.copy <- data.raw

print("Cleaning up the EVTYPE column. Has bad mix of spaces and capitalisation.")
data.copy$EVTYPE <- toupper(data.copy$EVTYPE)
data.copy$EVTYPE <- gsub("[[:punct:]]|[[:space:]]", " ", data.copy$EVTYPE)
data.copy$EVTYPE <- gsub("^[[:space:]]|[[:space:]]$", "", data.copy$EVTYPE)

print("Cleaning up the exponent columns.")
# Changes everything to upper case and removes irrelevant exponents.
data.copy$PROPDMGEXP <- toupper(data.copy$PROPDMGEXP)
data.copy$CROPDMGEXP <- toupper(data.copy$CROPDMGEXP)
data.copy$PROPDMGEXP <- gsub("[[:digit:]]|[[:punct:]]", "",
                             data.copy$PROPDMGEXP)
data.copy$CROPDMGEXP <- gsub("[[:digit:]]|[[:punct:]]", "",
                             data.copy$CROPDMGEXP)

# Switching exponents to numbers for easier mutation
data.copy$PROPDMGEXP <- gsub("^$", 0, data.copy$PROPDMGEXP)
data.copy$PROPDMGEXP <- gsub("H", 100, data.copy$PROPDMGEXP)
data.copy$PROPDMGEXP <- gsub("K", 1000, data.copy$PROPDMGEXP)
data.copy$PROPDMGEXP <- gsub("M", 1000000, data.copy$PROPDMGEXP)
data.copy$PROPDMGEXP <- gsub("B", 1000000000, data.copy$PROPDMGEXP)

data.copy$CROPDMGEXP <- gsub("^$", 0, data.copy$CROPDMGEXP)
data.copy$CROPDMGEXP <- gsub("H", 100, data.copy$CROPDMGEXP)
data.copy$CROPDMGEXP <- gsub("K", 1000, data.copy$CROPDMGEXP)
data.copy$CROPDMGEXP <- gsub("M", 1000000, data.copy$CROPDMGEXP)
data.copy$CROPDMGEXP <- gsub("B", 1000000000, data.copy$CROPDMGEXP)

# Coercion to numeric
data.copy$PROPDMGEXP <- as.numeric(data.copy$PROPDMGEXP)
data.copy$CROPDMGEXP <- as.numeric(data.copy$CROPDMGEXP)

print("Mutating a new variables to provide a single measure of economic damage.")
# Subsequently grouping by type of event and summarising
# the data for the average health/economic damage by event type. The deaths and
# injuries variables are kept for more in-depth analysis, but mutated to have a
# total measure as well.
data.damage <- data.copy %>%
                mutate(pdmg = PROPDMG * PROPDMGEXP,
                       cdmg = CROPDMG * CROPDMGEXP) %>%
                group_by(EVTYPE) %>%
                summarise(deaths = round(mean(FATALITIES)),
                          injuries = round(mean(INJURIES)),
                          pdmg = mean(pdmg),
                          cdmg = mean(cdmg)) %>%
                mutate(human = deaths + injuries,
                       econ = pdmg + cdmg)

# Reshaping damage table into other tables for easier use with ggplot2
data.human <- melt(data.damage, id = "EVTYPE",
                   measure = c("deaths", "injuries"), variable.name = "htype",
                   value.name = "amount")

data.econ <- melt(data.damage, id = "EVTYPE",
                   measure = c("pdmg", "cdmg"), variable.name = "etype",
                   value.name = "amount")

## Results
# Loading ggplot2
if (!require("ggplot2")){
    install.packages("ggplot2")
    require("ggplot2")
}

# # Plotting the human damage by event
# graph.human <- ggplot(data.damage, aes(x = EVTYPE, y = human)) +
#                 geom_bar(stat)