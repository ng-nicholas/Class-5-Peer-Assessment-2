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

if (!require("lubridate")){
    install.packages("lubridate")
    require("lubridate")
}

# Stores the directory where this script is located.
# Only works when script is saved.
# initial.options <- commandArgs(trailingOnly = FALSE)
# file.arg.name <- "--file="
# script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name,
#                                                            initial.options)])
# script.basename <- dirname(script.name)
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

# Checks if the data has been read into the environment, else reads the file.
if (!exists("data.raw")) {
    data.raw <- data.table(read.csv(bzfile(data.path), stringsAsFactors = F))
}

# Creates a copy of the data to keep original raw data as a backup
data.copy <- data.raw

# Cleaning up the EVTYPE column. Has bad mix of spaces and capitalisation.
data.copy$EVTYPE <- toupper(data.copy$EVTYPE)
data.copy$EVTYPE <- gsub("\\s\\s | \\-*", "\\s", data.copy$EVTYPE)
data.copy$EVTYPE <- gsub("^\\s | \\-$", "", data.copy$EVTYPE)

# Multiplying property and crop damage based on the exponent provided
i <- 1
for (i in 1:nrow(data.copy)) {
    if (data.copy$PROPDMGEXP[[i]] == "K") {
        data.copy$PROPDMG[[i]] <- data.copy$PROPDMG[[i]] * 1000
    } else if (data.copy$PROPDMGEXP[[i]] == "M") {
        data.copy$PROPDMG[[i]] <- data.copy$PROPDMG[[i]] * 1000000
    } else if (data.copy$PROPDMGEXP[[i]] == "B") {
        data.copy$PROPDMG[[i]] <- data.copy$PROPDMG[[i]] * 1000000000
    }

    if (data.copy$CROPDMGEXP[[i]] == "K") {
        data.copy$CROPDMG[[i]] <- data.copy$CROPDMG[[i]] * 1000
    } else if (data.copy$CROPDMGEXP[[i]] == "M") {
        data.copy$CROPDMG[[i]] <- data.copy$CROPDMG[[i]] * 1000000
    } else if (data.copy$CROPDMGEXP[[i]] == "B") {
        data.copy$CROPDMG[[i]] <- data.copy$CROPDMG[[i]] * 1000000000
    }
}

# Combining property and crop damage into a single estimate of economic damage
data.copy <- mutate(data.copy, econdmg = sum(PROPDMG, CROPDMG))

# Summarising data for health/economic damage by event type
data.damage <- data.copy %>%
                group_by(EVTYPE) %>%
                summarise(deaths = round(mean(FATALITIES)),
                          injuries = round(mean(INJURIES)),
                          economic = round(mean(econdmg)))

## Results
# Loading ggplot2
if (!require("ggplot2")){
    install.packages("ggplot2")
}
suppressMessages(require("ggplot2"))

