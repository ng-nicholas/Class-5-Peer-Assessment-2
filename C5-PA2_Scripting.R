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

print("Cleaning up the EVTYPE column.")
# Using subsetting rules with regexs to sort events into official 48 types.
# According to solution proposed by community TA David Hood.
data.copy$EVTYPE <- tolower(data.copy$EVTYPE)
data.copy$EVTYPE <- gsub("[[:space:]]*|[[:punct:]]*", "", data.copy$EVTYPE)
data.copy$cleanev <- ""

data.copy$cleanev[grep("astronomical", data.copy$EVTYPE)] <-
    "Astronomical Low Tide Z"
data.copy$cleanev[grep("avalan", data.copy$EVTYPE)] <-
    "Avalanche Z"
data.copy$cleanev[grep("blizz", data.copy$EVTYPE)] <-
    "Blizzard Z"
data.copy$cleanev[grep("coast", data.copy$EVTYPE)] <-
    "Coastal Flood Z"
data.copy$cleanev[grep("w(i)?nd", data.copy$EVTYPE)] <-
    "High Wind Z"
data.copy$cleanev[grep("^cold|^cool", data.copy$EVTYPE)] <-
    "Cold/Wind Chill Z"
data.copy$cleanev[grep("debris", data.copy$EVTYPE)] <-
    "Debris Flow C"
data.copy$cleanev[grep("fog", data.copy$EVTYPE)] <-
    "Dense Fog Z"
data.copy$cleanev[grep("smoke", data.copy$EVTYPE)] <-
    "Dense Smoke Z"
data.copy$cleanev[grep("drought", data.copy$EVTYPE)] <-
    "Drought Z"
data.copy$cleanev[grep("dustd", data.copy$EVTYPE)] <-
    "Dust Devil C"
data.copy$cleanev[grep("dustst", data.copy$EVTYPE)] <-
    "Dust Storm Z"
data.copy$cleanev[grep("heat", data.copy$EVTYPE)] <-
    "Heat Z"
data.copy$cleanev[grep("excessiveheat|extremeh", data.copy$EVTYPE)] <-
    "Excessive Heat Z"
data.copy$cleanev[grep("extremec|extremer|extremew", data.copy$EVTYPE)] <-
    "Extreme Cold/Wind Chill Z"
data.copy$cleanev[grep("flash", data.copy$EVTYPE)] <-
    "Flash Flood C"
data.copy$cleanev[grep("^flood", data.copy$EVTYPE)] <-
    "Flood C"
data.copy$cleanev[grep("frost|freez", data.copy$EVTYPE)] <-
    "Frost/Freeze Z"
data.copy$cleanev[grep("funnel", data.copy$EVTYPE)] <-
    "Funnel Cloud C"
data.copy$cleanev[grep("freezingfog", data.copy$EVTYPE)] <-
    "Freezing Fog Z"
data.copy$cleanev[grep("hail", data.copy$EVTYPE)] <-
    "Hail C"
data.copy$cleanev[grep("rain|tstm", data.copy$EVTYPE)] <-
    "Heavy Rain C"
data.copy$cleanev[grep("snow", data.copy$EVTYPE)] <-
    "Heavy Snow Z"
data.copy$cleanev[grep("surf", data.copy$EVTYPE)] <-
    "High Surf Z"
data.copy$cleanev[grep("hurricane|typhoon", data.copy$EVTYPE)] <-
    "Hurricane (Typhoon) Z"
data.copy$cleanev[grep("icestorm", data.copy$EVTYPE)] <-
    "Ice Storm Z"
data.copy$cleanev[grep("lakeeffectsnow", data.copy$EVTYPE)] <-
    "Lake-Effect Snow Z"
data.copy$cleanev[grep("lake(shore)?flood", data.copy$EVTYPE)] <-
    "Lakeshore Flood Z"
data.copy$cleanev[grep("lightning", data.copy$EVTYPE)] <-
    "Lightning C"
data.copy$cleanev[grep("marinehail", data.copy$EVTYPE)] <-
    "Marine Hail M"
data.copy$cleanev[grep("marinehighwind", data.copy$EVTYPE)] <-
    "Marine High Wind M"
data.copy$cleanev[grep("marinestormwind", data.copy$EVTYPE)] <-
    "Marine Strong Wind M"
data.copy$cleanev[grep("marinethunderstormwind|marinetstmwind",
                       data.copy$EVTYPE)] <- "Marine Thunderstorm Wind M"
data.copy$cleanev[grep("ripcurrent", data.copy$EVTYPE)] <-
    "Rip Current Z"
data.copy$cleanev[grep("seiche", data.copy$EVTYPE)] <-
    "Seiche Z"
data.copy$cleanev[grep("sleet", data.copy$EVTYPE)] <-
    "Sleet Z"
data.copy$cleanev[grep("stormsurge", data.copy$EVTYPE)] <-
    "Storm Surge/Tide Z"
data.copy$cleanev[grep("strongwind", data.copy$EVTYPE)] <-
    "Strong Wind Z"
data.copy$cleanev[grep("storm(.*)?wind|tstmw", data.copy$EVTYPE)] <-
    "Thunderstorm Wind C"
data.copy$cleanev[grep("tornado", data.copy$EVTYPE)] <-
    "Tornado C"
data.copy$cleanev[grep("tropicaldepression", data.copy$EVTYPE)] <-
    "Tropical Depression Z"
data.copy$cleanev[grep("tropicalstorm", data.copy$EVTYPE)] <-
    "Tropical Storm Z"
data.copy$cleanev[grep("tsunami", data.copy$EVTYPE)] <-
    "Tsunami Z"
data.copy$cleanev[grep("volcanic", data.copy$EVTYPE)] <-
    "Volcanic Ash Z"
data.copy$cleanev[grep("wa.erspout", data.copy$EVTYPE)] <-
    "Waterspout M"
data.copy$cleanev[grep("wild(.*)?fire", data.copy$EVTYPE)] <-
    "Wildfire Z"
data.copy$cleanev[grep("wint", data.copy$EVTYPE)] <-
    "Winter Weather Z"
data.copy$cleanev[grep("winterst", data.copy$EVTYPE)] <-
    "Winter Storm Z"
data.copy$cleanev[grep("^$|^[[:space:]]$", data.copy$cleanev)] <- "Other"

print("Cleaning up the exponent columns.")
# Changes everything to upper case and removes irrelevant exponents.
data.copy$PROPDMGEXP <- toupper(data.copy$PROPDMGEXP)
data.copy$CROPDMGEXP <- toupper(data.copy$CROPDMGEXP)
data.copy$PROPDMGEXP <- gsub("[[:digit:]]|[[:punct:]]", "",
                             data.copy$PROPDMGEXP)
data.copy$CROPDMGEXP <- gsub("[[:digit:]]|[[:punct:]]", "",
                             data.copy$CROPDMGEXP)

# Switching exponents to numbers for easier mutation
data.copy$PROPDMGEXP <- gsub("^$", 1, data.copy$PROPDMGEXP)
data.copy$PROPDMGEXP <- gsub("H", 100, data.copy$PROPDMGEXP)
data.copy$PROPDMGEXP <- gsub("K", 1000, data.copy$PROPDMGEXP)
data.copy$PROPDMGEXP <- gsub("M", 1000000, data.copy$PROPDMGEXP)
data.copy$PROPDMGEXP <- gsub("B", 1000000000, data.copy$PROPDMGEXP)

data.copy$CROPDMGEXP <- gsub("^$", 1, data.copy$CROPDMGEXP)
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
                group_by(cleanev) %>%
                summarise(deaths = round(mean(FATALITIES)),
                          injuries = round(mean(INJURIES)),
                          pdmg = mean(pdmg),
                          cdmg = mean(cdmg)) %>%
                mutate(human = deaths + injuries,
                       econ = pdmg + cdmg)

# Grabbing the top 10 human health/econ damage event types and storing them
# in the relevant tables
data.tophuman <- data.damage$cleanev[order(data.damage$human, decreasing=T)]
data.human <- subset(data.damage, cleanev %in% data.tophuman[1:10])

data.topecon <- data.damage$cleanev[order(data.damage$econ, decreasing=T)]
data.econ <- subset(data.damage, cleanev %in% data.topecon[1:10])

# Reshaping damage table into other tables for easier use with ggplot2
data.human <- melt(data.human, id = "cleanev",
                   measure = c("deaths", "injuries"), variable.name = "htype",
                   value.name = "amount")
data.human$htype <- gsub("deaths", "Deaths", data.human$htype)
data.human$htype <- gsub("injuries", "Injuries", data.human$htype)

data.econ <- melt(data.econ, id = "cleanev",
                   measure = c("pdmg", "cdmg"), variable.name = "etype",
                   value.name = "amount")
data.econ$etype <- gsub("pdmg", "Property", data.econ$etype)
data.econ$etype <- gsub("cdmg", "Crop", data.econ$etype)

## Results
# Loading ggplot2
if (!require("ggplot2")){
    install.packages("ggplot2")
    require("ggplot2")
}
if (!require("scales")){
    install.packages("scales")
    require("scales")
}

# Plotting the human damage by event
graph.human <- ggplot(data.human, aes(x = cleanev, y = amount, fill = htype)) +
                geom_bar(stat = "identity") +
                labs(title = "Average Deaths/Injuries by Event Type",
                     x = "Event Type",
                     y = "Number of Deaths/Injuries",
                     fill = "Categories") +
                scale_y_continuous(labels = comma)
print(graph.human)

graph.econ <- ggplot(data.econ, aes(x = cleanev, y = amount, fill = etype)) +
                geom_bar(stat = "identity") +
                labs(title = "Average Economic Damage by Event Type",
                     x = "Event Type",
                     y = "Cost of Damage (USD)",
                     fill = "Type of Damage") +
                scale_y_continuous(labels = comma)
print(graph.econ)