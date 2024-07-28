#This project was definitely written with ChatGPT. So much ChatGPT. 
#Just look at those libraries! Stack Overflow could never. :) 

library(ggplot2)         
library(dplyr)           
library(rnaturalearth)   
library(rnaturalearthdata) 
library(sf)              
library(countrycode)     
library(readxl)          
library(data.table)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(SnowballC)
library(stringdist)
library(quanteda)
library(textstem)

# Define file path 
file_path <- "/Users/akk/Downloads/Rscripts/complete.xlsx"

# Load dataset 
data <- read_excel(file_path)

# Extract month from the datetime column
data$datetime <- as.Date(data$datetime, format="%m/%d/%Y")
data$month <- format(data$datetime, "%B")

# Count number of sightings per month
month_counts <- data %>%
  group_by(month) %>%
  summarise(count = n())

# Ensure months are in the correct order
month_counts$month <- factor(month_counts$month, levels = month.name)

# Plot data as a line graph 
ggplot(month_counts, aes(x = month, y = count, group = 1)) +  
  geom_line(color = "skyblue") +
  geom_point(color = "blue", size = 3) +
  theme_minimal() +
  labs(title = "UFO Sightings by Month", x = "Month", y = "Number of Sightings") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  
    plot.title = element_text(hjust = 0.5, margin = margin(b = 20)),  
    axis.title.x = element_text(margin = margin(t = 10)),  
    axis.title.y = element_text(margin = margin(r = 10))  
  )

#Visualization 2

# Filter out NA, 'unknown', and 'other' shapes
filtered_data <- data %>%
  filter(!is.na(shape), !shape %in% c("unknown", "other"))

# Combine similar shapes
filtered_data$shape <- recode(filtered_data$shape, "flash" = "light")

# Group data by shape and count 
shape_counts <- filtered_data %>%
  group_by(shape) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(desc(count)) %>%
  slice_max(order_by = count, n = 10)  

# Reorder in descending order based on count
shape_counts$shape <- factor(shape_counts$shape, levels = shape_counts$shape[order(shape_counts$count, decreasing = TRUE)])

# Choose color palette 
palette <- brewer.pal(n = nrow(shape_counts), name = "Paired")  

# Plot pie chart
ggplot(shape_counts, aes(x = "", y = count, fill = shape)) +
  geom_bar(width = 1, stat = "identity", color = alpha("white", 0.5)) + 
  coord_polar(theta = "y") +
  scale_fill_manual(values = palette) +  
  theme_minimal() +
  labs(title = "Top 10 UFO Shapes Reported", fill = NULL) +  
  theme(
    axis.text.x = element_blank(),  
    axis.ticks = element_blank(),   
    plot.title = element_text(hjust = 0.5, margin = margin(t = 20)),  
    panel.grid = element_blank(),  
    legend.position = "right",      
    legend.title = element_blank(), 
    legend.text = element_text(size = 10),  
    legend.key = element_rect(fill = alpha("white", 0), colour = alpha("white", 0)),  
    axis.text.y = element_text(color = alpha("white", 0.5)),  
    axis.title.x = element_text(color = alpha("white", 0.5)), 
    axis.title.y = element_text(color = alpha("white", 0.5))  
  )

# Visualization 3 

setDT(data)

# Clean column names
setnames(data, gsub("[^[:alnum:] ]", "", names(data)))

# Convert 'country' column to lowercase
if ("country" %in% colnames(data)) {
  data[, country := tolower(country)]
}

# Define state abbreviations
us_states <- tolower(state.abb)

# Handle cases where state information indicates USA
if ("state" %in% colnames(data)) {
  data[, state := tolower(state)]
  data[tolower(state) %in% us_states, country := "usa"]
}

# Standardize country names for USA variations
data[country %in% c("usa", "us", "united states"), country := "usa"]

# Correct known discrepancies in country codes
corrections <- list('auss' = 'aus', 'ltu' = 'ltu', 'zef' = 'zaf', 'vtm' = 'vnm')
data[, country := sapply(country, function(x) if (x %in% names(corrections)) corrections[[x]] else x)]

# Special handling for Antarctica
antarctica_keywords <- c("antarctica", "south pole", "ross ice shelf", "mcmurdo station")

for (col in colnames(data)) {
  data[grepl(paste(antarctica_keywords, collapse = "|"), get(col), ignore.case = TRUE), country := "antarctica"]
}

# Assign Antarctica ISO code
data[country == "antarctica", iso_a3 := "ATA"]

# Fallback: Extract country names from 'city' and 'comments' columns if not already set
country_names <- unique(trimws(c(countrycode::codelist$country.name.en, countrycode::codelist$iso.name.en)))
extract_country <- function(text) {
  matches <- country_names[sapply(country_names, function(x) grepl(x, text, ignore.case = TRUE))]
  if (length(matches) > 0) return(matches[1])
  return(NA)
}

# Check the 'city' and 'comments' columns
columns_to_check <- c("city", "comments")
for (col in columns_to_check) {
  if (col %in% colnames(data)) {
    data[is.na(country) | country == "", country := sapply(get(col), extract_country)]
  }
}

# Convert standardized country names to ISO 3166-1 alpha-3 codes
data[, iso_a3 := toupper(countrycode(country, origin = 'country.name', destination = 'iso3c'))]

# Handle cases where ISO code is NA
data[is.na(iso_a3), iso_a3 := "UNKNOWN"]

# Remove duplicate columns
data <- data[, !duplicated(names(data)), with = FALSE]

# Count sightings by ISO country code
country_counts <- data[!is.na(iso_a3), .(count = .N), by = iso_a3][order(-count)]

# Ensure that USA is included in the country_counts
if (!"USA" %in% country_counts$iso_a3) {
  message("USA is missing from country_counts. Adding it manually.")
  country_counts <- rbind(country_counts, data.table(iso_a3 = "USA", count = 0))
}

# Debug: Check if USA is in the country counts
print("Country counts including USA:")
print(country_counts[iso_a3 == "USA"])

# Load world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Debug: Check if 'USA' is present in the world map data
print("ISO codes in world data:")
print(unique(world$iso_a3))

# Merge UFO data with world map data using ISO codes
world_data <- merge(world, country_counts, by.x = "iso_a3", by.y = "iso_a3", all.x = TRUE)

# Debug: Check the merged data for 'USA'
print("World data including USA after merge:")
print(world_data[world_data$iso_a3 == "USA", ])

# Replace NA values in the 'count' column with 0
world_data$count[is.na(world_data$count)] <- 0

# Plotting the heat map on a world map
ggplot(world_data) +
  geom_sf(aes(fill = count), color = "white") +
  scale_fill_gradient(
    low = "lightblue", high = "darkblue", na.value = "grey",
    trans = "log1p",
    breaks = c(0, 10, 100, 1000, 10000),  
    labels = c("0", "10", "100", "1k", "10k+"),
    limits = c(0, max(world_data$count, na.rm = TRUE)),
    guide = guide_colorbar(
      barwidth = 20,
      barheight = 0.5,
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  theme_minimal() +
  labs(title = "UFO Sightings by Country", fill = "Sightings") +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(t = 10, b = 10)),
    legend.position = "bottom",
    legend.title = element_text(hjust = 0.5)
  )


#Visualization 4 

# Ensure 'comments' column exists
if ("comments" %in% colnames(data)) {
  # Remove duplicate comments and ensure uniqueness
  unique_comments <- unique(data$comments)
  
  # Create a text corpus from the 'comments' column
  corpus <- Corpus(VectorSource(unique_comments))
  
  # Clean the text data
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  corpus <- tm_map(corpus, stripWhitespace)
  
  # Normalize similar words
  corpus <- tm_map(corpus, content_transformer(lemmatize_strings))
  
  # Create a Document-Term Matrix
  dtm <- DocumentTermMatrix(corpus, control = list(bounds = list(global = c(5, Inf)))) 
  m <- as.matrix(dtm)
  
  # Calculate word frequencies
  word_freqs <- sort(colSums(m), decreasing = TRUE)
  word_freqs <- data.frame(word = names(word_freqs), freq = word_freqs)
  
  # Aggregate frequencies of normalized words
  word_freqs <- aggregate(freq ~ word, data = word_freqs, sum)
  
  # Set margins for padding and increase plotting area size
  par(mar = c(0, 0, 0, 0), oma = c(2, 2, 2, 2))  # Set outer margins
  
  # Generate the word cloud 
  wordcloud(words = word_freqs$word,
            freq = word_freqs$freq,
            scale = c(6, 0.5),  
            max.words = 100,     
            random.order = FALSE,
            rot.per = 0.25,      
            use.r.layout = FALSE,
            colors = brewer.pal(8, "Paired")) 
  title(main = "UFO Sighting Comments: Most Common Terms", line = .5, outer = TRUE)
} else {
  cat("The 'comments' column is missing from the dataset.")
}

#Visualization 5

# Check column names and correct them if necessary
str(data)  

# Ensure correct column names
names(data)[names(data) == "date posted"] <- "date_posted"

# Convert date columns
data$datetime <- as.Date(data$datetime, format="%m/%d/%Y")
data$date_posted <- as.Date(data$date_posted, format="%m/%d/%Y")

# Calculate report delay
data <- data %>%
  mutate(report_delay = as.numeric(date_posted - datetime))

# Filter valid data and limit to delays up to 60 days
data_limited <- data %>% 
  filter(!is.na(report_delay) & report_delay >= 0 & report_delay <= 60)

# Define the breaks for binning
breaks <- seq(0, 60, by = 5)

# Bin the data and create clean labels
data_limited <- data_limited %>%
  mutate(bin = cut(report_delay, breaks = breaks, include.lowest = TRUE, right = FALSE, 
                   labels = paste0(breaks[-length(breaks)] + 1, "-", breaks[-1])))

# Count the number of reports for each bin
report_counts <- data_limited %>%
  group_by(bin) %>%
  summarise(count = n())

# Create a bar graph with formatted bin labels and padding
ggplot(report_counts, aes(x = bin, y = count)) +
  geom_bar(stat = "identity", fill = 'hotpink') +
  labs(title = "Number of Reports by Reporting Delay (Up to 60 Days)",
       x = "# of Days between UFO Sighting and Report", 
       y = "Number of Reports") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  
    plot.title = element_text(hjust = 0.5, vjust = 2),  
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),  
    axis.title.x = element_text(margin = margin(t = 15))  
  )
