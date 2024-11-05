Trends and Patterns in New York State Baby Names (2007-Present)
---
title: "Trends and Patterns in New York State Baby Names (2007-Present)"
author: "Patrick Mensah"
date: "`r Sys.Date()`"
output: html_document
---


## R Markdown

Introduction
This project explores trends and patterns in baby names across New York State using data from the New York State Department of Health. The dataset, titled "Baby Names: Beginning 2007," contains information on baby names aggregated by year, county, and the sex of the child, based on birth certificates issued in both New York State and New York City (NYC).

The primary goal of this project is to analyze popular names, regional differences, gender distributions, and trends over time. By examining data at the county and borough levels, we can uncover unique naming preferences and shifts influenced by cultural, social, and demographic factors. Additionally, limitations in the data—such as minimum frequency thresholds for names to be listed—will guide our interpretation, especially when analyzing less common names.

Through R, we will load, clean, and visualize the data to gain insights into New York's naming trends over recent years, ultimately providing a clearer picture of regional diversity and evolving name popularity.


```{r}
#Loading the necessary libraries
library(tools)
library(httr)
library(dplyr)
library(readr)
library(ggplot2)
library(wordcloud2)
```

Loading the data set
Since the API limits the response to 1,000 rows by default, I used the offset parameter to paginate through the data set

```{r}

# Function to fetch all baby names data
fetch_baby_names_data <- function(base_url) {
  all_data <- data.frame()  # Initialize an empty data frame
  limit <- 1000  # Number of rows per request
  offset <- 0  # Starting offset

  repeat {
    # Construct the API request URL with limit and offset
    request_url <- paste0(base_url, "?$limit=", limit, "&$offset=", offset)
    
    # Fetch the data
    response <- GET(request_url)

    # Check for successful response
    if (status_code(response) == 200) {
      # Convert the response to a data frame
      new_data <- read.csv(text = content(response, "text"))
      
      # Break the loop if no more data is returned
      if (nrow(new_data) == 0) {
        break
      }
      
      # Append the new data to the all_data data frame
      all_data <- bind_rows(all_data, new_data)
      
      # Increment the offset for the next request
      offset <- offset + limit
    } else {
      warning("Failed to fetch data: ", status_code(response))
      break
    }
  }
  
  return(all_data)
}

# Define the API base URL
api_base_url <- "https://health.data.ny.gov/resource/jxy9-yhdk.csv"

# Fetch all baby names data
baby_names_data <- fetch_baby_names_data(api_base_url)

```



```{r}
#Check data types
str(baby_names_data)
```


Data cleaning is an essential step to ensure the quality and integrity of the dataset before analysis. Here are some typical steps we can take to clean the dataset:

```{r}

# Convert first_name and county to proper case
# Custom function to convert to proper case
proper_case <- function(x) {
  s <- strsplit(x, " ") # Split the string by spaces
  s <- lapply(s, function(y) { paste(toupper(substring(y, 1, 1)), tolower(substring(y, 2)), sep="") }) # Capitalize first letter and lower the rest
  s <- sapply(s, paste, collapse=" ") # Collapse back to a single string
  return(s)
}

# Convert first_name and county to proper case
baby_names_data <- baby_names_data %>%
  mutate(
    first_name = proper_case(first_name),  # Apply custom function
    county = proper_case(county)             # Apply custom function
  )
```


```{r}
#Checking for missing values
missing_values <- sum(is.na(baby_names_data))
print(paste("Number of missing values:", missing_values))

```

```{r}
# Checking for duplicates 
# Count the number of duplicate rows in the dataset
duplicate_count <- sum(duplicated(baby_names_data))
print(paste("Number of duplicate rows:", duplicate_count))
```


```{r}
#Removing duplicates if there is any
baby_names_data <- baby_names_data %>% distinct()
```



```{r}
# Checking for inconsistent entries in 'sex'
unique_sex_values <- unique(baby_names_data$sex)
print(unique_sex_values)
```

Descriptive Analysis of Baby Names Data
Descriptive analysis is a fundamental step in data analysis that provides a summary of the main characteristics of a dataset. This type of analysis helps to simplify complex data sets and presents them in a form that is easier to understand. The descriptive analysis is useful for uncovering trends, identifying popular names, and understanding demographic distributions.

```{r}
# Total names by year
total_names_by_year <- baby_names_data %>%
  group_by(year) %>%
  summarise(total_names = sum(name_count))

print(total_names_by_year)
```
The data reveals a general decline in the total number of baby names from 2013 to 2019, followed by a slight recovery in 2020 and 2021, with 2008 being the peak year for total names recorded at 120,472, while 2020 had the lowest total at 90,140 names.

```{r}
# Calculate total names by county
total_names_by_county <- baby_names_data %>%
  group_by(county) %>%
  summarise(total_names = sum(name_count, na.rm = TRUE)) %>%
  arrange(desc(total_names)) 

# View the results
print(total_names_by_county, n = Inf)
```
The total_names_by_county data reveals that Kings County has the highest total of baby names recorded, with 336,469 names, followed by Queens with 227,560 and Suffolk with 166672. In contrast, several counties reported significantly lower totals, such as Schuyler with just 10 names and Yates with 21. This overview highlights the variations in baby name frequencies across New York State's counties.





Visualizations are a powerful tool for summarizing and interpreting data, allowing us to identify patterns and trends at a glance. The following visualizations will help us understand the distribution of baby names in New York State, highlighting the total number of names by county, the trend of names over the years, and the distribution of names by sex.

```{r}
library(ggplot2)


ggplot(total_names_by_county, aes(x = reorder(county, total_names), y = total_names)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Flip coordinates for better readability
  labs(title = "Total Baby Names by County in New York State",
       x = "County",
       y = "Total Names") +
  theme_minimal()

```
The total_names_by_county data reveals that Kings County has the highest total of baby names recorded, followed by Queens and Suffolk. In contrast, several counties reported significantly lower totals, such as Schuyler with just 10 names and Yates with 21. This overview highlights the variations in baby name frequencies across New York State's counties.

```{r}
# total_names_by_year
ggplot(total_names_by_year, aes(x = year, y = total_names)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point() +
  labs(title = "Trend of Total Baby Names in New York State (2007-2021)",
       x = "Year",
       y = "Total Names") +
  theme_minimal()

```
The graph shows a 14-year period in which the number of baby names in New York State trends downward overall. Starting at around 120,000 in 2007, there is a notable spike around 2014 before the numbers continue to decline. The lowest point is reached around 2020. There is a slight increase again in 2021. Overall, the graph highlights a decrease in the total number of baby names over the years.




```{r}
# Calculate the total counts of names by sex
sex_distribution <- baby_names_data %>%
  group_by(sex) %>%
  summarise(total_count = sum(name_count, na.rm = TRUE))

# Create a pie chart for sex distribution
ggplot(sex_distribution, aes(x = "", y = total_count, fill = sex)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Baby Names by Sex") +
  theme_void()  # Remove grid lines and background

```
The pie chart shows the proportion of baby names divided by sex. There are two segments: red for female (F) names and teal for male (M) names. The teal segment is larger, indicating there are more male baby names than female baby names. The chart's legend clearly denotes the colors representing each sex. Overall, it highlights a greater number of male baby names compared to female ones.



Identify the top baby names for each year

```{r}
# Calculate the total count of names by year and first name
top_names_by_year <- baby_names_data %>%
  group_by(year, first_name) %>%
  summarise(total_count = sum(name_count, na.rm = TRUE), .groups = 'drop') %>%
  arrange(year, desc(total_count)) %>%
  group_by(year) %>%
  slice_head(n = 10) # Adjust n to get the desired number of top names

# Create a bar plot for the top names by year
ggplot(top_names_by_year, aes(x = reorder(first_name, -total_count), y = total_count, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Top Baby Names by Year (2007-2021)",
       x = "Baby Names",
       y = "Total Count",
       fill = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

A word cloud can also be a fun way to visualize the popularity of names

```{r}

# Preparing data for word cloud
wordcloud_data <- baby_names_data %>%
  group_by(first_name) %>%
  summarise(total_count = sum(name_count, na.rm = TRUE)) %>%
  arrange(desc(total_count))

# Create the word cloud
wordcloud2(data = wordcloud_data, size = 1, color = "random-light", backgroundColor = "black")

```
