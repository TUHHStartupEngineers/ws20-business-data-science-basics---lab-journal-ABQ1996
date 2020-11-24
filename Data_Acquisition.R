#1. Establishing a connection to the database ----
library(RSQLite)
library(tidyverse)
con <- RSQLite::dbConnect(drv = SQLite(), dbname = "~/Documents/GitHub/ws20-business-data-science-basics---lab-journal-ABQ1996/DS_101/00_data/02_chinook//Chinook_Sqlite.sqlite")
#dbListTables(con)
#tbl(con, "Album")
#1.1 Downloading the information from the database into local memory
album_tbl <- tbl(con,"Album") %>% collect()
#1.2 Disconnecting from the database
dbDisconnect(con)

#2. Manipulating Strings ----
library(glue)
name <- "Fred"
glue('My name is {name}.')

library(httr)
resp <- GET("https://swapi.dev/api/people/1/")

# Wrapped into a function
sw_api <- function(path) {
  url <- modify_url(url = "https://swapi.dev", path = glue("/api{path}"))
  resp <- GET(url)
  stop_for_status(resp) # automatically throws an error if a request did not succeed
}

resp <- sw_api("/people/1")

#3. Get the data for the Wirecard stock values ----
resp <- GET('https://www.alphavantage.co/query?function=GLOBAL_QUOTE&symbol=WDI.DE')
resp

#4. Extract Data from Wikipedia
# get the URL for the wikipedia page with all S&P 500 symbols
url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
# use that URL to scrape the S&P 500 table using rvest
library(rvest)
sp_500 <- url %>%
  # read the HTML from the webpage
  read_html() %>%
  # Get the nodes with the id
  html_nodes(css = "#constituents") %>%
  # html_nodes(xpath = "//*[@id='constituents']"") %>% 
  # Extract the table and turn the list into a tibble
  html_table() %>% 
  .[[1]] %>% 
  as_tibble()

#5. Get the 250 top rates movies vom IMDB ----
#url  <- "https://www.imdb.com/chart/top/?ref_=nv_mv_250"
#For english translation use other code
resp <- GET(url = "https://www.imdb.com/chart/top/?ref_=nv_mv_250",  
            add_headers('Accept-Language' = "en-US, en;q=0.5")) 
html <- content(resp)
html <- url %>% 
  read_html()
rank <- html %>% 
  html_nodes(css = ".titleColumn") %>% 
  html_text() %>% 
  # Extrag all digits between " " and ".\n" The "\" have to be escaped
  # You can use Look ahead "<=" and Look behind "?=" for this
  stringr::str_extract("(?<= )[0-9]*(?=\\.\\n)")%>% 
  # Make all values numeric
  as.numeric()
title <- html %>% 
  html_nodes(".titleColumn > a") %>% 
  html_text()
year <- html %>% 
  html_nodes(".titleColumn .secondaryInfo") %>%
  html_text() %>% 
  # Remove all brackets --> "(" OR ")"
  stringr::str_replace_all(pattern = "\\(|\\)", replacement = "") %>% 
  as.numeric()
people <- html %>% 
  html_nodes(".titleColumn > a") %>% 
  html_attr("title")
rating <- html %>% 
  html_nodes(css = ".imdbRating > strong") %>% 
  html_text() %>% 
  as.numeric()
num_ratings <- html %>% 
  html_nodes(css = ".imdbRating > strong") %>% 
  html_attr('title') %>% 
  # Extract the numbers and remove the comma to make it numeric values
  stringr::str_extract("(?<=based on ).*(?=\ user ratings)" ) %>% 
  stringr::str_replace_all(pattern = ",", replacement = "") %>% 
  as.numeric()
imdb_tbl <- tibble(rank, title, year, people, rating, num_ratings)

#6. Open a JSON File ----
library(jsonlite)
bike_data_lst <- fromJSON("~/Documents/GitHub/ws20-business-data-science-basics---lab-journal-ABQ1996/DS_101/02_data_wrangling/bike_data.json")
# Open the data by clicking on it in the environment or by running View()
View(bike_data_lst)
bike_data_lst %>%
  purrr::pluck("productDetail", "variationAttributes", "values", 1, "displayValue")
#7. Extract the data from the canyon website
#7. WEBSCRAPING ----

# 1.0 LIBRARIES ----

library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing
library(httr)
library(glue)
library(tribbles)

# 1.1 COLLECT PRODUCT FAMILIES ----

url_home          <- "https://www.canyon.com/en-de"
#xopen(url_home) # Open links directly from RStudio to inspect them

# Read in the HTML for the entire webpage
html_home         <- read_html(url_home)

# Web scrape the ids for the families
bike_family_tbl <- html_home %>%
  
  # Get the nodes for the families ...
  html_nodes(css = ".js-navigationDrawer__list--secondary") %>%
  # ...and extract the information of the id attribute
  html_attr('id') %>%
  
  # Remove the product families Gear and Outlet and Woman 
  # (because the female bikes are also listed with the others)
  discard(.p = ~stringr::str_detect(.x,"WMN|WOMEN|GEAR|OUTLET")) %>%
  
  # Convert vector to tibble effectivly giving each entry a number
  enframe(name = "position", value = "family_class") %>%
  
  # Add a hashtag so we can get nodes of the categories by id (#)
  mutate(
    family_id = str_glue("#{family_class}")
  )
# 1.2 COLLECT PRODUCT CATEGORIES ----

# Combine all Ids to one string so that we will get all nodes at once
# (seperated by the OR operator ",")
family_id_css <- bike_family_tbl %>%
  pull(family_id) %>%
  stringr::str_c(collapse = ", ")
#family_id_css
## "#js-navigationList-ROAD, #js-navigationList-MOUNTAIN, #js-navigationList-EBIKES, #js-navigationList-HYBRID-CITY, #js-navigationList-YOUNGHEROES"

# Extract the urls from the href attribute
bike_category_tbl <- html_home %>%
  
  # Select nodes by the ids
  html_nodes(css = family_id_css) %>%
  
  # Going further down the tree and select nodes by class
  # Selecting two classes makes it specific enough
  html_nodes(css = ".navigationListSecondary__listItem .js-ridestyles") %>%
  html_attr('href') %>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "subdirectory") %>%
  
  # Add the domain, because we will get only the subdirectories
  mutate(
    url = glue("https://www.canyon.com{subdirectory}")
  ) %>%
  
  # Some categories are listed multiple times.
  # We only need unique values
  distinct(url)
# 2.0 COLLECT BIKE DATA ----

# 2.1 Get URL for each bike of the Product categories

# select first bike category url
bike_category_url <- bike_category_tbl$url[1]

# Alternatives for selecting values
# bike_category_url <- bike_category_tbl %$% url %>% .[1]
# bike_category_url <- bike_category_tbl %>% pull(url) %>% .[1]
# bike_category_url <- deframe(bike_category_tbl[1,])
# bike_category_url <- bike_category_tbl %>% first %>% first

#xopen(bike_category_url)

# Get the URLs for the bikes of the first category
html_bike_category  <- read_html(bike_category_url)
bike_url_tbl        <- html_bike_category %>%
  
  # Get the 'a' nodes, which are hierarchally underneath 
  # the class productTile__contentWrapper
  html_nodes(css = ".productTile__contentWrapper > a") %>%
  html_attr("href") %>%
  
  # Remove the query parameters of the URL (everything after the '?')
  str_remove(pattern = "\\?.*") %>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "url")

# 2.1.2 Extract the descriptions (since we have retrieved the data already)
bike_desc_tbl <- html_bike_category %>%
  
  # Get the nodes in the meta tag where the attribute itemprop equals description
  html_nodes('.productTile__productSummaryLeft > meta[itemprop="description"]') %>%
  
  # Extract the content of the attribute content
  html_attr("content") %>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "description")
# 2.1.3 Get even more data from JSON files
bike_json_tbl  <- html_bike_category %>%
  
  html_nodes(css = '.productGrid__listItem.xlt-producttile > div') %>%
  html_attr("data-gtm-impression") %>%
  
  # Convert the JSON format to dataframe
  # map runs that function on each element of the list
  map(fromJSON) %>% # need JSON ### need lists
  
  # Extract relevant information of the nested list
  map(purrr::pluck, 2, "impressions") %>% # Need purrr and expl above
  
  # Set "not defined" and emtpy fields to NA (will be easier to work with)
  map(na_if, "not defined") %>%
  map(na_if, "") %>%
  
  # The class of dimension56 and price varies between numeric and char.
  # This converts this column in each list to numeric
  # across allows to perform the same operation on multiple columns
  map(~mutate(., across(c("dimension56","price"), as.numeric))) %>%
  
  # Stack all lists together
  bind_rows() %>%
  # Convert to tibble so that we have the same data format
  as_tibble() %>%
  
  # Add consecutive numbers so that we can bind all data together
  # You could have also just use bind_cols()
  rowid_to_column(var='position') %>%
  left_join(bike_desc_tbl) %>%
  left_join(bike_url_tbl)
# 2.2 Wrap it into a function ----
get_bike_data <- function(url) {
  
  html_bike_category <- read_html(url)
  
  # Get the URLs
  bike_url_tbl  <- html_bike_category %>%
    html_nodes(css = ".productTile__contentWrapper > a") %>%
    html_attr("href") %>%
    str_remove(pattern = "\\?.*") %>%
    enframe(name = "position", value = "url")
  
  # Get the descriptions
  bike_desc_tbl <- html_bike_category %>%
    html_nodes(css = '.productTile__productSummaryLeft > 
                      meta[itemprop="description"]') %>%
    html_attr("content") %>%
    enframe(name = "position", value = "description")
  
  # Get JSON data
  bike_json_tbl <- html_bike_category %>%
    html_nodes(css = '.productGrid__listItem.xlt-producttile > div') %>%
    html_attr("data-gtm-impression") %>%
    map(fromJSON) %>% # need JSON ### need lists
    map(purrr::pluck, 2, "impressions") %>% 
    map(na_if, "not defined") %>%
    map(na_if, "") %>%
    map(~mutate(., across(c("dimension56","price"), as.numeric))) %>%
    bind_rows() %>%
    as_tibble() %>%
    rowid_to_column(var='position') %>%
    left_join(bike_desc_tbl) %>%
    left_join(bike_url_tbl)
}

# Run the function with the first url to check if it is working
bike_category_url <- bike_category_tbl$url[1]
bike_data_tbl     <- get_bike_data(url = bike_category_url)

# 2.3.1a Map the function against all urls

# Extract the urls as a character vector
bike_category_url_vec <- bike_category_tbl %>% 
  pull(url)

# Run the function with every url as an argument
bike_data_lst <- map(bike_category_url_vec, get_bike_data)

# Merge the list into a tibble
bike_data_tbl <- bind_rows(bike_data_lst)
saveRDS(bike_data_tbl, "bike_data_tbl.rds")

# Check for duplicates
bike_data_cleaned_tbl %>%
  group_by(id) %>%
  filter(n()>1) %>%
  arrange(id) %>% 
  View()

# Filter non Canyon bikes (based on id length) and add an empty column for the colors
bike_data_cleaned_tbl <- bike_data_tbl %>%
  
  # Filter for bikes. Only unique ones
  filter(nchar(.$id) == 4) %>%
  filter(!(name %>% str_detect("Frameset"))) %>%
  distinct(id, .keep_all = T) %>%
  
  # Split categories (Speedmax had to be treated individually)
  mutate(category = replace(category, 
                            name == "Speedmax CF SLX 8.0 SL", "Road/Triathlon Bike/Speedmax")) %>%
  separate(col = category, into = c("category_1",
                                    "category_2",
                                    "category_3"),
           sep = "(?<!\\s)/(?!\\s)") %>%
  
  # Renaming
  rename("year"       = "dimension50") %>%
  rename("model"      = "name") %>%
  rename("gender"     = "dimension63") %>%
  rename("price_euro" = "metric4") %>%
  
  # Fix years manually (have checked the website)
  mutate(year = replace_na(year, 2021)) %>%
  
  # Add frame material
  mutate(frame_material = case_when(
    model %>% str_detect(" CF ") ~ "carbon",
    model %>% str_detect(" CFR ") ~ "carbon",
    TRUE ~ "aluminium"
  )
  ) %>%
  
  # Select and order columns
  select(-c(position, brand, variant, starts_with("dim"), 
            quantity, feedProductId, price, metric5)) %>%
  select(id, model, year, frame_material, price_euro, everything())

saveRDS(bike_data_cleaned_tbl, "bike_data_cleaned_tbl.rds")

# 3.1a Get all color variations for each bike

# Extract all bike urls
bike_url_vec <- bike_data_cleaned_tbl %>% 
  pull(url)

# Create function to get the variations
get_colors <- function(url) {
  
  url %>%
    
    read_html() %>%
    
    # Get all 'script nodes' and convert to char
    html_nodes(css = "script") %>%
    as.character() %>%
    
    # Select the node, that contains 'window.deptsfra'
    str_subset(pattern = "window.deptsfra") %>%
    
    # remove the chars that do not belong to the json
    # 1. replace at the beginning everything until the first "{" with ""
    str_replace("^[^\\{]+", "") %>%
    # 2. replace at the end everything after the last "}" with ""
    str_replace("[^\\}]+$", "") %>%
    
    # Convert from json to an r object and pick the relevant values
    fromJSON() %>%
    purrr::pluck("productDetail", "variationAttributes", "values", 1, "value")
}

# Run the function over all urls and add result to bike_data_cleaned_tbl
# This will take a long time (~ 20-30 minutes) because we have to iterate over many bikes
bike_data_colors_tbl <- bike_data_cleaned_tbl %>% 
  mutate(colors = map(bike_url_vec, get_colors))

saveRDS(bike_data_colors_tbl, "bike_data_colors_tbl.rds")

#8. Challenge of this section: ----
#8.1 Get some data from an API (I will use Pokemon API) ----
#8.1.1. Get all pokemon spezies

Pokemon_API <- function(path) {
  url <- modify_url(url = "https://pokeapi.co", path = glue("/api/v2/pokemon-species/{path}/"))
  resp_raw <- GET(url)
  stop_for_status(resp_raw) # automatically throws an error if a request did not succeed
  resp_JSON <- fromJSON(rawToChar(resp_raw$content), flatten) 
}
Pokemon_species <- Pokemon_API("5")
Pokemon_list <- as_tibble(Pokemon_species$results)


#8.2 Get the the model names, categories and prices of the bikes from rosebikes.de
# 8.2.1 COLLECT PRODUCT FAMILIES ----
  url_home          <- "https://www.rosebikes.de/fahrräder" 

  # Read in the HTML for the entire webpage
  html_home         <- read_html(url_home) 

  # Web scrape the ids for the families
  bike_family_cop <- html_home %>% 
  
  # Get the nodes for the families ...
  html_nodes(css = ".catalog-navigation__link") %>%
  # ...and extract the information of the id attribute
  html_attr('title') %>% 
    
  # Remove the product families SALE and BIKE-FINDER 
  discard(.p = ~stringr::str_detect(.x,"Sale|Bike-Finder")) %>%
    
  # Convert vector to tibble effectivly giving each entry a number
  enframe(name = "position", value = "family_class") %>%
    
  # Add a hashtag so we can get nodes of the categories by id (#)
  mutate(
    family_id = str_glue("#{family_class}")
  )
    
#8.2.2 COLLECT PRODUCT CATEGORIES URL ----
  
  # Combine all Ids to one string so that we will get all nodes at once
  # (seperated by the OR operator ",")
  #family_id_css <- bike_family_cop %>%
  #  pull(family_id) %>%
  #  stringr::str_c(collapse = ", ")
  
  # Extract the urls from the href attribute
  bike_category_cop <- html_home %>%
    
  # Select nodes by the ids
  html_nodes(css = ".catalog-navigation__link") %>%
  html_attr('href') %>%
  
  # Remove the product families SALE and BIKE-FINDER 
  discard(.p = ~stringr::str_detect(.x,"sale|zoovu")) %>%  
    
  # Convert vector to tibble
  enframe(name = "position", value = "subdirectory") %>%
    
  # Add the domain, because we will get only the subdirectories
  mutate(
    url = glue("https://www.rosebikes.de{subdirectory}")
  ) %>%
    
  # Some categories are listed multiple times.
  # We only need unique values
  distinct(url)
  
#8.2.3 GET THE MODEL NAMES FROM THE URLs ----
    #First Category
    # select first bike category url
    bike_model_url1 <- bike_category_cop$url[1]
  
    # Get the URLs for the bikes of the first category
    html_bike_model_cop1  <- read_html(bike_model_url1) %>% 
  
    # Select nodes by the ids
    html_nodes(css = ".catalog-category-bikes__title") %>%
    html_text('catalog-category-bikes__title-text') %>%  
    
    # Convert vector to tibble
    enframe(name = "position", value = "name")
    
    #2nd Category
    # select first bike category url
    bike_model_url2 <- bike_category_cop$url[2]
    
    # Get the URLs for the bikes of the first category
    html_bike_model_cop2  <- read_html(bike_model_url2) %>% 
      
    # Select nodes by the ids
    html_nodes(css = ".catalog-category-bikes__title") %>%
    html_text('catalog-category-bikes__title-text') %>%  
      
    # Convert vector to tibble
    enframe(name = "position", value = "name")    
  
#8.2.4 GET THE MODEL PRICES FROM THE URLs ----
  #First Category
  # select first bike category url
  bike_model_url1 <- bike_category_cop$url[1]
  
  # Get the URLs for the bikes of the first category
  html_bike_model_price_cop1  <- read_html(bike_model_url1) %>% 
      
  # Select nodes by the ids
  html_nodes(css = ".catalog-category-bikes__price") %>%
  html_text('catalog-category-bikes__price-title') %>%  
    
  # Remove the query parameters of the URL (everything after the '?')
  str_remove(pattern = "ab ") %>%
  str_remove(pattern = "oder ab.*") %>%
    
  # Convert vector to tibble
  enframe(name = "position", value = "name")
    
  #2nd Category
  # select first bike category url
  bike_model_url2 <- bike_category_cop$url[2]
    
  # Get the URLs for the bikes of the first category
  html_bike_model_price_cop2  <- read_html(bike_model_url2) %>% 
      
  # Select nodes by the ids
  html_nodes(css = ".catalog-category-bikes__price") %>%
  html_text('catalog-category-bikes__price-title') %>%  
  
  # Remove the query parameters of the URL (everything after the '?')
  str_remove(pattern = "ab ") %>%
  str_remove(pattern = "oder ab.*") %>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "name")    
  
  #Join the vectors together
  bike_cat1_cop <- left_join(html_bike_model_cop1,html_bike_model_price_cop1, by = c("position"="position"))
  bike_cat2_cop <- left_join(html_bike_model_cop2,html_bike_model_price_cop2, by = c("position"="position"))
  
  