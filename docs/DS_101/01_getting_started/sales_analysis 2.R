# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----
  library(tidyverse)
  library(lubridate)
  library(writexl)
  #Read Excel Files
  library(readxl)

# 2.0 Importing Files ----
  bikes <- read_xlsx(path = "~/Documents/RStudio/DS_101/00_data/01_bike_sales/01_raw_data/bikes.xlsx")
  bike_shops <- read_xlsx(path = "~/Documents/RStudio/DS_101/00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")
  bike_orders <- read_xlsx(path = "~/Documents/RStudio/DS_101/00_data/01_bike_sales/01_raw_data/orderlines.xlsx")

# 3.0 Examining Data ----
  glimpse(bike_orders)

# 4.0 Joining Data ----
  bike_orderlines_joined <- bike_orders %>% left_join(bikes, by = c("product.id"="bike.id")) %>% left_join(bike_shops, by = c("customer.id" = "bikeshop.id"))
  bike_orderlines_joined %>% glimpse()
  #glimpse(bike_orderlines_joined)
  
# 5.0 Wrangling Data ----
  bike_orderlines_joined %>% 
  select(category) %>%
  filter(str_detect(category, "^Mountain")) %>% 
  unique()

# 6.0 Business Insights ----
  #Separate the joined category category into 3 separate columns
  bike_orderlines_joined <- separate(bike_orderlines_joined, col=category, into = c("cat.1","cat.2","cat.3"), sep = " - ")
  #Calculate the total price of the order
  bike_orderlines_joined <- mutate(bike_orderlines_joined, total.price = quantity*price)

  bike_orderlines_wrangled <- bike_orderlines_joined
  #Copied from the solution
  # 5.3 Optional: Reorganize. Using select to grab or remove unnecessary columns
  # 5.3.1 by exact column name
  bike_orderlines_wrangled %>% select(-...1, -gender) %>%
  
  # 5.3.2 by a pattern
  # You can use the select_helpers to define patterns. 
  # Type ?ends_with and click on Select helpers in the documentation
  select(-ends_with(".id")) %>%
  
  # 5.3.3 Actually we need the column "order.id". Let's bind it back to the data
  bind_cols(bike_orderlines_wrangled %>% select(order.id)) %>% 
  
  # 5.3.4 You can reorder the data by selecting the columns in your desired order.
  # You can use select_helpers like contains() or everything()
  select(order.id, contains("order"), contains("model"), contains("category"),
         price, quantity, total.price,
         everything()) %>%
  
  # 5.4 Rename columns because we actually wanted underscores instead of the dots
  # (one at the time vs. multiple at once)
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))
# 6.1 Sales by Year ----
  # Step 1 - Manipulate
  sales_by_year_tbl <- bike_orderlines_wrangled %>%
    
  # Select columns
  select(order.date, total.price) %>%
    
  # Add year column
  mutate(year = year(order.date)) %>%
    
  # Grouping by year and summarizing sales
  group_by(year) %>% 
  summarize(sales = sum(total.price)) %>%
    
  # Optional: Add a column that turns the numbers into a currency format 
  # (makes it in the plot optically more appealing)
  # mutate(sales_text = scales::dollar(sales)) <- Works for dollar values
  mutate(sales.text = scales::dollar(sales, big.mark = ".", 
                                   decimal.mark = ",", 
                                   prefix = "", 
                                   suffix = " €"))
  # Step 2 - Visualize
  sales_by_year_tbl %>%
  
  # Setup canvas with the columns year (x-axis) and sales (y-axis)
  ggplot(aes(x = year, y = sales)) +
  
  # Geometries
  geom_col(fill = "#2DC6D6") + # Use geom_col for a bar plot
  geom_label(aes(label = sales.text)) + # Adding labels to the bars
  geom_smooth(method = "lm", se = FALSE) + # Adding a trendline
  
  # Formatting
  # scale_y_continuous(labels = scales::dollar) + # Change the y-axis. 
  # Again, we have to adjust it for euro values
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title    = "Revenue by year",
    subtitle = "Upward Trend",
    x = "", # Override defaults for x and y
    y = "Revenue"
  )

# 6.2 Sales by Year and Category 2 ----

  # Step 1 - Manipulate
  sales_by_year_cat_1_tbl <- bike_orderlines_wrangled %>%
  
  # Select columns and add a year
  select(order.date, total.price, cat.1) %>%
  mutate(year = year(order.date)) %>%
  
  # Group by and summarize year and main catgegory
  group_by(year, cat.1) %>%
  summarise(sales = sum(total.price)) %>%
  ungroup() %>%
  
  # Format $ Text
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))
  # Step 2 - Visualize
  sales_by_year_cat_1_tbl %>%
  
  # Set up x, y, fill
  ggplot(aes(x = year, y = sales, fill = cat.1)) +
  
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot
  
  # Facet
  facet_wrap(~ cat.1) +
  
  # Formatting
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "Revenue by year and main category",
    subtitle = "Each product category has an upward trend",
    fill = "Main category" # Changes the legend name
  )


# 7.0 Writing Files ----

# 7.1 Excel ----
  bike_orderlines_wrangled %>%
    write_xlsx("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.xlsx")

# 7.2 CSV ----
  bike_orderlines_wrangled %>% 
    write_csv("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.csv")

# 7.3 RDS ----
  bike_orderlines_wrangled %>% 
    write_rds("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")


# 8.0 Challenge for section 2 ----
  #8.1 Which state sells the most of the bikes?
  bike_orderlines_bystate <- bike_orderlines_wrangled

  #Separate the joined location category into 2 separate columns
  bike_orderlines_bystate <- separate(bike_orderlines_wrangled, col=location, into = c("state","city"), sep = ", ")

  # Manipulate the data
  sales_by_state <- bike_orderlines_bystate
  
  # Select columns
  sales_by_state %>% 
  select(state, city, quantity) %>% 
    
  # Grouping by year and summarizing sales
  group_by(state) %>% 
  summarize(numberofsales = sum(quantity)) %>%

  # Step 2 - Visualize
    
  # Setup canvas with the columns year (x-axis) and sales (y-axis)
  ggplot(aes(x = state, y = numberofsales)) +
    
  # Geometries
  geom_col(fill = "#2DC6D6") + # Use geom_col for a bar plot
  geom_label(aes(label = numberofsales)) + # Adding labels to the bars
  geom_smooth(method = "lm", se = FALSE) + # Adding a trendline
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    
  # Formatting
  # scale_y_continuous(labels = scales::dollar) + # Change the y-axis. 
  # Again, we have to adjust it for euro values
  labs(
      title    = "Revenue by state",
      subtitle = "Upward Trend",
      x = "", # Override defaults for x and y
      y = "Revenue"
    )
  
  #8.2 Which state sells the most of the bikes in which year?
  sales_by_stateandyear <- bike_orderlines_bystate
  
  # Select columns
  sales_by_stateandyear %>% 
  select(state, city, quantity, order.date) %>% 
  mutate(year = year(order.date)) %>%
    
  # Group by and summarize year and main catgegory
  group_by(state,year) %>%
  summarise(numberofsales_2 = sum(quantity)) %>%
  ungroup() %>% 
    
  # Set up x, y, fill
  ggplot(aes(x = state, y = numberofsales_2, fill = year)) +
    
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot
    
  # Facet
  facet_wrap(~ year) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
      title = "Revenue by year and main category",
      subtitle = "Each product category has an upward trend",
      fill = "Main category" # Changes the legend name
    )
 