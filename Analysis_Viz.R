# Load required libraries
library(readr)
library(tidyverse)
library(stringi)
library(ggplot2)

# Open dataset
df <- read_csv("github_df.csv")

# Wrangle - regex the columns
# Price column, delete irrelevant characters and periods, convert to numeric
df$price <- stri_sub(df$i_price, 49, -8)
df$price <- str_remove_all(df$price, "[.]") %>% as.numeric()

# Area column, delete irrelevant characters and periods)
df$area <- stri_sub(df$i_area, 49, -8)

# Split area column to district and subdistrict columns
df$subdistrict <- str_split_fixed(df$area, ",", 2)[,1] %>% toupper() # to upper to match shapefile format
df$district <- str_split_fixed(df$area, ",", 2)[,2] %>% as.factor()

# Name column, delete irrelevant characters and periods
df$name <- stri_sub(df$i_name, 46, -8)

# Make new dataframe to select interested columns and subset values
df_new <- subset(df, select = c(price, district, subdistrict, name))

# Based on my domain knowledge, room rentals at the minimum is Rp. 300,000 and would not exceed Rp. 7,500,000
df_new <- subset(df_new, price <= 7500000 & price > 300000)

# Recode minor typo and set districts outside Jakarta as NA
df_new$district <- recode(df_new$district, " Jakarta Utara" = "North Jakarta",
                          " Jakarta Selatan" = "South Jakarta",
                          " Jakarta Pusat" = "Central Jakarta",
                          " Jakarta Barat" = "West Jakarta",
                          " Jakarta Timur" = "East Jakarta",
                          .default = NA_character_)

# Drop NA rows in districts outside Jakarta
df_new <- df_new[!is.na(df_new$district), ]

# Next create viz
# Visualize (bar chart) the distribution of room rentals collected from webscraping
ggplot(df_new) +
  aes(x = district) +
  geom_bar(fill = "#31688e") +
  labs(x = "", y = "Frequency", title = "Distribution of Room Rentals in Jakarta", caption = "Data from https://www.olx.co.id/ by @kristasinga") +
  ggthemes::theme_economist() + theme(
    plot.title = element_text(size = 16, margin = margin(-10,0,20,0)),
    axis.text.x = element_text(size = 10, margin = margin(10,0,10,0)),
    axis.text.y = element_text(size = 10, margin = margin(10,10,10,20)),
    axis.title.y = element_text(size = 12, margin = margin(0,5,0,-10)),
    plot.margin = unit(c(1,1,1,1), "cm"))

# Visualize (boxplot) the median price of monthly room rental by district
ggplot(df_new) +
  aes(x = district, y = price/1000) +
  geom_boxplot(fill = "#31688e") +
  #geom_jitter(alpha = 0.2, color = "blue") +
  labs(x = "", y = "Price in Rupiah (000s/month)", title = "Median Price of Monthly Room Rental in Jakarta", caption = "Data from https://www.olx.co.id/ by @kristasinga") +
  ggthemes::theme_economist() + theme(
    plot.title = element_text(size = 16, margin = margin(-10,0,20,0)),
    axis.text.x = element_text(size = 10, margin = margin(20,0,10,0)), 
    axis.text.y = element_text(size = 10, margin = margin(10,10,10,20)),
    axis.title.y = element_text(size = 12, margin = margin(0,5,0,-10)),
    plot.margin = unit(c(1,1,1,1), "cm"))

# Visualize (chloropleth map) the median price of month room rental by subdistrict
# Load shapefile
library(sf)
dki <- st_read("dki_kelurahan.shp")

# Aggregate data to show subdistrict median price
subd_med <- aggregate(price ~ subdistrict, data = df_new, FUN = median)

# Join shapefile and subdistrict median price (subd_med)
dki <- dki %>% rename(subdistrict = Kecamatan) # rename coloumn to match the join condition
dki_new <- left_join(dki, subd_med, by = c("subdistrict" = "subdistrict"))

ggplot(dki_new) +
  aes(fill = price/1000) +
  geom_sf(size = 1L) +
  scale_fill_distiller(palette = "RdYlBu", na.value= "#7c8080") +
  labs(title = "Median Monthly Room Rental Price in Jakarta", caption = "Data from https://www.olx.co.id/ by @kristasinga", fill = "Price in Rupiah \n(000s/month)") +
  theme_void() +
  theme(legend.position = "right")
