# Title: Perdue Farms Data Analysis
# Author: Alexander Zakrzeski
# Date: June 7, 2023

# Import the necessary packages

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(forcats)

library(janitor)
library(scales)

# Load the "Transportation Management Shipment History" data:
  # Shows the who, when, and where of the shipments

tmsh1 <- read_excel("TMSH_Data.xlsx") 

# Load the "Financial Reporting" data:
  # Shows the costs of the shipments

fr1 <- read_excel("FR_Data.xlsx")

# Load the "Delivery Performance Scorecard" data:
  # Shows the on-time performance and any delay time at delivery for the 
  # shipments

dps1 <- read_excel("DPS_Data.xlsx")

# Load the "States" data:
  # Shows the full name of each state as well as its abbreviation

states1 <- read_excel("States_Data.xlsx")

# Perform the data cleaning steps:
  # This includes filtering, dropping columns and rows, sorting, etc.

tmsh2 <- tmsh1 %>%
  clean_names() %>%
  filter(carrier_name == "Perdue", 
         number_of_stops == 1) %>%
  select(-c(carrier_name, number_of_stops)) %>%
  drop_na(driver_number) %>%
  unite(pickup_location, 
        pickup_city, pickup_state, sep = ", ") %>%
  unite(pickup_timestamp, 
        pickup_depart_date, pickup_depart_time, sep = " ") %>%
  unite(dropoff_location, 
        dropoff_city, dropoff_state, sep = ", ") %>%
  mutate(across(c(shipment_number, driver_number), as.character),
         pickup_timestamp = ymd_hms(pickup_timestamp)) %>%
  arrange(shipment_number)

# Perform the data cleaning steps:
  # This includes modifying column names and values, etc.

fr2 <- fr1 %>%
  clean_names() %>%
  rename(shipment_number = gen3_shipment_number,
         dropoff_id = customer) %>%
  select(-gen5_location) %>%
  mutate(shipment_number = str_remove(shipment_number, "^S000")) %>%
  arrange(shipment_number, dropoff_id)

# Perform the data cleaning steps:
  # This includes concatenating columns, changing data types, etc.

dps2 <- dps1 %>%
  clean_names() %>%
  rename(shipment_number = load_number,
         dropoff_id = sold_to) %>%
  filter(carrier_name == "Perdue") %>%
  drop_na(actual_arrive_time) %>%
  mutate(sched_arrive_time = format(sched_arrive_time, format = "%H:%M:%S"),
         actual_arrive_time = format(actual_arrive_time, format = "%H:%M:%S"),
         empty_time = format(empty_time, format = "%H:%M:%S")) %>%
  unite(sched_arrive_timestamp, 
        sched_arrive_date, sched_arrive_time, sep = " ") %>%
  unite(actual_arrive_timestamp,
        actual_arrive_date, actual_arrive_time, sep = " ") %>%
  unite(empty_timestamp,
        empty_date, empty_time, sep = " ") %>% 
  mutate(across(c(shipment_number, dropoff_id), as.character),
         across(c(sched_arrive_timestamp, actual_arrive_timestamp, 
                  empty_timestamp), ymd_hms), 
         late = if_else( 
           actual_arrive_timestamp - sched_arrive_timestamp > "00:30:00", 
           "Yes", 
           "No"), 
         actual_hours = round(as.numeric(difftime( 
           empty_timestamp, actual_arrive_timestamp, units = "hours")), 2),  
         dt_hours = 1, 
         actual_cost = actual_hours * 65,
         dt_cost = 25) %>%
  select(-c(carrier_name, late_yes_no, held)) %>%
  arrange(shipment_number, dropoff_id)

# Perform the data cleaning steps:
  # This includes merging, selecting and relocating columns, etc.

merged1 <- tmsh2 %>%
  inner_join(fr2, by = c("shipment_number", "dropoff_id")) %>% 
  inner_join(dps2, by = c("shipment_number", "dropoff_id")) %>%
  select(pickup_location, 
         dropoff_location, 
         actual_arrive_timestamp, 
         late, 
         actual_hours, 
         dt_hours, 
         actual_cost, 
         dt_cost) %>%
  mutate(pickup_location = str_sub(pickup_location, start = -2), 
         dropoff_location = str_sub(dropoff_location, start = -2)) %>% 
  left_join(states1, by = c("pickup_location" = "abbreviation")) %>%
  relocate(state, .after = pickup_location) %>% 
  select(-pickup_location) %>%
  rename(pickup_location = state) %>% 
  left_join(states1, by = c("dropoff_location" = "abbreviation")) %>% 
  relocate(state, .after = dropoff_location) %>% 
  select(-dropoff_location) %>%
  rename(dropoff_location = state) %>%
  mutate(delivery_date = as.Date(actual_arrive_timestamp),
         delivery_time = format(actual_arrive_timestamp, "%p")) %>% 
  relocate(delivery_date, delivery_time, .after = actual_arrive_timestamp) %>%
  select(-actual_arrive_timestamp) %>%
  filter(month(delivery_date) %in% c(4, 5, 6)) %>%
  arrange(delivery_date) 

# Perform the data wrangling and visualization steps:
  # This includes reshaping data, grouping by columns, etc.
  # Creating a stacked bar chart

viz1 <- merged1 %>%
  mutate(delivery_month = month(delivery_date, label = TRUE, abbr = FALSE)) %>%
  pivot_longer(cols = c(actual_hours, dt_hours),
               names_to = "type",
               values_to = "hours") %>%
  group_by(delivery_month, type) %>%
  summarize(hours = sum(hours)) %>%
  mutate(type = if_else(
    type == "actual_hours",
    "In-Person",
    "Drop Trailer"),
    type = factor(type, levels = c("In-Person", "Drop Trailer"))) %>%
  ggplot(data = ., aes(x = delivery_month, y = hours, fill = type)) +
  geom_col(position = "dodge", width = 0.65) +
  geom_hline(yintercept = 0, size = 0.8, color = "gray") +
  geom_text(aes(label = paste(round(hours / 1000, 1), "K", sep = "")), 
            position = position_dodge(0.65), vjust = -1, hjust = 0.5, 
            size = 4.25) + 
  scale_y_continuous(label = comma, 
                     limits = c(0, 6000), breaks = seq(0, 6000, by = 1500)) +
  scale_fill_manual(values = c("#006AB8", "#FFC522")) + 
  labs(title = "Held Time in Q2 2021 by Delivery Type", 
       x = "", y = "Held Time (hours)") +
  guides(fill = guide_legend(title = "")) +
  theme_void() +
  theme(text = element_text(family = "Arial"),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        panel.grid.major.y = element_line(linetype = 1, size = 0.3, 
                                          color = "gray"),
        plot.title = element_text(margin = margin(0, 0, 15, 0), hjust = 0.5, 
                                  size = 15, face = "bold"),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0), angle = 90, 
                                    size = 13),
        axis.text = element_text(size = 13, color = "black"),
        legend.text = element_text(size = 13),
        legend.spacing.x = unit(0.25, "cm"),
        legend.key.size = unit(0.5, "cm"),
        legend.position = "top")

# Perform the data wrangling and visualization steps:
  # This includes reshaping data, grouping by columns, etc.
  # Creating a stacked bar chart

viz2 <- merged1 %>%
  mutate(delivery_month = month(delivery_date, label = TRUE, abbr = FALSE)) %>%
  pivot_longer(cols = c(actual_cost, dt_cost),
               names_to = "type",
               values_to = "cost") %>%
  group_by(delivery_month, type) %>%
  summarize(cost = sum(cost)) %>%
  mutate(type = if_else(
    type == "actual_cost",
    "In-Person",
    "Drop Trailer"),
    type = factor(type, levels = c("In-Person", "Drop Trailer"))) %>%
  ggplot(data = ., aes(x = delivery_month, y = cost, fill = type)) +
  geom_col(position = "dodge", width = 0.65) +
  geom_hline(yintercept = 0, size = 0.8, color = "gray") +
  geom_text(aes(label = paste(round(cost / 1000, 0), "K", sep = "")), 
            position = position_dodge(0.65), vjust = -1, hjust = 0.5, 
            size = 4.25) + 
  scale_y_continuous(labels = dollar_format(prefix = "$"), 
                     limits = c(0, 400000), breaks = seq(0, 400000, 
                                                         by = 100000)) +
  scale_fill_manual(values = c("#006AB8", "#FFC522")) + 
  labs(title = "Held Cost in Q2 2021 by Delivery Type", 
       x = "", y = "Held Cost") +
  guides(fill = guide_legend(title = "")) +
  theme_void() +
  theme(text = element_text(family = "Arial"),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        panel.grid.major.y = element_line(linetype = 1, size = 0.3, 
                                          color = "gray"),
        plot.title = element_text(margin = margin(0, 0, 15, 0), hjust = 0.5, 
                                  size = 15, face = "bold"),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0), angle = 90, 
                                    size = 13),
        axis.text = element_text(size = 13, color = "black"),
        legend.text = element_text(size = 13),
        legend.spacing.x = unit(0.25, "cm"),
        legend.key.size = unit(0.5, "cm"),
        legend.position = "top")

# Perform the data wrangling and visualization steps:
  # This includes grouping by columns, using aggregate functions, etc.
  # Creating a line graph

viz3 <- merged1 %>%
  mutate(delivery_month = month(delivery_date, label = TRUE, abbr = FALSE)) %>%
  group_by(delivery_month, delivery_time, late) %>%
  summarize(count = n()) %>%
  mutate(pct = count / sum(count)) %>%
  filter(late == "Yes") %>%
  ggplot(data = ., aes(x = delivery_month, y = pct, group = delivery_time, 
                       color = delivery_time)) +
  geom_line(size = 1.25) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 0.15, size = 0.8, color = "gray") +
  scale_y_continuous(labels = percent_format(), 
                     limits = c(0.15, 0.325), breaks = seq(0.15, 0.325, 
                                                           by = 0.05)) +
  scale_color_manual(values = c("#006AB8", "#FFC522")) + 
  labs(title = "Monthly Percentage of Late Deliveries in Q2 2021", 
       x = "", y = "Percentage") +
  guides(color = guide_legend(title = "")) +
  theme_void() +
  theme(text = element_text(family = "Arial"),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        panel.grid.major.y = element_line(linetype = 1, size = 0.3, 
                                          color = "gray"),
        plot.title = element_text(margin = margin(0, 0, 15, 0), hjust = 0.5, 
                                  size = 15, face = "bold"),
        axis.text = element_text(size = 13, color = "black"),
        legend.text = element_text(size = 13),
        legend.spacing.x = unit(0.25, "cm"),
        legend.key.size = unit(0.5, "cm"),
        legend.position = "top")

# Perform the data wrangling and visualization steps:
# This includes grouping by columns, using aggregate functions, etc.
# Creating a lollipop chart

viz4 <- merged1 %>%
  filter(late == "Yes") %>%
  group_by(dropoff_location) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  slice(1:10) %>%
  mutate(dropoff_location = fct_reorder(dropoff_location, count)) %>%
  ggplot(data = ., aes(x = dropoff_location, y = count)) +
  geom_segment(aes(x = dropoff_location, xend = dropoff_location, 
                   y = 0, yend = count), color = "black") +
  geom_point(color = "#006AB8", size = 4, alpha = 0.6) +
  labs(title = "Top States for Late Deliveries in Q3 2021", 
       x = "", y = "Count of Late Deliveries") +
  coord_flip() +
  theme_void() +
  theme(text = element_text(family = "Arial"),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        panel.grid.major.x = element_line(linetype = 1, size = 0.3, 
                                          color = "gray"), 
        plot.title = element_text(margin = margin(0, 0, 15, 0), hjust = 0.5, 
                                  size = 15, face = "bold"),
        axis.title.x = element_text(margin = margin(10, 0, 0, 0), size = 13),
        axis.text = element_text(size = 13, color = "black"))
