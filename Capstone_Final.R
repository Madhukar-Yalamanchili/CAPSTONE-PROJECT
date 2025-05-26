library(readr)
library(readxl)
library(tsibble)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lubridate)
library(janitor)
library(RColorBrewer)
install.packages("ggthemes")

##loading dataset
Pharma_sales <- read_csv(
  "/Users/madhu/Downloads/Pharama.csv", 
  show_col_types = FALSE
)

##Cleaning and Preprocessing Dataset:

## Removing empty rows in the dataset
Pharama_sales =Pharama_sales%>%remove_empty(which = c("rows", "cols"))

## Removing duplicate observations in the dataset
Pharama_sales =Pharama_sales%>% distinct()

## Removing NUll NA NAN from the dataset
Pharama_sales=na.omit(Pharama_sales)

## Cleaning names in the dataset
Pharama_sales=Pharama_sales%>% clean_names()

## Converting date from chr to Date format
Pharama_sales$date = as.Date(Pharama_sales$date, format = "%m/%d/%y")

##Identifying Top-Selling Categories:
library(dplyr)
library(ggplot2)

sp1 <- Pharama_sales %>%
  group_by(date, product_line) %>%
  summarise(quantity = sum(quantity, na.rm = TRUE), .groups = "drop") %>% 
  arrange(date)

ggplot(sp1, aes(x = date, y = quantity, fill = product_line)) +
  geom_area(alpha = 0.8) +
  scale_fill_brewer(palette = "Spectral") +
  theme_minimal() +
  ggtitle("Top-Selling Categories Over Time") +
  xlab("Date") +
  ylab("Total Quantity Sold") +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),  
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold")
  )


##Determining which day has most sales:
library(dplyr)
library(ggplot2)
library(scales)

sp3 <- Pharama_sales %>%
  janitor::clean_names()

sp3$date <- as.Date(sp3$date, format = "%m/%d/%y")
sp3$day <- weekdays(sp3$date)

day_sales <- sp3 %>%
  group_by(day) %>%
  summarise(total_sales = sum(quantity, na.rm = TRUE)) %>%
  mutate(day = factor(day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))


ggplot(day_sales, aes(x = day, y = total_sales, size = total_sales, color = total_sales)) +
  geom_point(alpha = 0.75) +
  geom_text(aes(label = total_sales, color = total_sales), vjust = -1.2, size = 3) +  # Color-coded labels
  scale_size_continuous(range = c(6, 18)) +
  scale_color_gradientn(colors = c("#56B1F7", "#4DAF4A", "#984EA3", "#FF7F00"), 
                        name = "Total Sales", labels = scales::comma) +
  facet_wrap(~day, ncol = 4) +
  theme_minimal() +
  labs(title = "Sales by Day of the Week", x = "Day", y = "Total Quantity Sold") +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(color = "grey30"),
    strip.text = element_text(size = 12, face = "bold"),
    panel.spacing = unit(1, "lines"),
    legend.position = "bottom",
    legend.title = element_text(),
    legend.text = element_text()
  )


##Determining the most profitable product category:
# Load necessary libraries
library(dplyr)
library(ggplot2)

# Data preparation
sp2 <- Pharama_sales %>%
  janitor::clean_names() %>%
  group_by(product_line) %>%
  summarise(gross_income = sum(gross_income, na.rm = TRUE)) %>%
  mutate(average_rating = runif(n(), min = 3, max = 5))  # Dummy variable

# Apply k-means clustering
clustering_data <- sp2 %>%
  select(gross_income, average_rating) %>%
  scale()

set.seed(123)  # For reproducibility
kmeans_result <- kmeans(clustering_data, centers = 3)
sp2$cluster <- as.factor(kmeans_result$cluster)

# Scatter plot
kmeans_plot <- ggplot(sp2, aes(x = gross_income, y = average_rating, color = cluster)) +
  geom_point(size = 5, alpha = 0.7) +
  labs(
    title = "K-Means Clustering of Product Lines",
    x = "Gross Income",
    y = "Average Rating",
    color = "Cluster"
  ) +
  theme_minimal()

# Display the plot
print(kmeans_plot)


##Customer satisfaction by branch
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(rpart)  # For decision tree
library(rpart.plot)  # For visualizing decision tree

# Data preparation
sp5 <- Pharama_sales %>%
  janitor::clean_names() %>%
  group_by(branch) %>%
  summarise(rating = mean(rating, na.rm = TRUE)) %>%
  mutate(branch_id = as.numeric(as.factor(branch)))  # Add branch IDs for modeling

# Decision Tree Model
tree_model <- rpart(rating ~ branch_id, data = sp5, method = "anova")

# Visualize the decision tree (optional)
rpart.plot(tree_model, type = 3, box.palette = "RdYlGn", shadow.col = "gray", main = "Decision Tree for Branch Ratings")

# Predict satisfaction categories
sp5$predicted_category <- cut(
  predict(tree_model, sp5),
  breaks = 3,  # Create 3 categories
  labels = c("Low", "Medium", "High")
)

# Scatter Plot for Decision Tree Results
decision_tree_plot <- ggplot(sp5, aes(x = branch, y = rating, color = predicted_category)) +
  geom_point(size = 5, alpha = 0.8) +  # Points for branches
  geom_text(aes(label = round(rating, 2)), vjust = -1, size = 4, fontface = "bold") +  # Show exact ratings
  scale_color_manual(
    values = c("Low" = "#FF6347", "Medium" = "blue", "High" = "#32CD32"),  # Custom colors for categories
    name = "Satisfaction Level"
  ) +
  labs(
    title = "Customer Satisfaction by Branch (Decision Tree)",
    x = "Branch",
    y = "Average Rating",
    color = "Satisfaction Level"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12),
    legend.position = "right"
  )

# Display the scatter plot
print(decision_tree_plot)


##Customer Trends and Insights
# Load necessary libraries
library(dplyr)
library(ggplot2)

# Data Preparation
sp6 <- Pharama_sales %>%
  filter(gender == "Male") %>%
  group_by(product_line) %>%
  summarise(totalquantity = sum(quantity)) %>%
  mutate(gender = "Male")

sp7 <- Pharama_sales %>%
  filter(gender == "Female") %>%
  group_by(product_line) %>%
  summarise(totalquantity = sum(quantity)) %>%
  mutate(gender = "Female")

sp8 <- bind_rows(sp6, sp7)

# Enhanced Bar Chart
ggplot(sp8, aes(x = product_line, y = totalquantity, fill = gender)) +
  geom_bar(
    position = position_dodge(width = 0.8), 
    stat = "identity", 
    alpha = 0.9
  ) +
  geom_text(
    aes(label = totalquantity), 
    position = position_dodge(width = 0.8), 
    vjust = -0.5, 
    size = 4
  ) +
  coord_flip() +  # Horizontal orientation for readability
  scale_fill_manual(
    values = c("Male" = "#1E90FF", "Female" = "#FF69B4")  # Modern color palette
  ) +
  labs(
    title = "Top-Selling Categories by Gender",
    subtitle = "Customer Trends and Insights",
    x = "Product Category",
    y = "Items Sold",
    fill = "Gender"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "right"
  )


#Forecasting the sales for next month
# Load necessary libraries
library(ggplot2)

# Data Preparation
months <- c(1, 2, 3, 4)
quantity <- c(1965, 1654, 1891, 1752)
sp111 <- data.frame(months, quantity)

# Create Curved Line Graph with Adjusted Loess Span
ggplot(sp111, aes(x = months, y = quantity)) +
  geom_point(size = 3, color = "#1E90FF") +  # Add points for data
  geom_smooth(method = "loess", se = FALSE, color = "#FF4500", size = 1.2, span = 1) +  # Larger span
  labs(
    title = "Monthly Sales Trend",
    subtitle = "Sales from January to April",
    x = "Month",
    y = "Sales Quantity"
  ) +
  scale_x_continuous(breaks = months, labels = c("Jan", "Feb", "Mar", "Apr")) +  # Custom x-axis labels
  theme_classic() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, face = "italic", hjust = 0.5),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12)
  )



