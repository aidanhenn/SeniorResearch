rm(list = ls())

# Install and load pacman
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
library(pacman)

# Use pacman to install and load packages
p_load(openxlsx, readxl, summarytools, dplyr, corrplot, randomForest, ggplot2)

file_path = "C://Users//aidan//OneDrive//Desktop//CSC450_SeniorResearchSurvey.xlsx"


surveyData <- read_excel(file_path)

# Rename column names so they are easier to work with

newNames <- c('TIME','CONSENT','FAMILIARITY','OPTIMISM','INFO_SOURCE','PREPAREDNESS', 'WORK_EXP_YN','WORK_YRS','ENCOUNTERS', 'USED_AI_YN',
              'AI_TOOLS_USED', 'TOP_SKILLS', 'CONCERN', 'AI_FOR_LEARNING', 'AI_FOR_TASKS', 'COMPLEX_PROBS', 'PROS_CONS', 'LIMITATIONS',
              'HEALTHCARE', 'EDUCATION', 'TECHNOLOGY', 'RETAIL', 'MANUFACTURING', 'HARM_TO_CS_CAREERS', 'RAISE_BAR', 'AI_BENEFIT', 'AI_DRAWBACK', 'EXTRA')
colnames(surveyData) <- newNames

# Clean formatting in work yrs col
surveyData$WORK_YRS[is.na(surveyData$WORK_YRS)] <- 0
surveyData$WORK_YRS <- as.numeric(gsub("[^0-9.]", "", as.character(surveyData$WORK_YRS)))

### EXPLORING THE DATA ###


#****************

# Create correlation matrix

selected_columns <- surveyData[, c("WORK_YRS","FAMILIARITY","OPTIMISM","PREPAREDNESS", "CONCERN","AI_FOR_LEARNING", "AI_FOR_TASKS","COMPLEX_PROBS", "PROS_CONS")]

cor_matrix <- cor(selected_columns)

cor_matrix
corrplot(cor_matrix, method = "color")


########

summary(surveyData)

# Comparing Familiarity to Optimism
correlation <- cor(surveyData$FAMILIARITY, surveyData$OPTIMISM)

ggplot(surveyData, aes(x = FAMILIARITY, y = OPTIMISM)) +
  geom_point(color = "steelblue", size = 3, shape = 16) +

  # Add regression line
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed", size = 1.5) +

  theme_minimal() +
  labs(
    title = "Individuals Optimism about AI based on their Familiarity",
    subtitle = paste("r =", round(correlation, 3)),
    x = "Familiarity (1-10)",
    y = "Optimism (1-10)"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )


### Familiarity and Concern
correlation <- cor(surveyData$FAMILIARITY, surveyData$CONCERN)

ggplot(surveyData, aes(x = FAMILIARITY, y = CONCERN)) +
  geom_point(color = "steelblue", size = 3, shape = 16) +

  # Add regression line
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed", size = 1.5) +
  theme_minimal() +
  labs(
    title = "Individuals Concern about AI based on their Familiarity",
    subtitle = paste("r =", round(correlation, 3)),
    x = "Familiarity (1-10)",
    y = "Concern (1-10)"
  ) +

  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

##### work years and concern
correlation <- cor(surveyData$WORK_YRS, surveyData$CONCERN)

ggplot(surveyData, aes(x = jitter(WORK_YRS), y = CONCERN)) +
  geom_point(color = "steelblue", size = 3, alpha = 0.7) +

  # Add smoothed regression line
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed", size = 1.5) +

  labs(title = "Concern vs Work Years",
       subtitle = paste("r =", round(correlation, 3)),
       x = "Work Years",
       y = "Concern") +

  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        legend.position = "none") +
  annotate("text", x = max(surveyData$WORK_YRS), y = concern_mean, label = "",
           color = "red", hjust = -0.1, vjust = 1.5, size = 4)

### Work years and complex probs

correlation <- cor(surveyData$WORK_YRS, surveyData$COMPLEX_PROBS)

ggplot(surveyData, aes(x = jitter(WORK_YRS), y = COMPLEX_PROBS)) +
  geom_point(color = "steelblue", size = 3, shape = 16) +

  # Add smoothed regression line
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed", size = 1.5) +

  labs(
    title = "Confidence in AI Solving Complex Problems based on YOE",
    subtitle = paste("r =", round(correlation, 3)),
    x = "Year of Industry Experience",
    y = "Confidence in Solving Complex Problems"
  ) +

  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# Bar graph showing familiarity with AI
category_counts <- surveyData %>%
  count(FAMILIARITY)

ggplot(category_counts, aes(x = FAMILIARITY, y = n)) +
  geom_bar(stat = "identity",fill = "steelblue") +
  labs(title = "Individuals Familiarity With AI (1-10)",
       x = "Familiarity (1-10)",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Bar graph showing respondents years of experience
category_counts <- surveyData %>%
  count(WORK_YRS)

ggplot(category_counts, aes(x = WORK_YRS, y = n)) +
  geom_bar(stat = "identity",fill = "steelblue") +
  labs(title = "Individuals Years in Tech Industry",
       x = "Years",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



####  COUNTING AI LIMITATIONS ####

# Split categories by commas
split_categories <- strsplit(surveyData$LIMITATIONS, ", ")

category_counts <- list()
# Count number of occurences of each limitation
for (split_category in split_categories) {
  for (category in split_category) {
    if (category %in% names(category_counts)) {
      category_counts[[category]] <- category_counts[[category]] + 1
    } else {
      category_counts[[category]] <- 1
    }
  }
}
# Bar graph for counts of each limitaiton
category_df <- data.frame(Category = names(category_counts), Count = unlist(category_counts))
barplot(category_df$Count, names.arg = category_df$Category, col = "steelblue",
        main = "Total Counts for Each AI Limitation Category",
        xlab = "AI Limitation Category", ylab = "Count")

ggplot(category_df, aes(x = Category, y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Total Counts for Each AI Limitation",
       x = "Category", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#### AI TOOLS USED ####

split_categories <- strsplit(surveyData$AI_TOOLS_USED, ", ")
category_counts <- list()
for (split_category in split_categories) {
  for (category in split_category) {
    if (category %in% names(category_counts)) {
      category_counts[[category]] <- category_counts[[category]] + 1
    } else {
      category_counts[[category]] <- 1
    }
  }
}

filtered_category_counts <- category_counts[sapply(category_counts, function(x) x >= 2)]
category_df <- data.frame(Category = names(filtered_category_counts), Count = unlist(filtered_category_counts))

ggplot(category_df, aes(x = Category, y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Total Counts for Each AI Tool",
       x = "Tool", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#### SOURCE FOR AI ####

split_categories <- strsplit(surveyData$INFO_SOURCE, ", ")
category_counts <- list()
for (split_category in split_categories) {
  for (category in split_category) {
    if (category %in% names(category_counts)) {
      category_counts[[category]] <- category_counts[[category]] + 1
    } else {
      category_counts[[category]] <- 1
    }
  }
}

filtered_category_counts <- category_counts[sapply(category_counts, function(x) x >= 2)]
category_df <- data.frame(Category = names(filtered_category_counts), Count = unlist(filtered_category_counts))
category_df[category_df$Category == 'Peer-reviewed articles/journals', "Category"] <- "Articles / Journals"

ggplot(category_df, aes(x = Category, y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Where Individuals Most Commonly Encounter AI",
       x = "Source", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# TOP SKILLS

split_categories <- strsplit(surveyData$TOP_SKILLS, ", ")
category_counts <- list()
for (split_category in split_categories) {
  for (category in split_category) {
    if (category %in% names(category_counts)) {
      category_counts[[category]] <- category_counts[[category]] + 1
    } else {
      category_counts[[category]] <- 1
    }
  }
}

filtered_category_counts <- category_counts[sapply(category_counts, function(x) x >= 2)]
category_df <- data.frame(Category = names(filtered_category_counts), Count = unlist(filtered_category_counts))

ggplot(category_df, aes(x = Category, y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top Skills to Leverage AI's Potential",
       x = "Skill", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

median(surveyData$PROS_CONS)
