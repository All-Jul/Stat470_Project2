library(readxl)
library(dplyr)
library(plyr)
library(ggplot2)
path_to_file <- "/Users/allan/Downloads/Passengers.xlsx"
titanic <- read_xlsx(path_to_file)
library(car)
library(viridis)
library(ggridges)

```

```{r}
ggplot(titanic, aes(x = Age, y = factor(Class), fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "GRE score", option = "C") +
  labs(title = "Distribution of Age by travel Class",
       x = "Age of Passenger",
       y = "Travelling class") +
  theme_ridges() +
  theme(legend.position = "none")
```

```{r}
## Visualization 1: Age Distribution by Survival Status with Facets for Class and Sex
ggplot(titanic, aes(x = Age, fill = Survived)) +
  geom_density(alpha = 0.6) +
  facet_grid(Sex ~ Class, scales = "free_y") +
  labs(title = "Age Distribution by Survival Status, Class and Sex",
       subtitle = "Titanic Passenger Demographics",
       x = "Age", y = "Density", fill = "Survived") +
  theme_minimal() +
  scale_fill_manual(values = c("#e41a1c", "#4daf4a"))

## Visualization 2: Survival Rate by Age with LOESS Smoothing
ggplot(titanic, aes(x = Age, y = as.numeric(Survived == "Yes"), color = Sex)) +
  geom_point(position = position_jitter(height = 0.02, width = 0), alpha = 0.3) +
  geom_smooth(method = "loess", se = TRUE) +
  facet_wrap(~Class, nrow = 1) +
  labs(title = "Survival Probability by Age Across Passenger Classes",
       subtitle = "With LOESS smoothing by Sex",
       x = "Age", y = "Survival Probability", color = "Sex") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

## Visualization 3: Age Distribution by Class with Boxplots and Violins
```


```{r}


# Creative Visualization: Survival Landscape
ggplot(titanic, aes(x = Age, y = Class, height = after_stat(density), 
                    fill = Survived, color = Survived)) +
  geom_density_ridges(alpha = 0.6, scale = 1.2, stat = "density", 
                      panel_scaling = FALSE, size = 0.8) +
  scale_fill_manual(values = c("#8c510a", "#01665e")) +
  scale_color_manual(values = c("#543005", "#003c30")) +
  labs(
    title = "TITANIC SURVIVAL LANDSCAPE",
    subtitle = "Age distributions as 'mountain ranges' for each class\nPeaks show demographic concentrations",
    x = "Age", 
    y = "Passenger Class",
    caption = "Color valleys: Perished | Color peaks: Survived"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray30"),
    legend.position = "bottom",
    panel.grid.major.y = element_line(color = "gray90"),
    axis.text.y = element_text(face = "bold")
  ) +
  # Add creative annotations
  annotate("text", x = 5, y = 3.3, label = "CHILDREN IN 3RD CLASS\nLOWER SURVIVAL", 
           color = "#8c510a", size = 3.5, fontface = "bold") +
  annotate("curve", x = 5, xend = 7, y = 3.2, yend = 2.8, 
           curvature = -0.2, arrow = arrow(length = unit(2, "mm")), color = "#8c510a") +
  annotate("text", x = 50, y = 0.7, label = "ELDERLY 1ST CLASS\nHIGHER SURVIVAL", 
           color = "#01665e", size = 3.5, fontface = "bold") +
  annotate("curve", x = 50, xend = 55, y = 0.8, yend = 1.2, 
           curvature = 0.2, arrow = arrow(length = unit(2, "mm")), color = "#01665e")
```
```{r}
ggplot(titanic, aes(x = Age, y = factor(Class), color = Survived)) +
  geom_jitter(alpha = 1, width = 0, height = 0.2) +
  scale_color_manual(values = c("#e63946", "#2a9d8f")) +
  labs(
    title = "The Age Survival Gap on the Titanic",
    subtitle = "Each dot represents a passenger. Black bars show median age",
    x = "Age",
    y = "Passenger Class",
    color = "Outcome",
    caption = "Key insight: Survival bias toward children in 1st/2nd class, but not 3rd"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_line(color = "grey90")
  )
```
```{r}
ggplot(titanic, aes(x = Age, y = Survived, fill = Survived)) +
  geom_violin(alpha = 0.7, scale = "width") +
  geom_boxplot(width = 0.2, fill = "white", alpha = 0.7) +
  facet_grid(Sex ~ Class, scales = "free_y") +
  scale_fill_manual(values = c("#e63946", "#2a9d8f")) +  # Red = Died, Green = Survived
  labs(
    title = "Survival Bias: Women Had Dramatically Higher Survival Rates",
    subtitle = "Age distribution of survivors vs. deceased, by sex and class",
    x = "Age",
    y = "Outcome",
    caption = "Key Insight: Nearly all women in 1st/2nd class survived, regardless of age."
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend (color is obvious)
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold")  # Bold facet labels
  )
```


```{r}
# Load required libraries
library(dplyr)
library(ggplot2)
library(multcomp)  # For Tukey HSD test
library(readxl)    # For reading Excel files

# Read your data file
path_to_file <- "/Users/allan/Downloads/Passengers.xlsx"
titanic <- read_xlsx(path_to_file)

# Clean and prepare the data
titanic_clean <- titanic %>%
  # Ensure proper column names (case sensitive)
  rename(Class = Class, Age = Age) %>%
  # Remove rows with missing age values
  filter(!is.na(Age)) %>%
  # Convert Class to factor if it isn't already
  mutate(Class = factor(Class))

# Check the structure of your data
str(titanic_clean)

## Verify ANOVA assumptions
# 1. Normality check (Q-Q plot)
qqnorm(titanic_clean$Age)
qqline(titanic_clean$Age)

# 2. Homogeneity of variance (Levene's Test)
car::leveneTest(Age ~ Class, data = titanic_clean)

## Perform one-way ANOVA
age_anova <- aov(Age ~ Class, data = titanic_clean)
summary(age_anova)

## If ANOVA is significant, proceed with Tukey's HSD
tukey_results <- TukeyHSD(age_anova)
print(tukey_results)

## Visualize the results
# Convert to dataframe for plotting
tukey_df <- as.data.frame(tukey_results$Class)
tukey_df$comparison <- rownames(tukey_df)

# Create the plot
ggplot(tukey_df, aes(x = comparison, y = diff, ymin = lwr, ymax = upr)) +
  geom_pointrange(size = 1, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Tukey HSD Test: Age Differences Between Passenger Classes",
       subtitle = "95% family-wise confidence level",
       x = "Class Comparison",
       y = "Difference in Mean Age (years)") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

library(plyr)
#titanic$Survived <- revalue(titanic$Survived, c('Yes'=1, 'No'=0)) 

#titanic$Survived = as.numeric(titanic$Survived) 
#titanic$Class = as.factor(titanic$Class)
model <- glm(Survived ~  Age + Class , family="binomial", data=titanic) 

summary(model) 

```{r}
newdata1 <- with(titanic, data.frame(Age = mean(Age), Class = factor(1:3)))

## view data frame
newdata1
```

```{r}
newdata1$Survival_Prob <- predict(model, newdata = newdata1, type = "response")
newdata1
```

```{r}
# We can do something very similar to create a table of predicted probabilities varying the value of gre and rank. 
# We are going to plot these, so we will create 100 values of gre between 200 and 800, at each value of rank (i.e., 1, 2, 3, and 4).
newdata2 <- with(titanic, 
                data.frame(
                  Age = rep(seq(from = min(Age, na.rm = TRUE), 
                               to = max(Age, na.rm = TRUE), 
                               length.out = 43), 3),
                  Class = factor(rep(1:3, each = 43))
                ))

# Verify the structure

newdata2
```

```{r}
newdata3 <- cbind(newdata2, predict(model, newdata = newdata2, type = "link",
    se = TRUE))
newdata3 <- within(newdata3, {
    PredictedProb <- plogis(fit)
    LL <- plogis(fit - (1.96 * se.fit))
    UL <- plogis(fit + (1.96 * se.fit))
})

## view first few rows of final dataset
head(newdata3)
```

```{r}
# Load the ggplot2 for the plot
library(ggplot2)
# we make a plot with the predicted probabilities, and 95% confidence intervals.
ggplot(newdata3, aes(x = Age, y = PredictedProb)) + geom_ribbon(aes(ymin = LL,
    ymax = UL, fill = Class), alpha = 0.2) + geom_line(aes(colour = Class),
    size = 1)
```

ggplot(data = Passengers, aes(x = Sex)) + 

  geom_bar() + 

  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) + 

  labs( 

    title = "Count of Passengers of each Sex", 

    x = "Sex", 

    y = "Count" 

  ) + 

  theme_light() 

 

ggplot(data = Passengers, aes(x = Class)) + 

  geom_bar() + 

  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) + 

  labs( 

    title = "Count of Passengers for each Class", 

    x = "Class", 

    y = "Count" 

  ) + 

  theme_light() 
