su1 <- rio::import("~/Downloads/osfstorage-archive (9)/Survey 1/survey1_with_codebook.xlsx")

library(tidyverse)
su1l <- su1 %>% pivot_longer(cols = claim1_belief:claim26_evidence, values_transform = as.character) %>%
  separate(name, into = c("claim", "name"))

library(UpSetR)

comma_separated_to_columns <- function(df, col) {
  colname <- deparse(substitute(col))
  df$splitcol <- df %>% pull(colname)
  separate_rows(df, splitcol, convert = TRUE, sep = ", ") %>%
    mutate(splitcol = if_else(is.na(splitcol), "no",
                              if_else(splitcol == "" |
                                        splitcol %in% c(), "included", as.character(splitcol)))) %>%
    mutate(#splitcol = stringr::str_c(colname, "_", splitcol),
      value = 1) %>%
    spread(splitcol, value, fill = 0) %>%
    select(-colname)
}


max_endorsed <- su1l %>% filter(name == "evidence") %>%
  mutate(row = 1:n()) %>%
  # slice(1:2, 19) %>%
  separate_longer_delim(value, delim = ",") %>%
  mutate(value = coalesce(as.numeric(value), 0)) %>%
  group_by(row) %>%
  filter(value == max(value)) %>%
  ungroup()


# --- Configuration: Colors and Labels for Evidence Levels ---
# These are the labels you provided for your 'value' column (1-6)
evidence_labels <- c(
  `0` = "Didn't rate",
  `1` = "No evidence, only opinions, perspectives, general theory or anecdotes (1)",
  `2` = "Some correlational evidence (laboratories, surveys, online, field) (2)",
  `3` = "Some causal evidence but in limited settings (laboratories, surveys, and online, self-reported measures) (3)",
  `4` = "Causal evidence in a field study (4)",
  `5` = "Replicated causal evidence from field studies (5)",
  `6` = "Wide-scale causal evidence from multiple field studies, policy evaluations or other natural settings (6)"
)

# Colors matched to your 6 levels, from lightest (level 1) to darkest (level 6)
# These are picked from the provided image's legend
evidence_colors <- c(
  `0` = "gray",
  `1` = "#FFFFE0", # None (e.g., opinion)
  `2` = "#BCE2CB", # Correlational
  `3` = "#A8D8D0", # Causal limited
  `4` = "#66A9AE", # Causal field
  `5` = "#2E758C", # Replicated causal field
  `6` = "#2A2A5E"  # Wide-scale causal
)


# --- 1. Define Claims in Order of Appearance (Top to Bottom) ---
# Extracted from the image
claims_ordered_text <- c(
  "9. Sleep deprivation can reduce mental health",
  "15. Social deprivation can reduce mental health",
  "1. Adolescent mental health has declined",
  "13. Behavioural addiction can reduce mental health",
  "7. Childhood shifted from play to phones",
  "8. Social media can impair sleep",
  "17. Social media increases visual social comparison in girls",
  "20. Social media increases mental disorder exposure in girls",
  "12. Social media can cause behavioural addiction",
  "10. Social media can fragment attention",
  "4. Decline in Anglosphere mental health",
  "21. Social media increases predation/harassment in girls",
  "26. Phone-free schools would benefit mental health",
  "24. No smartphones before high school would benefit mental health",
  "3. Larger mental health decline in girls than boys",
  "16. Girls use visual social media more than boys",
  "14. Social media can cause social deprivation",
  "6. Decline in Western Europe mental health",
  "23. Most US parents would delay smartphones in children",
  "2. Mental health in girls declined in 2010s",
  "25. Social media age limit would improve mental health",
  "19. Social media increases relational aggression in girls",
  "18. Social media increases perfectionism in girls",
  "11. Attention fragmentation can reduce mental health",
  "5. Decline in Nordic mental health",
  "22. One third of college students prefer social media to not exist"
)

# Optional: Create a data frame of claims if you need numbers separately for other purposes
# For this plot, we primarily use the ordered text vector.
claims_df <- data.frame(
  claim_number = as.integer(sub("\\..*", "", claims_ordered_text)),
  claim_text = sub("^\\d+\\.\\s*", "", claims_ordered_text),
  full_claim_text = claims_ordered_text
)

# --- 3. Process Data for Plotting ---
# Count responses for each claim and evidence level
plot_data <- max_endorsed %>%
  group_by(claim, value) %>%
  summarise(n = n(), .groups = 'drop') %>%
  # Calculate proportion for 100% stacked bar
  group_by(claim) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup()

plot_data$value <- factor(plot_data$value, levels = 0:6)

plot_data <- plot_data %>% mutate(claim_number = as.numeric(str_sub(claim, 6))) %>% left_join(claims_df, by = c("claim_number"= "claim_number"))

ggplot(plot_data, aes(x = proportion, y = full_claim_text, fill = value)) +
  geom_col() + # geom_col is equivalent to geom_bar(stat="identity")
  scale_x_continuous(
    labels = scales::percent_format(),
    expand = c(0, 0.01) # Remove padding on the left, slight padding on right
  ) +
  scale_fill_manual(
    name = "Evidence:", # Legend title
    values = evidence_colors,
    labels = evidence_labels,
    guide = guide_legend(reverse = TRUE) # Show highest evidence at top of legend
  ) +
  labs(
    title = "Maximum level of evidence endorsed",
    x = NULL, # X-axis title (percentage is clear)
    y = NULL  # Y-axis title (claim labels are clear)
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b=15)),
    panel.grid.major.x = element_line(colour = "grey90"), # Keep horizontal major grid
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(), # Remove vertical grid lines
    panel.grid.minor.y = element_blank(),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    axis.text.y = element_text(hjust = 0) # Align y-axis labels to the left
  )


plot_data <- max_endorsed %>%
  filter(value != 0) %>%
  group_by(claim, value) %>%
  summarise(n = n(), .groups = 'drop') %>%
  # Calculate proportion for 100% stacked bar
  group_by(claim) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup()

plot_data$value <- factor(plot_data$value, levels = 0:6)

plot_data <- plot_data %>% mutate(claim_number = as.numeric(str_sub(claim, 6))) %>% left_join(claims_df, by = c("claim_number"= "claim_number"))

ggplot(plot_data, aes(x = proportion, y = full_claim_text, fill = value)) +
  geom_col() + # geom_col is equivalent to geom_bar(stat="identity")
  scale_x_continuous(
    labels = scales::percent_format(),
    expand = c(0, 0.01) # Remove padding on the left, slight padding on right
  ) +
  scale_fill_manual(
    name = "Evidence:", # Legend title
    values = evidence_colors,
    labels = evidence_labels,
    guide = guide_legend(reverse = TRUE) # Show highest evidence at top of legend
  ) +
  labs(
    title = "Maximum level of evidence endorsed",
    x = NULL, # X-axis title (percentage is clear)
    y = NULL  # Y-axis title (claim labels are clear)
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b=15)),
    panel.grid.major.x = element_line(colour = "grey90"), # Keep horizontal major grid
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(), # Remove vertical grid lines
    panel.grid.minor.y = element_blank(),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    axis.text.y = element_text(hjust = 0) # Align y-axis labels to the left
  )

su1l %>% filter(name == "evidence") %>%
  mutate(row = 1:n()) %>%
  slice(1:2, 19) %>%
  separate_longer_delim(value, delim = ",") %>%
  mutate(value = coalesce(as.numeric(value), 0)) %>%
  mutate(x = 1) %>%
  pivot_wider(names_from = value, values_from = x, values_fill = 0)

su1l %>% filter(name == "evidence") %>%
  group_by(value) %>%
  summarise(n()) %>%
  knitr::kable()

su1l %>% filter(name == "evidence") %>%
  group_by(value) %>%
  summarise(n()) %>% View


su1l %>% filter(name == "evidence") %>%
  group_by(value) %>%
  summarise(n()) %>%
  filter(str_detect(value, "1,")) %>%
  knitr::kable()

su1l %>% filter(name == "evidence") %>%
  filter(claim == "claim17") %>%
  drop_na() %>%
  mutate(total = n()) %>%
  group_by(str_detect(value,"(4|5|6)")) %>%
  summarise(n(), round(n()/first(total)*100)) %>%
  knitr::kable()


su4 <- rio::import("~/Downloads/osfstorage-archive (9)/Survey 4/survey4_with_codebook.xlsx", n_max = 67)
su4e <- rio::import("~/Downloads/osfstorage-archive (9)/Survey 4/survey4_with_codebook.xlsx", skip = 68)

names(su4e) <- names(su4)
su4 <- bind_rows(su4, su4e)

su4 <- su4 %>% mutate(id = row_number())
table(su4$accuracy10)

str(su4)


# --- 0. Load necessary packages ---
# install.packages(c("dplyr", "tidyr", "ggplot2", "scales", "patchwork"))
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(patchwork)

# --- 1. Simulate your su4 data (based on the str output) ---
# This is for reproducibility. Replace this with loading your actual su4 data.
set.seed(123)
n_obs <- 131 # Number of observations from your str output
accuracy_col_numbers <- 1:26 # For accuracy1 to accuracy26
accuracy_cols_su4 <- paste0("accuracy", accuracy_col_numbers)

# --- 2. Define Claims (Order of Appearance and Mapping) ---
claims_ordered_text <- c(
  "9. Sleep deprivation can reduce mental health",
  "15. Social deprivation can reduce mental health",
  "1. Adolescent mental health has declined",
  "13. Behavioural addiction can reduce mental health",
  "7. Childhood shifted from play to phones",
  "8. Social media can impair sleep",
  "17. Social media increases visual social comparison in girls",
  "20. Social media increases mental disorder exposure in girls",
  "12. Social media can cause behavioural addiction",
  "10. Social media can fragment attention",
  "4. Decline in Anglosphere mental health",
  "21. Social media increases predation/harassment in girls",
  "26. Phone-free schools would benefit mental health",
  "24. No smartphones before high school would benefit mental health",
  "3. Larger mental health decline in girls than boys",
  "16. Girls use visual social media more than boys",
  "14. Social media can cause social deprivation",
  "6. Decline in Western Europe mental health",
  "23. Most US parents would delay smartphones in children",
  "2. Mental health in girls declined in 2010s",
  "25. Social media age limit would improve mental health",
  "19. Social media increases relational aggression in girls",
  "18. Social media increases perfectionism in girls",
  "11. Attention fragmentation can reduce mental health",
  "5. Decline in Nordic mental health",
  "22. One third of college students prefer social media to not exist"
)


# Full text for each claim (extracted from your document)
# Note: The text here is what *follows* "Claim X. " in your document.
full_claim_definitions <- c(
  "1" = "Over the last two decades, there has been a decline in mental health among adolescents in the USA.",
  "2" = "The decline in mental health among girls in the USA began in the early 2010s.",
  "3" = "The decline in mental health among girls in the USA since the early 2010s is more pronounced than the decline among boys during the same period.",
  "4" = "Over the last two decades, there has been a decline in mental health among adolescents in the Anglosphere (Australia, Canada, Ireland, UK, New Zealand).",
  "5" = "Over the last two decades, there has been a decline in mental health among adolescents in the Nordic countries\n(e.g., Denmark, Finland, Iceland, Norway, Sweden).",
  "6" = "Over the last two decades, there has been a decline in mental health among adolescents in Western Europe overall, although with variation across countries.",
  "7" = "Play-based childhood has shifted towards phone-based childhood (i.e., time with friends and total time playing away from screens has decreased).",
  "8" = "Heavy daily use of smartphones and social media can cause sleep deprivation.",
  "9" = "Chronic sleep deprivation can cause a decline in mental health.", # Note: Original plot and document imply claim 9 might use a colon
  "10" = "Heavy daily use of smartphones and social media can cause attention fragmentation.",
  "11" = "Attention fragmentation can cause a decline in mental health (possibly through mediating factors such as its negative impact on social relationships).",
  "12" = "Heavy daily use of smartphones and social media can cause behavioral addiction.",
  "13" = "Behavioral addiction can cause a decline in mental health.",
  "14" = "Heavy daily use of smartphones and social media can cause social deprivation, such as isolation and lack of formative social experiences.",
  "15" = "Chronic social deprivation can cause a decline in mental health.",
  "16" = "Adolescent girls use visual social media platforms (e.g., TikTok and Instagram) more than adolescent boys.",
  "17" = "Social media increases visual social comparisons among adolescent girls.",
  "18" = "Social media increases perfectionism among adolescent girls.",
  "19" = "Social media increases relational aggression among adolescent girls, for example by providing tools for cyberbullying and exclusion.",
  "20" = "Among adolescent girls, social media increases exposure to other people displaying or discussing their mental disorders.",
  "21" = "Social media increases sexual predation and harassment of adolescent girls, for example by providing predators with access to potential victims.",
  "22" = "At least one third of US college students would prefer for social media platforms to simply not exist.",
  "23" = "Most US parents would like to delay the age at which their children receive smartphones.",
  "24" = "If most parents waited until their children were in high school to give them their first smartphones, it would benefit the mental health of adolescents overall.\n(Parents would give only basic phones or flip phones before high school).",
  "25" = "Imposing (and enforcing) a legal minimum age of 16 for opening social media accounts would benefit the mental health of adolescents overall.",
  "26" = "Phone-free schools would benefit the mental health of adolescents overall."
)

# Order of claim numbers as they appear on the plot (top to bottom)
plot_order_numbers <- c(9, 15, 1, 13, 7, 8, 17, 20, 12, 10, 4, 21, 26, 24, 3, 16, 14, 6, 23, 2, 25, 19, 18, 11, 5, 22)

# Create the full claim texts for plotting, including the number prefix
claims_ordered_text_for_plotting <- character(length(plot_order_numbers))
for (i in seq_along(plot_order_numbers)) {
  num <- plot_order_numbers[i]
  text <- full_claim_definitions[as.character(num)]
  prefix <- ifelse(num == 9, paste0(num, ": "), paste0(num, ". ")) # Use colon for claim 9, period for others
  claims_ordered_text_for_plotting[i] <- paste0(prefix, text)
}



# Create a mapping from accuracy column name (e.g., "accuracy1") to full claim text
claim_numbers_in_text <- as.integer(sub("\\..*", "", claims_ordered_text))
unique_claim_text_by_num <- claims_ordered_text[!duplicated(claim_numbers_in_text)]
unique_claim_numbers <- claim_numbers_in_text[!duplicated(claim_numbers_in_text)]

# Create mapping from accuracy column name (e.g., "accuracy1") to the full prefixed claim text
accuracy_col_names_map <- paste0("accuracy", 1:26)
full_claim_texts_numeric_order_map <- character(26)
for (i in 1:26) {
  text <- full_claim_definitions[as.character(i)]
  prefix <- ifelse(i == 9, paste0(i, ": "), paste0(i, ". "))
  full_claim_texts_numeric_order_map[i] <- paste0(prefix, text)
}
claim_column_map <- setNames(full_claim_texts_numeric_order_map, accuracy_col_names_map)



# --- 3. Plot 1: "Belief in claim" ---
# Define categories, colors, and mapping for Plot 1
belief_levels_p1 <- c("skipped claim","inaccurate", "somewhat inaccurate",  "I don't know", "somewhat accurate", "accurate")
belief_colors_p1 <- c(
  "skipped claim" = "gray",
  "accurate"            = "#37598A", # Blue (was "Probably true")
  "somewhat accurate"   = "#ABD9E9", # Light Blue (new intermediate)
  "I don't know"        = "#FEEB98", # Yellow (was "Don't know")
  "somewhat inaccurate" = "#FDAE61", # Orange/Light Red (new intermediate)
  "inaccurate"          = "#D73027"  # Red (was "Probably false")
)

map_response_to_belief <- function(response_value) {
  case_when(
    response_value == "5"             ~ "accurate",
    response_value == "4" ~  "somewhat accurate",
    response_value == "3"             ~ "I don't know",
    response_value == "2"             ~ "somewhat inaccurate",
    response_value == "1"             ~ "inaccurate",
    TRUE                              ~ "skipped claim"
  )
}

table(su4$accuracy6, exclude = NULL)

# Process su4 data for Plot 1
su4_long_belief <- su4 %>%
  select(all_of(accuracy_cols_su4)) %>% # Select only existing accuracy columns
  pivot_longer(cols = everything(), names_to = "accuracy_col", values_to = "response_value") %>%
  mutate(
    claim_text = claim_column_map[accuracy_col],
    belief_category = map_response_to_belief(response_value)
  )

table(su4_long_belief$belief_category)

# Calculate proportions for Plot 1
plot_data_belief <- su4_long_belief %>%
  count(claim_text, belief_category) %>%
  group_by(claim_text) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup() %>%
  # Ensure all categories are present for all claims (for consistent stacking order)
  complete(claim_text, belief_category = belief_levels_p1, fill = list(n = 0, proportion = 0))

# Set factor levels for correct plotting order
plot_data_belief$claim_text <- factor(plot_data_belief$claim_text)
plot_data_belief$belief_category <- factor(plot_data_belief$belief_category, levels = belief_levels_p1)

plot_data_belief <- plot_data_belief %>%
  group_by(claim_text) %>%
  mutate(mean = weighted.mean(as.numeric(belief_category[!belief_category %in% c("skipped claim", "I don't know")]), n[!belief_category %in% c("skipped claim", "I don't know")])) %>%
  arrange(mean) %>%
  ungroup() %>%
  mutate(claim_text = fct_inorder(claim_text))

# --- 5. Create and Combine Plots ---
# Common theme elements
common_theme <- theme_minimal(base_size = 9) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.3, colour = "grey85"),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "plain", size = 10.5, margin = margin(b = 8)),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.size = unit(0.4, "cm"),
    legend.spacing.x = unit(0.2, "cm"), # Spacing between legend items
    legend.text = element_text(size = 8),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 8),
    plot.background = element_rect(fill = "white", colour = "white") # Ensure white bg
  )

# Plot 1 (Left side)
p1 <- ggplot(plot_data_belief, aes(x = proportion, y = claim_text, fill = belief_category)) +
  geom_col(width = 0.82) + # Bar width
  scale_x_continuous(labels = percent_format(), expand = c(0,0.015), breaks = seq(0, 1, 0.25)) +
  scale_fill_manual(values = belief_colors_p1, name = "", drop = FALSE) + # drop=FALSE ensures all legend items show
  labs(title = "Survey 4 (Final): To what extent do you believe the statement represents an accurate description of the current state of knowledge regarding the claim?") +
  common_theme +
  theme(
    axis.text.y = element_text(hjust = 0, size = 8), # Claim text
    plot.margin = margin(t=5, r=1, b=5, l=5) # Top, Right, Bottom, Left
  ) +
  theme(plot.caption = element_text(hjust = 0, face= "italic"), #Default is hjust=1
        plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
        plot.caption.position =  "plot") #NEW parameter
p1

