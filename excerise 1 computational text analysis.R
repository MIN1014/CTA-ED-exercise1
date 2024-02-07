# Excerise 1 
## Data cleaning
# Load the data
abcddata <- read_csv("https://raw.githubusercontent.com/cjbarrie/RDL-Ed/main/02-text-as-data/data/edbookfestall.csv")

# Inspect and filter data
colnames(abcddata)
glimpse(abcddata)

# get simplified dataset with only event contents and year
vdes <- abcddata %>%
  select(description, genre, year)

head(vdes)

#get year and word for every word and date pair in the dataset
tidy_vdes <- vdes %>% 
  mutate(desc = tolower(description)) %>%
  unnest_tokens(word, desc) %>%
  filter(str_detect(word, "[a-z]"))

# Remove stop words
tidy_vdes <- tidy_vdes %>%
  filter(!word %in% stop_words$word)

# find out the most common word
tidy_vdes %>%
  count(word, sort = TRUE)

# Remove HTML words
remove_reg <- c("&amp;","&lt;","&gt;","<p>", "</p>","&rsquo", "&lsquo;",  "&#39;", "<strong>", "</strong>", "rsquo", "em", "ndash", "nbsp", "lsquo", "strong")

tidy_vdes <- tidy_vdes %>%
  filter(!word %in% remove_reg)

# Inspect again
tidy_vdes %>%
  count(word, sort = TRUE)
tidy_vdes


# collect these words into a data.frame object
abcd_term_counts <- tidy_vdes %>% 
  group_by(year, genre) %>%
  count(word, sort = TRUE)

## Analyze keywords 
# tag those words related to women
abcd_term_counts$womword <- as.integer(grepl("women|feminist|feminism|gender|harassment|sexism|sexist", 
                                             x = abcd_term_counts$word))
head(abcd_term_counts)
view(abcd_term_counts)

## Compute aggregate statistics 
#get counts by year and word
abcd_counts <- abcd_term_counts %>%
  group_by(year, genre) %>%
  mutate(year_total = sum(n)) %>%
  filter(womword==1) %>%
  summarise(sum_wom = sum(n),
            year_total= min(year_total))
head(abcd_counts)

##Q1
# Filter the books by genre (selecting e.g., "Literature" or "Children") 
# and plot frequency of women-related words over time
literature<- filter(abcd_counts, genre == "Literature")
children<- filter(abcd_counts, genre == "Children")

# Plot time trends of literature books
ggplot(literature, aes(year, sum_wom / year_total, group=1)) +
  geom_line() +
  xlab("Year") +
  ylab("% gender-related words") +
  scale_y_continuous(labels = scales::percent_format(),
                     expand = c(0, 0), limits = c(0, NA)) +
  theme_tufte(base_family = "Helvetica") 

# Plot time trends of children books
ggplot(children, aes(year, sum_wom / year_total, group=1)) +
  geom_line() +
  xlab("Year") +
  ylab("% gender-related words") +
  scale_y_continuous(labels = scales::percent_format(),
                     expand = c(0, 0), limits = c(0, NA)) +
  theme_tufte(base_family = "Helvetica") 

# Q2
# Choose another set of terms by which to filter (e.g., race-related words) 
# and plot their frequency over time
abcd_term_counts$raceword <- as.integer(grepl("race|racism|black|white|asian|ethnic|racial|colonialism", 
                                             x = abcd_term_counts$word))
head(abcd_term_counts)

#get counts by year and word
race_counts <- abcd_term_counts %>%
  group_by(year) %>%
  mutate(year_total = sum(n)) %>%
  filter(raceword==1) %>%
  summarise(sum_race = sum(n),
            year_total= min(year_total))
head(race_counts)

# Plot time trends of race-related words
ggplot(race_counts, aes(year, sum_race / year_total, group=1)) +
  geom_line() +
  xlab("Year") +
  ylab("% race-related words") +
  scale_y_continuous(labels = scales::percent_format(),
                     expand = c(0, 0), limits = c(0, NA)) +
  theme_tufte(base_family = "Helvetica")


# Q3
# Knit my document (click 'Knit' towards the top of my screen) when I've 
# completed my exercise. It should create an html output file.  
