surveys <- read_csv("data/portal_data_joined.csv")
plot(surveys$species)
surveys %>% 
  filter(!is.na(species)) %>% 
  plot(surveys$species)
plot(surveys$species)
species <- surveys$species
species
levels(species)
as.character(species)
species <- factor(surveys$species)
levels(species)
levels(species)[36] <- "sp"
levels(species)
species <- factor(surveys$species)
levels(species)
levels(species)[30] <- sp
levels(species)
levels(species)[30] <- "sp"
levels(species)
plot(species)
year <- factor(surveys$year)
levels(year)
plot(year)

year <- filter(surveys, year < 1990)
plot(year)
year <- factor(year)
year <- factor(surveys$(year < 1990))

year <- surveys %>% 
  filter(year < 1990) %>% 
  plot(year)
library(tidyverse)
surveys_complete <- read_csv("data/surveys_complete.csv")
ggplot(data = surveys_complete)
ggplot(data = surveys_complete,mapping = aes(x = weight,y = hindfoot_length))
ggplot(data = surveys_complete,mapping = aes(x = weight,y = hindfoot_length)) + geom_point()
surveys_plot = ggplot(data = surveys_complete,mapping=aes(x = weight,y = hindfoot_length))
surveys_plot + geom_point()
install.packages("hexbin")
library(hexbin)
surveys_plot + geom_hex()
ggplot(data = surveys_complete,mapping = aes(x = weight,y = hindfoot_length)) + geom_point(alpha = 0.1)
ggplot(data = surveys_complete,mapping = aes(x = weight,y = hindfoot_length)) + geom_point(alpha = 0.1, colour = "blue")
ggplot(data = surveys_complete,mapping = aes(x = weight,y = hindfoot_length)) + geom_point(alpha = 0.1, aes(color = species_id))
ggplot(data = surveys_complete,mapping = aes(x = weight,y = hindfoot_length, color = species_id)) + geom_jitter(alpha = 0.1)
ggplot(data = surveys_complete,mapping = aes(x = weight,y = hindfoot_length, color = species_id)) + geom_point(alpha = 0.1)
ggplot(data = surveys_complete, mapping = aes(x = species_id, y = weight)) + geom_boxplot()
ggplot(data = surveys_complete, mapping = aes(x = species_id, y = weight)) + geom_boxplot(alpha = 0) + geom_jitter(alpha = 0.3, color = "tomato")
surveys_plot + geom_violin()
ggplot(data = surveys_complete, mapping = aes(x = species_id, y = weight)) + geom_violin()
ggplot(data = surveys_complete, mapping = aes(x = species_id, y = weight)) + geom_violin(alpha=0.4, color = "tomato")
?scale_y_log10
plot_id <- factor(surveys_complete$plot_id) %>% 
  ggplot(data = surveys_complete, mapping = aes(x = species_id, y = hindfoot_length)) + geom_boxplot(alpha = 0.5) + geom_jitter(alpha = 0.1, color = plot_id)
plot_id <- factor(surveys_complete$plot_id) # always remember that you need to convert information to a factor before a graph is able to recognise what the hell is going on
ggplot(data = surveys_complete, mapping = aes(x = species_id, y = hindfoot_length)) + geom_boxplot(alpha = 0.5) + geom_jitter(alpha = 0.3, color = plot_id)

yearly_counts <- surveys_complete %>% 
  count(year, species_id)
ggplot(yearly_counts, mapping = aes(x = year, y = n)) + geom_line() # this doesn't work because this plotted data for all the species together
ggplot(yearly_counts, mapping = aes(x = year, y = n, group = species_id)) + geom_line() # this accounts for the individual species_id groups
ggplot(data = yearly_counts, mapping = aes(x = year, y = n, color = species_id)) + geom_line() #using colour automatically groups the data, and helps distinguish between the species_id groups


# Faceting
ggplot(data = yearly_counts, mapping = aes(x = year, y = n)) +
  geom_line() + 
  facet_wrap(~ species_id)

yearly_sex_counts <- surveys_complete %>% 
  count(year, species_id, sex)
ggplot(data = yearly_sex_counts, mapping = aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_wrap(~ species_id)

ggplot(data = yearly_sex_counts, mapping = aes(x = year, y = n, colour = sex)) +
  geom_line() +
  facet_wrap(~ species_id) +
  theme_bw() +
  theme(panel.grid = element_blank())


yearly_sex_weight <- surveys_complete %>% 
  group_by(year,sex,species_id) %>% 
  summarize(avg_weight = mean(weight))
view(yearly_sex_weight)
#one column, facet by row
ggplot(data = yearly_sex_weight,
       mapping = aes(x = year, y = avg_weight, color = species_id)) +
  geom_line() +
  facet_grid(sex ~ .) # important
#one row, facet by column
ggplot(data = yearly_sex_weight,
       mapping = aes(x = year,y = avg_weight, color = species_id)) +
  geom_line() +
  facet_grid(. ~ sex) # important

# Customisation
ggplot(data = yearly_sex_counts, mapping = aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_wrap(~ species_id) +
  labs(title = "Observed species in time", # labels
       x = "Year of Observation",
       y = "Number of individuals") +
  theme_bw()

ggplot(data = yearly_sex_counts, mapping = aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_wrap (~ species_id) +
  labs(title = "Observed species in time",
       x = "Year of observation",
       y = "Number of individuals") +
  theme_bw() +
  theme(text=element_text(size = 16)) #increases the size of the labels

ggplot(data = yearly_sex_counts, mapping = aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_wrap (~ species_id) +
  labs(title = "Observed species in time",
       x = "Year of Observation",
       y = "Number of individuals") +
  theme_bw() +
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 90, hjust = 0.5, vjust = 0.5), axis.text.y = element_text(color = "grey20", size = 12),
        text = element_text(size = 16)) # this changes the size, angle and colour of the x and y axis labels

grey_theme <- theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 90, hjust = 0.5, vjust = 0.5), axis.text.y = element_text(color = "grey20", size = 12),
                    text = element_text(size = 16))
ggplot(surveys_complete, aes(x = species_id, y = hindfoot_length)) +
  geom_boxplot() + 
  grey_theme


# Arranging and exporting plots
install.packages("gridExtra")
library(gridExtra)
spp_weight_weight_boxplot <- ggplot(data = surveys_complete, mapping = aes(x = species, y = weight)) +
  geom_boxplot() +
  grey_theme +
  xlab("Species") + ylab("Weight (g)") +
  scale_y_log10()
spp_count_plot <- ggplot(data = yearly_counts, mapping = aes(x = year, y = n, color = species_id)) +
  geom_line() +
  grey_theme +
  xlab("Year") + ylab("Abundance")
grid.arrange(spp_weight_weight_boxplot, spp_count_plot, ncol = 2, widths = c(4, 6)) #arranges the plots specified above

my_plot <- ggplot(yearly_sex_counts, mapping = aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_wrap(~ species_id) +
  labs(title = "Observed species in time",
       x = "Year of observation",
       y = "Number of individuals") +
  theme_bw() +
  grey_theme
ggsave("fig_output/yearly_sex_counts.png", my_plot, width=15, height = 10)
combo_plot <- grid.arrange(spp_weight_weight_boxplot, spp_count_plot, ncol = 2, widths = c(4,6))
ggsave("fig_output/combo_plot_abun_weight.png", combo_plot, width = 10, dpi = 300)
