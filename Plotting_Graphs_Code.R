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
plot_id <- factor(surveys_complete$plot_id)
ggplot(data = surveys_complete, mapping = aes(x = species_id, y = hindfoot_length)) + geom_boxplot(alpha = 0.5) + geom_jitter(alpha = 0.3, color = plot_id)
