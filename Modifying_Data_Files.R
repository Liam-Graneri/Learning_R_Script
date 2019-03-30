load(tidyverse)
library(tidyverse)
surveys <- read_csv("data/portal_data_joined.csv")

# inspect the data
str(surveys)
#preview the data
view(surveys)

# Selecing columns and filtering rows

select(surveys,plot_id,species_id,weight) # selecting plot_id, species_id, and weight
select(surveys,-record_id,-species_id) # selecing all columns except record_id and species_id

filter(surveys,year == 1995) # only shows rows based on specific criteria e.g. year 1995


# Pipes

surveys2 <- filter(surveys,weight < 5) #creating a temporary datafile which you can use in the next step
surveys_sml <- select(surveys2, species_id,sex,weight) # creates the filtered and selected dataset
# You can next functions to clean it up a but (i.e. to the above operations:)
surveys_sml <- select(filter(surveys,weight < 5), species_id,sex,weight)
# To avoid too many nested functions and a complicated list of shit, you can introduce pipes at the end of your functions. Pipes look like %>% and can be automatically done with ctrl + shift + M e.g.
surveys %>%
  filter(weight < 5) %>% 
  select(species_id,sex,weight)
surveys_sml
# Challenge
surveys_pipeschallenge <- surveys %>%
  filter(year < 1995) %>% 
  select(year,sex,weight)
surveys_pipeschallenge


# Using Mutate

surveys %>% 
  mutate(weight_kg = weight / 1000) # creates a new column based off the values of an existing column
surveys %>%
  mutate(weight_kg = weight / 1000, 
         weight_kg2 = weight_kg * 2) # creates a second column based on the first column within the same call of mutate()
surveys %>%
  mutate(weight_kg = weight/1000) %>% # creates a head of the new dataset with updated columns
  head()
surveys %>%
  filter(!is.na(weight)) %>% 
  mutate(weight_kg = weight / 1000) %>% #creates a head of mutated weight data, as long as there is an input value
  head()
# Mutate Challenge
#new data frame from surveys
#contains only species_id and a new column called 'hindfoot_half'which contains values that are half the hindfoot_length values
#in hindfoot_half, there are no N/a values and all values are less than 30
surveys_mutatechallenge <- surveys %>%
  filter(!is.na(hindfoot_length)) %>% 
  mutate(hindfoot_half = hindfoot_length / 2) %>% 
  filter(hindfoot_half < 30) %>% 
  select(species_id,hindfoot_half)
surveys_mutatechallenge
view(surveys_mutatechallenge)
# I feel so accomplished right now


# *Split-apply-combine data analysis and the syummarize function*
surveys %>%
  group_by(sex) %>% 
  summarize(mean_weight = mean(weight, na.rm = TRUE)) # this groups by sex but finds the mean weight of each
surveys %>% 
  group_by(sex,species_id) %>% 
  summarize(mean_weight = mean(weight, na.rm = TRUE)) #this groups by both sex and species_id, and finds the mean weight
surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(sex,species_id) %>% 
  summarize(mean_weight = mean(weight)) #here, we remove any weight data values which have not been entered
surveys %>%
  filter(!is.na(weight)) %>% 
  group_by(sex,species_id) %>% 
  summarize(mean_weight = mean(weight), # here we arrange both sex and species id to see mean and min weight values
            min_weight = min(weight))
surveys %>%
  filter(!is.na(weight)) %>% 
  group_by(sex,species_id) %>% 
  summarize(mean_weight = mean(weight),
            min_weight = min(weight)) %>% # arranged sex and species_id, and listed in ascending ordr of min weight
  arrange(min_weight)
surveys %>%
  filter(!is.na(weight)) %>% 
  group_by(sex,species_id) %>% 
  summarize(mean_weight = mean(weight),
            min_weight = min(weight)) %>% # arranged sex and species_id, listed in descending order of mean weight
  arrange (desc(mean_weight))


# Counting
surveys %>%
  count(sex) # counts the amount of values in the column 'sex'
surveys %>%
  group_by(sex) %>% 
  summarise(count = n()) # the longhand version of the above 'count' function
surveys %>% 
  count(sex, sort = TRUE) # this sorts the sexes into alphabetical/numerical order
surveys %>%
  count(sex,species) # this counts the interstections between sex and species
surveys %>% 
  count(sex,species) %>% 
  arrange(species,desc(n)) # this arranges the table in an alphabetical order of the levels of species, and descends the order of the count
 #Counting Challenge
surveys %>%
  count(plot_type)
surveys %>% 
  group_by(species_id) %>% 
  summarise(mean_hindfoot = mean(hindfoot_length),
            min_hindfoot = min(hindfoot_length),
            max_hindfoot = max(hindfoot_length)) %>% 
  count(n())
surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(year) %>% 
  filter(weight == max(weight)) %>% 
  select(year,genus,species,weight) %>% 
  arrange(year)

# Reshaping with gather and spread
surveys_gw <- surveys %>%
  filter(!is.na(weight)) %>% 
  group_by(genus,plot_id) %>% 
  summarize(mean_weight = mean(weight)) # this creates a separate datasheet where the observations are spread over multple rows
str(surveys_gw)
view(surveys_gw)

surveys_spread <- surveys_gw %>% 
  spread(key = genus, value = mean_weight) # the key is the column with the new variable names, the value is what will populate the values of the cells
print(surveys_spread)
view(survey)
surveys_gw %>% 
  spread(genus,mean_weight,fill = 0) %>% 
  head()

# Gathering
surveys_gather <- surveys_spread %>% 
  gather(key = genus,value = mean_weight,-plot_id)
view(surveys_gather)
surveys_spread %>% 
  gather(key=genus,value=mean_weight,Baiomys:Spermophilus) %>% 
  head()


# Exporting Data
surveys_complete <- surveys %>% 
  filter(!is.na(weight), # removes the missing weight
         !is.na(hindfoot_length), # removes the missing hindfoot_length
         !is.na(sex) #removes the missing sex
      ) 

species_counts <- surveys_complete %>% 
  count(species_id) %>% 
  filter(n>=50)

surveys_complete <- surveys_complete %>% 
  filter(species_id %in% species_counts$species_id)

write_csv(surveys_complete,path = "data/surveys_complete.csv")
