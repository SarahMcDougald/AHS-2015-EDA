
library(tidyverse)


#### Writing in and saving data

ahs_2015 <- read_csv("data//household.csv")

# NOTE: do write_rds() from `readr`, not saveRDS() from base R.
write_rds(ahs_2015, path = "ahs_2015_table")

# NOTE: If you want, you can take a look at the AHS 2015 variables. This data file is not included in the project;
#   see the links in the 'visualizations.html' file for more information.
#ahs_variables <- readxl::read_xlsx("data//2015_AHS_Table_Specifications.xlsx")
#write_rds(ahs_variables, file = "ahs_2015_variables")


#### Reading in data from saved R-object files.

ahs_data <- read_rds(path = "ahs_2015_table")
#ahs_variables <- read_rds(path = "ahs_2015_variables")


#### Some exploration

## Weird JXX... variables? See where they stop:

ahs_narrow <- ahs_data[,c(500:1000)]
#ahs_narrow

# Okay, somewhere before the last 500 variables, we stop having the variables have the JXXXX....
# NOTE: Here's the link to the codebook. https://www.census.gov/data-tools/demo/codebook/ahs/ahsdict.html?s_appName=ahsdict&s_searchvalue=HHERRND&s_year=2015&s_minicode=E_2015
# SO HELPFUL. You search the name of the variable. All the JXXX variables are not showing up.
# So, see 2015 "mini" (hah) codebook: https://www.census.gov/cgi-bin/nbroker?_service=sas_serv1&_debug=0&_program=cedr.sasapp_main.sas&s_output=mpdf&s_orderBy=topic_number%20asc,subtopic_number%20asc,variable_number%20asc&menu=variable_table&s_appName=ahsdict&s_searchvalue=JHCPAY&s_year=2015&s_topic=&s_variable=&s_minicode=E_2015&s_currenttopic=&s_currentvar=


## Playing around with one of those weird J variables:

x_dat <- ahs_data[,6]
range(x_dat$JACSECNDRY)
typeof(x_dat$JACSECNDRY)
x_dat %>%
  mutate(
    JACSECNDRY = as.factor(JACSECNDRY)
  )
x_dat

# Well, it looks like from the 2015 codebook that the J variables are NOT to be included.
# So, I need to pull out *every single variable that.. starts with J* EXCEPT: 7 variables that are job-related.




# LOOK UP: Next step: pull out all the variables that start with J, and add back in the job-related ones; 
#  in general, pull out variables by variable name. **

# 1. Pull out the J variables.
# 2. start looking at interesting variables / visualizing a few for your lab. ****


#### 1. Remove extraneous J variables [Pre-visualization step]

# Ways?
# - map an if_else to all columns that start with J, in the data matrix
# - transpose the matrix, add an index var. and remove all other vars, remove every name but starts w/ J, ditch the "job"
#   variables and then, by index (base R technique), remove the columns with those names from the data matrix; OR, by
#   making a list of the names and then using select() (Tidyverse technique) on those names. ***

# Try the second way first?

data_transpose <- ahs_data[1,]
data_transpose <- t(data_transpose)
# From this StackOverflow help post: https://stackoverflow.com/questions/42404392/how-do-i-shift-the-values-of-every-column-in-a-data-frame-to-the-left-or-right?rq=1
data_transpose[,1] = rownames(data_transpose)
head(data_transpose)
# Now as a tibble again:
data_transpose <- as_tibble(data_transpose) # There's ALL the variables in a tibble!

# Add an index.
# Check first letter: way to use is stringr::str_sub(word1, 1, 1) to check the FIRST letter. 
#  - Add a new column of 1st-letters.
#  - Add a new column of first THREE letters (from the codebook, that is how we distinguish the "job" variables from the
#    non-extraneous J variables).
data_transpose <- data_transpose %>%
  mutate(
    index = (1:nrow(data_transpose)),
    first_letter = str_sub(V1, 1, 1),
    three_let = str_sub(V1, 1, 3)
    ) 
# Now filter out those that DON'T start with J.
data_Jnames <- data_transpose %>%
  filter(first_letter == "J")
data_Jnames
# Now *get rid of* the ones that have "job" in their first three letters. 
# This tibble should be 8 rows shorter than the previous, because there are 8 J variables that are NOT extraneous.
data_extraneous <- data_Jnames %>%
  filter(V1 == "JOBTYPE") 
data_extraneous#... Are there no JOB variables in this main dataset?

# Check another var:
data_transpose %>%
  filter(V1 == "HELOCLIM") # Not in our data either.


# Quick check to see if vars are in other data:
ahs_person <- read_csv("data//person.csv")
ahs_project <- read_csv("data//project.csv")

#ahs_person %>%
#  select(JOBTYPE)
ahs_project %>%
  select(JOBTYPE)
ahs_project

# OKAY! Looks like the JOB vars are in the `ahs_project` dataset, i.e. the "project" data.
# Therefore, we can proceed with our "household" data in `ahs_data` by removing all the J variables, since for that dataset,
#   we now know that all the vars that start with J are the extraneous ones.


data_extraneous <- data_Jnames # 300 extraneous variables to remove.
extraneous_names <- pull(data_extraneous, index)

# Finally, remove this list of names from our dataframe using select().

# AHS household data with the *actual* non-extraneous variables:
ahs_household <- ahs_data %>%
  select(-extraneous_names)
ahs_household # Great!!! 1091 - 300 = 791 variables total.



# SAVE IT:
write_rds(ahs_household, "ahs_household") # Q: Why is it 300 MB *bigger* when it has 300 less variables?


#### 2. Start looking at interesting variables [Visualization step]

# Going to look through the codebook and see what looks interesting first.
# 2015 AHS Codebook link again: https://www.census.gov/cgi-bin/nbroker?_service=sas_serv1&_debug=0&_program=cedr.sasapp_main.sas&s_output=mpdf&s_orderBy=topic_number%20asc,subtopic_number%20asc,variable_number%20asc&menu=variable_table&s_appName=ahsdict&s_searchvalue=JHCPAY&s_year=2015&s_topic=&s_variable=&s_minicode=E_2015&s_currenttopic=&s_currentvar=

# TENURE - is this household owned or rented
# CONDO - is this household a condo
# OWNLOT - does this household own the lot it's on
# OCCYRRND - is this household occupied year-round, i.e. is it a "home"
# BLD - what type of housing unit is this household? (...what are the options)

# UNITSIZE - square feet of household
# TOTROOMS - number of rooms in the household
# UNITFLOORS - number of floors in the household
# NUMPEOPLE - number of people living in the household

# HINCP - household income


# Okay, I think those are enough variables. Some to start with:


# - Graphic showing whether owned or rented, percentage(?)


# - Graph of square feet distribution! In color: owned or rented.

# - Graph of distribution of number of people in the household. In color: owned or rented.


# - Graph of dist. of number of rooms in the household; In color: owned or rented.












