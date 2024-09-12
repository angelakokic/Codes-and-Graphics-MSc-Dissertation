
#### LOADING LIBRARIES AND DATASET####

library(dplyr)
library(tidyr)
library(moments)
library(reshape2)
library(RColorBrewer)
library(corrplot)
library(plotly)
library(ggforce)
library(ggcorrplot)
library(knitr)
library(kableExtra)
library(ggplot2)
library(Rcpp)
library(sf)
library(tidyverse)
library(ggmap)
library(lubridate)
library(GGally)
library(factoextra)
library(gridExtra)
library(caret)
library(class)
library(MASS)
library(rpart)
library(rpart.plot)
library(randomForest)
library(MLmetrics)
library(glmnet)
library(Metrics)
library(e1071)

#setting working directory and loading the dataset
setwd("~/OneDrive - University of Essex/Dissertation/data")
data <- read.csv("personality_q.csv")
data <- data.frame(data)
str(data)
colnames(data)

#### DATA PREPARATION ####

#dropping columns i won't be needing:
data <- data %>% 
  dplyr::select(-person, -Vsearch_Strategy, -Bucket_Strategy, -Hobbies_Type)
head(data, 5)

#                           1. bead_colour

#looking at the different levels of bead colours:
bead_colours <- unique(data$bead_colour)
num_categories <- length(bead_colours)
print(bead_colours)
#looking at missing values:
NA_count <- sum(is.na(data[["bead_colour"]]))
print(NA_count)
#transforming the variable:
data$bead_colour <- as.factor(data$bead_colour) #changing the variable into categorical type
levels(data$bead_colour) <- c(levels(data$bead_colour), "not specified") #adding a new level 'not specified' for all missing values
data$bead_colour[is.na(data$bead_colour)] <- "not specified"
str(data$bead_colour) #checking the data type
sum(is.na(data$bead_colour)) #checking if there are any missing values now
#making it into numerical variable where:
# black = 1
# orange = 2
# not specified = 3
data$bead_colour <- as.numeric(data$bead_colour)

#                         2. N_beads

#looking at missing values:
NA_count <- sum(is.na(data[["n_beads"]]))
print(NA_count)
data$n_beads[is.na(data$n_bea)] <- 0 #replacing the missing values with 0
sum(is.na(data$n_beads))

#                         3. Duration_in_Seconds

#cheking for outliers:
top_20_rows <- data %>%
  arrange(desc(Duration_in_Seconds)) %>%
  head(20) #sorting the largest values in descending order and showing first 2o rows
options(max.print = 1000000) #increasing maxx.print so I can see them all
print(top_20_rows)
data <- data %>%
  filter(Duration_in_Seconds <= 10800) #removing rows with values larger than 10800 seconds (3 hours)
#looking at missing values:
NA_count <- sum(is.na(data["Duration_in_Seconds"]))
print(NA_count)
mean_duration <- mean(data$Duration_in_Seconds, na.rm = TRUE) #getting the mean of the column
print(mean_duration)
data$Duration_in_Seconds[is.na(data$Duration_in_Seconds)] <- mean_duration #replacing missing values with mean
sum(is.na(data$Duration_in_Seconds))
summary(data$Duration_in_Seconds)


#                         4. Vsearch_YesNo

#looking at missing values:
NA_count <- sum(is.na(data["Vsearch_YesNo"]))
print(NA_count)
data <- data[!is.na(data$Vsearch_YesNo), ] #deleting the missing rows because this is my variable of interest and I need the exact responses
sum(is.na(data$Vsearch_YesNo))

#                         5. Bucket_YesNo

#looking at missing values:
NA_count <- sum(is.na(data["Bucket_YesNo"]))
print(NA_count)
data <- data[!is.na(data$Bucket_YesNo), ] #deleting the missing rows because there is only 1
sum(is.na(data$Bucket_YesNo))

#                         6. Gender

#looking at missing values:
NA_count <- sum(is.na(data["Gender"]))
print(NA_count)
#transforming the variable:
input_Gender <- unique(data$Gender)
print(input_Gender) #looking at unique values of Gender
map_Gender <- function(Gender) {
  if (Gender %in% c("Male", "male", "Man", "M", "MALE ")) {
    return("male") #returning 'male' for all of the mentioned inputs
  } else if (Gender %in% c("Female", "female", "woman", "FEMALE", "Woman", "F", "f", "femalw", "Femail")) {
    return("female") #returning 'female' for all of the mentioned inputs
  } else {
    return("other") #returning 'other' for all of the other inputs such as non-binary, other, N/A
  }
} #creating a mapping function to convert all values of Gender into 3 unique categories
data$Gender <- sapply(data$Gender, map_Gender) #applying the mapping function on the variable
data$Gender <- factor(data$Gender, levels = c("male", "female", "other")) #transforming it into categorical variable:
# male = 1
# female = 2
# other = 3
data$Gender <- as.numeric(data$Gender) #transforming it into numerical
str(data$Gender)

#                         7. Ethnicity

#looking at missing values:
NA_count <- sum(is.na(data["Ethnicity"]))
print(NA_count)
#transforming the variable:
input_Ethnicity <- unique(data$Ethnicity)
print(input_Ethnicity)
map_Ethnicity <- function(Ethnicity) {
  if (Ethnicity %in% c("British Pakistani", "indian", "Chinese", "Asian", "Asian Indian", "Hindu", "asian origin", "Asian-Indian", "Tamil, South Asian", "Asian, Chinese", "Kazakh", "Myanmar (Burmese)", "asian", "INDIAN", "yellow race", "nepal", "PAKISTANI",
                       "White british asian", "I am an Indian, Hindu", "Bangladeshi", "Asian British", "Asian(indian)", "ASIAN", "Pakistani", "asian british pakistani", "Pakistani- Muslim", "Indian-tamil", "Afghan", "Chinese(Hong Kong and Taiwan)",
                       "ASIAN- INDIAN", "Sri Lankan", "Filipino-Hong Konger")) {
    return("Asian/Asian British")
  } else if (Ethnicity %in% c("Black African", "African", "BLACK", "Black Nigerian", "Black African (Nigerian)", "AFRICAN", "Black Caribbean", "black", "Black British", "black African",
                              "african", "NIGERIAN", "eritrean", "yoruba/Nigerian")) {
    return("Black British/African/Caribbean")
  } else if (Ethnicity %in% c("Mixed Race", "Mixed: Black African and White Italian", "Swedish, Indian and Romanian", "mixed: white and asian", "Mixed", "Indian and Emarati", "Malaysian Indian, Spanish", "British/Mixed Caribbean",
                              "mixed black and white", "mixed, half Russian half Iranian. (mixed European/asian)")) {
    return("Mixed (any mixed background)")
  } else if (Ethnicity %in% c("White", "Dutch", "Slovak", "British", "White British", "white", "White/Scottish", "mixed white", "white British", "White Scottish", "white like the bread", "white European", "Caucasian", "SCOTTISH",
                              "White European", "european", "Scottish", "white-european", "Scottish Caucasian", "White Caucasian", "scottish", "Wide British", "white, Scottish", "white, caucasian", "white Scottish", "white caucasian",
                              "scottish white", "British white", "Greek", "White - South Eastern European", "White, non UK", "White, British", "white european", "British - white", "white british", "White-European (German-Dutch)",
                              "White Italian", "White, British, Irish", "White English", "white/ spanish", "white-British", "wjite", "white, slavic", "european white", "Whit British", "British White", "WHITE BRITISH", 
                              "white, Central European", "White - Other", "Bulgarian", "Polish (White European)", "White American")) {
    return("White (any white background)")
  } else if(Ethnicity %in% c("None", "muslim", "Christian", "concealing peoples' identity and privacy .")) {
    return("Not specified")
  } else {
    return("Any other ethnic group")
  }
}
data$Ethnicity <- sapply(data$Ethnicity, map_Ethnicity)
head(data$Ethnicity,10)
#converting into categorical and then numerical type:
# "White (any white background)" = 1
# "Mixed (any mixed background)" = 2
# "Any other ethnic group" = 3
# "Asian/Asian British" = 4
# "Not specified" = 5
# "Black British/African/Caribbean" = 6
data$Ethnicity <- factor(data$Ethnicity, levels = c("White (any white background)", "Mixed (any mixed background)", "Any other ethnic group", "Asian/Asian British", "Not specified", "Black British/African/Caribbean"))
data$Ethnicity <- as.numeric(data$Ethnicity)
str(data$Ethnicity)


#                              8. Age

#looking at missing values:
NA_count <- sum(is.na(data["Age"]))
print(NA_count)

#                              9. Height

#looking at missing values:
NA_count <- sum(is.na(data["Height"]))
print(NA_count)
mean_height <- mean(data$Height, na.rm = TRUE) #getting the mean of the column
print(mean_height)
data$Height[is.na(data$Height)] <- mean_height #replacing missing values with the mean of the column
sum(is.na(data$Height))
data$Height <- round(data$Height, 2) #rounding all values to 2 decimals

#                             10. Weight

#transforming the variable into numerical data type:
str(data$Weight)
data$Weight <- as.numeric(as.character(data$Weight))
#looking at missing values:
NA_count <- sum(is.na(data["Weight"]))
print(NA_count)
mean_weight <- mean(data$Weight, na.rm = TRUE) #getting the mean of the column
print(mean_weight)
data$Weight[is.na(data$Weight)] <- mean_weight #replacing missing values with mean of the column
sum(is.na(data$Weight))
data$Weight <- round(data$Weight, 2) #rounding all values to 2 decimals


#                             11. Occupation

#looking at missing values:
NA_count <- sum(is.na(data["Occupation"]))
print(NA_count)
#transforming the variable:
input_Occupation <- unique(data$Occupation)
print(input_Occupation)
map_Occupation <- function(Occupation) {
  if (Occupation %in% c("Student", "student", "undergraduate", "STUDENT", "full-time student", "PhD student", "Part time customer advisor/student", "Studying full-time", "students", 
                        "Masters student doing Psychology, but I used to be a pharmacist", "Research Master Student / Research Assistant", "undergraduate student", "Graduate Student", 
                        "Master Student", "Lawyer and Psychology student", "full time student", "Graduate student", "Undergraduate student", "Student/ Part Time Admin", "Student, CUOTC (army), and waiter",
                        "part time student", "Student/Retail supervisor at the SU Store", "Full-time student", "Umemployed (student)", "Full time Masters student", "I AM A STUDENT", "Just completed PhD", "Full time student", "student/lawyer")) {
    return("student") 
  } else if (Occupation %in% c("retired", "Retired", "Retired teacher", "retired teacher", "retired research  technician", "retired pharmacist", "RETIRED")) {
    return("retired") 
  } else if (Occupation %in% c("Unemployed", "volunteer with Aberdeenshire council", "None", "none", "non at present", "housewife")) {
    return("unemployed") 
  } else {
    return("employed") #returning 'employed' for all of the other inputs
  }
}
data$Occupation <- sapply(data$Occupation, map_Occupation) #applying the mapping function on the variable
head(data$Occupation, 5)
#converting into categorical and then numerical type:
# student = 1
# employed = 2
# unemployed = 3
# retired = 4
data$Occupation <- factor(data$Occupation, levels = c("student", "employed", "unemployed", "retired"))
data$Occupation <- as.numeric(data$Occupation)
str(data$Occupation)

#                             12. AreaOfStudy

#looking at missing values:
NA_count <- sum(is.na(data["AreaOfStudy"]))
print(NA_count)
#transforming the variable:
input_Area <- unique(data$AreaOfStudy)
print(input_Area)
map_Area <- function(AreaOfStudy) {
  if (AreaOfStudy %in% c("English Literature with Creative Writing", "English and Scottish Literature", "English", "Archaeology", "English / Politics & international Relations", "Anthropology", "Law", "MA Geography and Anthropology",
                         "English and History", "philosophy", "History and Archaeology", "Archaeology and History", "Langages", "law", "BA Psychology", "Modern Languages (German + Spanish) and International Relations", "language and linguistics",
                         "English Language and Linguistics", "Psycholinguistics", "literatrue", "English Literature", "Psychology and Anthropology", "Arts - English with Creative Writing", "Languages and cultures", "Human Rights Law", "Literature and creative writing")) {
    return("Arts and Humanities") 
  } else if (AreaOfStudy %in% c("Economics and Mathematics", "Master of data science", "Environmental sustainability", "Renewable Energy Engineering", "Renewable Energy Engineering (MSc)", "Mechanical Engineering", "Geoscience",
                                "Geosciences", "chemical engineering", "Renewable Engineering", "Computer science", "PLANETARY SCIENCE", "Data science", "Biomedical Engineering", "Computing", "MATHEMATICS", "biotechnology",
                                "Engineering", "Neural Imaging", "Geography", "Computer science and electronic engineering", "Artificial Intelligence", "Ms Data Science and its application")) {
    return("STEM") 
  } else if (AreaOfStudy %in% c("Psychology", "psychology", "PSYCHOLOGY", "genetics and immunology", "Chemistry", "Epidemiology", "Medical Sciences", "Pharmacology", "Biochemistry", "Biological Sciences", "Health Psychology", 
                                "Psychological studies with mental health", "Behavioural Neuroscience, Cognitive Psychology", "biomedical sciences", "Msc Medical Imaging", "Msc Global health and management", "Global Health",
                                "masters in psychology", "Cognitive Neuroscience and Neuropsychology", "Msc Cognitive Neuroscience and Neuropsychology", "MS Global Public Health", "Psychology with Cognitive Neuroscience",
                                "MSc. Cognitive Neuroscience and Neuropsychology", "neuroscience", "Master of Science in Psychology", "msc psychology", "psychology with cognitive neuroscience", "MSc Research Methods in Psychology", "MSc Tropical Marine Biology",
                                "MSc Psychology", "Neuroscience", "biochemistry", "Zoology", "Neuroscience with Psychology", "neuroscience with psychology", "MSC PSYCHOLOGY", "pyschology", "MSc psychology", "Pyschology")) {
    return("Health and Life Sciences") 
  } else if (AreaOfStudy %in% c("business and IR", "economics and business management", "Economics & International Relations", "business", "Economics and Politics", "Economics", "International business and finance",
                                "MA Financial Economics", "Business Management and Economics", "OIL AND GAS ENTERPRISE MANAGEMENT", "economics", "Oil and Gas", "Oil and Gas Enterprise Management", "Business", "MBA Energy Management",
                                "business management", "finance and investment", "Finance", "MSc(eco) accounting and finance", "financial economics", "Economics with psychology", "Marketing", "Msc Marketing", "INTERNATIONAL HOSPITALITY MANAGEMENT",
                                "economics and politics", "Management", "marketing", "Oil and gas", "Energy industry")) {
    return("Business and Economics") 
  } else if (AreaOfStudy %in% c("Politics", "Politics and International Relations", "Sociology", "International Relations and Sociology", "sociology", "social sciences", "Psychology and Economics", "Speech and Language Therapy",
                                "politics and international relations")) {
    return("Social Sciences") 
  } else {
    return("Not applicable")
  }
}
data$AreaOfStudy <- sapply(data$AreaOfStudy, map_Area) #applying the mapping function on the variable
head(data$AreaOfStudy, 5)
#converting into categorical and then numerical type:
# "Arts and Humanities" = 1
# "Business and Economics" = 2
# "Health and Life Sciences" = 3
# "Social Sciences" = 4
# "Not applicable" = 5
# "STEM" = 6
data$AreaOfStudy <- factor(data$AreaOfStudy, levels = c("Arts and Humanities", "Business and Economics", "Health and Life Sciences", "Social Sciences", "Not applicable", "STEM"))
data$AreaOfStudy <- as.numeric(data$AreaOfStudy)
str(data$AreaOfStudy)


#                             13. Education

#looking at missing values:
NA_count <- sum(is.na(data["Education"]))
print(NA_count)
#transforming the variable:
input_Education <- unique(data$Education)
print(input_Education)
map_Education <- function(Education) {
  if (Education %in% c("University", "university", "Degree", "BSc", "Undergraduate", "Undergraduate degree", "Bachelor's degree", "BSc HONS",
                       "University degree", "degree", "Bsc Electrical & Electronic Engineering", "undergraduate degree", "I have Bachelor's degree in pharmacy and now I am doing MSc. Psychological studies",
                       "Bachelor of  law", "Bachelor", "B.A,B.Ed", "BACHELORS", "B.Eng", "Bachelor of Technology", "Bachelor's Degree", "Bachelors", "Bachlors",
                       "undergraduate", "bachelor", "Bachelor degree", "bachelor degree", "graduate in psychology", "bachelors", "BSc (Hons) Psychology", "Undergrad",
                       "bacherlors", "Bachelor;s", "Psychology (Msc) in view", "Undergraduation", "undergraduate in psychology", "mbbs", "Bachelors in Commerce", "bachelor of science(UG)",
                       "Bachelor's", "bsc.psychology", "B.A. Psychology Hons.", "Undergraduate Degree", "BSc Psychology", "BSc Marine Biology with Hons", "Bachelors of science", "Bachelors - I'm currently doing my masters",
                       "Currently doing my masters", "BBA", "MY BATCHLORS", "Bachelors Degree (Honours)", "Honours degree", "Diploma of higher education", "MBBS", "Bachelors degree", "bachelor's",
                       "Bachelor Degree", "bschelor degree", "graduate university", "Bacholers", "bsc", "Bachelor of Arts")) {
    return("Undergraduate Level")
  } else if(Education %in% c("HNC", "HND", "Professional Certification", "professional qualification", "Chartered Accountant -C.A (ICAS)")) {
    return("Professional Certification")
  } else if (Education %in% c("Masters", "MSc", "Post grad diplome", "MSc (biohemistry)", "Masters of Science (MSc)", "Master", "Master's", "Masters degree",
                              "Master's degree", "Msc", "Post graduate diploma", "maters", "Postgraduate", "masters", "Master of Science (M.S) Offshore Engineering, TU Delft, Netherlands",
                              "MASTERS", "Master degree", "msc", "MASTER", "postgraduation", "M.SC.", "POST GRAD DIPLOMA", "MSc Physics", "MBA", "master", "post graduate diploma",
                              "MSc psychology and counselling")) {
    return("Postgraduate Level")
  } else if (Education %in% c("PhD", "Doctorate")) {
    return ("Doctorate Level")
  } else {
    return("A Levels/High School or equivalent")
  }
}
data$Education <- sapply(data$Education, map_Education)
head(data$Education, 5)
#converting into categorical and then numerical type:
# A Levels/High School or equivalent = 1
# Undergraduate Level = 2 
# Postgraduate Level = 3 
# Professional Certification = 4
# Doctorate Level = 5
data$Education <- factor(data$Education, levels = c("A Levels/High School or equivalent", "Undergraduate Level", "Postgraduate Level", "Professional Certification", "Doctorate Level"))
data$Education <- as.numeric(data$Education)
str(data$Education)

#                                 14. VidGame_YesNo

#looking at missing values:
NA_count <- sum(is.na(data["VidGame_YesNo"]))
print(NA_count)

#                                 15. VidGame_Type

#looking at missing values:
NA_count <- sum(is.na(data["VidGame_Type"]))
print(NA_count)
data$VidGame_Type[is.na(data$VidGame_Type) & data$VidGame_YesNo == 0] <- 0 #replacing all missing values in VidGame_Type to 0 if the answer to VidGame_YesNo is No=0
#transforming the variable:
input_VidGame_Type <- unique(data$VidGame_Type)
print(input_VidGame_Type)
map_VidGameType <- function(VidGame_Type) {
  if (VidGame_Type %in% c("FPS", "rpg", "Mario", "Various mainly action/survival", "MOBA", "League of Lengends", "Simulation games like car simulator", "Adventure", "First person shooter games", "adventure",
                          "temple run", "treasure hunt", "Adventure, horror", "Sports-SIM", "Action-adventure computer games", "Shooting game, Moba game", "Action", "shooting", "Shooting", "Car racing", "Action RPG",
                          "MMOPRG, First person shooters", "arcade, fantasy", "Adventure, action", "Mostly arcade/simulation ones.", "rpg,f2p", "RPG", "ACTION", "Open World", "Mostly role-playing story-driven games, like Assassin's Creed.", 
                          "driving/car games, adventure", "fps, open world","car race")) {
    return("Action")
  } else if(VidGame_Type %in% c("Strategy and Shooter", "Solitaire", "STORY BASED", "Bacamon; solitaire;soduko", "puzzle based & murder mystery", "match 3 type mostly", "Simple matching games", "Life role play games such as the sims", "Mostly various types of strategy, sometimes first person shooters",
                             "puzzle based platformers", "brain games, words spell, puzzal", "open world, strategic and creative", "Cards")) {
    return("Strategy")
  } else if (VidGame_Type %in% c("CANDY CRUSH", "Football", "basketball games", "Interactive games, roblox", "Candy crush, Cooking fever, Just dance", "minecraft. sims, pokemon", "candy crush, angry birds, ludo", "Sports games", "relaxing eg: swich game", "The Sims",
                              "cooking fever")) {
    return("Casual fun")
  } else if (VidGame_Type %in% c("0")) {
    return("Not applicable")
  } else {
    return("Multiple types")
  }
}
data$VidGame_Type <- sapply(data$VidGame_Type, map_VidGameType)
head(data$Education, 5)
#converting into categirical and then numerical type:
# Not applicable = 1
# Action = 2
# Strategy = 2 
# Casual fun = 3 
# Multiple types = 5
data$VidGame_Type <- factor(data$VidGame_Type, levels = c("Not applicable", "Action", "Strategy", "Casual fun", "Multiple types"))
data$VidGame_Type <- as.numeric(data$VidGame_Type)
str(data$VidGame_Type)

#                                  16. VidGame_Time

#looking at missing values:
NA_count <- sum(is.na(data["VidGame_Time"]))
print(NA_count)
data$VidGame_Time[is.na(data$VidGame_Time)] <- 0
input_VidGame_Time <- unique(data$VidGame_Time)
print(input_VidGame_Time)

#                                 17. Sports_YesNo

#looking at missing values:
NA_count <- sum(is.na(data["Sports_YesNo"]))
print(NA_count)

#                                 18. Sports_Type

#looking at missing values:
NA_count <- sum(is.na(data["Sports_Type"]))
print(NA_count)
data$Sports_Type[is.na(data$Sports_Type) & data$Sports_YesNo == 0] <- 0 #replacing all NA values in Sports_Type to 0 if the answer to Sports_YesNo is No=0
#transforming the variable:
input_Sports_Type <- unique(data$Sports_Type)
print(input_Sports_Type)
map_Sports_Type <- function(Sports_Type) {
  if (Sports_Type %in% c("gym", "Swimming, weightlifting", "Aerial Silks & Aerial Rope & Gym & hiking", "yoga, climbing", "walking/running on treadmill", "weightlifting, jogging, and yoga", "Classes at gym, yoga",
                         "dancing", "Gymnastics", "1", "Pole dancing", "Yoga", "Gym", "Dance and fitness", "golf", "sailing, golf and walking", "weightlifting, yoga", "Roller Derby", "Clay Shooting", "Yoga, Walking, Keep Fit", 
                         "swimming", "cycle hillwalking", "GOLF/SWIM/STRECHING CLASSES", "bowling/walking", "2", "cycling walking", "Cycling, Bowls", "Cheerleading", "dance and ice-skating", "Rollerblading", "Swim", "I run on the tread mill and do yoga",
                         "body pump", "Cycling", "horse riding", "Regular at the Gym, Rowing and Elliptical.", "running", "Running", "Dance", "ice-skating", "Climbing, Gym, running", "Dancing, swimming, high jump, long jump,", "Dance.",
                         "running, weightlifting", "swimming, morning exercises", "RUNNING,CIRCUITS", "Golf, gym work", "Gym, downhill skiing when I visit home in the winter (once a year)", "Gym, swimming", "Ice Skating, Running", "Ice skating",
                         "I do athletics and gymnastics", "Capoeira, rock climbing, running, gym", "swimming running", "Regular exercising, run, yoga", "Swimming")) {
    return("Active")
  } else if (Sports_Type %in% c("0")) {
    return("Not active")
  } else {
    return("Direct opponent")
  }
}
data$Sports_Type <- sapply(data$Sports_Type, map_Sports_Type)
head(data$Sports_Type, 5)
#converting into categorical and then numerical type:
# Active = 1
# Not active = 2 
# Direct opponent = 3 
data$Sports_Type <- factor(data$Sports_Type, levels = c("Active", "Not active", "Direct opponent"))
data$Sports_Type <- as.numeric(data$Sports_Type)
str(data$Sports_Type)

#                                 19. Sports_Time

#looking at missing values:
NA_count <- sum(is.na(data["Sports_Time"]))
print(NA_count) 
data$Sports_Time[is.na(data$Sports_Time)] <- 0
input_Sports_Time <- unique(data$Sports_Time)
print(input_Sports_Time)

#                                20. Hobbies_YesNo

#looking at missing values:
NA_count <- sum(is.na(data["Hobbies_YesNo"]))
print(NA_count) 

#                                21. Hobbies_Time

#looking at missing values:
NA_count <- sum(is.na(data["Hobbies_Time"]))
print(NA_count) 
data$Hobbies_Time[is.na(data$Hobbies_Time)] <- 0
input_Hobbies_Time <- unique(data$Hobbies_Time)
print(input_Hobbies_Time)

#                               22. MentIll_YesNo

#looking at missing values:
NA_count <- sum(is.na(data["MentIll_YesNo"]))
print(NA_count) 

#                               23. MentIll_Type

#looking at missing values:
NA_count <- sum(is.na(data["MentIll_Type"]))
print(NA_count) 
data$MentIll_Type[is.na(data$MentIll_Type)] <- 0
data$MentIll_Type
#transforming the variable:
input_MentIll_Type <- unique(data$MentIll_Type)
print(input_MentIll_Type)
map_MentIll_Type <- function(MentIll_Type) {
  if (MentIll_Type %in% c("I have anxiety", "Chronic anxiety", "anxiety")) {
    return("Anxiety disorders")
  } else if(MentIll_Type %in% c("Depression", "I have been diagnosed with depression.")) {
    return("Depression")
  } else if (MentIll_Type %in% c("Diagnosed with ADHD", "Spacial auditory processing disorder", "dyspraxia", "Dyslexia", "Dyslexia and Disgraphia", "Autism Spectrum Disorder",
                                 "dyslexia", "ADD", "ADHD", "undiagnosed autism spectrum disorder", "autism spectrum disorder")) {
    return("Neurodevelopmental disorders")
  } else if(MentIll_Type %in% c("0")) {
    return("No mental illness")
  } else {
    return("Multiple disorders")
  }
}
data$MentIll_Type <- sapply(data$MentIll_Type, map_MentIll_Type)
head(data$MentIll_Type, 5)
#converting into categorical and then numerical type:
# No mental illness = 1
# Anxiety disorders = 2 
# Depression = 3
# Neurodevelopmental disorders = 4
# Multiple disorders = 5
data$MentIll_Type <- factor(data$MentIll_Type, levels = c("No mental illness", "Anxiety disorders", "Depression", "Neurodevelopmental disorders", "Multiple disorders"))
data$MentIll_Type <- as.numeric(data$MentIll_Type)
str(data$MentIll_Type)


#                               Personality Traits

#checking missing values for the rest of the variables (all of the personality traits scores):
variables_to_check <- c("BFI2_01", "BFI2_02", "BFI2_03", "BFI2_04", "BFI2_05", "BFI2_06", "BFI2_07", "BFI2_08", "BFI2_09", "BFI2_10",
                      "BFI2_11", "BFI2_12", "BFI2_13", "BFI2_14", "BFI2_15", "BFI2_16", "BFI2_17", "BFI2_18", "BFI2_19", "BFI2_20",
                      "BFI2_21", "BFI2_22", "BFI2_23", "BFI2_24", "BFI2_25", "BFI2_26", "BFI2_27", "BFI2_28", "BFI2_29", "BFI2_30",
                      "BFI2_31", "BFI2_32", "BFI2_33", "BFI2_34", "BFI2_35", "BFI2_36", "BFI2_37", "BFI2_38", "BFI2_39", "BFI2_40",
                      "BFI2_41", "BFI2_42", "BFI2_43", "BFI2_44", "BFI2_45", "BFI2_46", "BFI2_47", "BFI2_48", "BFI2_49", "BFI2_50",
                      "BFI2_51", "BFI2_52", "BFI2_53", "BFI2_54", "BFI2_55", "BFI2_56", "BFI2_57", "BFI2_58", "BFI2_59", "BFI2_60",
                      "ProScale_01", "ProScale_02", "ProScale_03", "ProScale_04", "ProScale_05", "ProScale_06", "ProScale_07", "ProScale_08", "ProScale_09", "ProScale_10",
                      "ProScale_11", "ProScale_12", "ProScale_13", "ProScale_14", "ProScale_15", "ProScale_16", "ProScale_17", "ProScale_18", "ProScale_19", "ProScale_20",
                      "BIS11_01",	"BIS11_02",	"BIS11_03",	"BIS11_04",	"BIS11_05",	"BIS11_06",	"BIS11_07",	"BIS11_08",	"BIS11_09",	"BIS11_10",	"BIS11_11",	"BIS11_12",	"BIS11_13",	"BIS11_14",
                      "BIS11_15",	"BIS11_16",	"BIS11_17",	"BIS11_18",	"BIS11_19",	"BIS11_20",	"BIS11_21",	"BIS11_22",	"BIS11_23",	"BIS11_24",	"BIS11_25",	"BIS11_26",	"BIS11_27",	"BIS11_28",	"BIS11_29",	"BIS11_30")
na_counts <- sapply(data[variables_to_check], function(x) sum(is.na(x))) #getting all the missing values counts
print(na_counts)
#calculating the mode for all the variables to replace the missing values:
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
#creating function that will replace missingg values in variables_to_check with mode of the column:
for (var in variables_to_check) {
  mode_value <- get_mode(data[[var]])
  data[[var]][is.na(data[[var]])] <- mode_value
}
#checking for missing values again:
na_counts <- sapply(data[variables_to_check], function(x) sum(is.na(x)))
print(na_counts)


#                           Final check:

#checking total number of rows in the dataset after all the cleaning:
total_rows <- nrow(data)
print(total_rows)
#checking data types:
str(data)
#one more time checking all the missing values in data:
NA_values <- colSums(is.na(data))
print(NA_values)

#### EXPLORATORY DATA ANALYSIS ####

#getting summaries and standard deviations:
summary(data)
calculate_sds_all <- function(data) {
  sd_if_numeric <- function(x) {
    if (is.numeric(x)) {
      return(sd(x, na.rm = TRUE))
    } else {
      return(NA)
    }
  }
  sds_df <- data %>%
    summarise(across(everything(), sd_if_numeric, .names = "{col}_sd"))
  return(sds_df)
} #creating function that calculate all the standard deviations
sds_df <- calculate_sds_all(data)
print(sds_df)

#Vsearch_YesNo counts:
Vsearch_counts <- table(data$Vsearch_YesNo)
print(Vsearch_counts)

#                             Exploring demographic variables:

#Age:
age_hist <- ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 1, fill = "lightpink", color = "pink4") +
  labs(x = "Age",
       y = "Frequency") 
ggplotly(age_hist,height = 400,width = 600) #printing the age histogram
summary(data$Age)
age_sd <- sd(data$Age, na.rm = TRUE)
print(age_sd)

#Gender:
gender_counts <- table(data$Gender)
print(gender_counts)
gender_counts_df <- as.data.frame(gender_counts)
names(gender_counts_df) <- c("Gender", "Counts")
gender_labels <- c("Male", "Female", "Other")
gender_counts_df$Gender <- gender_labels[gender_counts_df$Gender]
gender_bar <- ggplot(gender_counts_df, aes(x = Gender, y = Counts, fill = Gender)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Female" = "lightpink", "Male" = "lightblue", "Other" = "darkseagreen3")) +
  labs(x = "Gender",
       y = "Number of Counts") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplotly(gender_bar,height = 400,width = 600) #creating a bar plot that shows the number of observations in each gender category

#Ethnicity:
ethnicity_counts <- table(data$Ethnicity)
print(ethnicity_counts)
ethnicity_counts_df <- as.data.frame(ethnicity_counts)
names(ethnicity_counts_df) <- c("Ethnicity", "Counts")
ethnicity_labels <- c("White (any white background)", 
                      "Mixed (any mixed background)", 
                      "Any other ethnic group", 
                      "Asian/Asian British", 
                      "Not specified", 
                      "Black British/African/Caribbean")
ethnicity_counts_df$Ethnicity <- ethnicity_labels[ethnicity_counts_df$Ethnicity]
print(ethnicity_counts_df)
kable(ethnicity_counts_df, caption = "Table 1: Number of Participants per Category of Ethnicity", 
      format = "html", booktabs = TRUE, ) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))%>%
  scroll_box(width = "100%", height = "500px") #creating Ethnicity Counts table
ethnicity_pie <- ggplot(ethnicity_counts_df, aes(x = "", y = `Counts`, fill = `Ethnicity`)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(fill = "Ethnicities:") +
  theme(legend.position = "right")
print(ethnicity_pie) #creating an Ethnicity Counts pie chart

#Occupation:
occupation_counts <- table(data$Occupation)
print(occupation_counts)
occupation_counts_df <- as.data.frame(occupation_counts)
names(occupation_counts_df) <- c("Occupation", "Counts")
occupation_labels <- c("student",
                      "employed",
                      "unemployed",
                      "retired")
occupation_counts_df$Occupation <- occupation_labels[occupation_counts_df$Occupation]
print(occupation_counts_df)
kable(occupation_counts_df, caption = "Table 2: Number of Participants per Category of Occupation", 
      format = "html", booktabs = TRUE, ) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))%>%
  scroll_box(width = "100%", height = "500px") #creating Occupation Counts table

#AreaOfStudy:
area_counts <- table(data$AreaOfStudy)
print(area_counts)
area_counts_df <- as.data.frame(area_counts)
names(area_counts_df) <- c("Area Of Study", "Counts")
area_labels <- c("Arts and Humanities",
                       "Business and Economics",
                       "Health and Life Sciences",
                       "Social Sciences",
                       "Not applicable",
                       "STEM")
area_counts_df$`Area Of Study` <- area_labels[area_counts_df$`Area Of Study`]
print(area_counts_df)
kable(area_counts_df, caption = "Table 3: Number of Participants per Area of Study", 
      format = "html", booktabs = TRUE, ) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))%>%
  scroll_box(width = "100%", height = "500px") #creating AreaOfStudy Counts table

#Education:
education_counts <- table(data$Education)
print(education_counts)
education_counts_df <- as.data.frame(education_counts)
names(education_counts_df) <- c("Highest Education Level", "Counts")
education_labels <- c("A Levels/High School or equivalent",
                      "Undergraduate Level",
                      "Postgraduate Level",
                      "Professional Certification",
                      "Doctorate Level")
education_counts_df$`Highest Education Level`<- education_labels[education_counts_df$`Highest Education Level`]
print(education_counts_df)
kable(education_counts_df, caption = "Table 4: Number of Participants per Their Highest Education Level", 
      format = "html", booktabs = TRUE, ) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))%>%
  scroll_box(width = "100%", height = "500px") #creating Education Counts table

#Height & Weight:
#displaying the violin plots to look at density distributions of height and weight:
violin_height <- ggplot(data, aes(x = factor(0), y = Height)) +
  geom_violin(fill = "lightblue", color = "steelblue4") +
  labs(x = "",
       y = "Height") +
  theme_minimal()
ggplotly(violin_height,height = 500,width = 600)
violin_weight <- ggplot(data, aes(x = factor(0), y = Weight)) +
  geom_violin(fill = "indianred1", color = "indianred4") +
  labs(x = "",
       y = "Weight") +
  theme_minimal()
ggplotly(violin_weight,height = 500,width = 600)

#BMI: calculating a new variable and adding it to dataset
data$Height_m <- data$Height / 100 #converting height in meters
data$BMI <- data$Weight / (data$Height_m^2)
#getting the density plot of BMI
BMI_dens <- ggplot(data, aes(x = BMI)) +
  geom_density(fill = "gold1", color = "darkorange3", alpha = 0.5) +
  labs(x = "BMI",
       y = "Density") +
  theme_minimal()
ggplotly(BMI_dens,height = 400,width = 600)

#                                Statistical testing:

#correlation between BMI and Ethnicity:
correlation <- cor.test(data$BMI, data$Ethnicity, method = "pearson")
print(correlation)
#correlation between BMI and Gender:
correlation <- cor.test(data$BMI, data$Gender, method = "pearson")
print(correlation)
#correlation between BMI and Sports_Time:
correlation <- cor.test(data$BMI, data$Sports_Time, method = "pearson")
print(correlation)
#correlation scatterplot between BMI and Sports_Time:
BMI_sports <- ggplot(data, aes(x = Sports_Time, y = BMI)) +
  geom_point(alpha = 0.6, color = "skyblue") +
  geom_smooth(method = "lm", se = TRUE, color = "navy") +
  labs(x = "Sports Time (hours)",
       y = "BMI") +
  theme_minimal()
ggplotly(BMI_sports, height = 500, width = 600)


#looking at logistic regression of Education and Occupation to see if there is any relationship:
#creating a dataset with Education and Occupation as factors so it's suited for logistic regression
fact_Education <- factor(data$Education, 
                         levels = c(1, 2, 3, 4, 5), 
                         labels = c("High School", "Undergraduate", "Postgraduate", "Professional Certification", "Doctorate"),
                         ordered = TRUE)
fact_Occupation <- factor(data$Occupation, 
                          levels = c(1, 2, 3, 4), 
                          labels = c("Student", "Employed", "Unemployed", "Retired"),
                          ordered = TRUE)
fact_data <- data.frame(Education = fact_Education, Occupation = fact_Occupation)
str(fact_data)
head(fact_data)
#fitting the logistic regression model
reg_model <- polr(Education ~ Occupation, data = fact_data, Hess = TRUE)
summary(reg_model)
#predicting probabilities
pred_probs <- predict(reg_model, fact_data, type = "probs")
#converting to long format for ggplot
pred_probs_long <- as.data.frame(pred_probs) %>%
  mutate(Occupation = fact_data$Occupation) %>%
  pivot_longer(cols = -Occupation, names_to = "Education", values_to = "Probability")
#plotting the predicted probabilities
edu_occup_plot <- ggplot(pred_probs_long, aes(x = Occupation, y = Probability, fill = Education)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_brewer(palette = "Set3") +
  labs(x = "Occupation",
       y = "Probability",
       fill = "Education Level") +
  theme_minimal()
print(edu_occup_plot)
ggplotly(edu_occup_plot)

#correlations with target variable Vsearch_YesNo:
#creating vectors to store variables
variables <- colnames(data)
target_variable <- "Vsearch_YesNo"
correlations <- numeric(length(variables))
p_values <- numeric(length(variables))
for (i in 1:length(variables)) {
  var <- variables[i]
  if (var != target_variable && is.numeric(data[[var]])) {
    cor_test <- cor.test(data[[target_variable]], data[[var]], method = "pearson")
    correlations[i] <- cor_test$estimate
    p_values[i] <- cor_test$p.value
  } else {
    correlations[i] <- NA
    p_values[i] <- NA
  }
} #function that loops over each variable and calculates pearson's r and significance level with target variable
results <- data.frame(
  Variable = variables,
  Correlation = correlations,
  P_Value = p_values
) #creating a data frame with p-values results
#filtering to only show statistically significant correlations (p < 0.05)
significant_results <- results[!is.na(results$P_Value) & results$P_Value < 0.05, ]
significant_results <- significant_results[order(significant_results$P_Value), ]
print(significant_results)
#creating a correlation heatmap
cor_vector <- significant_results$Correlation #creating a vector
names(cor_vector) <- significant_results$Variable
cor_matrix <- matrix(cor_vector, nrow = 1, dimnames = list("Vsearch_YesNo", names(cor_vector))) #creating a matrix from the vector
cor_melted <- melt(cor_matrix) #melting the correlation matrix
heatmap <- ggplot(cor_melted, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "lightskyblue4", high = "plum3", mid = "white", midpoint = 0, 
                       limit = c(-1, 1), name = "Pearson's r value",
                       breaks = c(-1, -0.5, 0, 0.5, 1), labels = c("-1.0", "-0.5", "0", "0.5", "1.0")) +
  theme_minimal() +
  labs(x = "Variables",
       y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplotly(heatmap)



#### MODELS IMPLEMENTATIONS ####

#                               1. Decision Tree

data <- data %>% 
  dplyr::select(-BMI)
head(data, 5) #removing BMI from the dataset
data$Vsearch_YesNo <- as.factor(data$Vsearch_YesNo) #converting the target variable to factor
#building a decision tree
decision_tree <- rpart(Vsearch_YesNo ~ ., data = data, method = "class")
summary(decision_tree)
#plotting the tree
rpart.plot(decision_tree, main = "Decision Tree for Vsearch_YesNo")
#getting the running time
tree_time <- system.time({
  decision_tree <- rpart(Vsearch_YesNo ~ ., data = data, method = "class")
})
print(paste(tree_time[3]))

#                              2. Random Forest

set.seed(42) #setting a seed for reproducibility
data$Vsearch_YesNo <- as.factor(data$Vsearch_YesNo) #making sure target variable is a factor
#splitting the dataset into training and testing
trainIndex <- createDataPartition(data$Vsearch_YesNo, p = .8, list = FALSE, times = 1)
trainData <- data[ trainIndex,]
testData  <- data[-trainIndex,]
#building random forrest model
rf_model <- randomForest(Vsearch_YesNo ~ ., data = trainData, importance = TRUE)
print(rf_model)
#making predictions
predictions <- predict(rf_model, testData)
conf_matrix <- confusionMatrix(predictions, testData$Vsearch_YesNo) #getting the confusion matrix
print(conf_matrix)
#calculating F1
precision_rf <- conf_matrix$byClass['Pos Pred Value']
recall_rf <- conf_matrix$byClass['Sensitivity']
f1_score_rf <- 2 * (precision_rf * recall_rf) / (precision_rf + recall_rf)
print(f1_score_rf)
#getting variable importances
importance(rf_model)
#plotting variable importances
#1. by Accuracy:
var_importance <- importance(rf_model)
var_importance_df <- as.data.frame(var_importance)
var_importance_df$Variables <- rownames(var_importance_df)
var_importance_df <- var_importance_df[order(var_importance_df$MeanDecreaseAccuracy, decreasing = TRUE), ] #sorting by MeanDecreaseAccuracy
top_n <- 20 #selecting top 20 variables
top_vars <- head(var_importance_df, top_n)
Accuracy <- ggplot(top_vars, aes(x = reorder(Variables, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  xlab("Variables") +
  ylab("Mean Decrease in Accuracy") +
  theme_minimal() #plotting
ggplotly(Accuracy)
#2. by Gini:
var_importance_df <- var_importance_df[order(var_importance_df$MeanDecreaseGini, decreasing = TRUE), ] #sorting by MeanDecreaseGini
top_n <- 20
top_vars <- head(var_importance_df, top_n)
Gini <- ggplot(top_vars, aes(x = reorder(Variables, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
  geom_bar(stat = "identity", fill = "plum") +
  coord_flip() +
  xlab("Variables") +
  ylab("Mean Decrease in Gini Impurity Criterion") +
  theme_minimal()
ggplotly(Gini)
#getting the running time
rf_time <- system.time({
  rf_model <- randomForest(Vsearch_YesNo ~ ., data = trainData, importance = TRUE, ntree = 500)
})
print(paste(rf_time[3]))
#getting the logarithmic loss
rf_predictions <- predict(rf_model, testData, type = "prob")[,2]  #probabilities for class 1
log_loss_rf <- LogLoss(y_true = as.numeric(as.character(testData$Vsearch_YesNo)), y_pred = rf_predictions)
print(log_loss_rf)

#                              3. Logistic Regression

set.seed(42)
data$Vsearch_YesNo <- as.factor(data$Vsearch_YesNo)
#splitting the dataset into training and testing
trainIndex <- createDataPartition(data$Vsearch_YesNo, p = .8, list = FALSE, times = 1)
trainData <- data[trainIndex,]
testData <- data[-trainIndex,]
#building logistic regression model
logit_model <- glm(Vsearch_YesNo ~ ., data = trainData, family = binomial)
summary(logit_model)
print(logit_model)
#predicting probabilities on the training set
train_predictions <- predict(logit_model, newdata = trainData, type = "response")
#converting probabilities to class labels
train_class <- ifelse(train_predictions > 0.5, 1, 0)
train_class <- as.factor(train_class)
#getting confusion matrix and accuracy for the training data
train_confusion <- confusionMatrix(train_class, trainData$Vsearch_YesNo)
print(train_confusion)
#predicting probabilities on the test set
logit_predictions <- predict(logit_model, newdata = testData, type = "response")
#converting probabilities to class labels
logit_class <- ifelse(logit_predictions > 0.5, 1, 0)
logit_class <- as.factor(logit_class)
#getting confusion matrix and accuracy
logit_confusion <- confusionMatrix(logit_class, testData$Vsearch_YesNo)
print(logit_confusion)
#getting F1 score
precision_logit <- logit_confusion$byClass['Pos Pred Value']
recall_logit <- logit_confusion$byClass['Sensitivity']
f1_score_logit <- 2 * (precision_logit * recall_logit) / (precision_logit + recall_logit)
print(f1_score_logit)
#getting logarithmic loss
log_loss_logit <- logLoss(as.numeric(as.character(testData$Vsearch_YesNo)), logit_predictions)
print(log_loss_logit)
#getting the running time
logit_time <- system.time({
  logit_model <- glm(Vsearch_YesNo ~ ., data = trainData, family = binomial)
  logit_predictions <- predict(logit_model, newdata = testData, type = "response")
  logit_class <- as.factor(ifelse(logit_predictions > 0.5, 1, 0))
  logit_confusion <- confusionMatrix(logit_class, testData$Vsearch_YesNo)
})
print(paste(logit_time[3]))

#                            4. LASSO Regularisation

set.seed(42)
data$Vsearch_YesNo <- as.factor(data$Vsearch_YesNo)
#splitting the dataset into training and testing
trainIndex <- createDataPartition(data$Vsearch_YesNo, p = .8, list = FALSE, times = 1)
trainData <- data[trainIndex,]
testData <- data[-trainIndex,]
x_train <- model.matrix(Vsearch_YesNo ~ ., trainData)[,-1]
y_train <- trainData$Vsearch_YesNo
x_test <- model.matrix(Vsearch_YesNo ~ ., testData)[,-1]
y_test <- testData$Vsearch_YesNo
#building the model using cross-validation
logit_model <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 1) #alpha=1 for Lasso regularisation
#getting the optimal lambda
print(paste("Optimal lambda is", logit_model$lambda.min))
#predictions on the training set
train_predictions <- predict(logit_model, s = logit_model$lambda.min, newx = x_train, type = "response")
train_class <- ifelse(train_predictions > 0.5, 1, 0)
train_class <- as.factor(train_class)
train_confusion <- confusionMatrix(train_class, y_train)
print(train_confusion)
#predictions on the test set 
logit_predictions <- predict(logit_model, s = logit_model$lambda.min, newx = x_test, type = "response")
logit_class <- ifelse(logit_predictions > 0.5, 1, 0)
logit_class <- as.factor(logit_class)
#getting confusion matrix and accuracy
logit_confusion <- confusionMatrix(logit_class, y_test)
print(logit_confusion)
print(logit_model)
#getting the F1 score
precision_logit <- logit_confusion$byClass['Pos Pred Value']
recall_logit <- logit_confusion$byClass['Sensitivity']
f1_score_logit <- 2 * (precision_logit * recall_logit) / (precision_logit + recall_logit)
print(f1_score_logit)
#getting the running time
logit_time <- system.time({
  logit_model <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 1)
})
print(paste(logit_time[3]))
#getting the logarithmic loss
log_loss_logit <- logLoss(as.numeric(as.character(y_test)), logit_predictions)
print(log_loss_logit)

#                          5. Ridge Regularisation

set.seed(42)
#preparing the data
x_train <- model.matrix(Vsearch_YesNo ~ ., trainData)[,-1]
y_train <- trainData$Vsearch_YesNo
x_test <- model.matrix(Vsearch_YesNo ~ ., testData)[,-1]
y_test <- testData$Vsearch_YesNo
#building the ridge regression model using cross-validation
ridge_model <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 0) #alpha=0 for ridge
#getting the optimal lambda value
cat("Optimal lambda is", ridge_model$lambda.min, "\n")
#predictions on the training set
train_predictions <- predict(ridge_model, s = ridge_model$lambda.min, newx = x_train, type = "response")
train_class <- ifelse(train_predictions > 0.5, 1, 0)
train_class <- as.factor(train_class)
#getting the confusion matrix and accuracy for training data
train_confusion <- confusionMatrix(train_class, y_train)
print(train_confusion)
#predictions on the test set
ridge_predictions <- predict(ridge_model, s = ridge_model$lambda.min, newx = x_test, type = "response")
ridge_class <- ifelse(ridge_predictions > 0.5, 1, 0)
ridge_class <- as.factor(ridge_class)
#getting the confusion matrix and accuracy
ridge_confusion <- confusionMatrix(ridge_class, y_test)
print(ridge_confusion)
#getting the F1 score
precision_ridge <- ridge_confusion$byClass['Pos Pred Value']
recall_ridge <- ridge_confusion$byClass['Sensitivity']
f1_score_ridge <- 2 * (precision_ridge * recall_ridge) / (precision_ridge + recall_ridge)
print(f1_score_ridge)
#calculating the running time
ridge_time <- system.time({
  ridge_model <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 0) # alpha = 0 for Ridge
})
print(paste(ridge_time[3]))
#getting the logarithmic loss
log_loss_ridge <- logLoss(as.numeric(as.character(y_test)), ridge_predictions)
print(log_loss_ridge)

#                               6. SVM

set.seed(42)
#ennsuring the target variable is a factor:
data$Vsearch_YesNo <- as.factor(data$Vsearch_YesNo)
#preparing data:
trainIndex <- createDataPartition(data$Vsearch_YesNo, p = .8, list = FALSE, times = 1)
trainData <- data[trainIndex,]
testData <- data[-trainIndex,]
#training the model:
svm_model <- svm(Vsearch_YesNo ~ ., data = trainData, kernel = "radial", cost = 1, scale = TRUE, probability = TRUE)
summary(svm_model)
#predictions on the training set:
train_predictions <- predict(svm_model, newdata = trainData)
#confusion matrix for the training set:
train_conf_matrix <- confusionMatrix(train_predictions, trainData$Vsearch_YesNo)
print(train_conf_matrix)
#predictions on the testing set:
svm_predictions <- predict(svm_model, newdata = testData)
#confusion matrix for the testing set:
svm_conf_matrix <- confusionMatrix(svm_predictions, testData$Vsearch_YesNo)
print(svm_conf_matrix)
#calculating F1:
precision_svm <- svm_conf_matrix$byClass['Pos Pred Value']
recall_svm <- svm_conf_matrix$byClass['Sensitivity']
f1_score_svm <- 2 * (precision_svm * recall_svm) / (precision_svm + recall_svm)
print(f1_score_svm)
#getting the logarithmic loss:
svm_predictions <- predict(svm_model, newdata = testData, probability = TRUE)
svm_prob <- attr(svm_predictions, "probabilities")[, 2]
log_loss_svm <- LogLoss(svm_prob, as.numeric(as.character(testData$Vsearch_YesNo)))
print(log_loss_svm)
#getting the running time:
svm_time <- system.time({
  svm_model <- svm(Vsearch_YesNo ~ ., data = trainData, kernel = "radial", cost = 1, scale = TRUE)
})
print(paste(svm_time[3]))


#                             7. Naive Bayes

set.seed(42)
#ensuring the target variable is a factor:
data$Vsearch_YesNo <- as.factor(data$Vsearch_YesNo)
#preparing data:
trainIndex <- createDataPartition(data$Vsearch_YesNo, p = .8, list = FALSE, times = 1)
trainData <- data[trainIndex,]
testData <- data[-trainIndex,]
#running the model:
nb_gaussian <- naiveBayes(Vsearch_YesNo ~ ., data = trainData)
#predictions on the training set:
nb_train_predictions <- predict(nb_gaussian, trainData)
#confusion matrix for training data:
nb_train_conf_matrix <- confusionMatrix(nb_train_predictions, trainData$Vsearch_YesNo)
print(nb_train_conf_matrix)
#predictions on the testing set:
nb_test_predictions <- predict(nb_gaussian, testData)
#confusion matrix for testing data:
nb_test_conf_matrix <- confusionMatrix(nb_test_predictions, testData$Vsearch_YesNo)
print(nb_test_conf_matrix)
#calculating F1:
precision_nb <- nb_test_conf_matrix$byClass['Pos Pred Value']
recall_nb <- nb_test_conf_matrix$byClass['Sensitivity']
f1_score_nb <- 2 * (precision_nb * recall_nb) / (precision_nb + recall_nb)
print(f1_score_nb)
#getting the running time:
nb_time <- system.time({
  nb_model <- naiveBayes(Vsearch_YesNo ~ ., data = trainData)
})
print(paste(nb_time[3]))
#getting the logarithmic loss
nb_predictions <- predict(nb_model, newdata = testData, type = "raw")
log_loss_nb <- LogLoss(nb_predictions[,2], as.numeric(as.character(testData$Vsearch_YesNo)))
print(log_loss_nb)

#                              8. kNN

set.seed(42)
#ensuring the target variable is a factor:
data$Vsearch_YesNo <- as.factor(data$Vsearch_YesNo)
#preparing data:
trainIndex <- createDataPartition(data$Vsearch_YesNo, p = .8, list = FALSE, times = 1)
trainData <- data[trainIndex,]
testData <- data[-trainIndex,]
#extracting predictor variables and target variable:
x_train <- trainData[, -which(names(trainData) == "Vsearch_YesNo")]
y_train <- trainData$Vsearch_YesNo
x_test <- testData[, -which(names(testData) == "Vsearch_YesNo")]
y_test <- testData$Vsearch_YesNo
#perform grid search for best k value:
tune_grid <- expand.grid(k = seq(1, 20, by = 1)) #sequence of 1 to 20 by 1
control <- trainControl(method = "cv", number = 10)
knn_model <- train(x_train, y_train, method = "knn", tuneGrid = tune_grid, trControl = control)
best_k <- knn_model$bestTune$k
print(best_k) #best value k is 1
#running the model using the best k value:
knn_best_model <- knn(train = x_train, test = x_train, cl = y_train, k = best_k)
#confusion matrix for training data:
train_conf_matrix <- confusionMatrix(knn_best_model, y_train)
print(train_conf_matrix)
#predictions on the testing set using the best k value:
knn_test_predictions <- knn(train = x_train, test = x_test, cl = y_train, k = best_k)
#confusion matrix for testing set:
test_conf_matrix <- confusionMatrix(knn_test_predictions, y_test)
print(test_conf_matrix)
#calculating F1 score:
precision_knn <- test_conf_matrix$byClass['Pos Pred Value']
recall_knn <- test_conf_matrix$byClass['Sensitivity']
f1_score_knn <- 2 * (precision_knn * recall_knn) / (precision_knn + recall_knn)
print(f1_score_knn)
#getting the running time:
knn_time <- system.time({
  knn_model <- knn(train = x_train, test = x_test, cl = y_train, k = best_k)
})
print(paste(knn_time[3]))
#approximating the probabilities to calculate log loss:
knn_prob <- as.numeric(knn_model == "1")
#calculating logarithmic loss:
log_loss_knn <- LogLoss(knn_prob, as.numeric(as.character(y_test)))
print(log_loss_knn)

#                           9. K means clustering

set.seed(42)
#checking data:
str(data)
#making sure to put Vsearch back to numeric:
data$Vsearch_YesNo <- as.numeric(data$Vsearch_YesNo)
#preprocessing the data:
data_scaled <- scale(data)
data_scaled <- as.data.frame(data_scaled)
str(data_scaled)
#performing K-means clustering with different numbers of centers:
kmeans2 <- kmeans(data_scaled, centers = 2, nstart = 20)
kmeans3 <- kmeans(data_scaled, centers = 3, nstart = 20)
kmeans4 <- kmeans(data_scaled, centers = 4, nstart = 20)
kmeans5 <- kmeans(data_scaled, centers = 5, nstart = 20)
kmeans6 <- kmeans(data_scaled, centers = 6, nstart = 20)
#visualising the clustering results:
f1 <- fviz_cluster(kmeans2, geom = "point", data = data_scaled) + ggtitle("k = 2")
f2 <- fviz_cluster(kmeans3, geom = "point", data = data_scaled) + ggtitle("k = 3")
f3 <- fviz_cluster(kmeans4, geom = "point", data = data_scaled) + ggtitle("k = 4")
f4 <- fviz_cluster(kmeans5, geom = "point", data = data_scaled) + ggtitle("k = 5")
f5 <- fviz_cluster(kmeans6, geom = "point", data = data_scaled) + ggtitle("k = 6")
#arranging plots in a grid for comparison:
grid.arrange(f1, f2, f3, f4, f5, nrow = 2)
#determining the optimal number of clusters using the Elbow method:
fviz_nbclust(data_scaled, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)
#looking at clusters with 2 and 3 centres:
kmeans3 <- kmeans(data_scaled, centers = 3, nstart = 20) 
kmeans2 <- kmeans(data_scaled, centers = 2, nstart = 20) 
#k = 3:
data_with_clusters <- data_scaled %>% mutate(cluster = kmeans3$cluster)
#calculating the mean of each variable grouped by cluster in k=3
cluster_means <- data_with_clusters %>%
  group_by(cluster) %>%
  summarise(across(everything(), mean))
print(cluster_means)
write.csv(cluster_means, "cluster_means.csv") #so i can look at the data set
#k = 2:
data_with_clusters <- data_scaled %>% mutate(cluster = kmeans2$cluster)
#calculating the mean of each variable grouped by cluster in k=2
cluster_means <- data_with_clusters %>%
  group_by(cluster) %>%
  summarise(across(everything(), mean))
print(cluster_means)
write.csv(cluster_means, "cluster_means.csv")
#visualising clusters with 2 and 3 centres:
k3 <- fviz_cluster(kmeans3, data = data_scaled)
k2 <- fviz_cluster(kmeans2, data = data_scaled)
ggplotly(k3)
ggplotly(k2)
#perform feature engineering (PCA):
pca <- prcomp(data_scaled, scale. = TRUE)
pca$rotation[, 1:3]  #loadings for the first three principal components
#removing variables with low PCA:
data_clus <- data %>% 
  dplyr::select(-bead_colour, -n_beads, -Duration_in_Seconds)
data_clus <- as.data.frame(data_clus)
data_clus_scaled <- scale(data_clus)
data_clus_scaled <- as.data.frame(data_clus_scaled)
#performing all the clustering steps again to see if feature engineering improved anything:
set.seed(42)
kmeans2 <- kmeans(data_clus_scaled, centers = 2, nstart = 20)
kmeans3 <- kmeans(data_clus_scaled, centers = 3, nstart = 20)
kmeans4 <- kmeans(data_clus_scaled, centers = 4, nstart = 20)
kmeans5 <- kmeans(data_clus_scaled, centers = 5, nstart = 20)
kmeans6 <- kmeans(data_clus_scaled, centers = 6, nstart = 20)
f1 <- fviz_cluster(kmeans2, geom = "point", data = data_clus_scaled) + ggtitle("k = 2")
f2 <- fviz_cluster(kmeans3, geom = "point", data = data_clus_scaled) + ggtitle("k = 3")
f3 <- fviz_cluster(kmeans4, geom = "point", data = data_clus_scaled) + ggtitle("k = 4")
f4 <- fviz_cluster(kmeans5, geom = "point", data = data_clus_scaled) + ggtitle("k = 5")
f5 <- fviz_cluster(kmeans6, geom = "point", data = data_clus_scaled) + ggtitle("k = 6")
grid.arrange(f1, f2, f3, f4, f5, nrow = 2)
fviz_nbclust(data_clus_scaled, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2) +
  ggtitle("Optimal number of clusters using Elbow Method")
kmeans2 <- kmeans(data_clus_scaled, centers = 2, nstart = 20) 
kmeans3 <- kmeans(data_clus_scaled, centers = 3, nstart = 20) 
fviz_cluster(kmeans2, data = data_clus_scaled)
fviz_cluster(kmeans3, data = data_clus_scaled)
#looking at cluster means with k = 2:
data_with_clusters <- data_scaled %>% mutate(cluster = kmeans2$cluster)
cluster_means <- data_with_clusters %>%
  group_by(cluster) %>%
  summarise(across(everything(), mean))
print(cluster_means)
write.csv(cluster_means, "cluster_means.csv")
#looking at cluster means with k = 3:
data_with_clusters <- data_scaled %>% mutate(cluster = kmeans3$cluster)
cluster_means <- data_with_clusters %>%
  group_by(cluster) %>%
  summarise(across(everything(), mean))
print(cluster_means)
write.csv(cluster_means, "cluster_means.csv")
#conclusion of feature engineering: model was not improved