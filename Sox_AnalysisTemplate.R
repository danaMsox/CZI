githubURL <- "https://github.com/ELJRussell/CZI/raw/refs/heads/main/MasterSurvey.RData"
load(url(githubURL))

library(tidyverse)

staffsurveys <- bind_rows(staffFairdale,staffGlenwood,staffRenaissance) |> 
  select(-Name) |> 
  rowid_to_column()

staffsurveysfactored <- staffsurveys |> 
  mutate(`How well do your colleagues at school understand you as a person?`=
           factor(`How well do your colleagues at school understand you as a person?`,
                  levels=c("Not well at all",
                           "Slightly well",
                           "Somewhat well",
                           "Mostly well",
                           "Extremely well")),
         `How connected do you feel to other adults at your school?`=
           factor(`How connected do you feel to other adults at your school?`,
                  levels=c("Not connected at all",
                           "Slightly connected",
                           "Somewhat connected",
                           "Mostly connected",
                           "Extremely connected")),
         `How respected do you feel around other colleagues at your school?`=
           factor(`How respected do you feel around other colleagues at your school?`,
                  levels=c("Not respected at all",
                           "Slightly respected",
                           "Somewhat respected",
                           "Mostly respected",
                           "Extremely respected")),
         `Overall, how much do you feel like you are accepted at your school?`=
           factor(`Overall, how much do you feel like you are accepted at your school?`,
                  levels=c("Not accepted at all",
                           "Slightly accepted",
                           "Somewhat accepted",
                           "Mostly accepted",
                           "Extremely accepted")),
         `How easy is it to interact with students at your school who have different backgrounds from your own?`=
           factor(`How easy is it to interact with students at your school who have different backgrounds from your own?`,
                  levels=c("Not at all easy",
                           "Slightly easy",
                           "Somewhat easy",
                           "Mostly easy",
                           "Extremely easy")),
         across(c(`If students with different backgrounds struggled to get along in your class, how comfortable would you be intervening?`,
                  `How comfortable would you be having conversations about race and gender with your students?`),
                ~factor(.x,
                        levels=c("Not at all comfortable",
                                 "Slightly comfortable",
                                 "Somewhat comfortable",
                                 "Mostly comfortable",
                                 "Extremely comfortable"))),
         `When a sensitive issue of diversity arises in class, how easily can you use strategies to address the situation?`=
           factor(`When a sensitive issue of diversity arises in class, how easily can you use strategies to address the situation?`,
                  levels=c("Not easily at all",
                           "Slightly easily",
                           "Somewhat easily",
                           "Mostly easily",
                           "Extremely easily")),
         across(`How confident are you that you can - engage students who struggle with motivation`:`How confident are you that you can - support your students' growth in developing self-management?`,
                ~factor(.x,
                        levels=c("Not at all confident",
                                 "Slightly confident",
                                 "Somewhat confident",
                                 "Mostly confident",
                                 "Extremely confident"))),
         `How often do you see students helping each other without being prompted?`=
           factor(`How often do you see students helping each other without being prompted?`,
                  levels=c("Almost never",
                           "Once in a while",
                           "Sometimes",
                           "Frequently",
                           "Almost always")),
         `How respectful are the relationships between staff and students?`=
           factor(`How respectful are the relationships between staff and students?`,
                  levels=c("Not at all respectful",
                           "Slightly respectful",
                           "Somewhat respectful",
                           "Mostly respectful",
                           "Extremely respectful")),
         across(c(`How supportive are the attitudes of your colleagues towards each other?`,
                  `Overall, how supportive is the working environment at your school?`),
                ~factor(.x,
                        levels=c("Not at all supportive",
                                 "Slightly supportive",
                                 "Somewhat supportive",
                                 "Mostly supportive",
                                 "Extremely supportive"))))

#constructs of staff survey
`Sense of Belonging`<- staffsurveysfactored |> 
  select(`How well do your colleagues at school understand you as a person?`:
           `Overall, how much do you feel like you are accepted at your school?`)
`Educating all students` <- staffsurveysfactored |> 
  select(`How easy is it to interact with students at your school who have different backgrounds from your own?`:
           `When a sensitive issue of diversity arises in class, how easily can you use strategies to address the situation?`)
`SEL in Classroom` <- staffsurveysfactored |> 
  select(starts_with("How confident are you that you can"))
`School Climate` <- staffsurveysfactored |> 
  select(`How often do you see students helping each other without being prompted?`:
           `Overall, how supportive is the working environment at your school?`)

##### **************Distribution of responses for each item***************** 
library(ggplot2)
library(tidyr)

#School Climate
df_long_SchoolClimate <- `School Climate`|>
  pivot_longer(cols=everything(), names_to="Variable", values_to= "Response") |>
  mutate(Response=as.factor(Response)) |>
  filter(!is.na(Response))
ggplot(df_long_SchoolClimate, aes(x = Response, fill = Variable)) +
  geom_bar(stat = "count", position = "dodge", show.legend = FALSE) +
  facet_wrap(~ Variable, scales = "free_x") +
  theme_minimal() +
  labs(title = "Distribution of Categorical Responses",
       x = "Response", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Noticing some evidence of right skew for a few of these variables and very few
#responses at the far left end of the scale (e.g., 'once in a while'). 

#Educating all Students
df_long_EducatingStudents <- `Educating all students`|>
  pivot_longer(cols=everything(), names_to="Variable", values_to= "Response") |>
  mutate(Response=as.factor(Response)) |>
  filter(!is.na(Response))
ggplot(df_long_EducatingStudents, aes(x = Response, fill = Variable)) +
  geom_bar(stat = "count", position = "dodge", show.legend = FALSE) +
  facet_wrap(~ Variable, scales = "free_x") +
  theme_minimal() +
  labs(title = "Distribution of Categorical Responses",
       x = "Response", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Similar to school climate there is some evidence of right skew and very few
#responses at the far left end of the scale (e.g., 'not at all comfortable'). 

#SEL in Classroom
df_long_SEL <- `SEL in Classroom`|>
  pivot_longer(cols=everything(), names_to="Variable", values_to= "Response") |>
  mutate(Response=as.factor(Response)) |>
  filter(!is.na(Response))
ggplot(df_long_SEL, aes(x = Response, fill = Variable)) +
  geom_bar(stat = "count", position = "dodge", show.legend = FALSE) +
  facet_wrap(~ Variable, scales = "free_x") +
  theme_minimal() +
  labs(title = "Distribution of Categorical Responses",
       x = "Response", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Again seeing some evidence of right skew, and very few responses at the far left
#end of the scale (e.g., 'Not at all confident.')

#Sense of Belonging
df_long_SenseBelonging <- `Sense of Belonging`|>
  pivot_longer(cols=everything(), names_to="Variable", values_to= "Response") |>
  mutate(Response=as.factor(Response)) |>
  filter(!is.na(Response))
ggplot(df_long_SenseBelonging, aes(x = Response, fill = Variable)) +
  geom_bar(stat = "count", position = "dodge", show.legend = FALSE) +
  facet_wrap(~ Variable, scales = "free_x") +
  theme_minimal() +
  labs(title = "Distribution of Categorical Responses",
       x = "Response", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Again seeing some evidence of right skew, and very few responses at the far left
#end of the scale (e.g., 'Not connected at all.')

#### **TAKEAWAYS: Evidence of right skew across all items, and there are very few
# responses at the far left end of the scale. 

##### ********************Mean/Average Item Response********************

#School Climate
response_mapping_Climate_q1 <- c(
  "Once in a while"=1, 
  "Sometimes"=2, 
  "Frequently"=3, 
  "Almost always"=4)
response_mapping_Climate_q2 <- c(
  "Slightly respectful"=1, 
  "Somewhat respectful"=2, 
  "Mostly respectful"=3, 
  "Extremely respectful"=4)
response_mapping_Climate_q3 <- c(
  "Not at all supportive"=1, 
  "Slightly supportive"=2, 
  "Somewhat supportive"=3, 
  "Mostly supportive"=4,
  "Extremely supportive"=5)
df_numeric_Climate <- `School Climate` %>%
  mutate(`How often do you see students helping each other without being prompted?`= as.numeric(factor(`How often do you see students helping each other without being prompted?`, levels = names(response_mapping_Climate_q1), ordered = TRUE)),
         `How respectful are the relationships between staff and students?`= as.numeric(factor(`How respectful are the relationships between staff and students?`, levels = names(response_mapping_Climate_q2), ordered = TRUE)),
         `How supportive are the attitudes of your colleagues towards each other?`= as.numeric(factor(`How supportive are the attitudes of your colleagues towards each other?`, levels = names(response_mapping_Climate_q3), ordered = TRUE)),
         `Overall, how supportive is the working environment at your school?`= as.numeric(factor( `Overall, how supportive is the working environment at your school?`, levels = names(response_mapping_Climate_q3), ordered = TRUE))
  )
mean_responses_Climate <- (df_numeric_Climate) %>%
  summarise(across(everything(), ~mean(., na.rm=TRUE)))
print(mean_responses_Climate)
#### **TAKEAWAYS: All mean item responses are above the middle scale value. 
# `How often do you see students helping each other without being prompted?` mean: 2.52
# `How respectful are the relationships between staff and students?` mean: 2.8 
#  `How supportive are the attitudes of your colleagues towards each other?` mean: 3.81
# `Overall, how supportive is the working environment at your school?` mean: 3.80

#Educating all Students
response_mapping_Educating_q1 <- c(
  "Not at all comfortable"=1, 
  "Slightly comfortable"=2, 
  "Somewhat comfortable"=3, 
  "Mostly comfortable"=4, 
  "Extremely comfortable"=5)
response_mapping_Educating_q2 <- c(
  "Slightly easy"=1, 
  "Somewhat easy"=2, 
  "Mostly easy"=3, 
  "Extremely easy"=4)
response_mapping_Educating_q3 <- c(
  "Slightly comfortable"=1, 
  "Somewhat comfortable"=2, 
  "Mostly comfortable"=3, 
  "Extremely comfortable"=4)
response_mapping_Educating_q4 <- c(
  "Slightly easily"=1, 
  "Somewhat easily"=2, 
  "Mostly easily"=3, 
  "Extremely easily"=4)
df_numeric_Educating <- `Educating all students` %>%
  mutate(`How easy is it to interact with students at your school who have different backgrounds from your own?`= as.numeric(factor(`How easy is it to interact with students at your school who have different backgrounds from your own?`, levels = names(response_mapping_Educating_q2), ordered = TRUE)),
         `If students with different backgrounds struggled to get along in your class, how comfortable would you be intervening?`= as.numeric(factor(`If students with different backgrounds struggled to get along in your class, how comfortable would you be intervening?`, levels = names(response_mapping_Educating_q3), ordered = TRUE)),
         `How comfortable would you be having conversations about race and gender with your students?`= as.numeric(factor(`How comfortable would you be having conversations about race and gender with your students?`, levels = names(response_mapping_Educating_q1), ordered = TRUE)),
         `When a sensitive issue of diversity arises in class, how easily can you use strategies to address the situation?`= as.numeric(factor( `When a sensitive issue of diversity arises in class, how easily can you use strategies to address the situation?`, levels = names(response_mapping_Educating_q4), ordered = TRUE))
  )
mean_responses_Educating <- (df_numeric_Educating) %>%
  summarise(across(everything(), ~mean(., na.rm=TRUE)))
print(mean_responses_Educating)
#### **TAKEAWAYS: Three mean responses above 3 [one of which is a 4 level Lickert scale]
#`How easy is it to interact with students at your school who have different backgrounds from your own?` mean:3.17
# `If students with different backgrounds struggled to get along in your class, how comfortable would you be intervening?`mean:3.05
#  `How comfortable would you be having conversations about race and gender with your students?`mean:3.91
#  `When a sensitive issue of diversity arises in class, how easily can you use strategies to address the situation?`mean: 2.82

#SEL IN CLASSROOM
response_mapping_SEL_q1 <- c(
  "Not at all confident"=1, 
  "Slightly confident"=2, 
  "Somewhat confident"=3, 
  "Mostly confident"=4, 
  "Extremely confident"=5)
response_mapping_SEL_q2 <- c(
  "Slightly confident"=1, 
  "Somewhat confident"=2, 
  "Mostly confident"=3, 
  "Extremely confident"=4)
df_numeric_SEL <- `SEL in Classroom` %>%
  mutate(`How confident are you that you can - engage students who struggle with motivation`= as.numeric(factor(`How confident are you that you can - engage students who struggle with motivation`, levels = names(response_mapping_SEL_q1), ordered = TRUE)),
         `How confident are you that you can - help your school's most challenging students to learn`= as.numeric(factor(`How confident are you that you can - help your school's most challenging students to learn`, levels = names(response_mapping_SEL_q1), ordered = TRUE)),
         `How confident are you that you can - support your students' growth in developing empathy?`= as.numeric(factor(`How confident are you that you can - support your students' growth in developing empathy?`, levels = names(response_mapping_SEL_q1), ordered = TRUE)),
         `How confident are you that you can - support your students' growth in developing self-awareness?`= as.numeric(factor(`How confident are you that you can - support your students' growth in developing self-awareness?`, levels = names(response_mapping_SEL_q2), ordered = TRUE)),
        `How confident are you that you can - support your students' growth in developing self-management?`= as.numeric(factor(`How confident are you that you can - support your students' growth in developing self-management?`, levels = names(response_mapping_SEL_q2), ordered = TRUE))
  )
mean_responses_SEL <- (df_numeric_SEL) %>%
  summarise(across(everything(), ~mean(., na.rm=TRUE)))
print(mean_responses_SEL)
### **TAKEAWAY SENSE OF BELONGING: Three mean item responses above 3 [middle scale value], two above 2 [middle scale value]
#`How confident are you that you can - engage students who struggle with motivation` mean: 3.56
#`How confident are you that you can - help your school's most challenging students to learn` mean:3.45
# `How confident are you that you can - support your students' growth in developing empathy?` mean: 3.69
# `How confident are you that you can - support your students' growth in developing self-awareness?` mean: 2.83
# `How confident are you that you can - support your students' growth in developing self-management?` mean: 2.75

#SENSE OF BELONGING
response_mapping_belong_q1 <- c(
  "Not connected at all"=1, 
  "Slightly connected"=2, 
  "Somewhat connected"=3, 
  "Mostly connected"=4, 
  "Extremely connected"=5)
response_mapping_belong_q2 <- c(
  "Not respected at all"=1, 
  "Slightly respected"=2, 
  "Somewhat respected"=3, 
  "Mostly respected"=4, 
  "Extremely respected"=5)
response_mapping_belong_q3 <- c(
  "Not well at all"=1, 
  "Slightly well"=2, 
  "Somewhat well"=3, 
  "Mostly well"=4, 
  "Extremely well"=5)
response_mapping_belong_q4 <- c(
  "Not accepted at all"=1, 
  "Slightly accepted"=2, 
  "Somewhat accepted"=3,
  "Mostly accepted"=4, 
  "Extremely accepted"=5)
df_numeric_belonging <- `Sense of Belonging` %>%
  mutate(`How connected do you feel to other adults at your school?`= as.numeric(factor(`How connected do you feel to other adults at your school?`, levels = names(response_mapping_belong_q1), ordered = TRUE)),
         `How well do your colleagues at school understand you as a person?`= as.numeric(factor(`How well do your colleagues at school understand you as a person?`, levels = names(response_mapping_belong_q3), ordered = TRUE)),
         `How respected do you feel around other colleagues at your school?`= as.numeric(factor(`How respected do you feel around other colleagues at your school?`, levels = names(response_mapping_belong_q2), ordered = TRUE)),
         `Overall, how much do you feel like you are accepted at your school?`= as.numeric(factor(`Overall, how much do you feel like you are accepted at your school?`, levels = names(response_mapping_belong_q4), ordered = TRUE))
  )
mean_responses_belonging <- (df_numeric_belonging) %>%
  summarise(across(everything(), ~mean(., na.rm=TRUE)))
print(mean_responses_belonging)
### **TAKEAWAY SENSE OF BELONGING:Mean item responses are all above 3 [middle scale value]
#`How well do your colleagues at school understand you as a person` mean: 3.39
#`How connected do you feel to other adults at your school` mean:3.37
# `How respected do you feel around other colleagues at your school?` mean: 3.76
# `Overall, how much do you feel like you are accepted at your school?` mean: 3.95

##### ***************Correlation Matrix for all variables********************
library(psych)
library(knitr)
library(kableExtra)
M<- cor(staffsurveysfactored |> 
          na.omit() |> 
          select(-rowid) |> 
          mutate_if(is.factor,as.numeric))

cor_table <- M %>%
  as.data.frame() %>%
  rownames_to_column("Items") %>%
  kable("html", caption = "Inter-Item Correlation Matrix") %>%
  kable_styling(full_width = F, position = "center", font_size = 11) %>%
  column_spec(1, bold = TRUE)
cor_table #builds correlation matrix 

#Takeaways: 'Sense of Belonging' items seem to be strongly correlated, but not too strongly correlated. (0.63-0.85)
#'Educating all students': some items are less correlated than I would like to see (0.35-0.69), but they are ok. 
#'SEL in Classroom' items seem to be ok. (0.44-0.73)
#'School Climate' worried about 'how often do you see students helping each other without being prompted.'
#Low correlations with the other items [0.15-0.31]. The other items correlations range from 0.46-0.82.

#Some cause for concerns regarding cross-construct for 'School Climate' and 'Sense of Belonging'
#'Overall, how supportive is the working environment at your school?' is highly correlated with
#'How respected do you feel around other colleagues at your school' (0.72) and 'Overall, 
#how much do you feel like you are accepted at your school?' (0.73)
#'

#### ************Cronbach's Alpha and Item-total correlations****************

#Educating all Students
psych::alpha(`Educating all students` |> 
               mutate_if(is.factor,as.numeric))
#0.79 Cronbach's alpha and item-total correlations are all above 0.3. (r.drop=0.47-0.73) 

#School Climate
psych::alpha(`School Climate` |> 
               mutate_if(is.factor,as.numeric))
#0.74 Cronbach's alpha. 'How often do you see students helping each other without being prompted has low item-total correlation (0.29), 
#meaning it doesn't correlate very well with the scale overall. Others are fine (0.62-0.84). Reliability also increases if we drop
#this item significantly (0.82). 

#SEL in Classroom
psych::alpha(`SEL in Classroom` |> 
               mutate_if(is.factor,as.numeric))
#0.86 Cronbach's alpha and item-total correlations are all above 0.3 (0.61-0.77).

# Sense of Belonging**
psych::alpha(`Sense of Belonging` |> 
               mutate_if(is.factor,as.numeric))
#0.91 Cronbach's alpha and item-total correlations are all above 0.3 (0.78-0.82)