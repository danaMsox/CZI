githubURL <- "https://github.com/ELJRussell/CZI/raw/refs/heads/main/MasterData.RData"
load(url(githubURL))
library(tidyverse)

############################################################################
#########################   Staff Survey   ###############################
############################################################################

eoy_staffsurveys <- subset(staff, Time == "EOY")

eoy_staffsurveysfactored <- eoy_staffsurveys |> 
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
`Sense of Belonging`<- eoy_staffsurveysfactored |> 
  select(`How well do your colleagues at school understand you as a person?`:
           `Overall, how much do you feel like you are accepted at your school?`)
`Educating all students` <- eoy_staffsurveysfactored |> 
  select(`How easy is it to interact with students at your school who have different backgrounds from your own?`:
           `When a sensitive issue of diversity arises in class, how easily can you use strategies to address the situation?`)
`SEL in Classroom` <- eoy_staffsurveysfactored |> 
  select(starts_with("How confident are you that you can"))
`School Climate` <- eoy_staffsurveysfactored |> 
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
#Normally distributed

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
#### **TAKEAWAYS:School Climate: Two mean item responses above 3 [middle scale value], two above 2 [middle scale value] 
# `How often do you see students helping each other without being prompted?` mean: 2.39
# `How respectful are the relationships between staff and students?` mean: 2.71
#  `How supportive are the attitudes of your colleagues towards each other?` mean: 3.94
# `Overall, how supportive is the working environment at your school?` mean: 3.96

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
#`How easy is it to interact with students at your school who have different backgrounds from your own?` mean:3.28
# `If students with different backgrounds struggled to get along in your class, how comfortable would you be intervening?`mean:3.15
#  `How comfortable would you be having conversations about race and gender with your students?`mean:3.97
#  `When a sensitive issue of diversity arises in class, how easily can you use strategies to address the situation?`mean: 2.91

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
#`How confident are you that you can - engage students who struggle with motivation` mean: 3.72
#`How confident are you that you can - help your school's most challenging students to learn` mean:3.73
# `How confident are you that you can - support your students' growth in developing empathy?` mean: 3.72
# `How confident are you that you can - support your students' growth in developing self-awareness?` mean: 2.82
# `How confident are you that you can - support your students' growth in developing self-management?` mean: 2.79

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
### **TAKEAWAY SENSE OF BELONGING: Mean item responses are all above 3 [middle scale value]
#`How well do your colleagues at school understand you as a person` mean: 3.48
#`How connected do you feel to other adults at your school` mean:3.51
# `How respected do you feel around other colleagues at your school?` mean: 3.78
# `Overall, how much do you feel like you are accepted at your school?` mean: 3.96

##### ***************Correlation Matrix for all variables********************
library(psych)
library(knitr)
library(kableExtra)

cor_data <- bind_cols(`Educating all students`, `School Climate`, `SEL in Classroom`, `Sense of Belonging`)

M<- cor(cor_data|> 
          na.omit() |> 
          mutate_if(is.factor,as.numeric))

cor_table <- M %>%
  as.data.frame() %>%
  rownames_to_column("Items") %>%
  kable("html", caption = "Inter-Item Correlation Matrix") %>%
  kable_styling(full_width = F, position = "center", font_size = 11) %>%
  column_spec(1, bold = TRUE)
cor_table #builds correlation matrix 

#Takeaways: 'Educating all students': some items are less correlated than I would 
# like to see (0.34-0.81), but they are ok. 
#'School Climate'-- 'how often do you see students helping each other without 
#'being prompted.' and 'Overall, how supportive is the working environment at 
#'your school? has a low correlation (0.18).
#'SEL in Classroom' items seem to be ok. (0.37-0.75)
#'Sense of Belonging' items seem to be strongly correlated, but not too strongly 
#'correlated. (0.60-0.85)

#With the BOY data there were some initial concerns regarding cross-construct for 
#'School Climate' and 'Sense of Belonging.' In particular, 'Overall, how supportive 
#'is the working environment at your school?' was highly correlated with 'How 
#'respected do you feel around other colleagues at your school' (0.72) and 
#'Overall, how much do you feel like you are accepted at your school?' (0.73)
#With the EOY data there is some 'How supportive are the attitudes of your 
#colleagues towards each other? and 'Overall, how supportive is the working environment 
#at your school?' items in 'School Climate' construct are relatively correlated 
#with all four items in the 'Sense of Belonging' scale (0.51-0.69), but honestly it's probably ok.

#### ************Cronbach's Alpha and Item-total correlations****************

#Educating all Students
psych::alpha(`Educating all students` |> 
               mutate_if(is.factor,as.numeric))
#0.81 Cronbach's alpha and item-total correlations are all above 0.3. (r.drop=0.40-0.77) 

#School Climate
psych::alpha(`School Climate` |> 
               mutate_if(is.factor,as.numeric))
#0.73 Cronbach's alpha. 'How often do you see students helping each other without being prompted has low item-total correlation (0.29), 
#meaning it doesn't correlate very well with the scale overall. Others are fine (0.53-0.66). Reliability also increases if we drop
#this item significantly (0.79). 

#SEL in Classroom
psych::alpha(`SEL in Classroom` |> 
               mutate_if(is.factor,as.numeric))
#0.86 Cronbach's alpha and item-total correlations are all above 0.3 (0.62-0.75).

# Sense of Belonging**
psych::alpha(`Sense of Belonging` |> 
               mutate_if(is.factor,as.numeric))
#0.91 Cronbach's alpha and item-total correlations are all above 0.3 (0.74-0.83)

#### ***************************CFA**************************

#install.packages("lavaan")
library(lavaan)

#writing dataframe with all items and shorter variable names [IMPORTANT: The code below is treating the Lickert scale items as numeric; however, 
# if we believe the distances between response options on the Likert scale are not equal (e.g., the difference between "Strongly agree" and 
# "Agree" might not be the same as "Agree" and "Neutral") then this needs to be adjusted so that Lickert scale items are ordinal]
cfa_data <- bind_cols(df_numeric_Educating, df_numeric_Climate, df_numeric_SEL, df_numeric_belonging)
print(cfa_data)
sum(is.na(cfa_data))
colnames(cfa_data)<-c("e1", "e2", "e3", "e4", "c1", "c2", "c3", "c4", "s1", "s2", "s3", "s4", "s5", "b1", "b2", "b3", "b4")
head(cfa_data)
#Educating all students items**
#e1: How easy is it to interact with students at your school who have different backgrounds from your own?
#e2: If students with different backgrounds struggled to get along in your class, how comfortable would you be intervening?
#e3: How comfortable would you be having conversations about race and gender with your students?
#e4: When a sensitive issue of diversity arises in class, how easily can you use strategies to address the situation?
#School Climate items**
#c1: How often do you see students helping each other without being prompted?
#c2: How respectful are the relationships between staff and students?
#c3: How supportive are the attitudes of your colleagues towards each other? 
#c4: Overall, how supportive is the working environment at your school?
#SEL in Classroom items**
#s1: How confident are you that you can - engage students who struggle with motivation
#s2: How confident are you that you can - help your school's most challenging students to learn
#s3: How confident are you that you can - support your students' growth in developing empathy?
#s4: How confident are you that you can - support your students' growth in developing self-awareness?
#s5: How confident are you that you can - support your students' growth in developing self-management?
#Sense of Belonging items**
#b1: How well do your colleagues at school understand you as a person?
#b2: How connected do you feel to other adults at your school? 
#b3: How respected do you feel around other colleagues at your school?
#b4: Overall, how much do you feel like you are accepted at your school?

cfa_model<-"
#Latent factors
F1_educate =~ e1 + e2 + e3 + e4 
F2_climate =~ c1+ c2 + c3+ c4
F3_SEL =~ s1 + s2 + s3 + s4 + s5
F4_belonging =~ b1 + b2 + b3 + b4
"
fit <- cfa(cfa_model, data=cfa_data, estimator = "MLR")
summary(fit, fit.measure=TRUE, standardized=TRUE)

#Weak Fit and all items under school climate construct are insignificant 
#tried removing climate from model to investigate fit and it did not change
#CFI=.883 and TLI=.860, CFI and TLI above 0.90 is accepted as good fit
#RMSEA=0.102 RMSEA values less than 0.08 is considered good fit
#SRMR=0.093 SRMR at 0.08 is considered good fit

#checking modification indices
mod_indices <-modindices(fit)
mod_indices_sorted <-mod_indices[order(-mod_indices$mi),]
head(mod_indices_sorted)
#high modification indices between:
# b1, b2 (49.299)  
# s1, s4 (20.960)  
# b3, b4 (18.037)
# b1, b3 (17.648)
# s1, s2 (17.140)

#checking residual covariance matrix
residuals_matrix <- residuals(fit)
residual_cov_matrix <- residuals_matrix$cov
print(residual_cov_matrix)
#residual covariance:  (1) between e1 and e2 is 0.070; because this is small it suggest there could be an unexplained relationship between the two variables (2) same with c1 and c2 (0.098) [Might want to add correlation between e1 and e2 AND/OR c1 and c2] 
#residual covariance continued: (3) between s2 and s1 is large (0.163) might want to add correlation or covariance between the two (4) between b1 and b2 is also large (0.132)
# negative residual covariance: (5) c1 and c4 have a negative covariance (-0.032) adding a correlation between two variables could improve fit (6) between s1 and s4 is also lower than we want (-0.091; also probably why we see it in modification indices) 

cfa_model_2 <-"
#Latent factors
F1_educate =~ e1 + e2 + e3 + e4 
F2_climate =~ c1 + c2 + c3+ c4
F3_SEL =~ s1 + s2 + s3 + s4 + s5
F4_belonging =~ b1 + b2 + b3 + b4

#Covariances between b1 and b2 based on modification indices and theory
#b1: How well do your colleagues at school understand you as a person?
#b2: How connected do you feel to other adults at your school? 
b1~~b2
"
fit2 <- cfa(cfa_model_2, data=cfa_data, estimator = "MLR")
summary(fit2, fit.measure=TRUE, standardized=TRUE)
#fit is better
#CFI=.921 and TLI=.904, CFI and TLI above 0.90 is accepted as good fit
#RMSEA=0.084 RMSEA values less than 0.08 is considered good fit
#SRMR=0.093 SRMR at 0.08 is considered good fit
#School climate items not statistically significant again; to be expected based on BOY results

#CFA MODEL 3: Drops first item of school climate construct
cfa_model_3 <-"
#Latent factors
F1_educate =~ e1 + e2 + e3 + e4 
F2_climate =~ c2 + c3+ c4
F3_SEL =~ s1 + s2 + s3 + s4 + s5
F4_belonging =~ b1 + b2 + b3 + b4

#Covariances between b1 and b2 based on modification indices and theory
#b1: How well do your colleagues at school understand you as a person?
#b2: How connected do you feel to other adults at your school? 
b1~~b2
"
fit3 <- cfa(cfa_model_3, data=cfa_data, estimator = "MLR")
summary(fit3, fit.measure=TRUE, standardized=TRUE)
#SCHOOL CLIMATE variables are significant
#CFI=.931 and TLI=.914, CFI and TLI above 0.90 is accepted as good fit
#RMSEA=0.083 RMSEA values less than 0.08 is considered good fit
#SRMR=0.080 SRMR at 0.08 is considered good fit
#I recommend we use this model; best fitting for both BOY & EOY datasets.