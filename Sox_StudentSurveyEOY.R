githubURL <- "https://github.com/ELJRussell/CZI/raw/refs/heads/main/MasterData.RData"
load(url(githubURL))
library(tidyverse)
############################################################################
#########################   Student Survey   ###############################
############################################################################

eoy_studentsurveys <- subset(studentsurvey, Time == "EOY")
#creating dataframe with just end of year data in them
table(eoy_studentsurveys$Identifier)
#double checking that this is only pilot schools

boy_studentsurveys <- subset(studentsurvey, Time == "BOY")
table(boy_studentsurveys$Identifier)
#there are 87 observations that are not pilot school; dropping them for comparison between eoy/boy
boy_studentsurveys <- boy_studentsurveys[boy_studentsurveys$Identifier!="O1", ]
table(boy_studentsurveys$Identifier) #double checking all 87 observations are dropped
##########################
########End of Year#######
##########################

#constructs of student survey
`Track Academics`<- eoy_studentsurveys |> 
  select(starts_with("How often do you track"))
names (`Track Academics`) <- c("TrackAcad_Crew", 
                               "TrackAcad_1Class", 
                               "TrackAcad_MostClass", 
                               "TrackAcad_School")
`Lead` <- eoy_studentsurveys |> 
  select(starts_with("How often do you get to lead"))
names (`Lead`) <- c("Lead_Crew", 
                    "Lead_1Class", 
                    "Lead_MostClass", 
                    "Lead_School")

`Need Help Academics` <- eoy_studentsurveys |> 
  select(starts_with("When you need help with an academic"))
names (`Need Help Academics`) <- c("NeedHelpAcad_1Classmate", 
                    "NeedHelpAcad_MostClassmate",
                    "NeedHelpAcad_1Teach",
                    "NeedHelpAcad_MostTeach",
                    "NeedHelpAcad_CrewTeach")

`Resolve Disagreements` <- eoy_studentsurveys |> 
  select(starts_with("When there are disagreements"))
names (`Resolve Disagreements`) <- c("ResolveDisagree_Crew",
                                   "ResolveDisagree_1Class",
                                   "ResolveDisagree_MostClass",
                                   "ResolveDisagree_School")

`Identity Conversations` <- eoy_studentsurveys |> 
  select(starts_with("How often are important conversations"))
names (`Identity Conversations`) <- c("IdentityConvo_Crew",
                                      "IdentityConvo_1Class",
                                      "IdentityConvo_MostClass",
                                      "IdentityConvo_School")

`Comfort Identity` <- eoy_studentsurveys |> 
  select(starts_with("How comfortable are you sharing"))
names (`Comfort Identity`) <- c("ComfortIdentity_Crew",
                                      "ComfortIdentity_1Class",
                                      "ComfortIdentity_MostClass",
                                      "ComfortIdentity_School")

`Feel Accepted` <- eoy_studentsurveys |> 
  select(starts_with("How much do you feel like you are accepted"))
names (`Feel Accepted`) <- c("FeelAccept_Crew",
                                "FeelAccept_1Class",
                                "FeelAccept_MostClass",
                                "FeelAccept_School")

`Honest Thought` <- eoy_studentsurveys |> 
  select(starts_with("How honest can you be"))
names (`Honest Thought`) <- c("HonestThought_Crew",
                                "HonestThought_1Class",
                                "HonestThought_MostClass",
                                "HonestThought_School")

`Feel Cared` <- eoy_studentsurveys |> 
  select(starts_with("How much do you feel like the following people"))
names (`Feel Cared`) <- c("FeelCared_CrewTeach",
                                    "FeelCared_CrewStudents",
                                    "FeelCared_MostClassStudents",
                                    "FeelCared_MostClassTeachers")

`Help Personal` <- eoy_studentsurveys |> 
  select(starts_with("When you need help with a personal problem"))
names (`Help Personal`) <- c("HelpPersonal_1Classmate",
                                "HelpPersonal_MostClassmate",
                                "HelpPersonal_1Teach",
                                "HelpPersonal_MostTeach",
                                "HelpPersonal_CrewTeach")

##### **************Distribution of responses for each item***************** 
library(ggplot2)
library(tidyr)

#Track Academics
df_long_TrackAcademics <- `Track Academics`|>
  pivot_longer(cols=everything(), names_to="Variable", values_to= "Response") |>
  mutate(Response=as.factor(Response)) |>
  filter(!is.na(Response))
ggplot(df_long_TrackAcademics, aes(x = Response, fill = Variable)) +
  geom_bar(stat = "count", position = "dodge", show.legend = FALSE) +
  facet_wrap(~ Variable, scales = "free_x") +
  theme_minimal() +
  labs(title = "Distribution of Categorical Responses",
       x = "Response", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Mostly Normal

#Lead
df_long_Lead <- `Lead`|>
  pivot_longer(cols=everything(), names_to="Variable", values_to= "Response") |>
  mutate(Response=as.factor(Response)) |>
  filter(!is.na(Response))
ggplot(df_long_Lead, aes(x = Response, fill = Variable)) +
  geom_bar(stat = "count", position = "dodge", show.legend = FALSE) +
  facet_wrap(~ Variable, scales = "free_x") +
  theme_minimal() +
  labs(title = "Distribution of Categorical Responses",
       x = "Response", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Mostly Normal, one with evidence of left skew.

#Need Help Academics
df_long_NeedHelpAcademics <- `Need Help Academics`|>
  pivot_longer(cols=everything(), names_to="Variable", values_to= "Response") |>
  mutate(Response=as.factor(Response)) |>
  filter(!is.na(Response))
ggplot(df_long_NeedHelpAcademics, aes(x = Response, fill = Variable)) +
  geom_bar(stat = "count", position = "dodge", show.legend = FALSE) +
  facet_wrap(~ Variable, scales = "free_x") +
  theme_minimal() +
  labs(title = "Distribution of Categorical Responses",
       x = "Response", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Majority of items show evidence of right skew.

#Resolve Disagreements
df_long_ResolveDisagreements <- `Resolve Disagreements`|>
  pivot_longer(cols=everything(), names_to="Variable", values_to= "Response") |>
  mutate(Response=as.factor(Response)) |>
  filter(!is.na(Response))
ggplot(df_long_ResolveDisagreements, aes(x = Response, fill = Variable)) +
  geom_bar(stat = "count", position = "dodge", show.legend = FALSE) +
  facet_wrap(~ Variable, scales = "free_x") +
  theme_minimal() +
  labs(title = "Distribution of Categorical Responses",
       x = "Response", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Normal Distribution

#Identity Conversations
df_long_IdentityConversations <- `Identity Conversations`|>
  pivot_longer(cols=everything(), names_to="Variable", values_to= "Response") |>
  mutate(Response=as.factor(Response)) |>
  filter(!is.na(Response))
ggplot(df_long_IdentityConversations, aes(x = Response, fill = Variable)) +
  geom_bar(stat = "count", position = "dodge", show.legend = FALSE) +
  facet_wrap(~ Variable, scales = "free_x") +
  theme_minimal() +
  labs(title = "Distribution of Categorical Responses",
       x = "Response", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Normal-ISH Distribution

#Comfort Identity
df_long_ComfortIdentity <- `Comfort Identity`|>
  pivot_longer(cols=everything(), names_to="Variable", values_to= "Response") |>
  mutate(Response=as.factor(Response)) |>
  filter(!is.na(Response))
ggplot(df_long_ComfortIdentity, aes(x = Response, fill = Variable)) +
  geom_bar(stat = "count", position = "dodge", show.legend = FALSE) +
  facet_wrap(~ Variable, scales = "free_x") +
  theme_minimal() +
  labs(title = "Distribution of Categorical Responses",
       x = "Response", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Mostly Normal

#Feel Accepted
df_long_FeelAccepted <- `Feel Accepted`|>
  pivot_longer(cols=everything(), names_to="Variable", values_to= "Response") |>
  mutate(Response=as.factor(Response)) |>
  filter(!is.na(Response))
ggplot(df_long_FeelAccepted, aes(x = Response, fill = Variable)) +
  geom_bar(stat = "count", position = "dodge", show.legend = FALSE) +
  facet_wrap(~ Variable, scales = "free_x") +
  theme_minimal() +
  labs(title = "Distribution of Categorical Responses",
       x = "Response", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Right skew

#Feel Accepted
df_long_HonestThought <- `Honest Thought`|>
  pivot_longer(cols=everything(), names_to="Variable", values_to= "Response") |>
  mutate(Response=as.factor(Response)) |>
  filter(!is.na(Response))
ggplot(df_long_HonestThought, aes(x = Response, fill = Variable)) +
  geom_bar(stat = "count", position = "dodge", show.legend = FALSE) +
  facet_wrap(~ Variable, scales = "free_x") +
  theme_minimal() +
  labs(title = "Distribution of Categorical Responses",
       x = "Response", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#trending towards normal-ish

#Feel Cared
df_long_FeelCared <- `Feel Cared`|>
  pivot_longer(cols=everything(), names_to="Variable", values_to= "Response") |>
  mutate(Response=as.factor(Response)) |>
  filter(!is.na(Response))
ggplot(df_long_FeelCared, aes(x = Response, fill = Variable)) +
  geom_bar(stat = "count", position = "dodge", show.legend = FALSE) +
  facet_wrap(~ Variable, scales = "free_x") +
  theme_minimal() +
  labs(title = "Distribution of Categorical Responses",
       x = "Response", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Trending towards normal-ish

#Help Personal
df_long_HelpPersonal<- `Help Personal`|>
  pivot_longer(cols=everything(), names_to="Variable", values_to= "Response") |>
  mutate(Response=as.factor(Response)) |>
  filter(!is.na(Response))
ggplot(df_long_HelpPersonal, aes(x = Response, fill = Variable)) +
  geom_bar(stat = "count", position = "dodge", show.legend = FALSE) +
  facet_wrap(~ Variable, scales = "free_x") +
  theme_minimal() +
  labs(title = "Distribution of Categorical Responses",
       x = "Response", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Trending toward normal-ish

df_long_HelpPersonal<- `Help Personal BOY`|>
  pivot_longer(cols=everything(), names_to="Variable", values_to= "Response") |>
  mutate(Response=as.factor(Response)) |>
  filter(!is.na(Response))
ggplot(df_long_HelpPersonal, aes(x = Response, fill = Variable)) +
  geom_bar(stat = "count", position = "dodge", show.legend = FALSE) +
  facet_wrap(~ Variable, scales = "free_x") +
  theme_minimal() +
  labs(title = "Distribution of Categorical Responses",
       x = "Response", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### **TAKEAWAYS: Some 'constructs' are normal and trending towards normal
# but others are not. May want to assume non-normal distribution for analyses to be safe. 

##### ***************Correlation Matrix for all variables********************
library(tidyverse)
library(ggplot2)
library(tidyr)
library(psych)
library(knitr)
library(kableExtra)
library(gridExtra)
cor_data <- bind_cols(`Track Academics`, `Lead`, `Need Help Academics`, `Resolve Disagreements`, `Identity Conversations`, `Comfort Identity`, `Feel Accepted`, `Honest Thought`, `Feel Cared`, `Help Personal`)

M2<- cor(cor_data |> 
           na.omit() |> 
           mutate_if(is.factor,as.numeric))

M2_rounded <- round(M2,2)

cor_table <- M2_rounded %>%
  as.data.frame() %>%
  rownames_to_column("Items") %>%
  kable("html", caption = "Inter-Item Correlation Matrix") %>%
  kable_styling(full_width = F, position = "center", font_size = 11) %>%
  column_spec(1, bold = TRUE)

cor_table #builds correlation matrix 


#### ************Cronbach's Alpha and Item-total correlations****************

###### ****Survey by Items****

#Track Academics
psych::alpha(`Track Academics` |> 
               mutate_if(is.factor,as.numeric))
#0.80 Cronbach's alpha and item-total correlations are all above 0.3. (r.drop=0.44-0.74) 

#Lead
psych::alpha(`Lead` |> 
               mutate_if(is.factor,as.numeric))
#0.87 Cronbach's alpha and item-total correlations are all above 0.3. (r.drop=0.65-0.80)

#Need Help Academics
psych::alpha(`Need Help Academics` |> 
               mutate_if(is.factor,as.numeric))
#0.86 Cronbach's alpha and item-total correlations are all above 0.3. (r.drop=0.59-0.80)

#Resolve Disagreements
psych::alpha(`Resolve Disagreements` |> 
               mutate_if(is.factor,as.numeric))
#0.91 Cronbach's alpha and item-total correlations are all above 0.3. (r.drop=0.72-0.86)

#Identity Conversations
psych::alpha(`Identity Conversations` |> 
               mutate_if(is.factor,as.numeric))
#0.94 Cronbach's alpha and item-total correlations are all above 0.3. (r.drop=0.86-0.94)

#Comfort Identity
psych::alpha(`Comfort Identity` |> 
               mutate_if(is.factor,as.numeric))
#0.96 Cronbach's alpha and item-total correlations are all above 0.3. (r.drop=0.86-0.94)

#Feel Accepted
psych::alpha(`Feel Accepted` |> 
               mutate_if(is.factor,as.numeric))
#0.93 Cronbach's alpha and item-total correlations are all above 0.3. (r.drop=0.79-0.89)

#Honest Thought
psych::alpha(`Honest Thought` |> 
               mutate_if(is.factor,as.numeric))
#0.94 Cronbach's alpha and item-total correlations are all above 0.3. (r.drop=0.83-0.92)

#Feel Cared
psych::alpha(`Feel Cared` |> 
               mutate_if(is.factor,as.numeric))
#0.73 Cronbach's alpha and item-total correlations are all above 0.3. (r.drop=0.49-0.58)

#Help Personal
psych::alpha(`Help Personal` |> 
               mutate_if(is.factor,as.numeric))
#0.90 Cronbach's alpha and item-total correlations are all above 0.3. (r.drop=0.71-0.81)


##### Survey by Context

Crew <- cor_data[, c('TrackAcad_Crew', 
                     'Lead_Crew', 
                     'NeedHelpAcad_CrewTeach', 
                     'ResolveDisagree_Crew', 
                     'IdentityConvo_Crew', 
                     'ComfortIdentity_Crew', 
                     'HonestThought_Crew', 
                     'FeelAccept_Crew',
                     'HelpPersonal_CrewTeach')]
psych::alpha(Crew |> 
               mutate_if(is.factor,as.numeric))
#0.82 Cronbach's alpha and item-total correlations are all above 0.3. (r.drop=0.35-0.62)

OneClass <- cor_data[, c('TrackAcad_1Class',
                         'Lead_1Class',
                         'NeedHelpAcad_1Classmate',
                         'NeedHelpAcad_1Teach',
                         'ResolveDisagree_1Class',
                         'IdentityConvo_1Class',
                         'ComfortIdentity_1Class',
                         'HonestThought_1Class',
                         'FeelAccept_1Class',
                         'HelpPersonal_1Classmate',
                         'HelpPersonal_1Teach')]
psych::alpha(OneClass |> 
               mutate_if(is.factor,as.numeric))
#0.85 Cronbach's alpha and item-total correlations are all above 0.3. (r.drop=0.36-0.64)

MostClass <- cor_data[, c('TrackAcad_MostClass',
                          'Lead_MostClass',
                          'NeedHelpAcad_MostClassmate',
                          'NeedHelpAcad_MostTeach',
                          'ResolveDisagree_MostClass',
                          'IdentityConvo_MostClass',
                          'ComfortIdentity_MostClass',
                          'HonestThought_MostClass',
                          'FeelAccept_MostClass',
                          'HelpPersonal_MostClassmate',
                          'HelpPersonal_MostTeach')]
psych::alpha(MostClass |> 
               mutate_if(is.factor,as.numeric))
#0.85 Cronbach's alpha and item-total correlations are all above 0.3. (r.drop=0.38-0.67)

School <- cor_data[, c('TrackAcad_School',
                       'Lead_School',
                       'ResolveDisagree_School',
                       'IdentityConvo_School',
                       'ComfortIdentity_School',
                       'HonestThought_School',
                       'FeelAccept_School')]
psych::alpha(School |> 
               mutate_if(is.factor,as.numeric))
#0.74 Cronbach's alpha and item-total correlations are all above 0.3. (r.drop=0.29-0.56)

##############################################################
#### ***************************CFA**********************####
#############################################################

#install.packages("lavaan")
library(lavaan)

#writing dataframe with all items and shorter variable names [IMPORTANT: The code below is treating the Lickert scale items as numeric; however, 
# if we believe the distances between response options on the Likert scale are not equal (e.g., the difference between "Strongly agree" and 
# "Agree" might not be the same as "Agree" and "Neutral") then this needs to be adjusted so that Lickert scale items are ordinal]
item_CFA_data <- cor_data %>%
  mutate_if(~!is.numeric(.), as.numeric)
print(item_CFA_data)
sum(is.na(item_CFA_data))
head(item_CFA_data)

### *****************By Items************************

cfa_model<-"
#Latent factors
F1_trackacad =~ TrackAcad_Crew + TrackAcad_1Class + TrackAcad_MostClass +TrackAcad_School
F2_lead =~ Lead_Crew + Lead_1Class + Lead_MostClass + Lead_School
F3_helpacad =~ NeedHelpAcad_1Classmate + NeedHelpAcad_MostClassmate + NeedHelpAcad_1Teach + NeedHelpAcad_MostTeach + NeedHelpAcad_CrewTeach
F4_resolve =~ ResolveDisagree_Crew + ResolveDisagree_1Class + ResolveDisagree_MostClass + ResolveDisagree_School
F5_convo =~ IdentityConvo_Crew + IdentityConvo_1Class + IdentityConvo_MostClass + IdentityConvo_School
F6_comfort =~ ComfortIdentity_Crew + ComfortIdentity_1Class + ComfortIdentity_MostClass + ComfortIdentity_School
F7_accept =~ FeelAccept_Crew + FeelAccept_1Class + FeelAccept_MostClass + FeelAccept_School 
F8_honest =~ HonestThought_Crew + HonestThought_1Class + HonestThought_MostClass + HonestThought_School
F9_cared =~ FeelCared_CrewTeach + FeelCared_CrewStudents + FeelCared_MostClassStudents + FeelCared_MostClassTeachers
F10_personal =~ HelpPersonal_1Classmate + HelpPersonal_MostClassmate + HelpPersonal_1Teach + HelpPersonal_MostTeach + HelpPersonal_CrewTeach"

fit <- cfa(cfa_model, data=item_CFA_data, estimator = "MLR")
summary(fit, fit.measure=TRUE, standardized=TRUE)
#Acceptable, but not excellent
#CFI=.891 and TLI=.893, CFI and TLI above 0.90 is accepted as good fit
#RMSEA=0.071 RMSEA values less than 0.08 is considered good fit
#SRMR=0.057 which is considered good fit

#checking modification indices
mod_indices <-modindices(fit)
mod_indices_sorted <-mod_indices[order(-mod_indices$mi),]
head(mod_indices_sorted)
#high modification indices between many of the items. Seems to be somewhat centered around different items in the same context (e.g., Crew), but this isn't always the case. 
# For example 'how comfortable are you sharing thoughts around identity' and 'how honest can you be sharing your personal thoughts and opinions, even when others disagree' in your Crew has a high MI (160.91)
# Academic and personal help for both crew teacher and most classmates has a high MI (132.17 & 126.94)

#checking residual covariance matrix
residuals_matrix <- residuals(fit)
residual_cov_matrix <- residuals_matrix$cov
print(residual_cov_matrix)

#### ***** By Location/Person
cfa_model_2 <-"
#Latent factors
F1_Crew =~ TrackAcad_Crew + Lead_Crew + NeedHelpAcad_CrewTeach + ResolveDisagree_Crew+ IdentityConvo_Crew + ComfortIdentity_Crew + FeelAccept_Crew + HonestThought_Crew + HelpPersonal_CrewTeach 
F2_Class =~ TrackAcad_1Class + Lead_1Class + NeedHelpAcad_1Classmate + ResolveDisagree_1Class + IdentityConvo_1Class + ComfortIdentity_1Class + FeelAccept_1Class + HonestThought_1Class + HelpPersonal_1Teach
F3_MostClasses =~ TrackAcad_MostClass + Lead_MostClass + NeedHelpAcad_MostClassmate + ResolveDisagree_MostClass + IdentityConvo_MostClass + ComfortIdentity_MostClass + FeelAccept_MostClass + HonestThought_MostClass + HelpPersonal_MostTeach
F4_School =~ TrackAcad_School + Lead_School + ResolveDisagree_School + IdentityConvo_School + ComfortIdentity_School + FeelAccept_School + HonestThought_School"

fit2 <- cfa(cfa_model_2, data=item_CFA_data, estimator = "MLR")
summary(fit2, fit.measure=TRUE, standardized=TRUE)
#Warning message given
summary(fit2, standardized = TRUE)
#upon further investigation the standardized latent covariances between Crew~Class; Class~MostClasses; and MostClasses~School are greater than one and thus highly correlated.
#Option 1. We could collapse factors that are highly correlated (e.g., treat most classes and school as a singular construct of school climate?)
#Option 2. We could build a higher-order actor model whereby the first-order factors are the 4 constructs and there is a higher order factor that contains the four construct maybe 'character development'?
#Option 3. Constrain our covariances to be values <1 or add penalty via regularized CFA (regsem package), but this method is not recommended

#Wonder: With these results, I am wondering if we want to continue trying to fit a model to all the items. It seems like the similar questions and similar locations are up against each other and
#causing some correlation and other issues when we try to fit everything together.

############################################################################
#########################   EOY & BOY Descriptive Stats#####################
############################################################################

###############################
#### EOY Descriptive Stats ####
###############################

library(dplyr)
library(purrr)
library(tidyr)

# Put your data frames in a named list
df_list_eoy <- list(
  `Track Academics` = `Track Academics`,
  Lead = Lead,
  `Need Help Academics` = `Need Help Academics`,
  `Resolve Disagreements` = `Resolve Disagreements`,
  `Identity Conversations` = `Identity Conversations`,
  `Comfort Identity` = `Comfort Identity`,
  `Feel Accepted` = `Feel Accepted`,
  `Honest Thought` = `Honest Thought`,
  `Feel Cared` = `Feel Cared`,
  `Help Personal` = `Help Personal`
)

summary_tidy_eoy <- map2_dfr(df_list_eoy, names(df_list_eoy), function(df, nm) {
  df %>%
    mutate(across(everything(), as.numeric)) %>%
    summarise(across(everything(), list(mean = ~mean(.x, na.rm=TRUE),
                                        sd = ~sd(.x, na.rm=TRUE)))) %>%
    mutate(dataset = nm) %>%
    pivot_longer(
      cols = -dataset,
      names_to = c("variable", "stat"),
      names_sep = "_(?=[^_]+$)",  # split at the last underscore
      values_drop_na = TRUE
    )
})

View(summary_tidy_eoy)

#########################################
## BOY Descriptive Statistics###########
#########################################


#constructs of student survey
#COPIED FROM ABOVE
#Must run this code if you are not running through entire file
#boy_studentsurveys <- subset(studentsurvey, Time == "BOY")
#table(boy_studentsurveys$Identifier)
#there are 87 observations that are not pilot school; dropping them for comparison between eoy/boy
#boy_studentsurveys <- boy_studentsurveys[boy_studentsurveys$Identifier!="O1", ]
#table(boy_studentsurveys$Identifier) #double checking all 87 observations are dropped

`Track Academics BOY`<- boy_studentsurveys |> 
  select(starts_with("How often do you track"))
names (`Track Academics`) <- c("TrackAcad_Crew", 
                               "TrackAcad_1Class", 
                               "TrackAcad_MostClass", 
                               "TrackAcad_School")

`Lead BOY` <- boy_studentsurveys |> 
  select(starts_with("How often do you get to lead"))
names (`Lead`) <- c("Lead_Crew", 
                    "Lead_1Class", 
                    "Lead_MostClass", 
                    "Lead_School")

`Need Help Academics BOY` <- boy_studentsurveys |> 
  select(starts_with("When you need help with an academic"))
names (`Need Help Academics`) <- c("NeedHelpAcad_1Classmate", 
                                   "NeedHelpAcad_MostClassmate",
                                   "NeedHelpAcad_1Teach",
                                   "NeedHelpAcad_MostTeach",
                                   "NeedHelpAcad_CrewTeach")

`Resolve Disagreements BOY` <- boy_studentsurveys |> 
  select(starts_with("When there are disagreements"))
names (`Resolve Disagreements`) <- c("ResolveDisagree_Crew",
                                     "ResolveDisagree_1Class",
                                     "ResolveDisagree_MostClass",
                                     "ResolveDisagree_School")

`Identity Conversations BOY` <- boy_studentsurveys |> 
  select(starts_with("How often are important conversations"))
names (`Identity Conversations`) <- c("IdentityConvo_Crew",
                                      "IdentityConvo_1Class",
                                      "IdentityConvo_MostClass",
                                      "IdentityConvo_School")

`Comfort Identity BOY` <- boy_studentsurveys |> 
  select(starts_with("How comfortable are you sharing"))
names (`Comfort Identity`) <- c("ComfortIdentity_Crew",
                                "ComfortIdentity_1Class",
                                "ComfortIdentity_MostClass",
                                "ComfortIdentity_School")

`Feel Accepted BOY` <- boy_studentsurveys |> 
  select(starts_with("How much do you feel like you are accepted"))
names (`Feel Accepted`) <- c("FeelAccept_Crew",
                             "FeelAccept_1Class",
                             "FeelAccept_MostClass",
                             "FeelAccept_School")

`Honest Thought BOY` <- boy_studentsurveys |> 
  select(starts_with("How honest can you be"))
names (`Honest Thought`) <- c("HonestThought_Crew",
                              "HonestThought_1Class",
                              "HonestThought_MostClass",
                              "HonestThought_School")

`Feel Cared BOY` <- boy_studentsurveys |> 
  select(starts_with("How much do you feel like the following people"))
names (`Feel Cared`) <- c("FeelCared_CrewTeach",
                          "FeelCared_CrewStudents",
                          "FeelCared_MostClassStudents",
                          "FeelCared_MostClassTeachers")


`Help Personal BOY` <- boy_studentsurveys |> 
  select(starts_with("When you need help with a personal problem"))
names (`Help Personal`) <- c("HelpPersonal_1Classmate",
                             "HelpPersonal_MostClassmate",
                             "HelpPersonal_1Teach",
                             "HelpPersonal_MostTeach",
                             "HelpPersonal_CrewTeach")

df_list_boy <- list(
  `Track Academics BOY` = `Track Academics BOY`,
  `Lead BOY` = `Lead BOY`,
  `Need Help Academics BOY` = `Need Help Academics BOY`,
  `Resolve Disagreements BOY` = `Resolve Disagreements BOY`,
  `Identity Conversations BOY` = `Identity Conversations BOY`,
  `Comfort Identity BOY` = `Comfort Identity BOY`,
  `Feel Accepted BOY` = `Feel Accepted BOY`,
  `Honest Thought BOY` = `Honest Thought BOY`,
  `Feel Cared BOY` = `Feel Cared BOY`,
  `Help Personal BOY` = `Help Personal BOY`
)

summary_tidy_boy <- map2_dfr(df_list_boy, names(df_list_boy), function(df, nm) {
  df %>%
    mutate(across(everything(), as.numeric)) %>%
    summarise(across(everything(), list(mean = ~mean(.x, na.rm=TRUE),
                                        sd = ~sd(.x, na.rm=TRUE)))) %>%
    mutate(dataset = nm) %>%
    pivot_longer(
      cols = -dataset,
      names_to = c("variable", "stat"),
      names_sep = "_(?=[^_]+$)",  # split at the last underscore
      values_drop_na = TRUE
    )
})

View(summary_tidy_boy)
