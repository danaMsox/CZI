githubURL <- "https://github.com/ELJRussell/CZI/raw/refs/heads/main/MasterData.RData"
load(url(githubURL))
library(tidyverse)

############################################################################
#########################   Staff Survey   ###############################
############################################################################

table(staff$Identifier)
#double checking that this is only pilot schools
pilot_staff <- staff[staff$Identifier!="O4", ]
pilot_staff <- staff[staff$Identifier!="O1", ]
table(pilot_staff$Identifier) 
#removes and double checks any observations that are not from the two pilot schools. 

pilot_staff_factored <- pilot_staff |> 
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
`Sense of Belonging`<- pilot_staff_factored |> 
  select(`How well do your colleagues at school understand you as a person?`:
           `Overall, how much do you feel like you are accepted at your school?`)
`Educating all students` <- pilot_staff_factored |> 
  select(`How easy is it to interact with students at your school who have different backgrounds from your own?`:
           `When a sensitive issue of diversity arises in class, how easily can you use strategies to address the situation?`)
`SEL in Classroom` <- pilot_staff_factored |> 
  select(starts_with("How confident are you that you can"))
`School Climate` <- pilot_staff_factored |> 
  select(`How often do you see students helping each other without being prompted?`:
           `Overall, how supportive is the working environment at your school?`)

###########################################################
################# SEM #####################################
###########################################################

colnames(pilot_staff_factored)<-c("name", "Time", "Identifier", "Academicindegree", "Academicoutdegree", "SELindegree", "SELoutdegree", "b1", "b2", "b3", "b4", "e1", "e2", "e3", "e4", "s1", "s2", "s3", "s4", "s5", "c1", "c2", "c3", "c4")

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

pilot_staff_numeric <- pilot_staff_factored %>%
  mutate(across(c("b1", "b2", "b3", "b4", "e1", "e2", "e3", "e4", "s1", "s2", "s3", "s4", "s5", "c1", "c2", "c3", "c4"), as.numeric))


###Checking fit of model with both EOY and BOY data
library(lavaan)
cfa_model <-"
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
fit <- cfa(cfa_model, data=pilot_staff_numeric, estimator = "MLR")
summary(fit, fit.measure=TRUE, standardized=TRUE)
#all variables are significant
#CFI=.938 and TLI=.923, CFI and TLI above 0.90 is accepted as good fit
#RMSEA=0.076 RMSEA values less than 0.08 is considered good fit
#SRMR=0.065 SRMR at 0.08 is considered good fit

####SEM Model that controls for school

model_control <-"
#Latent factors
F1_educate =~ e1 + e2 + e3 + e4 
F2_climate =~ c2 + c3+ c4
F3_SEL =~ s1 + s2 + s3 + s4 + s5
F4_belonging =~ b1 + b2 + b3 + b4

#Covariances between b1 and b2 based on modification indices and theory
#b1: How well do your colleagues at school understand you as a person?
#b2: How connected do you feel to other adults at your school? 
b1~~b2

# regress latent factors on school covariate
F1_educate ~ Identifier
F2_climate ~ Identifier
F3_SEL ~ Identifier
F4_belonging ~ Identifier
"

#SEM model that compares across time
fit_time <- sem(model_control,
           data = pilot_staff_numeric,
           group = "Time",
           estimator = "MLR",
           missing = "listwise", #Missing listwise excludes cases of missing values
           meanstructure = TRUE)

# View fit summary with fit measures and standardized estimates
summary(fit_time, fit.measures = TRUE, standardized = TRUE)
#Good fit statistics

# Test for Measurement Invariance [to compare factor means over time]
# 1. Configural Invariance (no constraints)
fit_config <- cfa(model_control, data = pilot_staff_numeric, group = "Time", meanstructure = TRUE)

# 2. Metric Invariance (equal loadings)
fit_metric <- cfa(model_control, data = pilot_staff_numeric, group = "Time", group.equal = "loadings", meanstructure = TRUE)

# 3. Scalar Invariance (equal loadings + intercepts)
fit_scalar <- cfa(model_control, data = pilot_staff_numeric, group = "Time", group.equal = c("loadings", "intercepts"), meanstructure = TRUE)

# Compare models (optional)
anova(fit_config, fit_metric, fit_scalar)
#Configural=same factor structure across time
#Metric=items relate similarly to factors across time
#Scalar=BORDERLINE!! Scalar invariance is marginal, so we need to be cautious interpreting latent mean differences over time
#B/c of this I am going to check modification indices to see which intercepts are non-invariant, so I can free those while keeping other constrained. 
lavTestScore(fit_scalar)
#checked for p-values <.05 found two (.p53 & .p57). Checking to see which items these constraints correspond to
parameterEstimates(fit_scalar)[c(53, 57), ]
#They correspond to c2 and s2, so adding them to the model

fit_partial_scalar <- cfa(
  model_control,
  data = pilot_staff_numeric,
  group = "Time",
  group.equal = c("loadings", "intercepts"),
  group.partial = c("c2~1", "s2~1"),  
  meanstructure = TRUE
)
summary(fit_partial_scalar, fit.measures = TRUE, standardized = TRUE)
anova(fit_metric, fit_partial_scalar)
#Fit did not significant worsen when we stopped constraining c2 and s2 to be equal
#B/c c2 and s2 are not constrained, they no longer bias the latent mean difference estimates

#Extracting Latent Means
parameterEstimates(fit_partial_scalar, standardized = TRUE) %>%
  filter(op == "~1", grepl("F", lhs))  # "~1" means intercepts; this filters for latent means
#Lavaan sets latent means to 0 for reference group in this case, BOY. So, latent means for EOY are interpreted as mean differences from BOY. 
#All latent mean differences (remember to look at time group 2 for the differences because this is EOY) are very close to zero and thus non-significant as we would expect
#Additionally, the confidence intervals include zero for all factors, which is another reinforcing indicator that there are no significant latent mean differences between time points.

#SUMMARY OF RESULTS: 

#Model and factors (Partial Scalar Model):
#Model has good fit based on indices; CFI=0.934; TLI=0.925; RMSEA=0.073; SRMR=0.072
#All factor loadings are positive and significant ranging from moderate (>0.5) to strong (>0.7)

#School control:
#For each factor regressed on school none of the regression coefficients are significant. 
#This suggests that school does not significantly predict the latent constructs. 
#[School doesn't strongly influence on the teachers' scores in these factors of interest.]

#Latent Mean Comparison:
#There is no evidence of significant change in the latent factor means across 
#time for any of the four latent factors (Educating all students, school climate, 
#SEL in classroom, and sense of belonging)