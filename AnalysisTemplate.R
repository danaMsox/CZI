githubURL <- "https://github.com/ELJRussell/CZI/raw/refs/heads/main/MasterSurvey.RData"
load(url(githubURL))

library(tidyverse)

## Staff Survey
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


## Creating a codebook
library(codebookr)
study_codebook <- codebookr::codebook(staffsurveysfactored)
print(study_codebook, "basic_staff_codebook.docx")

study_codebook <- codebookr::codebook(studentsurveyFairdale |> 
                                        select(rowid,`How often do you track your academic progress and figure out where you can do better in: - Your Crew?`:race2))
print(study_codebook, "basic_student_codebook.docx")

## Looking at Cronbach's alpha of staff survey
library(psych)

M<- cor(staffsurveysfactored |> 
          na.omit() |> 
          select(-rowid) |> 
          mutate_if(is.factor,as.numeric))

library(corrplot)

corrplot(M, method="number")
psych::alpha(`Sense of Belonging` |> 
               mutate_if(is.factor,as.numeric))
#raw alpha .91
psych::alpha(`Educating all students` |> 
               mutate_if(is.factor,as.numeric))
#raw alpha .79
psych::alpha(`SEL in Classroom` |> 
               mutate_if(is.factor,as.numeric))
#raw alpha .79
psych::alpha(`School Climate` |> 
               mutate_if(is.factor,as.numeric))
#raw alpha .74
