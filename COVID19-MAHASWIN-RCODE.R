#Mahaswin Ramalingam Balaji# People at risk
library(tidyverse) 
library(arsenal)
library(GGally)
library(party)

corona <- read.csv('COVID19_line_list_data.csv', stringsAsFactors = FALSE)
dim(corona)
corona$respiratory <- str_detect(corona$symptom, 'breath|pneumonia|breathlessness|dyspnea|respiratory')
corona$abdominal <- str_detect(corona$symptom, 'abdominal|diarrhea|vomiting')

corona <- corona %>% 
  select(reporting.date, country, gender, age, death, respiratory, abdominal, symptom)  %>% 
  mutate(
    death = ifelse(death == '0', 0, 1),
    country = factor(country),
    gender = factor(gender),
    death = factor(death, label = c('no','yes')),
    reporting.date = as.Date(reporting.date, format = c('$d/$m/$Y')),
    respiratory = factor(respiratory, label = c('no','yes')),
    abdominal = factor(abdominal, label = c('no','yes'))
  )

ggplot(corona, aes(death, age, fill = death))+
  geom_boxplot()+
  theme_classic()+
  scale_fill_manual(values = c('green','red'))

ggplot(corona, aes(death, fill = gender))+
  geom_bar(position ='fill')+
  theme_classic()+
  scale_fill_manual(values = c('red','green','blue'))


corona_sympton <- corona %>% 
  filter(!symptom == '') 


dim(corona_sympton)

ggplot(corona_sympton, aes(death, fill = respiratory))+
  geom_bar(position = 'fill')+
  theme_classic()+
  scale_fill_manual(values = c('green','red'))


ggplot(corona_sympton, aes(death, fill = abdominal))+
  geom_bar(position = 'fill')+
  theme_classic()+
  scale_fill_manual(values = c('green','red'))
table02 <- tableby(death ~ fe(respiratory) + fe(abdominal) + age + gender, data = corona_sympton, total = FALSE)
summary(table02, text = TRUE)



model_part2 <- ctree(death ~ age + gender + respiratory, data = corona_sympton)
plot(model_part2)
#end


