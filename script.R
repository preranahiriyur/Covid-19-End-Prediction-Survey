#setup
setwd("D:\\Prerana Hiriyur\\Miscellaneous\\My projects\\Covid19 Perception")
library(ggplot2)
library(dplyr)
library(ggsci)
library(scales)

#data cleaning and preparation
data <- read.csv("Covid-19 Perceptions Survey (Responses) - Form responses.csv", header = TRUE)
data <- subset(data, select = -c(timestamp))
print(head(data))
table(data$highest.level.of.edu)
table(data$predicted.end)
pred.end.levels <- c("The virus will mutate and become mild or harmless in the next few months", 
                     "An effective vaccine will be widely available within 4-6 months", 
                     "Without further lockdown, this virus will spread rapidly in the next 2-4 months to infect most of the population causing herd immunity", 
                     "With intermittent lockdown and releases, this virus will linger on it its current form for the next 1-2 years", 
                     "I do not see an end to this pandemic in the foreseeable future. This virus is here to stay.")
pred.end.labels <- c("Mutation", "Effective Vaccine", "Herd Immunity", "Prolonged Recovery", "Apocalypse?")
predicted.end.lev <- factor(data$predicted.end, levels = pred.end.levels , labels=c(1,2,3,4,5))
data$predicted.end.label <- factor(data$predicted.end, 
                                   levels=pred.end.levels, 
                                   labels=pred.end.labels)
profession.levels <- c("Healthcare (Doctor/Nurse/Paramedic)", 
                       "Healthcare related (Pharmaceuticals/Biotechnology/Biomedical Engineering, etc)", 
                       "Non-healthcare related profession", 
                       "Currently not working")
profession.labels <- c("Healthcare", 
                       "Healthcare related", 
                       "Non-healthcare", 
                       "Not working")
data$profession.label <- factor(data$profession, levels = profession.levels, label=profession.labels)
covid.stats.levels <- c("I am keeping track of real time Covid-19 data", 
                        "I check every day", 
                        "I check once every few days", 
                        "I check once a while (weekly/bi-weekly)",
                        "I don't pay attention to numbers")
covid.stats.labels <- c("Real Time", 
                        "Daily", 
                        "Once in a few days", 
                        "Weekly or Bi-weekly", 
                        "Anti-Stats")
data$covid19.stats.label <- factor(data$covid19.stats, covid.stats.levels, covid.stats.labels)
closest.covid.case.levels <- c("Myself", 
                               "Family member residing in the same household", 
                               "Family member but different household", 
                               "Neighbour (within apartment complex or immediate neighbourhood)", 
                               "Colleague", 
                               "Do not personally know anyone who has been infected")
closest.covid.case.labels <- c("Myself", 
                               "Family Member (Same household)", 
                               "Family Member (Different household)", 
                               "Neighbour", 
                               "Colleague", 
                               "Do not know anyone personally")
data$closest.covid.case.label <- factor(data$closest.covid.case, closest.covid.case.levels, closest.covid.case.labels)
edu.levels <- c("High school graduate, diploma or the equivalent",
                "Bachelorâ???Ts degree", 
                "Masterâ???Ts degree", 
                "Professional degree", 
                "Doctorate degree")
edu.labels <- c("High School", 
                "Bachelor's Degree", 
                "Master's degree", 
                "Professional degree", 
                "Doctorate degree")
data$highest.level.of.edu.label <- factor(data$highest.level.of.edu, edu.levels, edu.labels)
data$primary.source.of.news <- factor(data$primary.source.of.news, 
                                      levels = c("Television", 
                                                 "News apps/ Internet", 
                                                 "Social Media", 
                                                 "Newspaper", 
                                                 "Other"))

#descriptive stats 

#age - histogram
ggplot(data, aes(age)) + 
  geom_histogram(aes(y=..density..), fill="#00BFC4", alpha=0.7) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(title = "Distribution of Age Among Respondents", 
       x = "Age", y = "Density")

#gender - bar chart
ggplot(data, aes(gender)) + 
  geom_bar(width = 0.6, alpha=0.8, aes(fill=factor(gender))) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.3) +
  xlab("") + 
  ylab("Frequency") + 
  ggtitle("Gender") 

#country of residence - bar chart
ggplot(data, aes(country)) + 
  geom_bar(width = 0.6, alpha=0.8, aes(fill=factor(country))) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.3) +
  xlab("") + 
  ylab("Frequency") + 
  ggtitle("Country")

#highest level of education - bar chart 
ggplot(data, aes(highest.level.of.edu.label)) + 
  geom_bar(width = 0.6, alpha=0.8, aes(fill=factor(highest.level.of.edu.label))) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.3) +
  xlab("") + 
  ylab("Frequency") + 
  ggtitle("Level of Education") 

#profession - bar chart
ggplot(data, aes(profession.label)) + 
  geom_bar(width = 0.6, alpha=0.8, aes(fill=factor(profession.label))) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.3) +
  xlab("") + 
  ylab("Frequency") + 
  ggtitle("Distribution of Profession Among Respondents") 

#comorbid conditions - bar chart 
ggplot(data, aes(comorbid)) + 
  geom_bar(width = 0.6, alpha=0.8, aes(fill=factor(comorbid))) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.3) +
  xlab("") + 
  ylab("Frequency") + 
  ggtitle("Do you or anyone in your family have comorbid conditions?")

#infected by covid - bar chart 
ggplot(data, aes(infected)) + 
  geom_bar(width = 0.6, alpha=0.8, aes(fill=factor(infected))) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.3) +
  xlab("") + 
  ylab("Frequency") + 
  ggtitle("Have you been infected by Covid-19?") 

#primary source of news - bar chart
ggplot(data, aes(primary.source.of.news)) + 
  geom_bar(width = 0.6, alpha=0.8, aes(fill=factor(primary.source.of.news))) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.3) +
  xlab("") + 
  ylab("Frequency") + 
  ggtitle("Primary Source of News Among Respondents") 

#preferred social media platform - bar chart
ggplot(data, aes(preferred.sm.platform)) + 
  geom_bar(width = 0.6, alpha=0.8, aes(fill=factor(preferred.sm.platform))) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.3) +
  xlab("") + 
  ylab("Frequency") + 
  ggtitle("Preferred SM Platform") 

#social media activity - bar chart single colour
ggplot(data, aes(sm.activity)) + 
  geom_bar(width = 0.6, alpha=0.8, aes(fill=factor(sm.activity))) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.3) +
  xlab("") + 
  ylab("Frequency") + 
  ggtitle("SM Activity") 

#closest covid case - bar chart
ggplot(data, aes(closest.covid.case.label)) + 
  geom_bar(width = 0.6, alpha=0.8, aes(fill=factor(closest.covid.case.label))) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.3) +
  xlab("") + 
  ylab("Frequency") + 
  ggtitle("Closest Known Covid19 Case")

#tracking covid numbers - bar chart
ggplot(data, aes(covid19.stats.label)) + 
  geom_bar(width = 0.6, alpha=0.8, aes(fill=factor(covid19.stats.label))) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.3) +
  xlab("") + 
  ylab("Frequency") + 
  ggtitle("Answers to 'How often do you check covid 19 numbers' ") 

#preferred healthcare system - bar chart
ggplot(data, aes(preferred.healthcare.sys)) + 
  geom_bar(width = 0.6, alpha=0.8, aes(fill=factor(preferred.healthcare.sys))) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.3) +
  xlab("") + 
  ylab("Frequency") + 
  ggtitle("Preferred Healthcare System") 


#belief in vaccination - bar chart 
ggplot(data, aes(belief.in.vaccination)) + 
  geom_bar(width = 0.6, alpha=0.8, fill = "#F8766D") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.3) +
  xlab("") + 
  ylab("Frequency") + 
  ggtitle("Belief in Vaccination") +
  scale_x_continuous(breaks=c(seq(0,5))) + 
  scale_y_continuous(breaks = c(seq(0,600,100)))

#chosen end scenario - bar chart 
ggplot(data, aes(predicted.end.label)) + 
  geom_bar(width = 0.6, alpha=0.8, aes(fill=factor(predicted.end.label))) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.3) +
  xlab("") + 
  ylab("Frequency") + 
  ggtitle("How do you think Covid-19 will end?") 

#Hypothesis Testing

proptable.prof.end = as.data.frame(table(data$profession.lab, data$predicted.end.lab))

proptable.prof.end = proptable.prof.end %>%
  rename(
    Profession = Var1,
    Scenario = Var2
  ) %>%
  group_by(Profession) %>%
  mutate(Proportion = Freq / sum(Freq)) %>%
  mutate(Percentage = round(Proportion*100, 2))

ggplot(proptable.prof.end, aes(fill=Scenario, y=Proportion, x=Profession)) +
  geom_bar(position = "fill", stat="identity") +
  geom_text(aes(label = paste0(round(Percentage,2),"%")), 
            position = position_stack(vjust = 0.5), size = 2) +
  scale_y_continuous(labels = scales::percent) 

#2 sample test for population proportion
freq.vaccine.healthcare <- as.integer(count(data[which(data$profession.label=="Healthcare" &
                                              data$predicted.end.label=="Effective Vaccine"),]))
freq.vaccine.nonhealthcare <- as.integer(count(data[which(data$profession.label=="Non-healthcare" &
                                                            data$predicted.end.label=="Effective Vaccine"),]))
total.healthcare <- as.integer(count(data[which(data$profession.label=="Healthcare"),]))
total.nonhealthcare <- as.integer(count(data[which(data$profession.label=="Non-healthcare"),]))
test = prop.test(c(freq.vaccine.healthcare, freq.vaccine.nonhealthcare), c(total.healthcare,total.nonhealthcare))
print(test)

#Is age a confounder?
#age by profession
ggplot(data, aes(x=profession.label, y=age)) + 
  geom_boxplot(aes(fill=factor(profession.label))) + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  labs(x = "Profession", y = "Age")

ggplot(data, aes(y=age, x=predicted.end.label, color=predicted.end.label)) +
  geom_jitter() + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  labs(x = "Scenario", y = "Age", title = "Jitter Plot of Age by Chosen Scenario") 
      

# Hypothesis 2 
proptable.stats.end = as.data.frame(table(data$covid19.stats.label, data$predicted.end.lab))

proptable.stats.end = proptable.stats.end %>%
  rename(
    Rate = Var1,
    Scenario = Var2
  ) %>%
  group_by(Rate) %>%
  mutate(Proportion = Freq / sum(Freq)) %>%
  mutate(Percentage = round(Proportion*100, 2))

ggplot(proptable.stats.end, aes(fill=Scenario, y=Proportion, x=Rate)) +
  geom_bar(position = "fill", stat="identity") +
  geom_text(aes(label = paste0(round(Percentage,2),"%")), 
            position = position_stack(vjust = 0.5), size = 2) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "How often do you check covid19 stats?")

#3
proptable.news.end = as.data.frame(table(data$primary.source.of.news, data$predicted.end.lab))

proptable.news.end = proptable.news.end %>%
  rename(
    PrimarySource = Var1,
    Scenario = Var2
  ) %>%
  group_by(PrimarySource) %>%
  mutate(Proportion = Freq / sum(Freq)) %>%
  mutate(Percentage = round(Proportion*100, 2))

ggplot(proptable.news.end, aes(fill=Scenario, y=Proportion, x=PrimarySource)) +
  geom_bar(position = "fill", stat="identity") +
  geom_text(aes(label = paste0(round(Percentage,2),"%")), 
            position = position_stack(vjust = 0.5), size = 2) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "") 

#4 
ggplot(data, aes(sm.activity)) + 
  geom_bar(width = 0.6, alpha=0.8, aes(fill=factor(sm.activity))) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.3) +
  xlab("") + 
  ylab("Frequency") + 
  ggtitle("SM Activity") 


#Home remedies vs age
ggplot(data, aes(y=age, x=preferred.healthcare.sys, color=preferred.healthcare.sys)) +
  geom_jitter() + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  labs(x = "Scenario", y = "Age", title = "Jitter Plot of Age by Healthcare System") 

#closest covid case 
proptable.closest.covid = as.data.frame(table(data$closest.covid.case.label, data$predicted.end.lab))

proptable.closest.covid = proptable.closest.covid %>%
  rename(
    Closest.Case = Var1,
    Scenario = Var2
  ) %>%
  group_by(Closest.Case) %>%
  mutate(Proportion = Freq / sum(Freq)) %>%
  mutate(Percentage = round(Proportion*100, 2))

ggplot(proptable.closest.covid, aes(fill=Scenario, y=Proportion, x=Closest.Case)) +
  geom_bar(position = "fill", stat="identity") +
  geom_text(aes(label = paste0(round(Percentage,2),"%")), 
            position = position_stack(vjust = 0.5), size = 2) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Closest Known Covid-19 Case")

#sm activity 
proptable.sm.activity = as.data.frame(table(data$sm.activity, data$predicted.end.lab))

proptable.sm.activity = proptable.sm.activity %>%
  rename(
    SM.Activity = Var1,
    Scenario = Var2
  ) %>%
  group_by(SM.Activity) %>%
  mutate(Proportion = Freq / sum(Freq)) %>%
  mutate(Percentage = round(Proportion*100, 2))

ggplot(proptable.sm.activity, aes(fill=Scenario, y=Proportion, x=SM.Activity)) +
  geom_bar(position = "fill", stat="identity") +
  geom_text(aes(label = paste0(round(Percentage,2),"%")),
            position = position_stack(vjust = 0.5), size = 2) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Social Media Activity")

freq.vaccine.sm5 <- as.integer(count(data[which(data$sm.activity==5 &
                                                         data$predicted.end.label=="Apocalypse?"),]))
freq.vaccine.smnot5 <- as.integer(count(data[which(data$sm.activity!=5 &
                                                            data$predicted.end.label=="Apocalypse?"),]))
total.sm5 <- as.integer(count(data[which(data$sm.activity==5),]))
total.smnot5 <- as.integer(count(data[which(data$sm.activity!=5),]))
test = prop.test(c(freq.vaccine.sm5, freq.vaccine.smnot5), c(total.sm5,total.smnot5))
print(test)

ggplot(data, aes(y=age, x=sm.activity, color=sm.activity)) +
  geom_jitter() + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  labs(x = "Scenario", y = "Age", title = "Jitter Plot of Age by SM Activity") 

#preferred SM Platform
proptable.sm = as.data.frame(table(data$preferred.sm.platform, data$predicted.end.lab))

proptable.sm = proptable.sm %>%
  rename(
    SM = Var1,
    Scenario = Var2
  ) %>%
  group_by(SM) %>%
  mutate(Proportion = Freq / sum(Freq)) %>%
  mutate(Percentage = round(Proportion*100, 2))

ggplot(proptable.sm, aes(fill=Scenario, y=Proportion, x=SM)) +
  geom_bar(position = "fill", stat="identity") +
  geom_text(aes(label = paste0(round(Percentage,2),"%")),
            position = position_stack(vjust = 0.5), size = 2) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "")

ggplot(data, aes(y=age, x=preferred.sm.platform, color=preferred.sm.platform)) +
  geom_jitter() + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  labs(x = "Scenario", y = "Age", title = "Jitter Plot of Age by SM Platform") 

#Gender 
proptable.gender = as.data.frame(table(data$gender, data$predicted.end.lab))

proptable.gender = proptable.gender %>%
  rename(
    Gender = Var1,
    Scenario = Var2
  ) %>%
  group_by(Gender) %>%
  filter(Gender != 'Prefer not to say') %>%
  mutate(Proportion = Freq / sum(Freq)) %>%
  mutate(Percentage = round(Proportion*100, 2))

ggplot(proptable.gender, aes(fill=Scenario, y=Proportion, x=Gender)) +
  geom_bar(position = "fill", stat="identity") +
  geom_text(aes(label = paste0(round(Percentage,2),"%")),
            position = position_stack(vjust = 0.5), size = 2) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "")

#gender vs profession
proptable.gender.prof = as.data.frame(table(data$gender, data$profession.label))

proptable.gender.prof = proptable.gender.prof %>%
  rename(
    Gender = Var1,
    Profession = Var2
  ) %>%
  filter(Gender != 'Prefer not to say') %>%
  group_by(Profession) %>%
  mutate(Proportion = Freq / sum(Freq)) %>%
  mutate(Percentage = round(Proportion*100, 2))

ggplot(proptable.gender.prof, aes(fill=Gender, y=Proportion, x=Profession)) +
  geom_bar(position = "fill", stat="identity") +
  geom_text(aes(label = paste0(round(Percentage,2),"%")),
            position = position_stack(vjust = 0.5), size = 2) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "")

ggplot(proptable.prof.end, aes(fill=Scenario, y=Proportion, x=Profession)) +
  geom_bar(position = "fill", stat="identity") +
  geom_text(aes(label = paste0(round(Percentage,2),"%")), 
            position = position_stack(vjust = 0.5), size = 2) +
  scale_y_continuous(labels = scales::percent) 

