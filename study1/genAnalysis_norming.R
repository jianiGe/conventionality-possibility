#Please run from line 69

Full_results <- read.csv("data/Possibility generation.csv")
View(Full_results)
library(tidyverse)
library(lme4)
library(optimx)
install.packages("RColorBrewer")
library("RColorBrewer")
palette(brewer.pal(n=9,name="PuBu"))

# Select relevant variables

clicks <- grep("Click", names(Full_results))
data <- Full_results[-c(1,2),-c(1:6,8:12,clicks)]
data <- as.table(data) ##
#View(data)


ld <- data %>% gather(variable, value, -ResponseId, na.rm = TRUE) %>% 
  mutate(scenario = case_when(
    str_detect(variable,"X1") ~ "scenario1",
    str_detect(variable,"X2") ~ "scenario2",
    str_detect(variable,"X3") ~ "scenario3",
    str_detect(variable,"X4") ~ "scenario4",
    str_detect(variable,"X5") ~ "scenario5",
    str_detect(variable,"X6") ~ "scenario6",
    str_detect(variable,"X7") ~ "scenario7",
    str_detect(variable,"X8") ~ "scenario8"
  ), answerOrder = case_when(
    str_detect(variable, "q1") ~ "1",
    str_detect(variable, "q2") ~ "2",
    str_detect(variable, "q3") ~ "3",
    str_detect(variable, "q4") ~ "4",
    str_detect(variable, "q5") ~ "5",
    str_detect(variable, "q6") ~ "6",
    str_detect(variable, "q7") ~ "7",
    str_detect(variable, "q8") ~ "8",
    str_detect(variable, "ratings1") ~ "1",
    str_detect(variable, "ratings2") ~ "2",
    str_detect(variable, "ratings3") ~ "3",
    str_detect(variable, "ratings4") ~ "4",
    str_detect(variable, "ratings5") ~ "5",
    str_detect(variable, "ratings6") ~ "6",
    str_detect(variable, "ratings7") ~ "7",
    str_detect(variable, "ratings8") ~ "8"
  ), time = case_when(
    str_detect(variable, "Submit") ~ "time", 
    TRUE ~ "question"
  ), questionType = case_when(
    str_detect(variable, "Norm") ~ "normality",
    str_detect(variable, "Moral") ~ "morality",
    str_detect(variable, "hood") ~ "probability",
    str_detect(variable, "Rational") ~ "rationality",
    time == "time" ~ "time",
    TRUE ~ "question"
  )) %>%
  filter(!is.na(answerOrder)) %>%
  filter(value != "") %>%
  select(-c(variable,time))
  #filter(ResponseId != "R_3D704c1SnkF7le4")

wd <- ld %>% spread(questionType, value)

write.csv(wd, file="generationFull.csv",row.names = FALSE)

# Exclude non-answers 

wd_clean <- read.csv("data/generationFull_clean.csv")
#wd_clean <- wd_clean[wd_clean$exclude != 1,] ##


# Assign numbers to ratings, with "strongly agree" being a 7 (very moral, etc.) 
# and "strongly disagree" being a 1 (very immoral, etc.)

wd_clean$morality <- factor(wd_clean$morality, levels=c("Strongly Agree","Agree","Somewhat Agree","Neutral",
                                                        "Somewhat Disagree", "Disagree", "Strongly Disagree"),
                             labels=c("7","6","5","4","3","2","1"))
wd_clean$morality <- as.numeric(wd_clean$morality)

wd_clean$normality <- factor(wd_clean$normality, levels=c("Strongly Agree","Agree","Somewhat Agree","Neutral",
                                            "Somewhat Disagree", "Disagree", "Strongly Disagree"),
                      labels=c("7","6","5","4","3","2","1"))
wd_clean$normality <- as.numeric(wd_clean$normality)

wd_clean$probability <- factor(wd_clean$probability, levels=c("Strongly Agree","Agree","Somewhat Agree","Neutral",
                                              "Somewhat Disagree", "Disagree", "Strongly Disagree"),
                       labels=c("7","6","5","4","3","2","1"))
wd_clean$probability <- as.numeric(wd_clean$probability)

wd_clean$rationality <- factor(wd_clean$rationality, levels=c("Strongly Agree","Agree","Somewhat Agree","Neutral",
                                                  "Somewhat Disagree", "Disagree", "Strongly Disagree"),
                         labels=c("7","6","5","4","3","2","1"))
wd_clean$rationality <- as.numeric(wd_clean$rationality)

ld_clean <- pivot_longer(wd_clean,cols=c(morality,normality,probability,rationality), names_to="judgement")

#write.csv(ld_clean, file="generationLD.csv",row.names = FALSE)

##
#ld_clean <- read.csv("data/generationLD.csv")

ld.means <- ld_clean %>% 
  group_by(answerOrder,judgement) %>%
  summarize(mean = mean(value, na.rm = T),
            sd = sd(value, na.rm =T),
            N = length(value),
            se = sd / sqrt(N))
#  ) %>%
#  pivot_wider(names_from=subjectGroup, values_from = c(mean,sd,N,se))

# Figure

palette()
?scale_color_discrete()

ld.means$answerOrder <- as.character(ld.means$answerOrder)

fig1a <- ggplot(ld.means, aes(x=answerOrder, y=mean, fill=answerOrder)) +
  geom_bar(position="dodge", stat="identity")  +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, alpha=1, position= position_dodge(.9)) +
#                color=c("gray68","black","gray68","black","gray68","black","gray68","black")) +
#  scale_fill_viridis_d(option = "plasma",begin=0,end=.65) +
  facet_grid(~judgement) +
  labs(title="") +
  coord_cartesian(ylim=c(1,7)) +
  ylab("Average rating") +
  xlab("") +
  theme(
    panel.background = element_rect(fill="white",color="black")
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.title=element_blank()
    ,legend.position=("bottom")
    ,legend.text=element_text(size=14)
    ,axis.text.y=element_text(size=14)
    ,axis.text.x=element_blank()
    ,axis.title.y=element_text(size=16,face="bold")
    ,axis.ticks = element_blank()
    ,strip.text=element_text(size=16,face="bold")
    ,axis.title=element_text(size=12)   
  )

print(fig1a)


ld.time <- ld_clean %>% 
  group_by(answerOrder) %>%
  summarize(mean = mean(time, na.rm = T),
            sd = sd(time, na.rm =T),
            N = length(time),
            se = sd / sqrt(N))

ld.time.value <- ld_clean %>% 
  group_by(answerOrder,value) %>%
  summarize(mean_time = mean(time, na.rm = T),
            sd_time = sd(time, na.rm =T),
            N_time = length(time),
            se_time = sd_time / sqrt(N_time),
            N_value = length(value))

fig1b <- ggplot(ld.time, aes(x=answerOrder, y=mean, fill=answerOrder)) +
  geom_bar(position="dodge", stat="identity")  +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, alpha=1, position= position_dodge(.9)) +
  ylab("Time to answer (s)")
print(fig1b)

fig1c <- ggplot(ld.time.value, aes(x=value, y=mean_time, fill=N_value)) +
  geom_bar(position="dodge", stat="identity")  +
  geom_errorbar(aes(ymin=mean_time-se_time, ymax=mean_time+se_time), width=.1, alpha=1, position= position_dodge(.9)) +
  #                color=c("gray68","black","gray68","black","gray68","black","gray68","black")) +
  #  scale_fill_viridis_d(option = "plasma",begin=0,end=.65) +
  facet_wrap(~answerOrder) +
  labs(title="") +
#  coord_cartesian(ylim=c(1,7)) +
#  ylab("Average rating") +
#  xlab("") +
  theme(
    panel.background = element_rect(fill="white",color="black")
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.title=element_blank()
    ,legend.position=("bottom")
    ,legend.text=element_text(size=8)
    ,axis.text.y=element_text(size=14)
    ,axis.text.x=element_blank()
    ,axis.title.y=element_text(size=16,face="bold")
    ,axis.ticks = element_blank()
    ,strip.text=element_text(size=16,face="bold")
    ,axis.title=element_text(size=12)   
  )

print(fig1c)
