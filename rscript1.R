###################
##  MIDCAB + DM  ##
###################

# This is an rscript to clean and reformat the dataset.

library(easypackages)
libraries(c("tidyverse","rms","Hmisc","survival",
            "tableone", "readxl","survminer"))

# getting the dataset.

df <- read_csv("D:\\MIDCAB_DM\\midcab_dm.csv")

glimpse(df)

df %>% count(`Coronary_ disease_detail`)

# plan to only keep clean single vessel disease patients here.


df1 <- df %>% filter(`Coronary_ disease_detail` == 1) %>%
  rename(cor_details = `Coronary_ disease_detail`)

# now df1 contains only LIMA - LAD patients here.

glimpse(df1)

df1 %>% count(Diabetes)

# here we have 346 patients with DM and 1110 without.

df1 %>% count(`Oral_ antidiabetics` )

df1 %>% count(Insulin)


glimpse(df1)




# just look at survival according to DM or not...

df1$survyears <- ( 1 + df1$Survival_days)/365.24


s <- survfit(Surv(survyears, Died_total) ~ Diabetes, data = df1)

ggsurvplot(s, conf.int = T, risk.table = T,
           censor.size = 0, xlim = c(0,20))

cox.zph(coxph(Surv(survyears, Died_total) ~ Diabetes, data = df1))

library(timereg)

df2 <- event.split(data = df1,time = df1$survyears,
                   status = df1$Died_total,
                   cuts = 10)
