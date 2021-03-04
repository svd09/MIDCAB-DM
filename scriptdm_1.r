library(easypackages)
libraries(c("tidyverse","rms","Hmisc","survival",
            "tableone", "readxl","survminer","naniar",
            "tidylog"))

# getting the dataset.

df <- read_csv("D:\\MIDCAB_DM\\midcab_dm2.csv")

# str(df)

df %>% count(cad)


df1 <- df %>% filter(cad == 1) 

# now df1 contains only LIMA - LAD patients here.

# to remove any patients with hybrid completion.

df1 %>% count(Total_Hybrid)

df1 = df1 %>% filter(Total_Hybrid == 0)

# glimpse(df1)

df1 %>% count(diabetes)

# here we have 346 patients with DM and 1110 without.

df1 %>% count(ohga)

df1 %>% count(insulin)

# we also have diet treated DM.

df1$diet_rx = with(df1, ifelse(diabetes == 1 & ohga == 1 & insulin == 0 , 1, 0))

df1 %>% count(diet_rx)

# so for the dm patients, make a var for treatment. diet = 1, ohga = 2, insulin = 3

df1$treat <- with(df1, ifelse(diabetes == 1 & diet_rx == 1, 1, 
                              ifelse(diabetes == 1 & ohga == 1, 2,
                                     ifelse(diabetes == 1 & insulin == 1, 3, 0))))


df1 %>% count(treat)


df1$treat = factor(df1$treat, levels = c(0,1,2,3),
                   labels = c("no_dm","oral","meds","insulin"))


df1$itdm <- with(df1, ifelse(diabetes == 1 & insulin == 1, 2,
                             ifelse(diabetes == 1 & insulin == 0, 1, 0)))

df1$itdm = factor(df1$itdm, levels = c(0,1,2),
                  labels = c("no_dm","non_itdm","itdm"))

# table according to 



vars <- c( "time_period",  "age", "sex", 
 "bmi", "diabetes", "ohga", "insulin", "newer_antidm", 
"preop_aspirin", "preop_plavix",  "preop_creat_mg", 
"Preop_ dialysis", "htn", "pah", "smoke", "hyperlip", 
"copd", "pvd", "lvef", "lvfuncion", "ekg", "priority", "critical_state", 
"preop_cpr", "preop_vent", "cad", "lmca_d", "prior_pci", "pci_vessel")


factors <- c( "time_period",  "sex", 
  "diabetes", "ohga", "insulin", "newer_antidm", 
"preop_aspirin", "preop_plavix",  
"Preop_ dialysis", "htn", "pah", "smoke", "hyperlip", 
"copd", "pvd",  "lvfuncion", "ekg", "priority", "critical_state", 
"preop_cpr", "preop_vent", "cad", "lmca_d", "prior_pci", "pci_vessel")


t1 <- tableone::CreateTableOne(vars = vars, factorVars = factors,
                               strata = c("diabetes"),
                               data = df1)

tab1dm = print(t1, nonnormal = c("age","preop_creat_mg"))


write.csv(tab1dm, 
	"D:\\MIDCAB_DM\\tables\\tab1dm.csv")

# now to see intraoperative details ...


df1$pump_used <- with(df1, ifelse(cpb_time == 0, 0 ,1))

df1 %>% count(convert_onpump)

df1 %>% count(pump_used)

describe(df1$cpb_time)


df1 %>% count(cpb_time)


vars <- c("prior_pci", "preop_mi", "inc_revasc", "preop_stroke", "preop_cva", 
"preop_cs", "prior_ohs",  "log_euroscore", 
 "convert_onpump", "on_bh", "convert_sternotomy", 
"surg_time", "cpb_time", "clamp_time", "postop_locas", "post_iabp", 
"post_ecmo", "post_mi", "postop_cpr", "postop_af", "post_bypass_revise" ,"pump_used")

factors <- 
  c("prior_pci", "preop_mi", "inc_revasc", "preop_stroke", "preop_cva", 
"preop_cs", "prior_ohs",  
 "convert_onpump", "on_bh", "convert_sternotomy", 
 "postop_locas", "post_iabp", 
"post_ecmo", "post_mi", "postop_cpr", "postop_af", "post_bypass_revise" ,"pump_used")

t2 <- tableone::CreateTableOne(vars = vars,
                               factorVars = factors,
                               strata = c("diabetes"),
                               data = df1)

tab2dm = print(t2, nonnormal = c("cpb_time", "clamp_time"))

write.csv(tab2dm,
	"D:\\MIDCAB_DM\\tables\\tab2dm.csv")

# now to look at survival according to DM.

glimpse(df1)

df1 = df1 %>% rename(died = Died_total)

df1$surv_years = (1 + df1$Survival_days)/365.24

df1 %>% count(died)

s = survfit(Surv(surv_years, died) ~ 
	diabetes, data = df1)

# KM curve according to DM status.

ggsurvplot(s,
	risk.table = T,
	censor.size = 0,
	conf.int = T,
	xlim = c(0,20))

summary(s, times = c(0,5,10,15,20))

#                diabetes=0 
# time n.risk n.event survival std.err lower 95% CI upper 95% CI
#    0   1061       0    1.000 0.00000        1.000        1.000
#    5    816      59    0.938 0.00781        0.923        0.954
#   10    561      63    0.854 0.01241        0.830        0.879
#   15    272      42    0.771 0.01666        0.739        0.804
#   20    118      21    0.694 0.02221        0.652        0.739

#                diabetes=1 
# time n.risk n.event survival std.err lower 95% CI upper 95% CI
#    0    329       0    1.000  0.0000        1.000        1.000
#    5    241      28    0.904  0.0172        0.871        0.939
#   10    156      27    0.788  0.0259        0.739        0.840
#   15     59      37    0.549  0.0383        0.479        0.630
#   20     12      14    0.369  0.0486        0.285        0.478
