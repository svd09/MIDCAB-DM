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



