
# script to make the KM graphs for the MIDCAB paper.
# graphs made for mets, dm, obese ...
# maybe other graphs needed.
# prepare splines for bmi and then decide to 


library(easypackages)
libraries(c("survival","tidyverse",
	"rms","Hmisc","survminer","haven", "readxl"))

df <- read_stata("D:/MIDCAB_DM/dataset2.dta")

glimpse(df)

# now to create the KM curves for the paper.

# overall survival.

s <- survfit(Surv(fupyears, died_total) ~ 1, data = df)

tiff("D:/MIDCAB_DM/graphs/overall.tiff",
	height = 6, width = 8, units = "in",
	res = 900)

ggsurvplot(s,
	xlim = c(0,20),
	surv.scale = "percent",
	break.x.by = 5,
	censor.size = 0,
	risk.table = T)

dev.off()

# plot according to metabolic syndrome.

ms <- survfit(Surv(fupyears, died_total) ~ factor(mets),
	data = df)


tiff("D:/MIDCAB_DM/graphs/km_mets.tiff",
	height = 6, width = 8, units = "in",
	res = 900)

ggsurvplot(ms,
	xlim = c(0,20),
	surv.scale = "percent",
	break.x.by = 5,
	censor.size = 0,
	risk.table = T,
	legend.labs = c("MET-ve","MET+ve"),
	conf.int = T)

dev.off()

# according to diabetes.

sd <- survfit(Surv(fupyears, died_total) ~ diabetes, 
	data = df)


tiff("D:/MIDCAB_DM/graphs/km_dm.tiff",
	height = 6, width = 8, units = "in",
	res = 900)


ggsurvplot(sd,
	xlim = c(0,20),
	surv.scale = "percent",
	break.x.by = 5,
	censor.size = 0,
	risk.table = T,
	legend.labs = c("DM-ve","DM+ve"),
	conf.int = T)

dev.off()


# according to obesity

so <- survfit(Surv(fupyears, died_total) ~ obese,
	data = df)


tiff("D:/MIDCAB_DM/graphs/km_obese.tiff",
	height = 6, width = 8, units = "in",
	res = 900)


ggsurvplot(so,
	xlim = c(0,20),
	surv.scale = "percent",
	break.x.by = 5,
	censor.size = 0,
	risk.table = T,
	legend.labs = c("Not Obese","Obese"),
	conf.int = T)

dev.off()

# for now these graphs are good.
# will make more graphs depending upon the plan.

# get the dataset for plotting bmi as a spline.

d <- read_excel("D:/MIDCAB_DM/bmi_plot.xlsx", sheet = 1)

glimpse(d)

plot(x = d$bmi, y = d$hr, type = "l", col = "red")
lines(x = d$bmi, y = d$lb, lty = 2, col = "blue")
lines(x = d$bmi, y = d$ub, lty = 2, col = "blue")