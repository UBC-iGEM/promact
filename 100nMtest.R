### import data #####
library(readxl)
X100mM_test <- read_excel("data/100mM test.xlsx")
X40mM_test <- read_excel("data/40mM test.xlsx")

### params #####

x = X40mM_test$Time
t.f = x
t.b = x
f.data = X40mM_test$Fluorescence
b.data = X40mM_test$OD

### promact #####

promact = DIPA(x, t.f, f.data, t.b, b.data, m  = log(2)/0.45,
                 d  = log(2)/0.5, beta = 1, d.r = 1, data.type = 3)

plot(promact$t, promact$promact, main="40mM", xlab = "Time", ylab = "Promoter Activity")
