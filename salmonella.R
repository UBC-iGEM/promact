library(dplyr)
library(ggbiplot)

#### import data ####
setwd("./data")
filelist = list.files(pattern = ".*.txt")
data = lapply(filelist, read.csv, header=FALSE)
data = do.call(rbind.data.frame, data)

#### params ####
data = data %>% filter(row_number() %% 3 != 1)
data = aggregate(data, list(rep(1:(nrow(data)), each = 2, len = nrow(data))), mean)[-1];
rownames(data) = filelist

vec = seq(1, 11, 1)
time = seq(1, 801, 10)

cc.f.data = data[1:11,]
cc.b.data = data[12:22,]
media.f.data = data[23:33,]
media.b.data = data[34:44,]
rfp.f.data = data[45:55,]
rfp.b.data = data[56:66,]

fluro.data = cc.f.data - media.f.data
od.data = cc.b.data - media.b.data

#### promact #####

X100 = DIPA(time, time, fluro.data[7,], time, od.data[7,], m  = log(2)/0.45,
     d  = log(2)/0.5, beta = 1, d.r = 1, data.type = 3)
X40 = DIPA(time, time, fluro.data[10,], time, od.data[10,], m  = log(2)/0.45,
           d  = log(2)/0.5, beta = 1, d.r = 1, data.type = 3)
X20 = DIPA(time, time, fluro.data[9,], time, od.data[9,], m  = log(2)/0.45,
           d  = log(2)/0.5, beta = 1, d.r = 1, data.type = 3)
X10 = DIPA(time, time, fluro.data[6,], time, od.data[6,], m  = log(2)/0.45,
           d  = log(2)/0.5, beta = 1, d.r = 1, data.type = 3)
X5 = DIPA(time, time, fluro.data[11,], time, od.data[11,], m  = log(2)/0.45,
           d  = log(2)/0.5, beta = 1, d.r = 1, data.type = 3)
X2 = DIPA(time, time, fluro.data[8,], time, od.data[8,], m  = log(2)/0.45,
           d  = log(2)/0.5, beta = 1, d.r = 1, data.type = 3)
X1 = DIPA(time, time, fluro.data[5,], time, od.data[5,], m  = log(2)/0.45,
           d  = log(2)/0.5, beta = 1, d.r = 1, data.type = 3)
X0.5 = DIPA(time, time, fluro.data[4,], time, od.data[4,], m  = log(2)/0.45,
           d  = log(2)/0.5, beta = 1, d.r = 1, data.type = 3)
X0.1 = DIPA(time, time, fluro.data[3,], time, od.data[3,], m  = log(2)/0.45,
           d  = log(2)/0.5, beta = 1, d.r = 1, data.type = 3)
X0.05 = DIPA(time, time, fluro.data[2,], time, od.data[2,], m  = log(2)/0.45,
           d  = log(2)/0.5, beta = 1, d.r = 1, data.type = 3)
X0.01 = DIPA(time, time, fluro.data[1,], time, od.data[1,], m  = log(2)/0.45,
           d  = log(2)/0.5, beta = 1, d.r = 1, data.type = 3)

promact = data.frame(t(rbind(time, X100$promact, X40$promact, X20$promact, X10$promact, X5$promact, X2$promact, X1$promact, X0.5$promact, X0.1$promact, X0.05$promact, X0.01$promact)))
colnames = c("time","X100mM", "X40mM", "X20mM", "X10mM", "X5mM", "X2mM", "X1mM", "X0.5mM", "X0.1mM", "X0.05mM", "X0.01mM")
colnames(promact) = colnames

#### rfp test ####

odtest = rfp.b.data - media.b.data
Xtest = DIPA(time, time, rfp.f.data[1,], time, odtest[1,], m  = log(2)/0.45,
             d  = log(2)/0.5, beta = 1, d.r = 1, data.type = 3)
#### plots ####

ggplot(promact, aes(x = time)) +
  geom_line(aes(y = X100mM, color = "100mM")) +
  geom_line(aes(y = X40mM, color = "40mM")) +
  geom_line(aes(y = X20mM, color = "20mM")) +
  geom_line(aes(y = X10mM, color = "10mM")) +
  geom_line(aes(y = X5mM, color = "5mM")) +
  geom_line(aes(y = X2mM, color = "2mM")) +
  geom_line(aes(y = X1mM, color = "1mM")) +
  geom_line(aes(y = X0.5mM, color = "0.5mM")) +
  geom_line(aes(y = X0.1mM, color = "0.1mM")) +
  geom_line(aes(y = X0.05mM, color = "0.05mM")) +
  geom_line(aes(y = X0.01mM, color = "0.01mM")) +
  ggtitle("Inferred Promoter Activity by Concentration") +
  ylab("Promoter Activity")




