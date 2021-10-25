#### relative fluro plots ####
# requires promact data to run

relative.fluro.data = data.frame(t(fluro.data/max(fluro.data)))

ggplot(relative.fluro.data, aes(x = time)) +
  geom_line(aes(y = Construct_Fluro_100.00.txt, color = "100mM")) +
  geom_line(aes(y = Construct_Fluro_40.00.txt, color = "40mM")) +
  geom_line(aes(y = Construct_Fluro_20.00.txt, color = "20mM")) +
  geom_line(aes(y = Construct_Fluro_10.00.txt, color = "10mM")) +
  geom_line(aes(y = Construct_Fluro_5.00.txt, color = "5mM")) +
  geom_line(aes(y = Construct_Fluro_2.00.txt, color = "2mM")) +
  geom_line(aes(y = Construct_Fluro_1.00.txt, color = "1mM")) +
  geom_line(aes(y = Construct_Fluro_0.50.txt, color = "0.5mM")) +
  geom_line(aes(y = Construct_Fluro_0.10.txt, color = "0.1mM")) +
  geom_line(aes(y = Construct_Fluro_0.05.txt, color = "0.05mM")) +
  geom_line(aes(y = Construct_Fluro_0.01.txt, color = "0.01mM")) +
  ggtitle("Realtive Fluorescence by Concentration Over Time") +
  ylab("Relative Fluorescence") +
  xlab("Time")

write.csv(relative.fluro.data,"D:\\Users\\George\\Dropbox\\UBC\\iGEM\\Modeling\\relativefluro.csv", row.names = TRUE)
