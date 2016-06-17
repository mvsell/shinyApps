library(zoo)

CF1 <- 0.2
CF2 <- 3.2

df1 <- readRDS('Data/Au_Interval.Rds')
df1$meetsCf <- ifelse(df1$Au_ppm>CF1 & df1$Au_ppm<CF2, 1, 0)

df1$mavg2[2:329] <- rollapplyr(df1$meetsCf, width = 2, mean)
df1$mavg3[2:329] <- rollapplyr(df1$meetsCf, width = 3, mean)
df1$mavg4[2:329] <- rollapplyr(df1$meetsCf, width = 4, mean)
df1$mavg5[2:329] <- rollapplyr(df1$meetsCf, width = 8, mean)

df1$dil2 <- ifelse(df1$mavg2>=0.5 & df1$mavg2<=1, 1, NA)
df1$dil3 <- ifelse(df1$mavg3>=0.2 & df1$mavg3<=1, 1, 0)
df1$dil4 <- ifelse(df1$mavg4>=0.125 & df1$mavg4<=1, 1, 0)
df1$dil8 <- ifelse(df1$mavg5>0.083 & df1$mavg5<=1, 1, 0)

ggplot(df1) +
  geom_line(aes(y= df1$Au_ppm, x= df1$From)) +
  geom_line(aes(y = -2, x= df1$From, colour = log10( df1$Au_ppm)), size=2) +
  scale_colour_gradientn("Grade", colours = rev(rainbow(5))) +
  coord_flip() +
  scale_x_reverse() +
  geom_line(aes(y = -5, x= df1$From, colour = df1$dil2), size=2) +
  geom_line(aes(y = -8, x= df1$From, colour = df1$dil3), size=2) +
  geom_line(aes(y = -11, x= df1$From, colour = df1$dil4), size=2) +
  geom_line(aes(y = -14, x= df1$From, colour = df1$dil8), size=2) +
  labs(title ="DH Grade", x = "Depth (m)", y = "Assay")

