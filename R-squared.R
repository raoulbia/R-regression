rm(list = ls())
library(tidyverse)
require(ggplot2)
library(grid)
require(gridExtra)

# X1 <- data.frame(x=c(2,4,6,8,10,12), y=c(4,8,6,10,8,12))
# X2 <- data.frame(x=c(2,4,6,8,10,12), y=c(2,10,4,12,6,14))
# 
# df <- data.frame("Dataset" = c("X1", "X2")
#                  # "ybar" = c(mean(X1$y), mean(X2$y))
#                  # "variance" = c(var(X1$y), var(X2$y)),
#                  # "std.dev." = c(sd(X1$y), sd(X2$y))
# )

# 
x1 <- c(2,4,6,8,10,12)
y1 <- c(4,8,6,10,8,12)

# 1-squish
x2 <- c(6,6,8,8,10,10)
y2 <- c(4,8,6,10,8,12)


x3 <- c(2,2,6,6,10,10)
y3 <- c(2,10,4,12,6,14)
# 2-squish
x4 <- c(4,4,6,6,8,8)
y4 <- c(2,10,4,12,6,14)



xlim = c(0,15)
ylim = c(0,15)

p1 <- ggplot(, aes(x=x1, y=y1)) +
geom_point() +
geom_hline(yintercept=mean(y1), size=0.3) +
theme_minimal() +
geom_smooth(method = "lm", se = FALSE, size=0.5) +
coord_cartesian(xlim, ylim)

p2 <- ggplot(, aes(x=x2, y=y2)) +
geom_point() +
geom_hline(yintercept=mean(y2), size=0.3) +
theme_minimal() +
geom_smooth(method = "lm", se = FALSE, size=0.5) +
coord_cartesian(xlim, ylim)

p3 <- ggplot(, aes(x=x3, y=y3)) +
geom_point() +
geom_hline(yintercept=mean(y3), size=0.3) +
theme_minimal() +
geom_smooth(method = "lm", se = FALSE, size=0.5) +
  coord_cartesian(xlim, ylim)

p4 <- ggplot(, aes(x=x4, y=y4)) +
geom_point() +
geom_hline(yintercept=mean(y4), size=0.3) +
theme_minimal() +
geom_smooth(method = "lm", se = FALSE, size=0.5) +
coord_cartesian(xlim, ylim)


df <- data.frame("plot" = c(1:4),
               "ybar" = c(mean(y1), mean(y2), mean(y3), mean(y4)),
               "variance" = c(var(y1), var(y2), var(y3), var(y4)),
               "std.dev." = c(sd(y1), sd(y2), sd(y3), sd(y4)))


df <- df %>% 
  mutate(SSReg = c(sum((predict(lm(y1 ~ x1)) - mean(y1))**2), # \sum{(\hat{y}-\bar{y}^2)}
                   sum((predict(lm(y2 ~ x2)) - mean(y2))**2),
                   sum((predict(lm(y3 ~ x3)) - mean(y3))**2),
                   sum((predict(lm(y4 ~ x4)) - mean(y4))**2))) %>%
  mutate(SSResid = c(sum((y1 - predict(lm(y1 ~ x1)))**2), # \sum{(y-\hat{y}^2)}
                     sum((y2 - predict(lm(y2 ~ x2)))**2),
                     sum((y3 - predict(lm(y3 ~ x3)))**2),
                     sum((y4 - predict(lm(y4 ~ x4)))**2))) %>%
  mutate(SSTot = c(sum((y1 - mean(y1))**2), # \sum{(y-\bar{y})^2}
                   sum((y2 - mean(y2))**2),
                   sum((y3 - mean(y3))**2),
                   sum((y4 - mean(y4))**2))) %>%
  mutate(`%explained` = SSReg/SSTot*100) %>%
  arrange(plot) # sort by plot ID
df

df13 <- df %>%
  filter(plot==1 | plot==3)

# prepare for stacked bar chart
tmp <- df13 %>%
  pivot_longer(c(SSReg, SSResid), names_to = "SS", values_to = "value") 

p5 <- ggplot(tmp, aes(fill=SS, y=value, x=factor(plot))) + 
  geom_bar(position="stack", stat="identity")

# grid.arrange(p1,p2,p3,p4,p5, tableGrob(round(df,2), rows = NULL))
grid.arrange(p1,p3,p5, tableGrob(round(df13,2), rows = NULL))



# some other plot
# p5 <- ggplot(df, aes(fill=SSTot, y=variance, x=plot)) + 
#   geom_bar(position="stack", stat="identity")
# 
# grid.arrange(p1,p2,p3,p4, p5)
