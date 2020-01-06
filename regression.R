rm(list = ls())
library(tidyverse)
require(ggplot2)
library(grid)
require(gridExtra)

# create the same random numbers
set.seed(123)

# helper param
m = c(0.3, 1)

# helper data structures
plist = list()
data <- list()
rsquared <- vector()

for (i in seq_along(m)){
  
  # set up the data
  x <- rnorm(100)
  z <- rnorm(100)
  y <- x + rnorm(100) * m[[i]]
  ybar <- mean(y)
  data[[i]] <- data.frame(x,y,z)
  
  # produce linear model
  lm <- lm(data[[i]]$y ~ data[[i]]$x) # tilde is read as "is modeled as a function of".
  
  # get coefficients
  coeffs = round(coefficients(lm), 2)
  
  # get R-squared
  rsq = summary(lm)$r.squared
  rsquared <- c(rsquared, rsq)
  
  # setup annotations for displaying R-squared on plot
  annotations <- data.frame(
    xpos = -Inf, ypos = -Inf,
    annotateText = sprintf(TeX("$R^2$=%s", output = "character"), 
                           format(round(rsq, 2), nsmall = 2)),
    hjustvar = -0.2, vjustvar = -15)
  
  # set plot subtitle
  if (i==1) {subt="Low"} else {subt="High"}
  
  # add plot to list
  plist[[i]] <- ggplot(data[[i]], aes(x=x, y=y)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, size=0.5) +
    geom_hline(yintercept=ybar, size=0.3) +
    geom_text(label=TeX("$\\bar{y}$"), x=2, y=ybar, vjust=0) +
    geom_text(data = annotations, 
              aes(x=xpos, y=ypos, hjust=hjustvar,
                  vjust=vjustvar,
                  label=annotateText),
              parse = TRUE) +
    ylim(-3,3) + 
    labs(title = paste(subt,"Variance"),
         subtitle = paste("Linear Model: y =", coeffs[1], "+", coeffs[2], "x"),
         caption = "", x = "x", y = "y",tag = "") +
    theme_minimal() +
}

# produce linear model
low <- data[[1]]
high <- data[[2]]



# display plots 
margin = theme(plot.margin = unit(c(0,1,0,1), "cm"))
grid.arrange(grobs = lapply(plist, "+", margin), ncol=2)


# Compute Variations and R-squared

df <- data.frame("Dataset" = c("low var", "high var"),
                 "y_mean" = c(mean(low$y), mean(high$y)),
                 "y_variance" = c(var(low$y), var(high$y)))

# compute SSTot
df <- df %>% mutate(SSTot = c(sum((low$y - mean(low$y))**2),
                              sum((high$y - mean(high$y))**2)))

# compute SSReg
df <- df %>%
  mutate(SSReg = c(sum((predict(lm(low$y ~ low$x)) - mean(low$y))**2),
                   sum((predict(lm(high$y ~ high$x)) - mean(high$y))**2)))

# compute SSResid
df <- df %>%
  mutate(SSResid = c(sum((low$y - predict(lm(low$y ~ low$x)))**2),
                     sum((high$y - predict(lm(high$y ~ high$x)))**2)))

# compute R-squared
df <- df %>% mutate(RSquared = SSReg/SSTot*100)


# display dataframe
df %>%
  kable(digits = 1) %>%
  kable_styling(full_width = FALSE, position = 'left')


# Visualise breakdown of SSTot

# pivot longer: prepare for stacked bar chart
tmp <- df %>%
  pivot_longer(c(SSReg, SSResid), names_to = "SS", values_to = "value") 

# setup & display plot
ggplot(tmp, aes(fill=SS, y=value, x=factor(Dataset))) +
  geom_bar(position="stack", stat="identity") +
  theme_minimal() +
  theme(aspect.ratio = 2/(1+sqrt(5))) + 
  theme(plot.title=element_text(size=12),
        axis.title=element_text(size=8)) +
  labs(title = "SSReg vs. SSResid",
       fill = "", # hide legend title
       x = "", y = "Total Variation") + 
  theme_minimal()


