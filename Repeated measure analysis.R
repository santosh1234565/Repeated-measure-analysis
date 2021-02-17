setwd("C:\\Users\\HP\\OneDrive\\Documents\\R\\trial")
library(dplyr)
library(readxl)
x = read_excel('cafpH.xlsx')
head(x)
str(x)


#converting data to factor

x1 <- x %>%
  mutate(Depth= as.factor(Depth),
         CropRotation= as.factor(CropRotation),
         Year = as.factor(Year))



#repeated measure analysis

library(lme4)
library(lmerTest) # test of random intercepts using "lmerTest"
library(jtools) # output in full table format using "jtools"

rmaModel = lmer(pH ~Depth*Year*CropRotation + (1|Depth), data = x)
# if you need random intercept for more than 1 use the below formula
# rmaModel = lmer(pH ~Year*Depth + (1 + Year|Depth), data = x)
summary(rmaModel)
summ(rmaModel)
anova(rmaModel)


# probing interaction

library(reghelper)
library(ggplot2)
library(sjPlot)

simple_slopes(rmaModel)

plot_model(rmaModel, type = "int")



# regular ANOVA
nonRMModel = lm (pH ~ Year*CropRotation*Depth, data =x)
anova(nonRMModel)
summ(nonRMModel)

#boxplot

ggplot(data=x, aes(x=Depth, y = pH, color = Year)) + geom_boxplot()



