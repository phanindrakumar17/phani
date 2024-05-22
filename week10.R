salary_data <- read.csv("Salary.csv")
View(salary_data)

str(salary_data)


#Dropping the unwanted column




--------------------------------------------------------------------
  install.packages("psych")
library(psych)
windows(20,10)

pairs.panels(data_frame,
             smooth = FALSE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

--------------------------------------------------------
  
  #Examine linearity in more detail
  windows(20,12)
par(mfrow= c(4,2))

scatter.smooth(x = data_frame$Years ,
               y = data_frame$Salary,
               xlab = "Years",
               ylab = "Salary %", main = "Correlation of Salary ~ Years ")


scatter.smooth(x = data_frame$Rating,
               y = data_frame$Salary,
               main = "Correlation of Salary ~ Rating",
               xlab = "Rating %",
               ylab = "Salary %")

scatter.smooth(x = data_frame$Number.of.Subjects,
               y = data_frame$Salay,
               main = "Correlation of Salary ~ Number.of.Subjects ",
               xlab = "Number.of.Subjects ",
               ylab = "Salary %")




# Examining correlation between murder and Independent variables


cor(data_frame)

attach(data_frame)
# Examining the other variables
paste("Correlation for Salary and Years: ", round(cor(Salary, Years),2))
paste("Correlation for Salary and Rating: ", round(cor(Salary, Rating),2))
paste("Correlation for Salary and Number.of.Subjects: ", round(cor(Salary, Number.of.Subjects),2))

--------------------------------------------------------------------
  windows(20,10)
par(mfrow = c(2, 2)) # divide graph area in 3 rows by 2 columns
attach(data_frame)

boxplot(Years,
        main = "Years") # box plot for 'Years'

boxplot(Rating,
        main = "Rating") # box plot for 'Rating'

boxplot(Number.of.Subjects,
        main = "Number.of.Subjects") # box plot for 'Number.of.Subjects'

-------------------------------------------------------------  
  install.packages("readr")
library(readr)

model <- lm(Salary ~ Years + Rating + Number.of.Subjects, data = salary_data)

summary(model)

Salary ~ 20.340 + 0.481*Years + 0.067* Rating + 2.873*Number.of.Subjects

Salary
-----------------------------------------------------------
  install.packages("faraway")
  library(faraway)
v2 <- vif(salary_model_2)

v2

AIC(salary_model_1)
AIC(salary)