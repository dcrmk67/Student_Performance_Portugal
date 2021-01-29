

# From: https://www.kaggle.com/larsen0966/student-performance-data-set


#####################################################################
# Libraries
#####################################################################

if (!require('tidyverse')) install.packages('tidyverse')
if (!require('visdat')) install.packages('visdat')
if (!require('funModeling')) install.packages('funModeling')
if (!require('inspectdf')) install.packages('inspectdf')
if (!require('dlookr')) install.packages('dlookr')
if (!require('PerformanceAnalytics')) install.packages('PerformanceAnaytics')
if (!require('caret')) install.packages('caret')

# Assign options
options(digits=4)

#####################################################################
# Read data file(s)
#####################################################################

data <- read_csv('./student-por.csv')
names(data) <- tolower(names(data)) # change col names to lowercase


#####################################################################
# Intitial data set review
#####################################################################

glimpse(data)

# Visualize missing data
vis_miss(data)


#####################################################################
# Data pre-processing
#####################################################################

data <- data %>%
   mutate(across(school:health, as.factor)) %>%
   mutate(sex = fct_recode(sex,'Female'='F','Male'='M'),
          address = fct_recode(address,'Urban'='U','Rural'='R'),
          pstatus = fct_recode(pstatus,'Together'='T','Apart'='A'),
          medu = fct_recode(medu,'None'='0', '< 4th Grade'='1','5th-9th Grade'='2','Secondary'='3','Higher'='4'),
          fedu = fct_recode(fedu,'None'='0', '< 4th Grade'='1','5th-9th Grade'='2','Secondary'='3','Higher'='4'),
          traveltime = fct_recode(traveltime,'<15 min'='1','15-30 min'='2','30-60 min'='3','>60 min'='4'),
          studytime = fct_recode(studytime,'<2 hrs'='1','2-5 hrs'='2','5-10 hrs'='3','>10 hrs'='4'),
          famrel = fct_recode(famrel,'Very Bad'='1','Bad'='2','Neutral'='3','Good'='4','Excellent'='5'),
          freetime = fct_recode(freetime,'Very Low'='1','Low'='2','Neutral'='3','High'='4','Very High'='5'),
          goout = fct_recode(goout,'Very Low'='1','Low'='2','Neutral'='3','High'='4','Very High'='5'),
          dalc = fct_recode(dalc,'Very Low'='1','Low'='2','Neutral'='3','High'='4','Very High'='5'),
          walc = fct_recode(walc,'Very Low'='1','Low'='2','Neutral'='3','High'='4','Very High'='5'),
          goout = fct_recode(goout,'Very Low'='1','Low'='2','Neutral'='3','High'='4','Very High'='5'),
          health = fct_recode(health,'Very Bad'='1','Bad'='2','Neutral'='3','Good'='4','Very Good'='5'),
          absences = log(absences))

# Visualize data classes
vis_dat(data)

# Visualize numeric data
plot_num(data)

# Visualize categorical data
data %>% inspect_cat %>% show_plot

# Identify outliers
diagnose_outlier(data)
plot_outlier(data)

# Visualize Correlations
data %>% select(where(is.numeric)) %>% chart.Correlation()


#####################################################################
# Exploratory Data Analysis (EDA)
#####################################################################

# HISTOGRAMS
# Final test score
data %>%
   ggplot(aes(g3)) +
   geom_histogram(fill='steelblue', bins=10, color='black') +
   geom_hline(yintercept=0, color='black') +
   labs(title='Distribution of Final Test Scores',
        x = 'Final Test Scores',
        y = 'Number of Students') +
   theme_bw()

# Absences
data %>%
   ggplot(aes(absences)) +
   geom_histogram(fill='steelblue', bins=10, color='black') +
   geom_hline(yintercept=0, color='black') +
   labs(title='Distribution of Student Absences',
        x = 'Log of Student Absences',
        y = 'Number of Students') +
   theme_bw()


# Boxplots of Final Test Scores
myboxplot <- function(xvar) {
   data %>%
      ggplot(aes(x={{xvar}},y=g3, fill={{xvar}})) + 
      geom_boxplot() +
      labs(title = paste0('Boxplot of Final Test Scores By Variable "',deparse(substitute(xvar)),'"'),
           x = NULL,
           y = 'Final Test Score') +
      theme_bw()
}

myboxplot(school)
myboxplot(sex)
myboxplot(address)
myboxplot(famsize)
myboxplot(pstatus)
myboxplot(medu)
myboxplot(fedu)
myboxplot(mjob)
myboxplot(fjob)
myboxplot(reason)
myboxplot(guardian)
myboxplot(traveltime)
myboxplot(studytime)
myboxplot(failures)
myboxplot(schoolsup)
myboxplot(famsup)
myboxplot(paid)
myboxplot(activities)
myboxplot(nursery)
myboxplot(higher)
myboxplot(romantic)
myboxplot(famrel)
myboxplot(freetime)
myboxplot(goout)
myboxplot(dalc)
myboxplot(walc)
myboxplot(health)
