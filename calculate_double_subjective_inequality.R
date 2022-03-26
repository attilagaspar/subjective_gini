# ISSP V is downloadable from here: https://search.gesis.org/research_data/ZA7600
# Proceed with downloading the STATA version; copy the data file to the working directory

# this package is needed to load .dta files made by more recen  Stata versions
library(haven)
# load the data
ISSP5 <- read_dta("ZA7600_v1-0-0.dta")


# recode subjective class sizes from type of society question (Q15a)
ISSP5$f_top <- NaN
ISSP5$f_middle <- NaN
ISSP5$f_bottom <- NaN

ISSP5$f_top[ISSP5$v48==1] <- 0.176
ISSP5$f_middle[ISSP5$v48==1] <- 0.176
ISSP5$f_bottom[ISSP5$v48==1] <- 0.648

ISSP5$f_top[ISSP5$v48==2] <- 0.125
ISSP5$f_middle[ISSP5$v48==2] <- 0.375
ISSP5$f_bottom[ISSP5$v48==2] <- 0.5

ISSP5$f_top[ISSP5$v48==3] <- 0.12
ISSP5$f_middle[ISSP5$v48==3] <- 0.49
ISSP5$f_bottom[ISSP5$v48==3] <- 0.39

ISSP5$f_top[ISSP5$v48==4] <- 0.15
ISSP5$f_middle[ISSP5$v48==4] <- 0.70
ISSP5$f_bottom[ISSP5$v48==4] <- 0.15

ISSP5$f_top[ISSP5$v48==5] <- 0.40
ISSP5$f_middle[ISSP5$v48==5] <- 0.475
ISSP5$f_bottom[ISSP5$v48==5] <- 0.125

# calculate subjective earnings (Bottom: Q2c, Q2d; Top: Q2a, Q2b, Q2e )
# v11 v12 v15 
# v13 v14
ISSP5$w_top <- NaN
ISSP5$w_middle <- NaN
ISSP5$w_bottom <- NaN

ISSP5$w_bottom[ISSP5$v13>=0 && ISSP5$v14>=0] <- ( ISSP5$v13 + ISSP5$v14 ) / 2
ISSP5$w_top[ISSP5$v11>=0 && ISSP5$v12>=0 && ISSP5$v15>=0 ] <- ( ISSP5$v11 + ISSP5$v12  + ISSP5$v15 ) / 3

# make sure nothing is negative
ISSP5$w_bottom[ISSP5$w_bottom<=0]<-NaN
ISSP5$w_bottom[ISSP5$w_top<=0]<-NaN

# calculate middle income
ISSP5$w_middle <- ( ISSP5$w_top + ISSP5$w_bottom ) / 2

# calculate subjective national income
ISSP5$w_bar <- ISSP5$w_top * ISSP5$f_top  + ISSP5$w_middle * ISSP5$f_middle + ISSP5$w_bottom * ISSP5$f_bottom

# calculate subjective income shares of classes
ISSP5$q_top <-(  ISSP5$w_top * ISSP5$f_top ) /ISSP5$w_bar
ISSP5$q_middle <- ( ISSP5$w_middle * ISSP5$f_middle ) /ISSP5$w_bar
ISSP5$q_bottom <- ( ISSP5$w_bottom * ISSP5$f_bottom ) /ISSP5$w_bar

# calculate area under the Lorenz curve
ISSP5$A <- (ISSP5$f_top * ISSP5$q_top)/2 + (ISSP5$f_middle * ISSP5$q_middle)/2 + (ISSP5$f_bottom * ISSP5$q_bottom)/2 + ISSP5$f_middle * ISSP5$q_bottom + ISSP5$f_top * ISSP5$q_middle + ISSP5$f_top * ISSP5$q_bottom  

# calculate double-subjective Gini coefficient
ISSP5$DSGini <- 1 - 2*ISSP5$A