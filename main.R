## Project 2
# _____.dta is a panel with firm data for estimating
# a production function.

# There are the following variables:
#   
# · Individual identifier: ivar
# 
# · Time identifier: tvar
# 
# · Dependent variable: y= ln(output)
# 
# · Explanatory variables: l=ln(labour) and k=ln(capital)
# 
# Try the panel data techniques that you know and
# indicate your preferred estimator.
# Justify your choice based on specification tests.


# to import .dta
library(haven)
df =  read_dta ("prodfn_data_6_7.dta")

# to perform panel data analysis
library(plm)

# to test
library(lmtest)

############################ EXPLORATORY ANALYSIS ##############################

head(df)
summary(df)
cor(df)

df = plm::pdata.frame(df)
head(df)
dim(df)
sprintf("N = %d", length(unique(df$ivar)))
sprintf("T = %d", length(unique(df$tvar)))

############################# PANEL DATA MODELS ################################

# Fixed Effects
FE <- plm(y ~ l + k, data = df, model = "within")
summary(FE)

# Random Effects
RE <- plm(y ~ l + k, data = df, model = "random")
summary(RE) 


## Additional Models

# Fixed Effects two-way
FE_twoway <- plm(y ~ l + k, data = df, model = "within", effect = 'twoways')
summary(FE_twoway)

# POLS
POLS <-lm(y ~ l+k, data = df)
summary(POLS)


############################### POST ESTIMATION TESTS ##########################

# Hausman
phtest(FE, RE)

# Heteroskedasticity
bptest(FE)

# Autocorrelation
pbgtest(FE)
bgtest(FE, order = 1)

# Hausman Robust
phtest(y ~ l + k, data = df, method = "aux", vcov = function(x) vcovHC(x, method="arellano"))

