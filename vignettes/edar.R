# summary tables
data %>% summarise_alln(.) # summarise all numerical variables of the data in a table
data %>% summarise_allc(.) # summarise all categorical variables of the data in a table 
data %>% summarise_allcbundle(.) # summarise all categorical variables of the data in a table
data %>% ebalance(., treatmentVar="treat") ## summary of numerical variables for different levels of "treat"

# summary plots
s %>% gge_describe(.)  ## marginal distribution of all variables
s %>% gge_density(.) ## marginal distribution of numerical variables only
s %>% gge_histogram(.)## marginal distribution of numerical variables only using histograms
s %>% gge_barplot(.) ## marginal distribution of non-numerical variables

# afeter fitting the models model1, model2, etc ...
tidye(model1, hc=T) ## summarise put summary in a tidy data.frame (using robust std.errors)
tidye(list(model1,model2)) ## same, but summarise both models at once

# plots
gge_coef(model1) ## dotwisker plot (plot with coefficients and std. errors)
model %>%  gge_fit(., data, "y", "x1") ## plot with fitted values as function of covariate x1

# multiple imputation and post-stratification
emultimputation(data, formula,  dep.vars = c(...), ind.vars=c(...)) 
epoststrat(data, population.proportion, strata = ~ stratification.variable1 + stratification.variable2...)

library(magrittr)
library(edar)

data(edar_survey)
help(edar_survey)

data = edar_survey

options(crayon.enabled = FALSE)
options(tibble.width=100)
options(tibble.digits=4)
options(dplyr.width=100)
options(scipen=999)
options(digits=4)

data %>% summarise_alln(., digits=2)

data %>% summarise_allc(.)

tab = data %>% summarise_allc(.)
tab$Table[[6]]

data %>% summarise_allcbundle(.)

tab = data %>% summarise_allcbundle(.)
tab$Table[[5]]

tab$Tablep[[5]]

tab$Tablel[[5]]

data %>% ebalance(., treatmentVar='treat') %>% print(., digits=2)

g = data[,1:8] %>% gge_describe(.)
print(g)

g = data[,1:9] %>% gge_describe(., group='educ')
print(g)

set.seed(77)
data = tibble::data_frame(n = 300,
                          x1   = rnorm(n,3,1),
                          x2   = rexp(n),
                          cat1 = sample(c(0,1), n, replace=T),
                          cat2 = sample(letters[1:4], n, replace=T),
                          y    = -10*x1*cat1 + 10*x2*(3*(cat2=='a') -3*(cat2=='b') +1*(cat2=='c') -1*(cat2=='d')) + 
                              rnorm(n,0,10), 
                          y.bin = ifelse(y < mean(y), 0, 1),
                          y.mul = 1+ifelse( - x1 - x2 + rnorm(n,sd=10) < 0, 0,
                                    ifelse( - 2*x2 + rnorm(n,sd=10) < 0, 1, 2)),
                          )

formula1    = y ~ x1
formula2    = y ~ x1 + x2
formula3    = y ~ x1*cat1 + x2*cat2
formula4bin = y.bin ~ x1+x2*cat2
formula4bin1 = y.bin ~ x1+x2
formula4bin2 = y.bin ~ x1*cat1+x2*cat2
formula5mul = y.mul ~ x1 + x2

model.g1    = lm(formula1, data)
model.g2    = lm(formula2, data)
model.g3    = lm(formula3, data)
model.bin   = glm(formula4bin, data=data, family='binomial')
model.bin1  = glm(formula4bin, data=data, family='binomial')
model.bin2  = glm(formula4bin, data=data, family='binomial')
model.mul   = nnet::multinom(formula5mul, data)

tidye(model.g3)

## works with other types of dependent variables
# tidye(model.bin)
# tidye(model.mul)

## with robust std.errors
tidye(model.g3, hc=T)

tidye(model.g3, hc=T, keep.nohc=T) # keep no heterocedastic corrected std.errors

## list of models
tidye(list(Gaussian=model.g3, Binomial=model.bin, Multinomial=model.mul)) %>% print(., n=Inf)

tidye(list(Gaussian=model.g3, Binomial=model.bin, Multinomial=model.mul)) %>%
    kableExtra::kable(., "latex", booktabs = T ) %>%
    kableExtra::kable_styling(latex_options = c("scale_down"))

list(Binomial=model.bin, Multinomial=model.mul,Gaussian=model.g3) %>%
    etab

model.g1 %>% gge_fit(., data, 'y', "x1")

model.g3 %>% gge_fit(., data, 'y', "x2", cat.values=list(cat2=c('a',"b")))

g1 = model.g3 %>% gge_fit(., data, 'y', "x2",  cat.values=list(cat2=c('a')), title='Variable cat2 fixed at a')
g2 = model.g3 %>% gge_fit(., data, 'y', "x2",  cat.values=list(cat2=c('b')), title='Variable cat2 fixed at b')
ggpubr::ggarrange(g1,g2)

model.g3 %>% gge_fit(., data, 'y', "x2", facets='cat2' )

model.g3 %>% edar::gge_fit(., data, 'y', 'x1', facets='cat2', pch.col.cat='cat1', pch.col.palette=c(brewer="Set2"))

formulas = list("Model 1" = formula1, "Model 2" = formula2, "Model 3" = formula3)
models   = list("Model 1" = model.g1, "Model 2" = model.g2, "Model 3" = model.g3)

models %>%  gge_fit(., data, "y", "x2", formulas)

formulas = list("Model 1" = formula1, "Model 2" = formula2, "Model 3" = formula3)
models   = list("Model 1" = model.g1, "Model 2" = model.g2, "Model 3" = model.g3)

models %>%  gge_fit(., data, "y", "x2", formulas,  legend.ncol.fill=3, facets='cat2')

formula.bin1 = y.bin ~ x1+x2
formula.bin2 = y.bin ~ x1+x2*cat2
model.bin1   = glm(formula.bin1, data=data, family='binomial')
model.bin2   = glm(formula.bin2, data=data, family='binomial')

formulas = list("Model 1" = formula.bin1, "Model 2" = formula.bin2)
models   = list("Model 1" = model.bin1, "Model 2" = model.bin2)

models %>%  gge_fit(., data, "y.bin", "x1", formulas)

models %>%  gge_fit(., data, "y.bin", "x2", formulas, facets='cat2')

models=tidye(list('Standard Model'=model.bin2)) %>%
    dplyr::bind_rows(tidye(list('Robust std. error'=model.bin2), hc=T) )
gge_coef(models, model.id='model')

data = tibble::data_frame(x1 = rnorm(200,3,1),
                          x2 = rexp(200),
                          cat.var  = sample(c(0,1), 200, replace=T),
                          cat.var2 = sample(letters[1:4], 200, replace=T),
                          y1 = 10*x1*cat.var+rnorm(200,0,10) +
                              3*x2*(6*(cat.var2=='a') -3*(cat.var2=='b') +
                                    1*(cat.var2=='c') +1*(cat.var2=='d')),
                          y2 = -10*x1*cat.var+rnorm(200,0,10) +
                              10*x2*(3*(cat.var2=='a') -3*(cat.var2=='b') +
                                     1*(cat.var2=='c') -1*(cat.var2=='d'))
                          )  %>%
    dplyr::mutate(cat.var=as.factor(cat.var)) 
data$x1[sample(1:nrow(data), 10)] = NA


formula = "x1*cat.var+x2*cat.var2"
imp = emultimputation(data, formula,  dep.vars = c("y1", "y2"), ind.vars=c("x1", "x2", "cat.var", "cat.var2"))
imp

data = tibble::data_frame(educ = sample(c("Low", "High"), 200, T), gender=sample(c('Man', "Woman"), 200, T), other.variable=rnorm(200)) 
pop.prop = tibble::data_frame(educ = c("Low", "High"))  %>%
    tidyr::crossing(gender=c("Man", "Woman")) %>%
    dplyr::mutate(Freq = 100*c(.3,.25,.3,.15)) 

epoststrat(data, pop.prop, strata = ~educ+gender)
