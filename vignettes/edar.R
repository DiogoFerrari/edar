
options(crayon.enabled = FALSE)
options(tibble.width=100)
options(tibble.digits=4)
options(dplyr.width=100)
options(scipen=999)
options(digits=4)

data = edar_survey
data %>% summarise_alln(., digits=2)
    

data %>% summarise_allc(.)

tab = data %>% summarise_allc(.)
tab$Table[[6]]

data %>% summarise_allcbundle(.)

tab = data %>% summarise_allcbundle(.)
tab$Table[[5]]

tab$Tablep[[5]]

tab$Tablel[[5]]

data %>% ebalance(., treatmentVar='treat') 

png(filename="gge_describe.png",height=600,width=600)
g = data[,1:9] %>% gge_describe(.)
print(g)
dev.off()

png(filename="gge_describe_group.png",height=600,width=600)
g = data[,1:9] %>% gge_describe(., group='educ')
print(g)
dev.off()

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
formula5mul = y.mul ~ x1 + x2

model.g1    = lm(formula1, data)
model.g2    = lm(formula2, data)
model.g3    = lm(formula3, data)
model.bin   = glm(formula4bin, data=data, family='binomial')
model.mul   = nnet::multinom(formula5mul, data)

## digits and output format

tidye(model.g3)
tidye(model.g3, digits=12)
tidye(model.g3, tibble=F)
tidye(model.g3, digits=12, tibble=F)


## other model types
tidye(model.bin)
tidye(model.mul)

## with robust std.errors
tidye(model.g3, hc=T)
tidye(model.g3, hc=T, keep.nohc=T) # keep no heterocedastic corrected std.errors
tidye(model.bin, hc=T)

## list of models
tidye(list(model.g3), hc=T) %>% print(., n=Inf)
tidye(list(Gaussian=model.g3, Binomial=model.bin, Multinomial=model.mul)) %>% print(., n=Inf)
