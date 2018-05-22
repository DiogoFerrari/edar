Overview
========

Exploratory Data Analysis in R (`edar`) is a R package designed to
reduce the amount of code needed to do EDA in R. It provides a set of
wrap functions that incapsulate some
[tidyverse](https://www.tidyverse.org/) functionalities.

Instalation
===========

``` {.r .rundoc-block rundoc-language="R" rundoc-exports="code"}
devtools::install_github("DiogoFerrari/edar")
# If you don't want to update the dependencies, use: (you may need to install some dependencies manually)
devtools::install_github("DiogoFerrari/edar", dependencies=F)

```

Usage
=====

    library(edar)
    library(magrittr)

    data(edar_survey)
    data = edar_survey
    # help(data)

    ## summarise all numerical variables
    data %>% summarise_alln(., group=NULL, weight=NULL, spread=F)
    data %>% summarise_alln(., group="gender", weight=NULL, spread=F)

    ## summarise all categorical variables
    data %>% summarise_allc(., group=NULL)
    data %>% summarise_allc(., group="gender")

    ## bundle all cateorical variables based on their categories and summarise them
    tab = data %>% summarise_allcbundle(., group=NULL)
    tab
    tab$Table[[1]]  ## Table with counts
    tab$Tablep[[1]] ## Table with percentages
    tab$Tablel[[1]] ## Table with counts and percentages

    ## check balance of covariates between two groups (ex: treatment vs control, see Imbens, G. W., & Rubin, D. B., Causal inference in statistics, social, and biomedical sciences: an introduction (2015), : Cambridge University Press.) 
    data %>% ebalance(., treatmentVar='treat')

var N NAs Mean sd se Median Min Max q.025 q.25 q.75 q.975 &lt;chr&gt;
&lt;dbl&gt; &lt;int&gt; &lt;dbl&gt; &lt;dbl&gt; &lt;dbl&gt; &lt;dbl&gt;
&lt;dbl&gt; &lt;dbl&gt; &lt;dbl&gt; &lt;dbl&gt; &lt;dbl&gt; &lt;dbl&gt;
1 age 700 0 45.3 16.4 0.621 45.0 18.0 92.0 18.0 32.0 57.0 78.5 2
racial.frag.ratio 700 0 0.872 0.139 0.00526 0.871 0.489 1.05 0.489 0.864
0.963 1.05 3 treat 700 0 0.481 0.500 0.0189 0 0 1.00 0 0 1.00 1.00 4 yi
618 82 931 1544 62.1 600 0 20000 83.9 300 1000 4986 5 yi.iht 618 82 6.96
1.08 0.0433 7.09 0 10.6 5.12 6.40 7.60 9.21 6 ys.gini 700 0 0.527 0.0317
0.00120 0.523 0.431 0.591 0.486 0.507 0.539 0.586 7 ys.mean 700 0 974
300 11.3 792 560 2011 573 688 1233 1347

gender var N NAs Mean sd se Median Min Max q.025 q.25 q.75 &lt;fct&gt;
&lt;chr&gt; &lt;dbl&gt; &lt;int&gt; &lt;dbl&gt; &lt;dbl&gt; &lt;dbl&gt;
&lt;dbl&gt; &lt;dbl&gt; &lt;dbl&gt; &lt;dbl&gt; &lt;dbl&gt; &lt;dbl&gt;
1 man age 289 0 43.4 16.6 0.975 43.0 18.0 87.0 19.0 29.0 56.0 2 woman
age 411 0 46.7 16.2 0.799 46.0 18.0 92.0 18.0 35.0 59.0 3 man
racial.frag.ratio 289 0 0.880 0.134 0.00788 0.871 0.489 1.05 0.489 0.864
0.963 4 woman racial.frag.ratio 411 0 0.867 0.143 0.00703 0.871 0.489
1.05 0.489 0.864 0.963 5 man treat 289 0 0.471 0.500 0.0294 0 0 1.00 0 0
1.00 6 woman treat 411 0 0.489 0.500 0.0247 0 0 1.00 0 0 1.00 7 man yi
260 29 1168 1667 103 750 0 16600 112 400 1200 8 woman yi 358 53 758 1426
75.4 500 0 20000 80.0 250 741 9 man yi.iht 260 29 7.22 1.13 0.0704 7.31
0 10.4 5.40 6.68 7.78 10 woman yi.iht 358 53 6.78 0.994 0.0525 6.91 0
10.6 5.08 6.21 7.30 11 man ys.gini 289 0 0.526 0.0317 0.00187 0.521
0.431 0.591 0.486 0.507 0.539 12 woman ys.gini 411 0 0.527 0.0318
0.00157 0.527 0.431 0.591 0.486 0.507 0.544 13 man ys.mean 289 0 964 278
16.4 792 560 2011 595 698 1233 14 woman ys.mean 411 0 980 315 15.5 792
560 2011 573 688 1294 q.975 &lt;dbl&gt; 1 76.8 2 78.8 3 1.05 4 1.05 5
1.00 6 1.00 7 5683 8 3790 9 9.34 10 8.93 11 0.583 12 0.586 13 1347 14
1347

var N NAs Categories &lt;chr&gt; &lt;dbl&gt; &lt;int&gt; &lt;int&gt; 1
educ 700 0 2 2 gender 700 0 2 3 minimum.wage 695 5 4 4 red.to.poor 679
21 4 5 reduce.income.gap 700 0 5 6 region 700 0 5 7 state 700 0 27 8
transfer.state.tax 700 0 5 9 trust 694 6 3 10 unemployment.policy 699 1
4 Frequency Table Categories.Labels &lt;chr&gt; &lt;list&gt; &lt;chr&gt;
1 high (39.71 %), low (60.29 %) &lt;data.… high, low 2 man (41.29 %),
woman (58.71 %) &lt;data.… man, woman 3 Each (8.63 %), Each (16.40 %),
Shoul (74.96 %) &lt;data.… Each city should decide, Each… 4 Each (11.78
%), Each (18.56 %), Shoul (69.66 %) &lt;data.… Each city should decide,
Each… 5 A (72.43 %), A+ (10.57 %), D (9.71 %), D+ (6.00 %), N (1.2…
&lt;data.… A, A+, D, D+, N 6 CO (6.14 %), NE (47.57 %), NO (6.57 %), SE
(24.86 %), SU (14.… &lt;data.… CO, NE, NO, SE, SU 7 AC (0.29 %), AL
(0.86 %), AM (1.00 %), AP (0.43 %), BA (17.43… &lt;data.… AC, AL, AM,
AP, BA, CE, DF, E… 8 A (70.14 %), A+ (14.71 %), D (8.14 %), D+ (5.43 %),
N (1.5… &lt;data.… A, A+, D, D+, N 9 high (56.92 %), low (43.08 %)
&lt;data.… high, low 10 Each (9.59 %), Each (17.17 %), Shoul (73.25 %)
&lt;data.… Each city should decide, Each…

gender var N NAs Categories &lt;fct&gt; &lt;chr&gt; &lt;dbl&gt;
&lt;int&gt; &lt;int&gt; 1 man educ 289 0 2 2 woman educ 411 0 2 3 man
minimum.wage 288 1 4 4 woman minimum.wage 407 4 4 5 man red.to.poor 282
7 4 6 woman red.to.poor 397 14 4 7 man reduce.income.gap 289 0 5 8 woman
reduce.income.gap 411 0 5 9 man region 289 0 5 10 woman region 411 0 5
Frequency Table Categories.Labels &lt;chr&gt; &lt;list&gt; &lt;chr&gt; 1
high (44.98 %), low (55.02 %) &lt;data.f… high, low 2 high (36.01 %),
low (63.99 %) &lt;data.f… high, low 3 Each (6.60 %), Each (16.67 %),
Shoul (76.74 %) &lt;data.f… Each city should decide,… 4 Each (10.07 %),
Each (16.22 %), Shoul (73.71 %) &lt;data.f… Each city should decide,… 5
Each (11.35 %), Each (16.31 %), Shoul (72.34 %) &lt;data.f… Each city
should decide,… 6 Each (12.09 %), Each (20.15 %), Shoul (67.76 %)
&lt;data.f… Each city should decide,… 7 A (74.39 %), A+ (10.73 %), D
(9.69 %), D+ (4.15 %), N (1.04 %) &lt;data.f… A, A+, D, D+, N 8 A (71.05
%), A+ (10.46 %), D (9.73 %), D+ (7.30 %), N (1.46 %) &lt;data.f… A, A+,
D, D+, N 9 CO (7.96 %), NE (46.71 %), NO (6.92 %), SE (25.61 %), SU
(12.80 %) &lt;data.f… CO, NE, NO, SE, SU 10 CO (4.87 %), NE (48.18 %),
NO (6.33 %), SE (24.33 %), SU (16.30 %) &lt;data.f… CO, NE, NO, SE, SU

N.Variables Variables &lt;int&gt; &lt;list&gt; 1 2 &lt;chr \[2\]&gt; 2 1
&lt;chr \[1\]&gt; 3 1 &lt;chr \[1\]&gt; 4 3 &lt;chr \[3\]&gt; 5 2
&lt;chr \[2\]&gt; 6 1 &lt;chr \[1\]&gt; Categories.Labels Table Tablep
&lt;chr&gt; &lt;list&gt; &lt;list&gt; 1 A, A+, D, D+, N &lt;data.frame
\[2 ×… &lt;data.frame \[2 … 2 AC, AL, AM, AP, BA, CE, DF, ES, GO, MA,
MG, MS, MT, PA, PB, PE, PI, PR, RJ, RN, R… &lt;data.frame \[1 ×…
&lt;data.frame \[1 … 3 CO, NE, NO, SE, SU &lt;data.frame \[1 ×…
&lt;data.frame \[1 … 4 Each city should decide, Each state should
decide, Should be the same accros the … &lt;data.frame \[3 ×…
&lt;data.frame \[3 … 5 high, low &lt;data.frame \[2 ×… &lt;data.frame
\[2 … 6 man, woman &lt;data.frame \[1 ×… &lt;data.frame \[1 … Tablel
&lt;list&gt; 1 &lt;data.… 2 &lt;data.… 3 &lt;data.… 4 &lt;data.… 5
&lt;data.… 6 &lt;data.… Variable A A+ D D+ N NA 1 reduce.income.gap 507
74 68 42 9 0 2 transfer.state.tax 491 103 57 38 11 0 Variable A A+ D D+
N NA 1 reduce.income.gap 72.43 10.57 9.71 6.00 1.29 0 2
transfer.state.tax 70.14 14.71 8.14 5.43 1.57 0 Variable A A+ D D+ N NA
1 reduce.income.gap 72.43 % (N=507) 10.57 % (N=74) 9.71 % (N=68) 6 %
(N=42) 1.29 % (N=9) 0 % (N=0) 2 transfer.state.tax 70.14 % (N=491) 14.71
% (N=103) 8.14 % (N=57) 5.43 % (N=38) 1.57 % (N=11) 0 % (N=0)

mut st muc sc NorDiff lnRatioSdtDev pit pic age 45.8278932 16.61271188
44.9035813 16.2687737 0.05621774 0.02092063 0.03264095 0.05509642 yi
946.3622896 1671.84395939 916.0373209 1418.3188873 0.01956105 0.16445490
0.03367003 0.06542056 yi.iht 6.9479163 1.07004727 6.9797149 1.0832093
-0.02953488 -0.01222538 0.03367003 0.06542056 ys.mean 981.5406709
297.96655048 966.0358753 302.5965614 0.05163266 -0.01541920 0.04451039
0.02203857 ys.gini 0.5240431 0.03006502 0.5289748 0.0330827 -0.15601692
-0.09564814 0.03264095 0.04958678 racial.frag.ratio 0.8738877 0.13393425
0.8709335 0.1439706 0.02124634 -0.07226022 0.00000000 0.04683196
MahalanobisDist 0.21500000 pscore 0.4980000 0.50100000 0.4640000
0.4990000 0.06900000 0.00300000 0.01700000 0.04000000 LinPscore
-0.0890000 26.61000000 -1.9150000 26.5390000 0.06900000 0.00300000
0.04300000 0.07400000 N 337.0000000 363.0000000

See other functions in the package vignette.

``` {.r .rundoc-block rundoc-language="R" rundoc-exports="code"}
vignette(edar)
```

More information
================
