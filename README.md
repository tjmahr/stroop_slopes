Does the stroop effect vary across people?
================
TJ Mahr
August 26, 2016

An attempt to work through [this example](https://jeffrouder.blogspot.com/2016/08/where-bayes-and-classical-inference.html).

Download and prepare data
-------------------------

``` r
library(dplyr, warn.conflicts = FALSE)
library(lme4)
#> Loading required package: Matrix
#> 
#> Attaching package: 'lme4'
#> The following object is masked from 'package:stats':
#> 
#>     sigma
library(rstanarm)
#> Loading required package: Rcpp
#> Warning: replacing previous import 'lme4::sigma' by 'stats::sigma' when
#> loading 'rstanarm'
#> rstanarm (Version 2.11.1, packaged: 2016-07-29 14:31:44 UTC)
#> - Do not expect the default priors to remain the same in future rstanarm versions.
#> Thus, R scripts should specify priors explicitly, even if they are just the defaults.
#> - For execution on a local, multicore CPU with excess RAM we recommend calling
#> options(mc.cores = parallel::detectCores())
#> 
#> Attaching package: 'rstanarm'
#> The following object is masked from 'package:lme4':
#> 
#>     sigma
options(mc.cores = parallel::detectCores())

# Download dataset
filename <- curl::curl("https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/contexteffects/FlankerStroopSimon/LEF_stroop.csv")
raw_stroop <- readr::read_csv2(filename)
#> Parsed with column specification:
#> cols(
#>   ID = col_integer(),
#>   congruency = col_character(),
#>   RT = col_integer(),
#>   accuracy = col_integer()
#> )

# Keep a local copy just in case
readr::write_csv(raw_stroop, "./stroop_data.csv")

# Data cleaning
stroop <- raw_stroop %>%
  # Add trial numbers for each participant
  group_by(ID) %>%
  mutate(trial = seq_len(n()),
         rt_sec = RT / 1000) %>%
  ungroup %>%
  # Keep correct responses, ignore neutral trials, discard extreme RTs
  filter(accuracy == 1, congruency != "neutral", .2 < rt_sec, rt_sec < 2)
```

Ten random rows from the dataset:

``` r
stroop %>% 
  sample_n(10) %>% 
  knitr::kable()
```

|   ID| congruency  |    RT|  accuracy|  trial|  rt\_sec|
|----:|:------------|-----:|---------:|------:|--------:|
|   18| congruent   |   719|         1|     36|    0.719|
|   99| congruent   |   605|         1|     36|    0.605|
|   41| incongruent |   578|         1|     32|    0.578|
|   32| congruent   |   437|         1|    123|    0.437|
|   79| incongruent |   627|         1|    124|    0.627|
|   61| congruent   |   653|         1|     68|    0.653|
|    4| incongruent |   669|         1|    109|    0.669|
|  115| incongruent |   659|         1|    141|    0.659|
|   29| congruent   |  1079|         1|     28|    1.079|
|   81| incongruent |   761|         1|     89|    0.761|

Sanity check to make sure that people contribute roughly the same numbers of trials.

``` r
n_trials_per_condition_per_id <- stroop %>% count(ID, congruency)
range(n_trials_per_condition_per_id$n)
#> [1] 34 48
```

lme4
----

``` r
# Allow intercepts to vary within participant
model1 <- lmer(RT ~ congruency + (1 | ID), stroop)
summary(model1)
#> Linear mixed model fit by REML ['lmerMod']
#> Formula: RT ~ congruency + (1 | ID)
#>    Data: stroop
#> 
#> REML criterion at convergence: 151270.7
#> 
#> Scaled residuals: 
#>     Min      1Q  Median      3Q     Max 
#> -3.5868 -0.5958 -0.1857  0.3556  6.7862 
#> 
#> Random effects:
#>  Groups   Name        Variance Std.Dev.
#>  ID       (Intercept)  9478     97.35  
#>  Residual             39404    198.50  
#> Number of obs: 11245, groups:  ID, 121
#> 
#> Fixed effects:
#>                       Estimate Std. Error t value
#> (Intercept)            708.280      9.230   76.74
#> congruencyincongruent   64.582      3.746   17.24
#> 
#> Correlation of Fixed Effects:
#>             (Intr)
#> cngrncyncng -0.199

# Allow condition effect to vary within participants
model2 <- lmer(RT ~ congruency + (congruency | ID), stroop)
summary(model2)
#> Linear mixed model fit by REML ['lmerMod']
#> Formula: RT ~ congruency + (congruency | ID)
#>    Data: stroop
#> 
#> REML criterion at convergence: 151259.8
#> 
#> Scaled residuals: 
#>     Min      1Q  Median      3Q     Max 
#> -3.5447 -0.5996 -0.1807  0.3559  6.8254 
#> 
#> Random effects:
#>  Groups   Name                  Variance Std.Dev. Corr 
#>  ID       (Intercept)           10641.5  103.16        
#>           congruencyincongruent   517.7   22.75   -0.56
#>  Residual                       39274.4  198.18        
#> Number of obs: 11245, groups:  ID, 121
#> 
#> Fixed effects:
#>                       Estimate Std. Error t value
#> (Intercept)            708.266      9.736   72.75
#> congruencyincongruent   64.680      4.274   15.13
#> 
#> Correlation of Fixed Effects:
#>             (Intr)
#> cngrncyncng -0.426
```

Use a simple model comparison to test whether the two additional parameters improves model fit.

``` r
anova(model1, model2)
#> refitting model(s) with ML (instead of REML)
#> Data: stroop
#> Models:
#> model1: RT ~ congruency + (1 | ID)
#> model2: RT ~ congruency + (congruency | ID)
#>        Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)   
#> model1  4 151289 151319 -75641   151281                            
#> model2  6 151283 151327 -75635   151271 10.732      2   0.004674 **
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Slower responders have a smaller stroop effect.

``` r
plot(ranef(model2))
#> $ID
```

![](README_files/figure-markdown_github/slope-intercept%20correlation-1.png)

rstanarm
--------

``` r
refit <- FALSE

# It took a couple hours to fit both of these models, so let's not refit them
# whenever this document is generated.
if (refit) {
  # Allow intercepts to vary within participant
  b_model1 <- stan_lmer(
    formula = RT ~ congruency + (1 | ID),
    data = stroop,
    prior = normal(0, 5),
    prior_covariance = decov(regularization = 1),
    prior_intercept = normal(0, 10))
  save(b_model1, file = "model1.Rdata")

  b_model2 <- update(b_model1, formula = RT ~ congruency + (congruency | ID))
  save(b_model2, file = "model2.Rdata")
} else {
  load("./model1.Rdata")
  load("./model2.Rdata")
}
```

There are too many effects for us to `summary` on each model.

``` r
b_model1
#> stan_lmer(formula = RT ~ congruency + (1 | ID), data = stroop, 
#>     prior = normal(0, 5), prior_intercept = normal(0, 10), prior_covariance = decov(regularization = 1))
#> 
#> Estimates:
#>                       Median MAD_SD
#> (Intercept)           707.7    8.9 
#> congruencyincongruent  64.6    3.8 
#> sigma                 198.5    1.3 
#> 
#> Error terms:
#>  Groups   Name        Std.Dev.
#>  ID       (Intercept)  97     
#>  Residual             199     
#> Num. levels: ID 121 
#> 
#> Sample avg. posterior predictive 
#> distribution of y (X = xbar):
#>          Median MAD_SD
#> mean_PPD 740.2    2.7
b_model2
#> stan_lmer(formula = RT ~ congruency + (congruency | ID), data = stroop, 
#>     prior = normal(0, 5), prior_intercept = normal(0, 10), prior_covariance = decov(regularization = 1))
#> 
#> Estimates:
#>                       Median MAD_SD
#> (Intercept)           707.7    9.8 
#> congruencyincongruent  64.7    4.2 
#> sigma                 198.2    1.3 
#> 
#> Error terms:
#>  Groups   Name                  Std.Dev. Corr 
#>  ID       (Intercept)           103           
#>           congruencyincongruent  22      -0.55
#>  Residual                       198           
#> Num. levels: ID 121 
#> 
#> Sample avg. posterior predictive 
#> distribution of y (X = xbar):
#>          Median MAD_SD
#> mean_PPD 740.2    2.7
```

``` r
posterior_interval(b_model1) %>% 
  as.data.frame %>% 
  tibble::rownames_to_column()
#>                   rowname           5%          95%
#> 1             (Intercept)  692.0423651  724.0343910
#> 2   congruencyincongruent   58.4996470   70.7826762
#> 3     b[(Intercept) ID:1]  -93.0939690  -20.4966662
#> 4     b[(Intercept) ID:2] -131.9517473  -58.9393790
#> 5     b[(Intercept) ID:3]  -35.8451970   37.6823935
#> 6     b[(Intercept) ID:4]  -55.4938326   17.1982895
#> 7     b[(Intercept) ID:5]   48.7520204  122.7316796
#> 8     b[(Intercept) ID:6]  -92.6201337  -19.0347165
#> 9     b[(Intercept) ID:7]  -85.7847764  -12.2045338
#> 10    b[(Intercept) ID:8]  -47.6622201   24.4397220
#> 11    b[(Intercept) ID:9]  -98.6152916  -23.5940002
#> 12   b[(Intercept) ID:10]  -33.1602570   39.3435640
#> 13   b[(Intercept) ID:11]    1.8501141   73.6819267
#> 14   b[(Intercept) ID:12]   92.7510639  161.4168593
#> 15   b[(Intercept) ID:13]  -43.6802689   32.6876145
#> 16   b[(Intercept) ID:14]    0.2318171   71.6085525
#> 17   b[(Intercept) ID:15]   94.5044899  166.9850974
#> 18   b[(Intercept) ID:16]  -15.1046715   55.1602286
#> 19   b[(Intercept) ID:17]  -21.1095529   53.0962070
#> 20   b[(Intercept) ID:18]   37.0896133  110.8879006
#> 21   b[(Intercept) ID:19] -108.6111898  -36.0408039
#> 22   b[(Intercept) ID:20]  -95.0062597  -23.4858758
#> 23   b[(Intercept) ID:21]   13.4377410   85.1403435
#> 24   b[(Intercept) ID:22] -148.3842506  -77.2961924
#> 25   b[(Intercept) ID:23]  -99.3117416  -28.0033600
#> 26   b[(Intercept) ID:24]  -40.3494425   31.6173237
#> 27   b[(Intercept) ID:25]  -64.4636101    5.8965744
#> 28   b[(Intercept) ID:26]   -5.2682010   67.7682687
#> 29   b[(Intercept) ID:27]   90.8283263  161.4065980
#> 30   b[(Intercept) ID:28]   89.4151267  166.4951986
#> 31   b[(Intercept) ID:29]   31.8732981  103.8279319
#> 32   b[(Intercept) ID:30] -178.9509591 -107.8807921
#> 33   b[(Intercept) ID:31]   78.1435778  150.9949227
#> 34   b[(Intercept) ID:32]   32.0951148  101.8621135
#> 35   b[(Intercept) ID:33]   67.7644959  141.2792683
#> 36   b[(Intercept) ID:34]  -43.1700991   33.2621235
#> 37   b[(Intercept) ID:35]  -93.3959945  -22.2246866
#> 38   b[(Intercept) ID:36]  -66.5610082    5.1300306
#> 39   b[(Intercept) ID:37]  -43.8748832   30.2840230
#> 40   b[(Intercept) ID:38]  -87.2084947  -17.2129518
#> 41   b[(Intercept) ID:39] -189.7114482 -116.9432534
#> 42   b[(Intercept) ID:40]   72.5332657  144.7380292
#> 43   b[(Intercept) ID:41]  -71.2857402   -1.6751833
#> 44   b[(Intercept) ID:42]  -72.3294809   -0.6068312
#> 45   b[(Intercept) ID:43] -122.9544477  -51.1331047
#> 46   b[(Intercept) ID:44]  -24.1609382   47.1702416
#> 47   b[(Intercept) ID:45]  -83.1092811  -11.5720603
#> 48   b[(Intercept) ID:46]  -89.7742606  -18.6097801
#> 49   b[(Intercept) ID:47]   -9.3516997   63.2043666
#> 50   b[(Intercept) ID:48]  -21.3025016   51.5486944
#> 51   b[(Intercept) ID:49] -140.5828239  -69.0160429
#> 52   b[(Intercept) ID:50] -192.5891101 -119.7494607
#> 53   b[(Intercept) ID:51]    5.6853060   75.8557128
#> 54   b[(Intercept) ID:52] -193.4627646 -121.3028009
#> 55   b[(Intercept) ID:53]  -99.6863268  -26.9013624
#> 56   b[(Intercept) ID:54] -112.2668929  -40.5844151
#> 57   b[(Intercept) ID:55]  -43.4554688   32.4099756
#> 58   b[(Intercept) ID:56] -159.9229812  -86.6077104
#> 59   b[(Intercept) ID:57]   43.4231694  115.9789704
#> 60   b[(Intercept) ID:58] -116.3086011  -42.8450975
#> 61   b[(Intercept) ID:59]   -8.4459757   62.3008640
#> 62   b[(Intercept) ID:60]  192.1377588  265.3602366
#> 63   b[(Intercept) ID:61]   95.3670934  169.5709929
#> 64   b[(Intercept) ID:62] -144.1909421  -69.9407935
#> 65   b[(Intercept) ID:63]   41.9700350  113.5736245
#> 66   b[(Intercept) ID:64] -245.7727117 -173.6485588
#> 67   b[(Intercept) ID:65]   66.7716228  138.2797791
#> 68   b[(Intercept) ID:66]  -87.5836813  -15.9240645
#> 69   b[(Intercept) ID:67] -112.2750926  -37.0897819
#> 70   b[(Intercept) ID:68]  -96.4172640  -20.5706551
#> 71   b[(Intercept) ID:69] -250.1938388 -174.2391517
#> 72   b[(Intercept) ID:70]  118.9903229  193.2529798
#> 73   b[(Intercept) ID:71]   78.9452282  153.0267361
#> 74   b[(Intercept) ID:72]   50.3427047  121.0810310
#> 75   b[(Intercept) ID:73]  -68.6939272    1.7167080
#> 76   b[(Intercept) ID:74]  212.3187503  286.5311887
#> 77   b[(Intercept) ID:75]  149.9213842  220.6978391
#> 78   b[(Intercept) ID:76]   27.1212802   99.4511456
#> 79   b[(Intercept) ID:77]   68.8951628  142.1336100
#> 80   b[(Intercept) ID:78]   80.1231644  152.0431760
#> 81   b[(Intercept) ID:79]   13.1700852   84.2409862
#> 82   b[(Intercept) ID:80]  -78.6829555   -6.5733832
#> 83   b[(Intercept) ID:81] -162.7092958  -89.1379142
#> 84   b[(Intercept) ID:82]  -99.8728436  -26.9032451
#> 85   b[(Intercept) ID:83]   -7.6113513   63.8079560
#> 86   b[(Intercept) ID:84]  -57.9645576   15.3911532
#> 87   b[(Intercept) ID:85]  162.7949775  233.7217247
#> 88   b[(Intercept) ID:86]  -85.8602060  -17.5198181
#> 89   b[(Intercept) ID:87]    5.6668076   77.3553280
#> 90   b[(Intercept) ID:88] -128.9363723  -58.3716239
#> 91   b[(Intercept) ID:89]  203.5815149  273.6011964
#> 92   b[(Intercept) ID:90] -111.8920501  -38.6145033
#> 93   b[(Intercept) ID:91]  -41.0343822   31.1610205
#> 94   b[(Intercept) ID:92] -105.2222421  -32.9382743
#> 95   b[(Intercept) ID:93] -125.5822273  -51.1868762
#> 96   b[(Intercept) ID:94] -109.8004119  -34.6886323
#> 97   b[(Intercept) ID:95]  -37.4908502   35.4243628
#> 98   b[(Intercept) ID:96]  -69.2889396    3.3801679
#> 99   b[(Intercept) ID:97]   35.8473773  107.5362198
#> 100  b[(Intercept) ID:98]  -20.4569704   52.9908132
#> 101  b[(Intercept) ID:99] -139.7167314  -68.4575904
#> 102 b[(Intercept) ID:100]   79.3524683  149.0403496
#> 103 b[(Intercept) ID:101]   -1.6498509   68.0842518
#> 104 b[(Intercept) ID:102] -146.6313884  -71.8320499
#> 105 b[(Intercept) ID:103]  -39.1846696   34.1389331
#> 106 b[(Intercept) ID:104]  134.2639663  207.4491648
#> 107 b[(Intercept) ID:105] -115.5656885  -43.9094717
#> 108 b[(Intercept) ID:106]  -77.5236308   -5.3180660
#> 109 b[(Intercept) ID:107] -112.2387987  -41.0429272
#> 110 b[(Intercept) ID:108]  -99.1133386  -26.8832950
#> 111 b[(Intercept) ID:109]   42.1171549  114.4840752
#> 112 b[(Intercept) ID:110]  -49.3597553   22.6368759
#> 113 b[(Intercept) ID:111] -170.1820472  -99.9586741
#> 114 b[(Intercept) ID:112]  -29.0178660   42.2914636
#> 115 b[(Intercept) ID:113]  -47.4358805   24.8663667
#> 116 b[(Intercept) ID:114]   14.8744599   87.8058149
#> 117 b[(Intercept) ID:115]   14.9245388   85.3957232
#> 118 b[(Intercept) ID:116]  174.3806105  246.8723738
#> 119 b[(Intercept) ID:117]  -20.7373179   50.5824920
#> 120 b[(Intercept) ID:118]   50.0015464  122.8697502
#> 121 b[(Intercept) ID:119]   -9.9852960   63.1745436
#> 122 b[(Intercept) ID:120] -203.0952566 -132.8276220
#> 123 b[(Intercept) ID:121] -148.6733885  -77.4612661
#> 124                 sigma  196.2856914  200.7410159

# newdata <- stroop %>% 
#   distinct(ID, congruency) %>% 
#   filter(congruency == "congruent")
# newdata2 <- stroop %>% 
#   distinct(ID, congruency) %>% 
#   filter(congruency == "incongruent")
# 
# posterior <- posterior_predict(b_model1, newdata) %>% 
#   apply(2, function(xs) quantile(xs, probs = c(.025, .1, .5, .9, .975)))
```
