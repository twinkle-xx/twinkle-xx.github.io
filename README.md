# Pima-Indians-Diabetes-Data-Analysis
The National Institute of Diabetes and Digestive and Kidney Diseases conducted a study on 768 adult female Pima Indians living near Phoenix. 
The purpose of the study was to investigate factors related to diabetes. The data may be found in the dataset *pima*.

**Jiaxin Yang**


```R
#Introducing the data
> library(faraway)
> data(pima)
```

**Create a factor version of the test results and use this to produce an interleaved histogram to show how the distribution of insulin differs between those testing positive and negative.**

```R
#Create a factor version of the test 
> pima$test.F <- factor(pima$test)
> str(pima)
'data.frame':	768 obs. of  10 variables:
 $ pregnant : int  6 1 8 1 0 5 3 10 2 8 ...
 $ glucose  : int  148 85 183 89 137 116 78 115 197 125 ...
 $ diastolic: int  72 66 64 66 40 74 50 0 70 96 ...
 $ triceps  : int  35 29 0 23 35 0 32 0 45 0 ...
 $ insulin  : int  0 0 0 94 168 0 88 0 543 0 ...
 $ bmi      : num  33.6 26.6 23.3 28.1 43.1 25.6 31 35.3 30.5 0 ...
 $ diabetes : num  0.627 0.351 0.672 0.167 2.288 ...
 $ age      : int  50 31 32 21 33 30 26 29 53 54 ...
 $ test     : int  1 0 1 0 1 0 1 0 1 1 ...
 $ test.F   : Factor w/ 2 levels "0","1": 2 1 2 1 2 1 2 1 2 2 ...
```

```R
#Interleaved histogram
> ggplot(pima,aes(x=insulin,color=test.F,fill=test.F))+geom_histogram(position='dodge',binwidth=60)
```

<img src="https://raw.githubusercontent.com/yjjjjxx/Pima-Indians-Diabetes-Data-Analysis/main/Screen%20Shot%202021-09-05%20at%2017.21.08.png" style="zoom:50%;" />

From this plot, there are a large number of observations' insulin value which equal to 0. This is unbelievable and not in line with common sense. I think that they might be missing values.

**Replace the zero values of insulin withthe missing value code NA.Recreatethe interleaved histogram plot and comment on the distribution.**

```R
> library(dplyr)
#Replace 0 of insulin with NA
> pima$insulinNA <- pima$insulin
> pima$insulinNA <- na_if(pima$insulin, 0)
> summary(pima$insulinNA)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  14.00   76.25  125.00  155.55  190.00  846.00     374 
```

```R
#Interleaved histogram
> ggplot(pima,aes(x=insulinNA,color=test.F,fill=test.F))+geom_histogram(position='dodge',binwidth=30)
Warning message:
Removed 374 rows containing non-finite values (stat_bin). 
```

<img src="https://raw.githubusercontent.com/yjjjjxx/Pima-Indians-Diabetes-Data-Analysis/main/Screen%20Shot%202022-01-07%20at%2020.53.40.png" style="zoom:50%;" />

From this plot, we can find that there are huge differences in diabetes test results below 125 of insulin. Below 125, there are much more negative cases than positive cases. For the rest of the distribution of insulin, it is slightly different between test results. Therefore, *insulin* does affect the response variable.

**Replace the incredible zeroes in other variables with the missing value code. Fit a model with the result of the diabetes test as the response and all the other variables as predictors. **

```R
#Replace the incredible zeroes in other variables with NA
> summary(pima)
    pregnant         glucose        diastolic         triceps     
 Min.   : 0.000   Min.   :  0.0   Min.   :  0.00   Min.   : 0.00  
 1st Qu.: 1.000   1st Qu.: 99.0   1st Qu.: 62.00   1st Qu.: 0.00  
 Median : 3.000   Median :117.0   Median : 72.00   Median :23.00  
 Mean   : 3.845   Mean   :120.9   Mean   : 69.11   Mean   :20.54  
 3rd Qu.: 6.000   3rd Qu.:140.2   3rd Qu.: 80.00   3rd Qu.:32.00  
 Max.   :17.000   Max.   :199.0   Max.   :122.00   Max.   :99.00  
                                                                  
    insulin           bmi           diabetes           age       
 Min.   :  0.0   Min.   : 0.00   Min.   :0.0780   Min.   :21.00  
 1st Qu.:  0.0   1st Qu.:27.30   1st Qu.:0.2437   1st Qu.:24.00  
 Median : 30.5   Median :32.00   Median :0.3725   Median :29.00  
 Mean   : 79.8   Mean   :31.99   Mean   :0.4719   Mean   :33.24  
 3rd Qu.:127.2   3rd Qu.:36.60   3rd Qu.:0.6262   3rd Qu.:41.00  
 Max.   :846.0   Max.   :67.10   Max.   :2.4200   Max.   :81.00  
                                                                 
      test       test.F    insulinNA     
 Min.   :0.000   0:500   Min.   : 14.00  
 1st Qu.:0.000   1:268   1st Qu.: 76.25  
 Median :0.000           Median :125.00  
 Mean   :0.349           Mean   :155.55  
 3rd Qu.:1.000           3rd Qu.:190.00  
 Max.   :1.000           Max.   :846.00  
                         NA's   :374     
```

According to common sense, *glucose*, *diastolic*, *triceps*, and *bmi* cannot be zero.

```R
#Replace 0 of glucose with NA
> pima$glucoseNA <- pima$glucose
> pima$glucoseNA <- na_if(pima$glucose, 0)

#Replace 0 of diastolic with NA
> pima$diastolicNA <- pima$diastolic
> pima$diastolicNA <- na_if(pima$diastolic, 0)

#Replace 0 of triceps with NA
> pima$tricepsNA <- pima$triceps
> pima$tricepsNA <- na_if(pima$triceps, 0)

#Replace 0 of bmi with NA
> pima$bmiNA <- pima$bmi
> pima$bmiNA <- na_if(pima$bmi, 0)
```

```R
#Fit a model with the result of the diabetes test as the response and all the other variables as predictors
> lm1 <- glm(test~pregnant+glucoseNA+diastolicNA+tricepsNA+insulinNA+bmiNA+diabetes+age,family=binomial,pima)
> summary(lm1)

Call:
glm(formula = test ~ pregnant + glucoseNA + diastolicNA + tricepsNA + 
    insulinNA + bmiNA + diabetes + age, family = binomial, data = pima)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.7823  -0.6603  -0.3642   0.6409   2.5612  

Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept) -1.004e+01  1.218e+00  -8.246  < 2e-16 ***
pregnant     8.216e-02  5.543e-02   1.482  0.13825    
glucoseNA    3.827e-02  5.768e-03   6.635 3.24e-11 ***
diastolicNA -1.420e-03  1.183e-02  -0.120  0.90446    
tricepsNA    1.122e-02  1.708e-02   0.657  0.51128    
insulinNA   -8.253e-04  1.306e-03  -0.632  0.52757    
bmiNA        7.054e-02  2.734e-02   2.580  0.00989 ** 
diabetes     1.141e+00  4.274e-01   2.669  0.00760 ** 
age          3.395e-02  1.838e-02   1.847  0.06474 .  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 498.10  on 391  degrees of freedom
Residual deviance: 344.02  on 383  degrees of freedom
  (376 observations deleted due to missingness)
AIC: 362.02

Number of Fisher Scoring iterations: 5
```

```R
# Number of observations used in the fitting model
> nobs(lm1)
[1] 392
```

There were 392 observations used in the model fitting for the reason of missing values. 

**Refit the model but now without the insulin and triceps predictors.**

```R
#Refit the model without the insulin and triceps predictors
> lm2 <- glm(test~pregnant+glucoseNA+diastolicNA+bmiNA+diabetes+age,family=binomial,pima)
> summary(lm2)

Call:
glm(formula = test ~ pregnant + glucoseNA + diastolicNA + bmiNA + 
    diabetes + age, family = binomial, data = pima)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.8062  -0.7229  -0.4049   0.7173   2.3959  

Coefficients:
             Estimate Std. Error z value Pr(>|z|)    
(Intercept) -8.962146   0.820892 -10.918  < 2e-16 ***
pregnant     0.117863   0.033418   3.527  0.00042 ***
glucoseNA    0.035194   0.003605   9.763  < 2e-16 ***
diastolicNA -0.008916   0.008618  -1.035  0.30084    
bmiNA        0.090926   0.015740   5.777 7.61e-09 ***
diabetes     0.960515   0.306415   3.135  0.00172 ** 
age          0.016944   0.009834   1.723  0.08489 .  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 931.94  on 723  degrees of freedom
Residual deviance: 672.86  on 717  degrees of freedom
  (44 observations deleted due to missingness)
AIC: 686.86

Number of Fisher Scoring iterations: 5
```

```R
# Number of observations used in the fitting model
> nobs(lm2)
[1] 724
```

After removing the insulin and triceps predictors, there were 724 observations used in fitting this model.

Hypothesis test to check whether *insulin* and *triceps* have significant effects on the response variable: 

H~0~: insulin = triceps = 0 

H~a~: insulin ≠ triceps ≠ 0 

First, we need to create a new dataset called *pimaNA* which exclude all the rows with any missing values

```R
#Create a new dataset which exclude all the rows with any missing values
> pimaNA <- na.omit(pima)
#Refit two models with the same size of dataset
> lm1NA <- glm(test~pregnant+glucoseNA+diastolicNA+tricepsNA+insulinNA+bmiNA+diabetes+age,family=binomial,pimaNA)
> lm2NA <- glm(test~pregnant+glucoseNA+diastolicNA+bmiNA+diabetes+age,family=binomial,pimaNA)

#Comapre two models with ANOVA
> anova(lm2NA,lm1NA,test='Chi')
Analysis of Deviance Table

Model 1: test ~ pregnant + glucoseNA + diastolicNA + bmiNA + diabetes + 
    age
Model 2: test ~ pregnant + glucoseNA + diastolicNA + tricepsNA + insulinNA + 
    bmiNA + diabetes + age
  Resid. Df Resid. Dev Df Deviance Pr(>Chi)
1       385     344.88                     
2       383     344.02  2  0.85931   0.6507
```

From the ANOVA, we can get a p-value=0.6507 which is greater than 0.05. We fail to reject the null hypothesis and conclude that *insulin* and *triceps* do not have significant effects on the response variable. Thus, the model without *insulin* and *triceps* is the better choice. 

**Use AIC to select a model.**

```R
> lm3NA <- stepAIC(lm1NA,trace=FALSE)
> summary(lm3NA)

Call:
glm(formula = test ~ pregnant + glucoseNA + bmiNA + diabetes + 
    age, family = binomial, data = pimaNA)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.8827  -0.6535  -0.3694   0.6521   2.5814  

Coefficients:
             Estimate Std. Error z value Pr(>|z|)    
(Intercept) -9.992080   1.086866  -9.193  < 2e-16 ***
pregnant     0.083953   0.055031   1.526 0.127117    
glucoseNA    0.036458   0.004978   7.324 2.41e-13 ***
bmiNA        0.078139   0.020605   3.792 0.000149 ***
diabetes     1.150913   0.424242   2.713 0.006670 ** 
age          0.034360   0.017810   1.929 0.053692 .  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 498.10  on 391  degrees of freedom
Residual deviance: 344.89  on 386  degrees of freedom
AIC: 356.89

Number of Fisher Scoring iterations: 5
 > nobs(lm3NA)
[1] 392
```

Based on AIC, the *pregnant*, *glucoseNA*, *bmiNA*, *diabetes* and *age* were selected as predictors. They are also used in my selected model. There were 392 observations used in the model fitting

**Create a variable that indicates whether the case contains a missing value. Use this variable as a predictor of the test result. Refit the selected model, but now using as much of the data as reasonable.**

```R
#Create a missing indicator
> pima$missing <- ifelse(apply(pima, 1, anyNA), 1, 0)
#Test missingness
> lmMissing <- glm(test.F~missing, family=binomial,pima) 
> summary(lmMissing)

Call:
glm(formula = test.F ~ missing, family = binomial, data = pima)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-0.9564  -0.9564  -0.8977   1.4159   1.4857  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -0.7008     0.1073  -6.533 6.47e-11 ***
missing       0.1558     0.1515   1.028    0.304    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 993.48  on 767  degrees of freedom
Residual deviance: 992.43  on 766  degrees of freedom
AIC: 996.43

Number of Fisher Scoring iterations: 4
```

We can use hypothesis test to check whether missingness is associated with the test result. 

H~0~: missing = 0 

H~a~: missing ≠ 0 

```R
# Test whether missing is significant predictor
> anova(lmMissing,test="Chi")
Analysis of Deviance Table

Model: binomial, link: logit

Response: test.F

Terms added sequentially (first to last)


        Df Deviance Resid. Df Resid. Dev Pr(>Chi)
NULL                      767     993.48         
missing  1   1.0579       766     992.43   0.3037
```

From the ANOVA, we can get the p-value = 0.3037 which is greater than 0.05. We fail to reject the null hypothesis and conclude that *missing* do not have significant effect on the response variable. Thus, missingness is not associated with the test result. We can exclude all missing values from the dataset and refit the model. 

```R
#Exclude all missing values
> pima <- na.exclude(pima)
> lmod <- glm(test.F~pregnant+glucose+bmi+diabets+age,family=binomial,pima)
> summary(lmod)

Call:
glm(formula = test.F ~ pregnant + glucose + bmi + diabetes + 
    age, family = binomial, data = pima)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.8827  -0.6535  -0.3694   0.6521   2.5814  

Coefficients:
             Estimate Std. Error z value Pr(>|z|)    
(Intercept) -9.992080   1.086866  -9.193  < 2e-16 ***
pregnant     0.083953   0.055031   1.526 0.127117    
glucose      0.036458   0.004978   7.324 2.41e-13 ***
bmi          0.078139   0.020605   3.792 0.000149 ***
diabetes     1.150913   0.424242   2.713 0.006670 ** 
age          0.034360   0.017810   1.929 0.053692 .  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 498.10  on 391  degrees of freedom
Residual deviance: 344.89  on 386  degrees of freedom
AIC: 356.89

Number of Fisher Scoring iterations: 5
```

**Using the last fitted model of the previous part**

```R
> summary(pima$bmi)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  18.20   28.40   33.20   33.09   37.10   67.10
#Get the first and third quartile of bmi
> q1 <- quantile(pima$bmi,.25)
> q3 <- quantile(pima$bmi,.75)
#Get the coef. of parameters
> beta=coef(lmod)
> beta
(Intercept)    pregnant     glucose         bmi    diabetes         age 
-9.99207971  0.08395301  0.03645776  0.07813866  1.15091285  0.03436036 
# Calculate difference in log odds 
> log_odds_diff = beta[4]*q1 -beta[4]*q3
> log_odds_diff
       bmi 
-0.6798063
> exp(log_odds_diff)
      bmi 
0.5067151
> 0.5067151+1
[1] 1.506715
```

The woman with a BMI at the third quartile is 1.5067151 times more likely to have positive results than those at the first quartile.

```R
#95% CI for bmi parameter
> confint(lmod,'bmi')
Waiting for profiling to be done...
     2.5 %     97.5 % 
0.03874896 0.11984439 
> ci = confint(lmod,'bmi')
> exp(ci*(q1-q3))
    2.5 %    97.5 % 
0.7138261 0.3525206 
#Probability
> 0.3525206 / (1+0.3525206)
[1] 0.2606397
> 0.7138261 / (1+0.7138261)
[1] 0.4165102
```

Thus, odds of diabetes occurrence for a women with a BMI at the first quartile are (26%, 41%) less than a women with a BMI at the third quartile.

**Do women who test positive have higher diastolic blood pressures? Is the diastolic blood pressure significant in the regression model?**

```R
#The correlations between diastolic with others
> cor(pima)['diastolic',]
  pregnant    glucose  diastolic    triceps    insulin        bmi   diabetes 
 0.2133548  0.2100266  1.0000000  0.2325712  0.0985115  0.3044034 -0.0159711 
       age       test 
 0.3000389  0.1926733
```

From the results, there is indeed a positive correlation between diastolic blood pressures and test results. That means that the positive test results might be more likely to occur with higher diastolic blood pressures. Also, the diastolic blood pressures have positive correlation with *pregnant*, *glucose*, *triceps*, *insulin*, *bmi*, and *age*. It should be noted that the correlation coefficient does not exceed 0.2. This may prove that the relationship is weakly correlated between diastolic blood pressures and test results.

```R
> boxplot(diastolic~ as.factor(test),pima)
```

<img src="https://raw.githubusercontent.com/yjjjjxx/Pima-Indians-Diabetes-Data-Analysis/main/Screen%20Shot%202021-09-08%20at%2013.09.34.png" style="zoom:30%;" />

Then, the boxplot of *diastolic* in both positive and negative results do not have significant difference. 

```R
> summary(lm1)

Call:
glm(formula = test ~ pregnant + glucoseNA + diastolicNA + tricepsNA + 
    insulinNA + bmiNA + diabetes + age, family = binomial, data = pima)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.7823  -0.6603  -0.3642   0.6409   2.5612  

Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept) -1.004e+01  1.218e+00  -8.246  < 2e-16 ***
pregnant     8.216e-02  5.543e-02   1.482  0.13825    
glucoseNA    3.827e-02  5.768e-03   6.635 3.24e-11 ***
diastolicNA -1.420e-03  1.183e-02  -0.120  0.90446    
tricepsNA    1.122e-02  1.708e-02   0.657  0.51128    
insulinNA   -8.253e-04  1.306e-03  -0.632  0.52757    
bmiNA        7.054e-02  2.734e-02   2.580  0.00989 ** 
diabetes     1.141e+00  4.274e-01   2.669  0.00760 ** 
age          3.395e-02  1.838e-02   1.847  0.06474 .  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 498.10  on 391  degrees of freedom
Residual deviance: 344.02  on 383  degrees of freedom
  (376 observations deleted due to missingness)
AIC: 362.02

Number of Fisher Scoring iterations: 5
```

The previous model also indicates that the *diastolic* is not significant enough in the model with the presence of other predictors. Although there is positive correlation between the *diastolic* and test results, the *diastolic* is still not strong enough to affect the response variable in the model. Thus, it is just apparently contradictory.
