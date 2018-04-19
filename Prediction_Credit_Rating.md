Predicting\_Credit\_Rating
================

``` r
library(tree)
library(rpart)
library(rpart.plot)
library(caret)
```

    ## Loading required package: lattice

    ## Loading required package: ggplot2

    ## Warning: package 'ggplot2' was built under R version 3.4.3

``` r
library(e1071)
library(randomForest)
```

    ## randomForest 4.6-12

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

Function to convert to factors
------------------------------

``` r
to.factors = function(credit, variables){
  for (variable in variables) {
    credit[variable] = as.factor(credit[[variable]])
    
  }
  return(credit)
}
```

``` r
credit = read.csv("D://credit.csv",header = T,sep = ",")
dim(credit)
```

    ## [1] 1000   21

Data Availibility
=================

``` r
x=c()
for(i in 1:ncol(credit))
{
  nulls_count=length(which(is.na(credit[i])))
  x=append(x,nulls_count)
  print(nulls_count)
}
```

    ## [1] 0
    ## [1] 0
    ## [1] 0
    ## [1] 0
    ## [1] 0
    ## [1] 0
    ## [1] 0
    ## [1] 0
    ## [1] 0
    ## [1] 0
    ## [1] 0
    ## [1] 0
    ## [1] 0
    ## [1] 0
    ## [1] 0
    ## [1] 0
    ## [1] 0
    ## [1] 0
    ## [1] 0
    ## [1] 0
    ## [1] 0

``` r
x=c()
num.na=colSums(is.na(credit))
num.na
```

    ##                  credit.rating                account.balance 
    ##                              0                              0 
    ##         credit.duration.months previous.credit.payment.status 
    ##                              0                              0 
    ##                 credit.purpose                  credit.amount 
    ##                              0                              0 
    ##                        savings            employment.duration 
    ##                              0                              0 
    ##               installment.rate                 marital.status 
    ##                              0                              0 
    ##                      guarantor             residence.duration 
    ##                              0                              0 
    ##                 current.assets                            age 
    ##                              0                              0 
    ##                  other.credits                 apartment.type 
    ##                              0                              0 
    ##                   bank.credits                     occupation 
    ##                              0                              0 
    ##                     dependents                      telephone 
    ##                              0                              0 
    ##                 foreign.worker 
    ##                              0

``` r
num.na=colSums(!is.na(credit))*100/nrow(credit)
num.na
```

    ##                  credit.rating                account.balance 
    ##                            100                            100 
    ##         credit.duration.months previous.credit.payment.status 
    ##                            100                            100 
    ##                 credit.purpose                  credit.amount 
    ##                            100                            100 
    ##                        savings            employment.duration 
    ##                            100                            100 
    ##               installment.rate                 marital.status 
    ##                            100                            100 
    ##                      guarantor             residence.duration 
    ##                            100                            100 
    ##                 current.assets                            age 
    ##                            100                            100 
    ##                  other.credits                 apartment.type 
    ##                            100                            100 
    ##                   bank.credits                     occupation 
    ##                            100                            100 
    ##                     dependents                      telephone 
    ##                            100                            100 
    ##                 foreign.worker 
    ##                            100

Converting variables to factors
-------------------------------

``` r
categorical.vars=c("credit.rating","account.balance","previous.credit.payment.status",
                   "credit.purpose","savings","employment.duration","installment.rate",
                   "marital.status","guarantor","residence.duration",
                   "current.assets","other.credits","apartment.type","bank.credits",
                   "occupation","dependents","telephone","foreign.worker")

credit=to.factors(credit, categorical.vars)
```

Creating training and test data

``` r
indexes=sample(1:nrow(credit),size = 0.7*nrow(credit))

train.data=credit[indexes,]

test.data=credit[-indexes,]
```

Building a Random forest Model
------------------------------

``` r
formula.init="credit.rating~."
formula.init=as.formula(formula.init)
formula.init
```

    ## credit.rating ~ .

``` r
rf.model=randomForest(formula.init,data=train.data,
                      importance = T,proximity = T)


rf.model
```

    ## 
    ## Call:
    ##  randomForest(formula = formula.init, data = train.data, importance = T,      proximity = T) 
    ##                Type of random forest: classification
    ##                      Number of trees: 500
    ## No. of variables tried at each split: 4
    ## 
    ##         OOB estimate of  error rate: 24.43%
    ## Confusion matrix:
    ##    0   1 class.error
    ## 0 87 127  0.59345794
    ## 1 44 442  0.09053498

Predicting the Credit Rating of the test data
---------------------------------------------

``` r
test.feature.vars=test.data[,-1]
test.class.vars=test.data[,1]




rf.predictions=predict(rf.model,test.feature.vars,
                       type="class")
rf.predictions
```

    ##   2   5  10  15  16  17  20  23  29  32  38  39  55  56  60  61  65  67 
    ##   1   1   1   1   1   1   1   0   1   1   1   1   1   1   1   1   1   1 
    ##  69  73  74  75  79  84  85  96  98  99 102 103 105 110 114 120 121 125 
    ##   1   1   1   1   1   1   1   1   0   1   0   1   1   1   1   1   0   1 
    ## 133 134 148 149 157 160 161 164 176 178 183 184 185 187 188 190 191 192 
    ##   1   1   1   1   1   1   1   1   1   1   0   1   1   1   1   1   1   1 
    ## 195 197 198 208 211 213 214 216 224 226 234 239 240 242 244 245 246 247 
    ##   1   1   1   1   1   1   1   1   1   1   0   1   1   1   1   1   1   1 
    ## 249 254 256 257 258 259 261 264 268 274 279 283 287 294 304 307 308 309 
    ##   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   0 
    ## 310 314 315 317 319 321 322 324 325 326 327 335 344 348 354 355 363 365 
    ##   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
    ## 368 371 374 377 386 389 390 392 398 402 403 408 409 411 412 419 420 421 
    ##   1   0   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
    ## 422 433 434 439 440 441 443 445 449 451 456 458 462 465 473 479 483 487 
    ##   1   1   1   0   1   1   0   1   1   1   1   1   0   1   1   1   1   1 
    ## 489 491 492 494 498 499 501 507 509 511 512 513 515 516 517 522 527 532 
    ##   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   0   0   1 
    ## 536 537 538 553 555 556 557 559 561 570 573 575 576 578 581 585 596 599 
    ##   0   1   1   1   1   1   1   1   1   1   1   1   0   0   0   1   0   1 
    ## 602 607 608 609 620 625 634 639 640 641 645 646 647 651 657 661 662 664 
    ##   1   1   1   0   1   1   0   1   1   0   1   0   1   1   0   1   1   1 
    ## 667 668 674 675 678 680 686 697 698 706 715 722 723 725 726 728 730 731 
    ##   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   0 
    ## 733 736 737 738 739 741 743 744 747 755 759 772 775 779 785 793 800 801 
    ##   1   1   1   1   1   1   1   1   1   0   0   1   1   1   0   0   1   1 
    ## 804 814 816 818 819 820 823 829 834 836 837 840 842 843 849 856 857 860 
    ##   1   1   1   1   1   1   0   1   1   0   1   1   1   1   1   0   1   0 
    ## 861 866 868 869 871 872 873 874 875 876 880 882 886 890 895 898 900 901 
    ##   1   1   0   0   0   1   1   1   0   0   1   1   0   0   0   1   1   1 
    ## 902 907 911 912 916 918 919 920 926 927 929 931 934 935 940 941 942 943 
    ##   1   0   1   1   1   0   0   0   0   0   0   1   1   1   1   1   0   1 
    ## 952 962 969 970 973 978 980 981 982 990 991 997 
    ##   1   1   0   1   1   0   1   1   1   0   0   1 
    ## Levels: 0 1

Confusion Matrix to check the accuracy of our model
---------------------------------------------------

``` r
confusionMatrix(data=rf.predictions,reference = test.class.vars,
                positive = "1")
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0  34  18
    ##          1  52 196
    ##                                           
    ##                Accuracy : 0.7667          
    ##                  95% CI : (0.7146, 0.8134)
    ##     No Information Rate : 0.7133          
    ##     P-Value [Acc > NIR] : 0.02228         
    ##                                           
    ##                   Kappa : 0.353           
    ##  Mcnemar's Test P-Value : 8.005e-05       
    ##                                           
    ##             Sensitivity : 0.9159          
    ##             Specificity : 0.3953          
    ##          Pos Pred Value : 0.7903          
    ##          Neg Pred Value : 0.6538          
    ##              Prevalence : 0.7133          
    ##          Detection Rate : 0.6533          
    ##    Detection Prevalence : 0.8267          
    ##       Balanced Accuracy : 0.6556          
    ##                                           
    ##        'Positive' Class : 1               
    ## 

Parameter tuning for our model
------------------------------

``` r
nodesize.vals = c(2,3,4,5)
ntree.vals = c(200, 500, 1000, 2000)
tuning.results = tune.randomForest(formula.init,
                                   data = train.data,
                                   mtry = 3,
                                   nodesize = nodesize.vals,
                                   ntree = ntree.vals)
print(tuning.results)
```

    ## 
    ## Parameter tuning of 'randomForest':
    ## 
    ## - sampling method: 10-fold cross validation 
    ## 
    ## - best parameters:
    ##  nodesize mtry ntree
    ##         4    3   200
    ## 
    ## - best performance: 0.2328571

important variables for our model
---------------------------------

``` r
rf.model$importance
```

    ##                                            0            1
    ## account.balance                 0.0724156672 0.0262194215
    ## credit.duration.months          0.0143623203 0.0183293628
    ## previous.credit.payment.status  0.0102242432 0.0082901950
    ## credit.purpose                  0.0036642284 0.0022485875
    ## credit.amount                   0.0124038248 0.0155798421
    ## savings                         0.0079166191 0.0032820006
    ## employment.duration             0.0040304640 0.0025205256
    ## installment.rate               -0.0003465050 0.0024655313
    ## marital.status                  0.0049381001 0.0008566176
    ## guarantor                       0.0019060784 0.0032392493
    ## residence.duration             -0.0006371437 0.0005198332
    ## current.assets                  0.0026178020 0.0058771841
    ## age                             0.0031298270 0.0034710054
    ## other.credits                  -0.0003495171 0.0027199981
    ## apartment.type                  0.0005710256 0.0041478476
    ## bank.credits                    0.0010407821 0.0020058171
    ## occupation                     -0.0010405346 0.0004660360
    ## dependents                      0.0002506519 0.0011599974
    ## telephone                       0.0006501522 0.0016365666
    ## foreign.worker                  0.0003656660 0.0001206646
    ##                                MeanDecreaseAccuracy MeanDecreaseGini
    ## account.balance                        4.024872e-02        33.258229
    ## credit.duration.months                 1.713717e-02        30.521306
    ## previous.credit.payment.status         8.881254e-03        14.242767
    ## credit.purpose                         2.670250e-03        14.624310
    ## credit.amount                          1.458995e-02        41.445023
    ## savings                                4.728945e-03        15.253716
    ## employment.duration                    2.860526e-03        16.101474
    ## installment.rate                       1.550013e-03        13.068839
    ## marital.status                         2.078336e-03         9.539367
    ## guarantor                              2.799584e-03         3.859842
    ## residence.duration                     1.655869e-04        14.341083
    ## current.assets                         4.880628e-03        14.885565
    ## age                                    3.313698e-03        31.852765
    ## other.credits                          1.829464e-03         6.176642
    ## apartment.type                         2.990309e-03         8.354433
    ## bank.credits                           1.685436e-03         5.187177
    ## occupation                            -1.303145e-05        10.385652
    ## dependents                             8.987224e-04         4.208245
    ## telephone                              1.338074e-03         5.155918
    ## foreign.worker                         1.907281e-04         1.046762

Buiding a model with the important variables
--------------------------------------------

``` r
formula.new="credit.rating~account.balance, previous.credit.payment.status,
marital.status, apartment.type"
formula.new=as.formula(formula.init)
formula.new
```

    ## credit.rating ~ .

``` r
rf.model.best = tuning.results$best.model
rf.predictions.best = predict(rf.model.best, test.feature.vars,
                              type = "class")
```

Confusion matrix for our tuned model
------------------------------------

``` r
confusionMatrix(data=rf.predictions.best,
                reference = test.class.vars, positive = "1")
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0  29  14
    ##          1  57 200
    ##                                           
    ##                Accuracy : 0.7633          
    ##                  95% CI : (0.7111, 0.8103)
    ##     No Information Rate : 0.7133          
    ##     P-Value [Acc > NIR] : 0.03033         
    ##                                           
    ##                   Kappa : 0.3196          
    ##  Mcnemar's Test P-Value : 6.213e-07       
    ##                                           
    ##             Sensitivity : 0.9346          
    ##             Specificity : 0.3372          
    ##          Pos Pred Value : 0.7782          
    ##          Neg Pred Value : 0.6744          
    ##              Prevalence : 0.7133          
    ##          Detection Rate : 0.6667          
    ##    Detection Prevalence : 0.8567          
    ##       Balanced Accuracy : 0.6359          
    ##                                           
    ##        'Positive' Class : 1               
    ##
