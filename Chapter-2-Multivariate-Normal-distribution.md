Chapter-2 Multivariate Normal distribution
================
Sitthatka Jarutsang, Student M.SC.

# วิธีการ cleansing data และการทดสอบการแจกแจงปกติ

# data mapulation

``` r
#install.packages('DataExplorer')
library(DataExplorer)
library(tidyr)
```

import file

``` r
file = 'C:/Users/User/Documents/data/CreditApproval.csv'
data = read.csv(file)
head(data)
```

show about data

``` r
introduce(data)
```

view missing data

``` r
plot_missing(data)
```

``` r
datac = apply(is.na(data),2,which)
datac = drop_na(data)
head(datac)
```

plot data

``` r
data1 = data[1:200,c('A1','A4','A5')]
plot_bar(data[,c('A1','A4','A5')])
```

``` r
plot_scatterplot(data[,c('A2','A3')],by = 'A2')
```

``` r
plot_histogram(data[,c('A2')])
```

Summary data show distribution

``` r
summary(data)
```

# Simulation multivariate normal distribution

``` r
#install.packages("MASS")
#install.packages("MVN")
library(MASS)
library(MVN)
library(ggplot2)
```

``` r
n = 100 
mu = c(0,0)
sigma = matrix(c(10,3,3,2),2,2)
set.seed(1)
data2 = mvrnorm(n,mu,sigma)
```

normality test for

H0 : data2 is multivariate is normal distribution

H1 : data2 is multivariate is not normal distribution

we will reject H0 if p-value \< 0.005

``` r
result = mvn(data2,mvnTest = 'energy')
```

p-value = 0.22 Accept H0, So data2 is multivariate is normal
distribution

``` r
result = mvn(data2,mvnTest = 'energy', univariatePlot = 'qqplot', multivariatePlot = 'qq')
```

``` r
result = mvn(data2,mvnTest = 'energy', multivariatePlot = 'qq')
```

test in iris data

``` r
library(readxl)
iris = read_excel("C:/Users/User/Documents/data/iris_xcel.xlsx")
data_iris = iris[c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')]
result_data_iris = mvn(data_iris,mvnTest = 'energy')
result_data_iris = mvn(data_iris,mvnTest = 'energy',univariatePlot = 'qqplot')
```

``` r
result_data_iris = mvn(data_iris,mvnTest = 'energy',multivariatePlot = 'qq')
```
