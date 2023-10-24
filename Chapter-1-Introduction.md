Chapter 1 : Introduction
================
Sitthatka Jarutsang, Student M.SC.

## เริ่มต้นด้วยการติดตั้ง

Install และเรียกใช้ Library

``` r
#install.packages("dplyr")
#install.packages("tidyvers")
library(dplyr)
```

``` r
library(tidyr)
```

สร้าง DATA FRAME สมมติขึ้นมา

``` r
data1 = data.frame(CustomerId = c(1:6), 
                 Product = c("Refrigerator", "Microwave","Toaster","Rice cooker","Dishwasher","Coffee maker"))
data1
```

``` r
data2 = data.frame(CustomerId = c(2, 4, 6, 7, 8), 
                 State = c("New York","Washington","Texas","Nevada","Florida"))
data2
```

``` r
data3 = data.frame(CustomerId = c(10,30,100,50,60,70), 
                 Product = c("Refrigerator", "Microwave","Toaster","Rice cooker","Dishwasher","Coffee maker"))
data3
```

การรวมข้อมูลด้วย Join ประกอบด้วย 4 ประเภทได้แก่ Inner join

### Inner join

This function allows you to combine data from two sources based on a
common key or keys.

``` r
data_Inner1 = inner_join(data1,data2, by ='CustomerId')
data_Inner1
```

``` r
data_Inner2= data1 %>% inner_join(data2,by="CustomerId")
data_Inner2
```

### outer join

A full outer join includes all the rows from both data frames and fills
in missing values with NA where there are no matches.

full_join() keeps all observations in x and y.

``` r
data_outer = full_join(x=data1,y=data2,by="CustomerId") 
data_outer
```

### left join left_join()

A left join returns all the rows from the left data frame and the
matched rows from the right data frame.

keeps all observations in x.

``` r
data_left = left_join(x=data1,y=data2,by="CustomerId") 
data_left
```

### right join right_join()

A right join returns all the rows from the right data frame and the
matched rows from the left data frame.

keeps all observations in y.

``` r
data_right = right_join(x=data1,y=data2,by="CustomerId") 
data_right
```

show result

``` r
data_outer %>% glimpse()
```

``` r
glimpse(data_outer)
```

check missing data in each column

``` r
results = apply(is.na(data_left),2,which)
```

Fillter out the missing values by drop_na

``` r
data_NEW = drop_na(data_outer)
```
