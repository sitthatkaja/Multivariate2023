Individual work clustering pca and no pca
================
Sitthatka Jarutsang, Student M.SC.

# Library

``` r
library(DataExplorer)
library(ClusterR)
library(cluster)
library(ggfortify)
```

    ## Loading required package: ggplot2

``` r
library(stats)
library(fpc)
library(factoextra)
```

    ## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa

``` r
library(corrplot)
```

    ## corrplot 0.92 loaded

# import

``` r
file = 'C:/Users/User/Documents/data/Country-data.csv'
data = read.csv(file)
head(data)
```

    ##               country child_mort exports health imports income inflation
    ## 1         Afghanistan       90.2    10.0   7.58    44.9   1610      9.44
    ## 2             Albania       16.6    28.0   6.55    48.6   9930      4.49
    ## 3             Algeria       27.3    38.4   4.17    31.4  12900     16.10
    ## 4              Angola      119.0    62.3   2.85    42.9   5900     22.40
    ## 5 Antigua and Barbuda       10.3    45.5   6.03    58.9  19100      1.44
    ## 6           Argentina       14.5    18.9   8.10    16.0  18700     20.90
    ##   life_expec total_fer  gdpp
    ## 1       56.2      5.82   553
    ## 2       76.3      1.65  4090
    ## 3       76.5      2.89  4460
    ## 4       60.1      6.16  3530
    ## 5       76.8      2.13 12200
    ## 6       75.8      2.37 10300

``` r
introduce(data)
```

    ##   rows columns discrete_columns continuous_columns all_missing_columns
    ## 1  167      10                1                  9                   0
    ##   total_missing_values complete_rows total_observations memory_usage
    ## 1                    0           167               1670        25112

``` r
new_df <- data[,2:10]
head(new_df)
```

    ##   child_mort exports health imports income inflation life_expec total_fer  gdpp
    ## 1       90.2    10.0   7.58    44.9   1610      9.44       56.2      5.82   553
    ## 2       16.6    28.0   6.55    48.6   9930      4.49       76.3      1.65  4090
    ## 3       27.3    38.4   4.17    31.4  12900     16.10       76.5      2.89  4460
    ## 4      119.0    62.3   2.85    42.9   5900     22.40       60.1      6.16  3530
    ## 5       10.3    45.5   6.03    58.9  19100      1.44       76.8      2.13 12200
    ## 6       14.5    18.9   8.10    16.0  18700     20.90       75.8      2.37 10300

``` r
standardized_data <- scale(new_df)
head(standardized_data)
```

    ##      child_mort     exports      health     imports      income  inflation
    ## [1,]  1.2876597 -1.13486665  0.27825140 -0.08220771 -0.80582187  0.1568645
    ## [2,] -0.5373329 -0.47822017 -0.09672528  0.07062429 -0.37424335 -0.3114109
    ## [3,] -0.2720146 -0.09882442 -0.96317624 -0.63983800 -0.22018227  0.7869076
    ## [4,]  2.0017872  0.77305618 -1.44372888 -0.16481961 -0.58328920  1.3828944
    ## [5,] -0.6935483  0.16018613 -0.28603389  0.49607554  0.10142673 -0.5999442
    ## [6,] -0.5894047 -0.81019144  0.46756001 -1.27594958  0.08067776  1.2409928
    ##      life_expec   total_fer        gdpp
    ## [1,] -1.6142372  1.89717646 -0.67714308
    ## [2,]  0.6459238 -0.85739418 -0.48416709
    ## [3,]  0.6684130 -0.03828924 -0.46398018
    ## [4,] -1.1756985  2.12176975 -0.51472026
    ## [5,]  0.7021467 -0.54032130 -0.04169175
    ## [6,]  0.5897009 -0.38178486 -0.14535428

# PCA

``` r
res.pca = prcomp(standardized_data, scale = TRUE)
print(summary(res.pca))
```

    ## Importance of components:
    ##                           PC1    PC2    PC3    PC4    PC5     PC6    PC7
    ## Standard deviation     2.0336 1.2435 1.0818 0.9974 0.8128 0.47284 0.3368
    ## Proportion of Variance 0.4595 0.1718 0.1300 0.1105 0.0734 0.02484 0.0126
    ## Cumulative Proportion  0.4595 0.6313 0.7614 0.8719 0.9453 0.97015 0.9828
    ##                            PC8     PC9
    ## Standard deviation     0.29718 0.25860
    ## Proportion of Variance 0.00981 0.00743
    ## Cumulative Proportion  0.99257 1.00000

``` r
fviz_eig(res.pca,addlabels = TRUE)
```

![](Individual-work-sitthatka-for-clustering_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
##cutting in PC4
##show correlation circle
var = get_pca_var(res.pca)

fviz_pca_var(res.pca, col.var = "black")
```

![](Individual-work-sitthatka-for-clustering_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

``` r
corrplot(var$cos2, is.corr = FALSE)
```

![](Individual-work-sitthatka-for-clustering_files/figure-gfm/unnamed-chunk-3-3.png)<!-- -->

``` r
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB","#E7B880","#FC4E07"))
```

![](Individual-work-sitthatka-for-clustering_files/figure-gfm/unnamed-chunk-3-4.png)<!-- -->

``` r
##show Graph of individual
ind <- get_pca_ind(res.pca)
fviz_pca_ind(res.pca, col.ind = "cos2",
             gradient.cols = c("#00AFBB","#E7B880","#FC4E07"))
```

![](Individual-work-sitthatka-for-clustering_files/figure-gfm/unnamed-chunk-3-5.png)<!-- -->

``` r
## to select pca = 4
pca_components <- res.pca$x[, 1:4]
head(pca_components) # with PCA
```

    ##              PC1         PC2        PC3         PC4
    ## [1,] -2.90428986 -0.09533386  0.7159652  1.00224038
    ## [2,]  0.42862224  0.58639208  0.3324855 -1.15757715
    ## [3,] -0.28436983  0.45380957 -1.2178421 -0.86551146
    ## [4,] -2.92362976 -1.69047094 -1.5204709  0.83710739
    ## [5,]  1.03047668 -0.13624894  0.2250441 -0.84452276
    ## [6,]  0.02234007  1.77385167 -0.8673884 -0.03685602

``` r
head(standardized_data) # without PCA
```

    ##      child_mort     exports      health     imports      income  inflation
    ## [1,]  1.2876597 -1.13486665  0.27825140 -0.08220771 -0.80582187  0.1568645
    ## [2,] -0.5373329 -0.47822017 -0.09672528  0.07062429 -0.37424335 -0.3114109
    ## [3,] -0.2720146 -0.09882442 -0.96317624 -0.63983800 -0.22018227  0.7869076
    ## [4,]  2.0017872  0.77305618 -1.44372888 -0.16481961 -0.58328920  1.3828944
    ## [5,] -0.6935483  0.16018613 -0.28603389  0.49607554  0.10142673 -0.5999442
    ## [6,] -0.5894047 -0.81019144  0.46756001 -1.27594958  0.08067776  1.2409928
    ##      life_expec   total_fer        gdpp
    ## [1,] -1.6142372  1.89717646 -0.67714308
    ## [2,]  0.6459238 -0.85739418 -0.48416709
    ## [3,]  0.6684130 -0.03828924 -0.46398018
    ## [4,] -1.1756985  2.12176975 -0.51472026
    ## [5,]  0.7021467 -0.54032130 -0.04169175
    ## [6,]  0.5897009 -0.38178486 -0.14535428

# K-means Euclidean without pca

``` r
sse <-numeric(20)
for (k in 1:20) {
  kmeans_model <- kmeans(standardized_data, centers = k)
  sse[k] <- kmeans_model$tot.withinss
}

ggplot(data.frame(K = 1:20, SSE = sse), aes(x = K, y = SSE)) +
  geom_line() +
  geom_point() +
  labs(x = "จำนวนกลุ่ม (K)", y = "SSE") +
  theme_minimal()
```

![](Individual-work-sitthatka-for-clustering_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
## เลือก k=8

kmeans_result <- kmeans(standardized_data,center = 8)
kmeans_result
```

    ## K-means clustering with 8 clusters of sizes 32, 3, 35, 34, 25, 30, 7, 1
    ## 
    ## Cluster means:
    ##   child_mort     exports       health     imports     income  inflation
    ## 1  1.6501476 -0.51467773  0.046962690 -0.05406804 -0.7456612  0.1087805
    ## 2 -0.8464575  4.92087313 -0.008138555  4.53442030  2.4322274 -0.5026943
    ## 3  0.2125719 -0.33305022 -0.723107957 -0.47866703 -0.5166405  0.4896981
    ## 4 -0.5722662 -0.36738425  0.143229814 -0.31510036 -0.1681573 -0.1899793
    ## 5 -0.8442093  0.03440185  1.263966791 -0.28444163  1.1621139 -0.6161172
    ## 6 -0.4877407  0.64428051 -0.099880422  0.87636563 -0.2077156 -0.3162734
    ## 7 -0.6836298  0.89187793 -1.286665139 -0.33830459  2.5950080  0.4479655
    ## 8  2.2745443 -0.57671714 -0.635526719 -1.21812126 -0.6221935  9.1023425
    ##   life_expec  total_fer       gdpp
    ## 1 -1.4315127  1.6131320 -0.6304711
    ## 2  1.2231457 -1.0357477  2.4334786
    ## 3 -0.4300221  0.2716118 -0.5293764
    ## 4  0.5159497 -0.6949720 -0.2507178
    ## 5  1.1514802 -0.7678211  1.6906729
    ## 6  0.2324980 -0.4683193 -0.3271711
    ## 7  0.7166040 -0.4365177  1.1508171
    ## 8 -1.1307201  1.9103878 -0.5801913
    ## 
    ## Clustering vector:
    ##   [1] 1 4 3 1 6 4 4 5 5 3 4 7 3 4 6 5 6 1 6 3 4 3 4 7 6 1 1 6 1 5 6 1 1 4 4 4 1
    ##  [38] 1 3 4 1 4 5 6 5 4 4 3 4 1 3 6 6 5 5 3 1 4 5 3 5 4 3 1 1 6 1 6 5 3 3 4 3 5
    ##  [75] 5 5 4 5 6 3 3 1 7 6 3 6 4 1 1 6 6 2 4 3 1 6 6 1 2 1 6 6 6 3 4 4 1 3 3 3 5
    ## [112] 5 1 8 5 7 3 6 6 4 3 4 5 7 4 4 1 3 7 1 4 6 1 2 6 5 6 3 4 5 3 4 3 4 5 5 3 1
    ## [149] 6 1 1 3 6 4 3 1 4 7 5 5 4 3 3 3 6 3 1
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1] 128.55086  20.87409 104.57119  48.13778  64.69144  66.92968  25.38483
    ## [8]   0.00000
    ##  (between_SS / total_SS =  69.3 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    ## [6] "betweenss"    "size"         "iter"         "ifault"

``` r
autoplot(kmeans_result,standardized_data,frame=TRUE)
```

![](Individual-work-sitthatka-for-clustering_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

# K-means Euclidean with pca

``` r
sse <-numeric(20)
for (k in 1:20) {
  kmeans_model <- kmeans(pca_components, centers = k)
  sse[k] <- kmeans_model$tot.withinss
}

ggplot(data.frame(K = 1:20, SSE = sse), aes(x = K, y = SSE)) +
  geom_line() +
  geom_point() +
  labs(x = "จำนวนกลุ่ม (K)", y = "SSE") +
  theme_minimal()
```

![](Individual-work-sitthatka-for-clustering_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
## เลือก k=8

kmeans_result_pca <- kmeans(pca_components,center = 8)
kmeans_result_pca
```

    ## K-means clustering with 8 clusters of sizes 61, 1, 10, 24, 29, 15, 24, 3
    ## 
    ## Cluster means:
    ##          PC1          PC2        PC3        PC4
    ## 1  0.6536619  0.007002587  0.3204757 -0.7958660
    ## 2 -4.8973373  0.094215330 -6.0918369  2.4051877
    ## 3 -1.2759358 -0.570866841  1.5872780  0.5838040
    ## 4  2.6525335  1.122470532  0.2012760  1.1867057
    ## 5 -1.4269974  0.049491512 -0.4869747 -0.4357251
    ## 6  0.6729485  0.331783925 -1.6906933 -0.2478379
    ## 7 -2.9550143 -0.496477422  0.2481299  1.0611347
    ## 8  5.4438521 -5.416184028 -0.2110133  0.9033408
    ## 
    ## Clustering vector:
    ##   [1] 7 1 6 7 1 6 1 4 4 6 1 6 5 1 1 4 1 7 1 5 1 3 1 6 1 7 7 1 7 4 1 7 7 1 1 1 5
    ##  [38] 7 5 1 7 1 1 1 4 1 1 5 1 7 5 1 1 4 4 5 7 1 4 5 4 1 5 7 7 1 7 1 4 5 5 6 5 4
    ##  [75] 4 4 1 4 1 6 5 3 6 1 5 1 1 3 3 6 1 8 1 5 7 1 1 7 8 5 1 3 1 5 1 1 7 5 3 5 4
    ## [112] 4 7 2 4 6 5 1 1 1 5 1 4 4 1 6 3 1 6 5 1 1 7 8 1 1 3 3 1 4 6 1 5 1 4 4 5 7
    ## [149] 1 7 3 5 1 1 5 7 1 6 4 4 1 5 5 6 1 5 7
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1] 103.49119   0.00000  17.04890  69.62318  44.87684  39.41904  43.49784
    ## [8]  17.57130
    ##  (between_SS / total_SS =  74.2 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    ## [6] "betweenss"    "size"         "iter"         "ifault"

``` r
autoplot(kmeans_result_pca,pca_components,frame=TRUE)
```

![](Individual-work-sitthatka-for-clustering_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

# K-means Manhattan without pca

``` r
sse <-numeric(20)
for (k in 1:20) {
  kmeans_model <- kmeans(standardized_data, centers = k,algorithm = "Lloyd")
  sse[k] <- kmeans_model$tot.withinss
}
```

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

``` r
ggplot(data.frame(K = 1:20, SSE = sse), aes(x = K, y = SSE)) +
  geom_line() +
  geom_point() +
  labs(x = "จำนวนกลุ่ม (K)", y = "SSE") +
  theme_minimal()
```

![](Individual-work-sitthatka-for-clustering_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
## เลือก k=10

kmeans_result_man <- kmeans(standardized_data,center = 10,algorithm = "Lloyd")
kmeans_result_man
```

    ## K-means clustering with 10 clusters of sizes 21, 30, 10, 15, 5, 26, 15, 21, 20, 4
    ## 
    ## Cluster means:
    ##    child_mort    exports      health    imports     income   inflation
    ## 1  -0.8425610 -0.1839453  1.36590913 -0.5235913  1.1838312 -0.59182201
    ## 2  -0.3544203  0.2290124 -0.46320734  0.4259931 -0.4286402 -0.25364432
    ## 3   0.4465762 -0.3476205  1.27467001  0.8868298 -0.6354210 -0.22258048
    ## 4  -0.4052523 -0.1318999 -0.77192600 -0.7433782  0.0920897  1.13819298
    ## 5  -0.7421486  1.7769957 -1.11899180  0.4407256  3.5727290  0.22876127
    ## 6  -0.5847315 -0.3802844  0.31311667 -0.3001760 -0.1568780 -0.34740660
    ## 7  -0.7791774  1.7388129  0.08675845  1.5625951  0.7270081 -0.59703047
    ## 8   0.7018815 -0.6733397 -0.70175006 -0.5287724 -0.6923695  0.39458345
    ## 9   1.9543027 -0.5961977 -0.02755482 -0.2835742 -0.8105864  0.02108825
    ## 10  1.6788181  0.8578730 -1.12882128 -0.1410687 -0.2417093  3.33167668
    ##    life_expec  total_fer       gdpp
    ## 1   1.1696000 -0.7601962  1.7647383
    ## 2   0.1252997 -0.2992138 -0.4638965
    ## 3  -1.1936898  0.3455011 -0.5537301
    ## 4   0.2073851 -0.3311413 -0.2575644
    ## 5   0.8955535 -0.6763984  2.3927410
    ## 6   0.5987831 -0.6853923 -0.2212965
    ## 7   0.7711135 -0.8058698  0.5284522
    ## 8  -0.5668847  0.8415251 -0.6198871
    ## 9  -1.7334297  1.7610994 -0.6716953
    ## 10 -1.1335313  1.7122173 -0.3567713
    ## 
    ## Clustering vector:
    ##   [1]  9  6  4 10  2  4  2  1  1  4  6  7  8  6  2  7  2  9  2  2  6  3  6  5  2
    ##  [26]  9  9  2  9  1  2  9  9  6  6  6  8  9 10  6  9  6  7  7  1  6  6  4  6 10
    ##  [51]  8  7  2  1  1  8  8  6  1  8  1  6  6  9  9  2  9  7  1  8  4  4  8  7  1
    ##  [76]  1  2  1  2  4  8  3  5  2  8  2  6  3  3  4  7  5  6  8  9  7  2  9  7  8
    ## [101]  2  3  3  4  6  2  9  8  3  8  1  1  9 10  1  4  8  7  2  6  2  6  1  5  6
    ## [126]  4  3  2  4  8  6  7  9  7  7  7  3  3  6  1  4  2  8  2  1  1  8  8  2  8
    ## [151]  9  2  2  6  2  9  2  5  1  1  6  4  2  4  2  8  9
    ## 
    ## Within cluster sum of squares by cluster:
    ##  [1] 42.21176 43.86835 31.81076 35.16546 51.28281 31.18914 86.11069 47.01038
    ##  [9] 54.00217 54.15482
    ##  (between_SS / total_SS =  68.1 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    ## [6] "betweenss"    "size"         "iter"         "ifault"

``` r
autoplot(kmeans_result_man,standardized_data,frame=TRUE)
```

![](Individual-work-sitthatka-for-clustering_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

# K-means manhattan with pca

``` r
sse <-numeric(20)
for (k in 1:20) {
  kmeans_model <- kmeans(pca_components, centers = k,algorithm = "Lloyd")
  sse[k] <- kmeans_model$tot.withinss
}
```

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

``` r
ggplot(data.frame(K = 1:20, SSE = sse), aes(x = K, y = SSE)) +
  geom_line() +
  geom_point() +
  labs(x = "จำนวนกลุ่ม (K)", y = "SSE") +
  theme_minimal()
```

![](Individual-work-sitthatka-for-clustering_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
## เลือก k=10

kmeans_result_pca_man <- kmeans(pca_components,center = 10,algorithm = "Lloyd")
```

    ## Warning: did not converge in 10 iterations

``` r
kmeans_result_pca_man
```

    ## K-means clustering with 10 clusters of sizes 6, 19, 9, 24, 19, 22, 3, 26, 14, 25
    ## 
    ## Cluster means:
    ##            PC1        PC2         PC3        PC4
    ## 1  -1.32945640 -0.9677584  2.06328581  0.6870217
    ## 2   2.23068792  1.1338178 -0.16409590  0.6878064
    ## 3   3.41961190  0.4609668  0.02374926  1.8731352
    ## 4   0.64390329 -0.9676554  0.17556666 -0.9220355
    ## 5  -1.72789605 -0.2612594  0.10099715  0.0870188
    ## 6  -3.10908896 -0.5179946 -0.03970331  1.2202136
    ## 7   5.44385209 -5.4161840 -0.21101332  0.9033408
    ## 8   0.90831016  0.5725556  0.55044531 -0.6328016
    ## 9  -1.31627142  0.6191887 -1.14992683 -0.6428502
    ## 10 -0.03705426  0.4956926 -0.49257086 -0.7070060
    ## 
    ## Clustering vector:
    ##   [1]  6  8 10  6  8 10 10  2  3 10  8  4  9  8  4  3  4  5  4 10  8  5  8  2  8
    ##  [26]  6  6  4  6  2  4  6  6  8 10  8  5  6  5  8  6  8  2  8  3 10 10  9  8  6
    ##  [51]  9  4  4  2  2  9  5  8  2  5  2  8 10  6  6  4  6  4  2  9  9 10  5  3  2
    ##  [76]  2 10  2  4 10  5  1  2  4  5  8  8  1  1 10  4  7  8  5  6  4  4  6  7  5
    ## [101]  4  1  8  9  8 10  6  9  5  9  3  2  6  6  3 10  9  4  4 10 10  8  2  3  8
    ## [126] 10  5 10 10  5  8  4  6  7  4  8  1  5  8  2  9 10  5 10  2  3  5  5  4  6
    ## [151]  1 10  4 10  4  6 10  2  2  3  8  9  5  9  4  9  6
    ## 
    ## Within cluster sum of squares by cluster:
    ##  [1]  7.554108 41.924299 39.022554 29.556328 26.175159 83.695345 17.571296
    ##  [8] 25.499426 19.028261 31.095887
    ##  (between_SS / total_SS =  75.3 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    ## [6] "betweenss"    "size"         "iter"         "ifault"      
    ## Warning: did *not* converge in specified number of iterations

``` r
autoplot(kmeans_result_pca_man,pca_components,frame=TRUE)
```

![](Individual-work-sitthatka-for-clustering_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

# Hierarchical euclidean without pca

``` r
test = (standardized_data[1:50,])
head(test)
```

    ##      child_mort     exports      health     imports      income  inflation
    ## [1,]  1.2876597 -1.13486665  0.27825140 -0.08220771 -0.80582187  0.1568645
    ## [2,] -0.5373329 -0.47822017 -0.09672528  0.07062429 -0.37424335 -0.3114109
    ## [3,] -0.2720146 -0.09882442 -0.96317624 -0.63983800 -0.22018227  0.7869076
    ## [4,]  2.0017872  0.77305618 -1.44372888 -0.16481961 -0.58328920  1.3828944
    ## [5,] -0.6935483  0.16018613 -0.28603389  0.49607554  0.10142673 -0.5999442
    ## [6,] -0.5894047 -0.81019144  0.46756001 -1.27594958  0.08067776  1.2409928
    ##      life_expec   total_fer        gdpp
    ## [1,] -1.6142372  1.89717646 -0.67714308
    ## [2,]  0.6459238 -0.85739418 -0.48416709
    ## [3,]  0.6684130 -0.03828924 -0.46398018
    ## [4,] -1.1756985  2.12176975 -0.51472026
    ## [5,]  0.7021467 -0.54032130 -0.04169175
    ## [6,]  0.5897009 -0.38178486 -0.14535428

``` r
distance_mat <- dist(test,method = 'euclidean')
head(distance_mat)
```

    ## [1] 4.130922 3.885865 2.990652 4.400212 4.169492 3.993478

``` r
Hierar_cl <- hclust(distance_mat)
Hierar_cl
```

    ## 
    ## Call:
    ## hclust(d = distance_mat)
    ## 
    ## Cluster method   : complete 
    ## Distance         : euclidean 
    ## Number of objects: 50

``` r
plot(Hierar_cl)

plot(Hierar_cl)
abline(h=5,col = "green")
```

![](Individual-work-sitthatka-for-clustering_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
fit<- cutree(Hierar_cl,k=3)
fit
```

    ##  [1] 1 2 2 1 2 2 2 3 3 2 2 3 2 2 2 3 2 1 2 2 2 2 2 3 2 1 1 2 1 3 2 1 1 2 2 2 1 1
    ## [39] 1 2 1 2 3 3 3 2 2 2 2 1

``` r
plot(Hierar_cl)
table(fit)
```

    ## fit
    ##  1  2  3 
    ## 13 28  9

``` r
rect.hclust(Hierar_cl,k=3,border = "green")
```

![](Individual-work-sitthatka-for-clustering_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

# Hierarchical euclidean with pca

``` r
test_pca = (pca_components[1:50,])
head(test_pca)
```

    ##              PC1         PC2        PC3         PC4
    ## [1,] -2.90428986 -0.09533386  0.7159652  1.00224038
    ## [2,]  0.42862224  0.58639208  0.3324855 -1.15757715
    ## [3,] -0.28436983  0.45380957 -1.2178421 -0.86551146
    ## [4,] -2.92362976 -1.69047094 -1.5204709  0.83710739
    ## [5,]  1.03047668 -0.13624894  0.2250441 -0.84452276
    ## [6,]  0.02234007  1.77385167 -0.8673884 -0.03685602

``` r
distance_mat_pca <- dist(test_pca,method = 'euclidean')
head(distance_mat_pca)
```

    ## [1] 4.047829 3.793896 2.752045 4.374426 3.955471 3.909383

``` r
Hierar_cl_pca <- hclust(distance_mat_pca)
Hierar_cl_pca
```

    ## 
    ## Call:
    ## hclust(d = distance_mat_pca)
    ## 
    ## Cluster method   : complete 
    ## Distance         : euclidean 
    ## Number of objects: 50

``` r
plot(Hierar_cl_pca)

plot(Hierar_cl_pca)
abline(h=5,col = "green")
```

![](Individual-work-sitthatka-for-clustering_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
fit<- cutree(Hierar_cl_pca,k=3)
fit
```

    ##  [1] 1 2 2 1 2 2 2 3 3 2 2 3 2 2 2 3 2 1 2 2 2 1 2 3 2 1 1 2 1 3 2 1 1 2 2 2 1 1
    ## [39] 1 2 1 2 3 3 3 2 2 2 2 1

``` r
plot(Hierar_cl_pca)
table(fit)
```

    ## fit
    ##  1  2  3 
    ## 14 27  9

``` r
rect.hclust(Hierar_cl_pca,k=3,border = "green")
```

![](Individual-work-sitthatka-for-clustering_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

# Hierarchical Manhattan without pca

``` r
distance_mat_man <- dist(test,method = 'manhattan')
head(distance_mat_man)
```

    ## [1]  9.117009 10.041736  6.700760 11.472220 10.570106  8.574499

``` r
Hierar_cl_man <- hclust(distance_mat_man)
Hierar_cl_man
```

    ## 
    ## Call:
    ## hclust(d = distance_mat_man)
    ## 
    ## Cluster method   : complete 
    ## Distance         : manhattan 
    ## Number of objects: 50

``` r
plot(Hierar_cl_man)

plot(Hierar_cl_man)
abline(h=13,col = "green")
```

![](Individual-work-sitthatka-for-clustering_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
fit<- cutree(Hierar_cl_man,k=3)
fit
```

    ##  [1] 1 2 2 1 2 2 2 3 3 2 2 3 2 2 2 3 2 1 2 2 2 2 2 3 2 1 1 2 1 3 2 1 1 2 2 2 1 1
    ## [39] 1 2 1 2 3 3 3 2 2 2 2 1

``` r
plot(Hierar_cl_man)
table(fit)
```

    ## fit
    ##  1  2  3 
    ## 13 28  9

``` r
rect.hclust(Hierar_cl_man,k=3,border = "green")
```

![](Individual-work-sitthatka-for-clustering_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

# Hierarchical manhattan with pca

``` r
distance_mat_pca_man <- dist(test_pca,method = 'manhattan')
head(distance_mat_pca_man)
```

    ## [1] 6.557935 6.970623 4.016046 6.313366 7.418265 6.886333

``` r
Hierar_cl_pca_man <- hclust(distance_mat_pca_man)
Hierar_cl_pca_man
```

    ## 
    ## Call:
    ## hclust(d = distance_mat_pca_man)
    ## 
    ## Cluster method   : complete 
    ## Distance         : manhattan 
    ## Number of objects: 50

``` r
plot(Hierar_cl_pca_man)

plot(Hierar_cl_pca_man)
abline(h=9,col = "green")
```

![](Individual-work-sitthatka-for-clustering_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
fit<- cutree(Hierar_cl_pca_man,k=3)
fit
```

    ##  [1] 1 2 2 1 2 2 2 3 3 2 2 2 2 2 2 3 2 1 2 2 2 2 2 3 2 1 1 2 1 3 2 1 1 2 2 2 2 1
    ## [39] 1 2 1 2 2 2 3 2 2 2 2 1

``` r
plot(Hierar_cl_pca_man)
table(fit)
```

    ## fit
    ##  1  2  3 
    ## 12 32  6

``` r
rect.hclust(Hierar_cl_pca_man,k=3,border = "green")
```

![](Individual-work-sitthatka-for-clustering_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

# DBScan euclidean without pca

``` r
dist_matrix <- proxy::dist(standardized_data,method = "Euclidean")

Db_cl <- dbscan::dbscan(dist_matrix,eps = 1, minPts = 2)
Db_cl 
```

    ## DBSCAN clustering for 167 objects.
    ## Parameters: eps = 1, minPts = 2
    ## Using Euclidean distances and borderpoints = TRUE
    ## The clustering contains 12 cluster(s) and 60 noise points.
    ## 
    ##  0  1  2  3  4  5  6  7  8  9 10 11 12 
    ## 60  8 59 16  2  2  4  2  3  5  2  2  2 
    ## 
    ## Available fields: cluster, eps, minPts, dist, borderPoints

``` r
Db_cl$cluster
```

    ##   [1]  1  2  0  0  2  0  2  3  3  0  2  0  4  2  0  5  2  6  2  2  2  0  2  7  2
    ##  [26]  6  0  2  6  3  2  0  8  2  2  2  9  0  0  2  1  2  2  2  3  2  2  2  2  0
    ##  [51]  0  2  2  3  3  0  9  2  3  0  3  2  2  1  6  2  0  2  3  4  0  2  0  0  0
    ##  [76]  3  2  3  0  0  9  0  7  2  0  2  2  0  0  0  2  0  2  9  1  0  2  8  0  0
    ## [101]  2  0  0  0  2  2  1  0  0 10  5  3  8  0  0 11  0  2  2  2  2  2  3  0  2
    ## [126]  2  0 12 11  9  2  0  0  0  2  2  0  0  2  3  0  2  0  2  3  0  0  1  0  0
    ## [151]  0 12  2  2  0  1  0  0  3  0  2 10  2  0  0  0  1

``` r
plot(standardized_data, col = Db_cl$cluster)
```

![](Individual-work-sitthatka-for-clustering_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

# DBScan euclidean with pca

``` r
dist_matrix_pca <- proxy::dist(pca_components,method = "Euclidean")

Db_cl_pca <- dbscan::dbscan(dist_matrix_pca,eps = 1, minPts = 2)
Db_cl_pca 
```

    ## DBSCAN clustering for 167 objects.
    ## Parameters: eps = 1, minPts = 2
    ## Using Euclidean distances and borderpoints = TRUE
    ## The clustering contains 7 cluster(s) and 21 noise points.
    ## 
    ##   0   1   2   3   4   5   6   7 
    ##  21 133   2   2   2   2   2   3 
    ## 
    ## Available fields: cluster, eps, minPts, dist, borderPoints

``` r
Db_cl_pca$cluster
```

    ##   [1] 1 1 1 0 1 1 1 1 1 1 1 0 1 1 1 2 1 1 1 1 1 1 1 3 1 1 1 1 1 1 1 1 1 1 1 1 1
    ##  [38] 1 0 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 4 0
    ##  [75] 1 1 1 1 1 1 1 5 3 1 1 1 1 6 6 7 1 0 1 1 1 1 1 1 0 0 1 5 1 1 1 1 1 0 1 1 2
    ## [112] 1 1 0 0 7 1 1 1 1 1 1 1 0 1 1 0 1 7 1 1 0 1 0 1 1 1 4 1 1 1 1 1 1 1 0 1 1
    ## [149] 1 1 0 1 1 1 1 1 1 0 1 0 1 1 1 0 1 1 1

``` r
plot(standardized_data, col = Db_cl_pca$cluster)
```

![](Individual-work-sitthatka-for-clustering_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

# DBScan Manhatton without pca

``` r
dist_matrix_man <- proxy::dist(standardized_data,method = "Manhattan")

Db_cl_man <- dbscan::dbscan(dist_matrix_man,eps = 1, minPts = 2)
Db_cl_man 
```

    ## DBSCAN clustering for 167 objects.
    ## Parameters: eps = 1, minPts = 2
    ## Using Manhattan distances and borderpoints = TRUE
    ## The clustering contains 3 cluster(s) and 161 noise points.
    ## 
    ##   0   1   2   3 
    ## 161   2   2   2 
    ## 
    ## Available fields: cluster, eps, minPts, dist, borderPoints

``` r
Db_cl_man$cluster
```

    ##   [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0
    ##  [38] 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    ##  [75] 0 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0
    ## [112] 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0 0
    ## [149] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

``` r
plot(standardized_data, col = Db_cl_man$cluster)
```

![](Individual-work-sitthatka-for-clustering_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

# DBScan Manhatton with pca

``` r
dist_matrix_pca_man <- proxy::dist(pca_components,method = "Manhattan")

Db_cl_pca_man <- dbscan::dbscan(dist_matrix_pca_man,eps = 1, minPts = 2)
Db_cl_pca_man 
```

    ## DBSCAN clustering for 167 objects.
    ## Parameters: eps = 1, minPts = 2
    ## Using Manhattan distances and borderpoints = TRUE
    ## The clustering contains 16 cluster(s) and 66 noise points.
    ## 
    ##  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 
    ## 66 18 44  3  5  3  2  2  4  2  2  2  2  2  6  2  2 
    ## 
    ## Available fields: cluster, eps, minPts, dist, borderPoints

``` r
Db_cl_pca_man$cluster
```

    ##   [1]  1  2  3  0  2  0  2  0  4  3  2  0  2  2  0  0  2  1  2  2  5  0  0  6  2
    ##  [26]  1  0  2  1  7  2  8  8  0  9  0  1  0  0  0  1  2 10 11  4 12 12  2  2  0
    ##  [51] 13  2  2  4  7  0  1  5  0  1 14  2  0  1  1  2  0  2  4  2  0 15  0  0  0
    ##  [76] 14  2  0  0  3  1  0  6  2  0  2  2  0  0  0  2  0  2  1  1  0  2  8  0  0
    ## [101]  2  0  0  0  2  2  1  0  0  2  0 14  8  0  0  0 13  0  2  9  2  2 14  0  2
    ## [126] 15  0  2  0  1  5  0  0  0 11  0  0  0 10 14  0  2  0  2  4  0  0  1  0  0
    ## [151]  0  2  2 16  0  1  2  0 14  0 16  2  2  0  0  0  1

``` r
plot(pca_components, col = Db_cl_pca_man$cluster)
```

![](Individual-work-sitthatka-for-clustering_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->
