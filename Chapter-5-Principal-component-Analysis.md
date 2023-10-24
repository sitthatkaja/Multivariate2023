Principal component Analysis PCA
================
Sitthatka Jarutsang, Student M.SC.

# เริ่มต้น PCA

ติดตั้ง package and เรียกใช้

``` r
library(ggplot2)
library(factoextra)
```

    ## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa

ทำการเรียกข้อมูล

``` r
data(decathlon2)
head(decathlon2)
```

    ##           X100m Long.jump Shot.put High.jump X400m X110m.hurdle Discus
    ## SEBRLE    11.04      7.58    14.83      2.07 49.81        14.69  43.75
    ## CLAY      10.76      7.40    14.26      1.86 49.37        14.05  50.72
    ## BERNARD   11.02      7.23    14.25      1.92 48.93        14.99  40.87
    ## YURKOV    11.34      7.09    15.19      2.10 50.42        15.31  46.26
    ## ZSIVOCZKY 11.13      7.30    13.48      2.01 48.62        14.17  45.67
    ## McMULLEN  10.83      7.31    13.76      2.13 49.91        14.38  44.41
    ##           Pole.vault Javeline X1500m Rank Points Competition
    ## SEBRLE          5.02    63.19  291.7    1   8217    Decastar
    ## CLAY            4.92    60.15  301.5    2   8122    Decastar
    ## BERNARD         5.32    62.77  280.1    4   8067    Decastar
    ## YURKOV          4.72    63.44  276.4    5   8036    Decastar
    ## ZSIVOCZKY       4.42    55.37  268.0    7   8004    Decastar
    ## McMULLEN        4.42    56.37  285.1    8   7995    Decastar

เลือกข้อมูลสำหรับการใช้

``` r
decathlon2.active = decathlon2[1:23,1:10]
head(decathlon2.active[,1:6],4)
```

    ##         X100m Long.jump Shot.put High.jump X400m X110m.hurdle
    ## SEBRLE  11.04      7.58    14.83      2.07 49.81        14.69
    ## CLAY    10.76      7.40    14.26      1.86 49.37        14.05
    ## BERNARD 11.02      7.23    14.25      1.92 48.93        14.99
    ## YURKOV  11.34      7.09    15.19      2.10 50.42        15.31

Explore data

``` r
pairs(decathlon2.active[,1:10],pch =19,lower.panel =NULL)
```

![](Chapter-5-Principal-component-Analysis_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Conducting PCA by prcomp และทำ Standardization

``` r
res.pca = prcomp(decathlon2.active, scale =TRUE)
```

เราสามารถตัดสินใจเลือกข้อมูลได้เมือค่า Cumulative รวมกันมากกว่า 80%

``` r
get_eigenvalue(res.pca)
```

    ##        eigenvalue variance.percent cumulative.variance.percent
    ## Dim.1   4.1242133        41.242133                    41.24213
    ## Dim.2   1.8385309        18.385309                    59.62744
    ## Dim.3   1.2391403        12.391403                    72.01885
    ## Dim.4   0.8194402         8.194402                    80.21325
    ## Dim.5   0.7015528         7.015528                    87.22878
    ## Dim.6   0.4228828         4.228828                    91.45760
    ## Dim.7   0.3025817         3.025817                    94.48342
    ## Dim.8   0.2744700         2.744700                    97.22812
    ## Dim.9   0.1552169         1.552169                    98.78029
    ## Dim.10  0.1219710         1.219710                   100.00000

ภายใต้การตัดสินใจควรตัดที่ PC4

Scree plot variance percent

``` r
fviz_eig(res.pca, addlabels =TRUE, ylim = c(0,50))
```

![](Chapter-5-Principal-component-Analysis_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

# Graph of variables

สำหรับดูตัวแปรว่ามีความสัมพันธ์แบบใดมากน้อยเพียงใดด้วยวิธีการ Correlation Circle
เริ่มจากเก็บค่าตัวแปรก่อน

``` r
var = get_pca_var(res.pca)
var$coord
```

    ##                     Dim.1       Dim.2       Dim.3       Dim.4      Dim.5
    ## X100m        -0.850625692 -0.17939806 -0.30155643  0.03357320 -0.1944440
    ## Long.jump     0.794180641  0.28085695  0.19054653 -0.11538956  0.2331567
    ## Shot.put      0.733912733  0.08540412 -0.51759781  0.12846837 -0.2488129
    ## High.jump     0.610083985 -0.46521415 -0.33008517  0.14455012  0.4027002
    ## X400m        -0.701603377  0.29017826 -0.28353292  0.43082552  0.1039085
    ## X110m.hurdle -0.764125197 -0.02474081 -0.44888733 -0.01689589  0.2242200
    ## Discus        0.743209016  0.04966086 -0.17652518  0.39500915 -0.4082391
    ## Pole.vault   -0.217268042  0.80745110 -0.09405773 -0.33898477 -0.2216853
    ## Javeline      0.428226639  0.38610928 -0.60412432 -0.33173454  0.1978128
    ## X1500m        0.004278487  0.78448019  0.21947068  0.44800961  0.2632527
    ##                     Dim.6        Dim.7        Dim.8       Dim.9       Dim.10
    ## X100m        -0.035374780 -0.091336386 -0.104716925 -0.30306448  0.044417974
    ## Long.jump     0.033727883 -0.154330810 -0.397380703 -0.05158951  0.029719453
    ## Shot.put      0.239789034 -0.009886612  0.024359049  0.04778655  0.217451948
    ## High.jump     0.284644846  0.028157465  0.084405578 -0.11213822 -0.133566774
    ## X400m         0.049289996  0.286106008 -0.233552216  0.08216041 -0.034170673
    ## X110m.hurdle -0.002632395 -0.370072158 -0.008344682  0.16176025 -0.015629914
    ## Discus       -0.198544870 -0.142725641 -0.039559255  0.01336209 -0.172590426
    ## Pole.vault    0.327464549 -0.010393176  0.032914942 -0.02576874 -0.137211339
    ## Javeline     -0.362097598  0.133564318  0.052841099 -0.04045397 -0.003854347
    ## X1500m       -0.042050151 -0.111367083  0.194469730 -0.10224014  0.062834809

``` r
var$cor
```

    ##                     Dim.1       Dim.2       Dim.3       Dim.4      Dim.5
    ## X100m        -0.850625692 -0.17939806 -0.30155643  0.03357320 -0.1944440
    ## Long.jump     0.794180641  0.28085695  0.19054653 -0.11538956  0.2331567
    ## Shot.put      0.733912733  0.08540412 -0.51759781  0.12846837 -0.2488129
    ## High.jump     0.610083985 -0.46521415 -0.33008517  0.14455012  0.4027002
    ## X400m        -0.701603377  0.29017826 -0.28353292  0.43082552  0.1039085
    ## X110m.hurdle -0.764125197 -0.02474081 -0.44888733 -0.01689589  0.2242200
    ## Discus        0.743209016  0.04966086 -0.17652518  0.39500915 -0.4082391
    ## Pole.vault   -0.217268042  0.80745110 -0.09405773 -0.33898477 -0.2216853
    ## Javeline      0.428226639  0.38610928 -0.60412432 -0.33173454  0.1978128
    ## X1500m        0.004278487  0.78448019  0.21947068  0.44800961  0.2632527
    ##                     Dim.6        Dim.7        Dim.8       Dim.9       Dim.10
    ## X100m        -0.035374780 -0.091336386 -0.104716925 -0.30306448  0.044417974
    ## Long.jump     0.033727883 -0.154330810 -0.397380703 -0.05158951  0.029719453
    ## Shot.put      0.239789034 -0.009886612  0.024359049  0.04778655  0.217451948
    ## High.jump     0.284644846  0.028157465  0.084405578 -0.11213822 -0.133566774
    ## X400m         0.049289996  0.286106008 -0.233552216  0.08216041 -0.034170673
    ## X110m.hurdle -0.002632395 -0.370072158 -0.008344682  0.16176025 -0.015629914
    ## Discus       -0.198544870 -0.142725641 -0.039559255  0.01336209 -0.172590426
    ## Pole.vault    0.327464549 -0.010393176  0.032914942 -0.02576874 -0.137211339
    ## Javeline     -0.362097598  0.133564318  0.052841099 -0.04045397 -0.003854347
    ## X1500m       -0.042050151 -0.111367083  0.194469730 -0.10224014  0.062834809

``` r
var$cor2
```

    ## NULL

# Correlation Circle

การแสดงกราฟว่าแตละตัวแปรและ PC มีความสัมพันธ์มากน้อยเพียงใด

``` r
fviz_pca_var(res.pca, col.var ="black")
```

![](Chapter-5-Principal-component-Analysis_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
library("corrplot")
```

    ## corrplot 0.92 loaded

``` r
corrplot(var$cos2, is.corr=FALSE)
```

![](Chapter-5-Principal-component-Analysis_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
fviz_pca_var(res.pca, col.var = "cos2",gradient.cols = c("#00AFBB","#E7B800","#FC4E07"),repel =TRUE)
```

![](Chapter-5-Principal-component-Analysis_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
var$contrib
```

    ##                     Dim.1      Dim.2      Dim.3       Dim.4     Dim.5
    ## X100m        1.754429e+01  1.7505098  7.3386590  0.13755240  5.389252
    ## Long.jump    1.529317e+01  4.2904162  2.9300944  1.62485936  7.748815
    ## Shot.put     1.306014e+01  0.3967224 21.6204325  2.01407269  8.824401
    ## High.jump    9.024811e+00 11.7715838  8.7928883  2.54987951 23.115504
    ## X400m        1.193554e+01  4.5799296  6.4876363 22.65090599  1.539012
    ## X110m.hurdle 1.415754e+01  0.0332933 16.2612611  0.03483735  7.166193
    ## Discus       1.339309e+01  0.1341398  2.5147385 19.04132022 23.755756
    ## Pole.vault   1.144592e+00 35.4618611  0.7139512 14.02307063  7.005084
    ## Javeline     4.446377e+00  8.1086683 29.4531777 13.42963254  5.577615
    ## X1500m       4.438531e-04 33.4728757  3.8871610 24.49386930  9.878367
    ##                     Dim.6       Dim.7       Dim.8      Dim.9      Dim.10
    ## X100m         0.295915322  2.75705260  3.99520353 59.1740009  1.61756139
    ## Long.jump     0.269003613  7.87159392 57.53322220  1.7146826  0.72414393
    ## Shot.put     13.596858744  0.03230371  0.21618512  1.4712015 38.76768578
    ## High.jump    19.159607001  0.26202607  2.59565787  8.1015517 14.62649091
    ## X400m         0.574509906 27.05274658 19.87344405  4.3489667  0.95730504
    ## X110m.hurdle  0.001638634 45.26163460  0.02537025 16.8579392  0.20028870
    ## Discus        9.321746508  6.73226823  0.57016606  0.1150295 24.42174410
    ## Pole.vault   25.357622290  0.03569883  0.39472201  0.4278065 15.43559151
    ## Javeline     31.004964393  5.89573984  1.01729950  1.0543458  0.01217993
    ## X1500m        0.418133591  4.09893563 13.77872941  6.7344755  3.23700871

``` r
fviz_contrib(res.pca, choice ="var", axes =1, top =10)
```

![](Chapter-5-Principal-component-Analysis_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
fviz_contrib(res.pca, choice ="var", axes =2, top =10)
```

![](Chapter-5-Principal-component-Analysis_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

------------------------------------------------------------------------

# Graph of individuals

กราฟแสดงว่าแต่ละข้อมูลมีความสัมพันธ์กันมากน้อยเพียงใด โดยเทียบ Correlation Circle

``` r
ind <- get_pca_ind(res.pca)
ind
```

    ## Principal Component Analysis Results for individuals
    ##  ===================================================
    ##   Name       Description                       
    ## 1 "$coord"   "Coordinates for the individuals" 
    ## 2 "$cos2"    "Cos2 for the individuals"        
    ## 3 "$contrib" "contributions of the individuals"

``` r
fviz_pca_ind(res.pca, col.ind ="cos2",gradient.cols =c("#00AFBB","#E7B800","#FC4E07"),repel=TRUE)
```

![](Chapter-5-Principal-component-Analysis_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

# Biplot

``` r
fviz_pca_biplot(res.pca, repel=TRUE,col.var ="#2E9FDF",col.ind ="#696969")
```

![](Chapter-5-Principal-component-Analysis_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

# Transform data

สำหรับการนำข้อมูลไปทำ classification หรือต้องการเปลี่ยนแปลง

``` r
ind.sup = decathlon2[24:27,1:10]
ind.sup.coord <- predict(res.pca,newdata = ind.sup)
ind.sup.coord
```

    ##                PC1         PC2       PC3        PC4         PC5       PC6
    ## KARPOV   0.7772521  0.76237804 1.5971253  1.6863286 -0.73420295 0.8764709
    ## WARNERS -0.3779697 -0.11891968 1.7005146 -0.6908084 -0.03159013 1.1580162
    ## Nool    -0.5468405  1.93402211 0.4724184 -2.2283706 -0.24901831 0.5459418
    ## Drews   -1.0848227  0.01703198 2.9818031 -1.5006207 -0.31924693 0.4645028
    ##                PC7         PC8         PC9       PC10
    ## KARPOV  -1.0746432  1.24674643 -1.09172761 -0.2155002
    ## WARNERS -0.3181706 -0.41754716 -0.69099653  0.5251023
    ## Nool    -0.4656852 -0.05883861  0.65860586 -0.2815011
    ## Drews    0.3406706  0.01529952 -0.01678235 -0.1657992
