# Area under curve

Extracting areas under the curve (AUC) values from published abstracts, and examining patterns in the distribution around the thresholds at 0.7, 0.8 and 0.9.

The published paper is available [here](https://bmcmedicine.biomedcentral.com/articles/10.1186/s12916-023-03048-6).

## R version and packages

```
R version 4.2.1 (2022-06-23 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19045)

Matrix products: default

locale:
[1] LC_COLLATE=English_Australia.utf8  LC_CTYPE=English_Australia.utf8   
[3] LC_MONETARY=English_Australia.utf8 LC_NUMERIC=C                      
[5] LC_TIME=English_Australia.utf8    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] openxlsx_4.2.5.1 vecsets_1.3      purrr_0.3.4      tidyr_1.2.0      modelbased_0.8.6
 [6] broom_0.8.0      dplyr_1.0.9      stringr_1.4.0    flextable_0.7.0  gridExtra_2.3   
[11] ggplot2_3.3.6   

loaded via a namespace (and not attached):
 [1] tidyselect_1.1.2  xfun_0.31         colorspace_2.0-3  vctrs_0.4.1      
 [5] generics_0.1.2    htmltools_0.5.2   yaml_2.3.5        base64enc_0.1-3  
 [9] pracma_2.3.8      utf8_1.2.2        rlang_1.0.2       pillar_1.7.0     
[13] glue_1.6.2        withr_2.5.0       DBI_1.1.2         gdtools_0.2.4    
[17] uuid_1.1-0        lifecycle_1.0.1   munsell_0.5.0     gtable_0.3.0     
[21] zip_2.2.0         evaluate_0.15     knitr_1.39        fastmap_1.1.0    
[25] datawizard_0.6.5  fansi_1.0.3       Rcpp_1.0.8.3      scales_1.2.0     
[29] backports_1.4.1   systemfonts_1.0.4 digest_0.6.29     stringi_1.7.8    
[33] insight_0.18.8    grid_4.2.1        cli_3.3.0         tools_4.2.1      
[37] magrittr_2.0.3    tibble_3.1.7      crayon_1.5.1      pkgconfig_2.0.3  
[41] ellipsis_0.3.2    data.table_1.14.2 xml2_1.3.3        rstudioapi_0.13  
[45] assertthat_0.2.1  rmarkdown_2.14    officer_0.4.2     R6_2.5.1         
[49] compiler_4.2.1  
```
