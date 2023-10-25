# A test script for pipeline building

 Karim

This report was automatically generated with the R package **knitr**
(version 1.44).


```r
'# estimate overall CFR'
```

```
## [1] "# estimate overall CFR"
```

```r
overall_cfr <- episoap::calculate_overall_cfr(
  data              = data,
  infection_type    = "direct_contact",
  cases_status      = "Status",
  outcomes          = c("dead", "recovered"),
  diagnosis_status  = "Type",
  diagnosis_outcome = "confirmed"
)

### CFR among all cases
print_cfr(overall_cfr$cfr_total)
```

<table class="table table-hover" style="width: auto !important; margin-left: auto; margin-right: auto;border-bottom: 0;">
 <thead>
  <tr>
   <th style="text-align:center;"> estimated_cfr </th>
   <th style="text-align:center;"> lower_ci </th>
   <th style="text-align:center;"> upper_ci </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #81a4ce">88</span> </td>
   <td style="text-align:center;"> 73 </td>
   <td style="text-align:center;"> 96 </td>
  </tr>
</tbody>
<tfoot><tr><td style="padding: 0; " colspan="100%">
<span style="font-style: italic;">Note: </span> <sup></sup> Estimated CFR (c.f. {cfr} package)</td></tr></tfoot>
</table>

```r
### CFR among confirmed cases only
print_cfr(overall_cfr$cfr_confirmed_only)
```

<table class="table table-hover" style="width: auto !important; margin-left: auto; margin-right: auto;border-bottom: 0;">
 <thead>
  <tr>
   <th style="text-align:center;"> estimated_cfr </th>
   <th style="text-align:center;"> lower_ci </th>
   <th style="text-align:center;"> upper_ci </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #81a4ce">71</span> </td>
   <td style="text-align:center;"> 44 </td>
   <td style="text-align:center;"> 90 </td>
  </tr>
</tbody>
<tfoot><tr><td style="padding: 0; " colspan="100%">
<span style="font-style: italic;">Note: </span> <sup></sup> Estimated CFR (c.f. {cfr} package)</td></tr></tfoot>
</table>

```r
## estimate CFR by accounting for the delay distribution
onset_death <- episoap::get_onset_to_death_distro(
  data         = data,
  disease      = "Marburg Virus Disease",
  type         = "range",
  values       = c(8, 2, 16),
  distribution = "gamma"
)
```

```
## Using list(author = list(list(given = NULL, family = "Pavlin", role = NULL, email = NULL, comment = NULL)), year = "2014", title = "Calculation of incubation period and serial interval from multiple outbreaks of Marburg virus disease", journal = "BMC Research Notes", doi = "10.1186/1756-0500-7-906", pmid = "25495697"). 
## To retrieve the short citation use the 'get_citation' function
```

```
## Stochastic numerical optimisation used. 
## Rerun function multiple times to check global optimum is found
```

```
## Citation cannot be created as author, year, journal or title is missing
```

```r
delay_cfr <- episoap::calculate_delay_cfr(
  data                   = data,
  onset_death            = onset_death,
  onset_date_variable    = "Onset_week",
  deaths_date_variable   = "date_death",
  date_scale             = "day",
  cases_status           = "Status",
  outcome                = "dead"
)
episoap::print_cfr(delay_cfr)
```

<table class="table table-hover" style="width: auto !important; margin-left: auto; margin-right: auto;border-bottom: 0;">
 <thead>
  <tr>
   <th style="text-align:center;"> estimated_cfr </th>
   <th style="text-align:center;"> lower_ci </th>
   <th style="text-align:center;"> upper_ci </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> <span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #81a4ce">88</span> </td>
   <td style="text-align:center;"> 73 </td>
   <td style="text-align:center;"> 96 </td>
  </tr>
</tbody>
<tfoot><tr><td style="padding: 0; " colspan="100%">
<span style="font-style: italic;">Note: </span> <sup></sup> Estimated CFR (c.f. {cfr} package)</td></tr></tfoot>
</table>

```r
# rmarkdown::render(input = "/Users/karimmane/Documents/Karim/LSHTM/TRACE_dev/Packages/On_trace_github/episoap/run_pipeline.md",
#                   output_dir = "/Users/karimmane/Documents/Karim/LSHTM/TRACE_dev/Packages/On_trace_github/episoap/",
#                   output_file = "test.html")
```

The R session information (including the OS info, R version and all
packages used):


```r
sessionInfo()
```

```
## R version 4.2.2 (2022-10-31)
## Platform: aarch64-apple-darwin20 (64-bit)
## Running under: macOS Ventura 13.5
## 
## Matrix products: default
## LAPACK: /Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/lib/libRlapack.dylib
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods  
## [7] base     
## 
## other attached packages:
## [1] episoap_0.0.0.9000 formattable_0.2.1  lintr_3.1.0       
## [4] testthat_3.1.8    
## 
## loaded via a namespace (and not attached):
##  [1] colorspace_2.1-0        ellipsis_0.3.2         
##  [3] rprojroot_2.0.3         markdown_1.7           
##  [5] fs_1.6.3                rstudioapi_0.15.0      
##  [7] roxygen2_7.2.3          farver_2.1.1           
##  [9] remotes_2.4.2.1         fansi_1.0.5            
## [11] xml2_1.3.5              cachem_1.0.8           
## [13] knitr_1.44              pkgload_1.3.2          
## [15] jsonlite_1.8.7          shiny_1.7.5.1          
## [17] compiler_4.2.2          httr_1.4.7             
## [19] backports_1.4.1         grates_1.0.1           
## [21] fastmap_1.1.1           lazyeval_0.2.2         
## [23] cli_3.6.1               later_1.3.1            
## [25] htmltools_0.5.6.1       prettyunits_1.2.0      
## [27] tools_4.2.2             gtable_0.3.3           
## [29] glue_1.6.2              dplyr_1.1.2            
## [31] Rcpp_1.0.11             jquerylib_0.1.4        
## [33] vctrs_0.6.4             svglite_2.1.1          
## [35] xfun_0.40               stringr_1.5.0          
## [37] epiparameter_0.0.0.9000 ps_1.7.5               
## [39] brio_1.1.3              rvest_1.0.3            
## [41] mime_0.12               miniUI_0.1.1.1         
## [43] lifecycle_1.0.3         renv_1.0.3             
## [45] devtools_2.4.5          scales_1.2.1           
## [47] cfr_0.1.0               promises_1.2.1         
## [49] rex_1.2.1               yaml_2.3.7             
## [51] memoise_2.0.1           ggplot2_3.4.2          
## [53] sass_0.4.7              stringi_1.7.12         
## [55] desc_1.4.2              checkmate_2.2.0        
## [57] cyclocomp_1.1.0         pkgbuild_1.4.2         
## [59] incidence2_2.1.0        rlang_1.1.1            
## [61] pkgconfig_2.0.3         systemfonts_1.0.4      
## [63] commonmark_1.9.0        distributional_0.3.2   
## [65] evaluate_0.22           purrr_1.0.2            
## [67] htmlwidgets_1.6.2       processx_3.8.2         
## [69] tidyselect_1.2.0        magrittr_2.0.3         
## [71] R6_2.5.1                generics_0.1.3         
## [73] profvis_0.3.8           pillar_1.9.0           
## [75] withr_2.5.1             tibble_3.2.1           
## [77] crayon_1.5.2            utf8_1.2.3             
## [79] rmarkdown_2.22          urlchecker_1.0.1       
## [81] usethis_2.1.6           grid_4.2.2             
## [83] data.table_1.14.8       callr_3.7.3            
## [85] digest_0.6.33           webshot_0.5.4          
## [87] xtable_1.8-4            httpuv_1.6.11          
## [89] munsell_0.5.0           viridisLite_0.4.2      
## [91] kableExtra_1.3.4        bslib_0.5.1            
## [93] sessioninfo_1.2.2
```

```r
Sys.time()
```

```
## [1] "2023-10-25 12:42:22 GMT"
```

