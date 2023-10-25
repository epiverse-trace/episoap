

```r
# run_severity_pipeline <- function(data,
#                          epidemic_phase = NULL,
#                          infection_type = "close_contact",
#                          scope          = NULL,
#                          study_question = "severity",
#                          ...) {

  overall_cfr <- episoap::calculate_overall_cfr(
    data              = system.file("extdata", "Marburg_EqGuinea_linelist.csv",
                                    package = "readepi"),
    infection_type    = "direct_contact",
    cases_status      = "Status",
    outcomes          = c("dead", "recovered"),
    diagnosis_status  = "Type",
    diagnosis_outcome = "confirmed"
  )
```

```
## Error in episoap::calculate_overall_cfr(data = system.file("extdata", : Assertion on 'data' failed: Must be of type 'data.frame', not 'character'.
```

```r
  print_cfr(overall_cfr)
```

```
## Error in print_cfr(overall_cfr): could not find function "print_cfr"
```

```r
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
  rmarkdown::render(input = "/Users/karimmane/Documents/Karim/LSHTM/TRACE_dev/Packages/On_trace_github/episoap/run_pipeline.md",
                    output_dir = "/Users/karimmane/Documents/Karim/LSHTM/TRACE_dev/Packages/On_trace_github/episoap/",
                    output_file = "test.html")
```

```
## /Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/pandoc +RTS -K512m -RTS run_pipeline.md --to html4 --from markdown+autolink_bare_uris+tex_math_single_backslash --output /Users/karimmane/Documents/Karim/LSHTM/TRACE_dev/Packages/On_trace_github/episoap/test.html --lua-filter /Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/library/rmarkdown/rmarkdown/lua/pagebreak.lua --lua-filter /Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/library/rmarkdown/rmarkdown/lua/latex-div.lua --embed-resources --standalone --variable bs3=TRUE --section-divs --template /Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/library/rmarkdown/rmd/h/default.html --no-highlight --variable highlightjs=1 --variable theme=bootstrap --mathjax --variable 'mathjax-url=https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML' --include-in-header /var/folders/xb/2fd1sx_j3pjdlqm5jnxczl6m0000gn/T//RtmpVU6wde/rmarkdown-str16cf732653019.html
```

```
## 
## Output created: /Users/karimmane/Documents/Karim/LSHTM/TRACE_dev/Packages/On_trace_github/episoap/test.html
```

```r
# }
```

