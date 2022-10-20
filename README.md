# Welcome to our data pipelines repository

This repository provides a collection of curated outbreak analytics pipelines using different infrastructures for automation.

# Target audience

The analyses presented here are largely automated, and should be of use to any
outbreak analyst. A basic R literacy will be required to adapt the report to
other datasets, most importantly for identifying the variables to be used for
dates of events and stratification. The data used in this report are daily case
incidence. For raw linelists, alternative commands will be supplied in the
section on building epicurves.

# The data

To illustrate the different analyses, we use real data reporting daily numbers
of COVID-19 hospitalisations in England as of the 24 October 2020, broken down
to the hospital and National Health Service (NHS) region level. The data is
available online from the NHS England's
[website](https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-hospital-activity/).
The dataset analysed here is a simplified version, providing incidence of
hospital admissions by NHS trust, stored in the file
[covid_admissions_uk_2020_10_24.xlsx](https://github.com/epiverse-trace/data_pipelines/raw/main/data/covid_admissions_uk_2020_10_24.xlsx).

# Generating a report

The most basic command to create a report, without tweaking any of the default
settings is to run:

```r
rmarkdown::render("reports/transmissibility.Rmd")
```

You can, however, modify some of the settings, and re-create a new report in 
a single line of code. For instance, to generate a report with epicurves showing
daily incidence, instead of weekly, one can use:

```r
rmarkdown::render("reports/transmissibility.Rmd",
                  params = list(epicurve_unit = "day"))
```
