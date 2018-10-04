## Installation
It is recommended that you use the [latest version of R](https://cran.r-project.org/), 3.5.1

In RStudio, install the following libraries or update the package, data.table, to at least version 1.11
``install.packages(c("tidyverse", "data.table", "openxlsx", "foreign"), dependencies = TRUE)``

In git bash, navigate to the repository and confirm that it is set on the master branch.

## Customize settings

### Generate screening factors only:  

- Edit the settings in ``screening_factors/settings.R`` if necessary for the following objects: ``run.dir`` and ``out.dir``
- Edit the object ``wrk.dir`` in ``screening_factors/create_screening_factors.R`` to reference where the repository resides on your machine
- Run ``screening_factors/create_screening_factors.R``

### Generate long-list (with screening factors option):  

- Edit the settings in ``full_set/settings.R`` for the following objects: ``rund``, ``run.dir``, ``bdir`` 
- Edit the object ``wrk.dir`` in ``full_set/create_indicators.R`` to reference where the repository resides on your machine
    - To generate screening factors in addition to the long-list, set ``run.screening.factors`` to TRUE. If not, set to FALSE
- Run ``full_set/create_indicators.R``