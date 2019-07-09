## Installation
It is recommended that you use the [latest version of R](https://cran.r-project.org/), or at least 3.5.1

In RStudio, install the following libraries or update the package, data.table, to at least version 1.11
``install.packages(c("tidyverse", "data.table", "openxlsx", "foreign"), dependencies = TRUE)``

In git bash, navigate to the repository and confirm that it is set on the master branch.

## File Check and Setup

### GQ file

If the Group Quarters file has been updated with new columns or values, it was likely exported from another program or software. R will have difficulty reading in the file because the column headers themselves (`2000` through `2050`) need to be converted to a number. Highlight those column header cells and click on the green triangle located on the corner of a cell (if displayed) and select `Convert to Number`. Save the file and it will be ready.

``J:\Projects\V2050\SEIS\Data_Support\script_input\group_quarters_geo.xlsx``

### Control Totals file

Indicator 30, Jobs and Pop in TOD Areas (``screening_factors/jobs_pop_tod_areas.R``), relies on a control totals workbook. Each new run needs a tab with the name that represents the run. That name should be written in the same way in ``settings.R`` in the ``run.dir`` list. Check with Brian which set of control totals should be used.

``J:\Projects\V2050\SEIS\Data_Support\script_input\2017_actuals_2050_controls.xlsx``


## Customize settings

### Generate screening factors only:  

- Edit the settings in ``screening_factors/settings.R`` if necessary for the following objects: ``run.dir`` and ``out.dir``
- Edit the object ``wrk.dir`` in ``screening_factors/create_screening_factors.R`` to reference where the repository resides on your machine
- Run ``screening_factors/create_screening_factors.R``

### Generate long-list (with screening factors option):  

- Edit the settings in ``full_set/settings.R`` for the following objects: ``run.dir``, ``out.dir``, ``out.dir.maps`` 
- Edit the object ``wrk.dir`` in ``full_set/create_indicators.R`` to reference where the repository resides on your machine
    - To generate screening factors in addition to the long-list, set ``run.screening.factors`` to TRUE. If not, set to FALSE
- Run ``full_set/create_indicators.R``

