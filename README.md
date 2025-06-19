# ORCHID Code Repository

This repository contains code for exploring and analysing the Organ Retrieval and Collection of Health Information for Donation (ORCHID) Dataset. For a background to the creation and release of ORCHID, see the following announcement by the Federation of American Scientists: https://fas.org/publication/fas-announces-organ-procurement-organization-innovation-cohort/

## Accessing the data

ORCHID can be downloaded via the following project on PhysioNet:

Adam, H., Suriyakumar, V., Pollard, T., Moody, B., Erickson, J., Segal, G., Adams, B., Brockmeier, D., Lee, K., McBride, G., Ranum, K., Wadsworth, M., Whaley, J., Wilson, A., & Ghassemi, M. (2023). Organ Retrieval and Collection of Health Information for Donation (ORCHID) (version 1.0.0). PhysioNet. https://doi.org/10.13026/eytj-4f29.

The data is also available on Google BigQuery. To access the data on BigQuery, you must first request access using the link at:
https://physionet.org/content/orchid/1.0.0/#files

## Plots

The `plots_NSD.R` file can be used to generate simple plots and tables that summarize the ORCHID dataset.

## Mappings

The ORCHID events tables captures data as it was recorded in the underlying OPO database, with minimal modifications. However, for research and analysis, it may be helpful to map and combine certain data fields. For example, identical serological tests are often referred to using different names (e.g., Anti HBc vs. Anti-HBc). In the `mappings` folder of this repository, we intend to provide a set of dictionaries that facilitate more meaningful data analysis of these fields. So far, we have added:

* serology_name_map: This file maps the raw serology names captured in the OPO data (`serology_name`) to de-deduplicated names (`serology_name_deduplicated`) that combine identical tests into unique categories.
