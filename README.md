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

The ORCHID events tables capture data as it was recorded in the underlying OPO database, with minimal modifications. However, for research and analysis, it may be helpful to map and combine certain data fields. For example, identical serological tests are often referred to using different names (e.g., Anti HBc vs. Anti-HBc). In the `mappings` folder of this repository, we intend to provide a set of dictionaries that facilitate more meaningful data analysis of these fields. So far, we have added:

* serology_map: This file maps the raw serology names captured in the OPO data to classes of tests and deduplicated test names. This map contains the following fields:
  - `serology_name`: Name of serological test in the ORCHID data
  - `serology_name_Other`: If serology_name is "Other", a free-text description of the test
  - `serology_class`: This mapped column indicates the organism / antibody that the test aimed to detect (e.g., HBV, HIV, etc.)
  - `serology_standardized_name`: This mapped column indicates the organism and specific test conducted (e.g., IgG, IgM, NAT). Unlike the raw `serology_name`, this column de-duplicates and combines identical tests (e.g., HbsAg, HBsAg, and HbsAg#)
