
# Data Cleaning and EDA scripts

This folder should contain the EDA of the data and any preprocessing scripts. Each dataset should 
have one (and only one, ideally) `.R` file, named in the standard way. 
 If there is a dataset called "mydataset1", for instance, the 
corresponding preprocessing script should be called `clean_mydataset1.R`, and if an EDA was carried out - `clean_mydataset1.R`.

The preprocessing script should read data files from the raw data folder (e.g., [./data_raw/mydataset1](./data_raw/mydataset1))
but should not write anything to that folder. The only thing it should do is
generate the clean data file, e.g. [./data_clean/mydataset1.Rds](.data_clean/mydataset1.Rds).
