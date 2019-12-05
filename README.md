# Reproduction excercise

### What is this rep for ? 


Reproduction of output of the work "Warming increases the risk of civil war in Africa" by the authors Marshall B. Burke, Edward Miguel, Shanker Satyanath, John A. Dykema, and David B. Lobell.

Framework adapted from BITSS, documented at https://github.com/fhoces/ACRE.git. 

Complementary OSF repository, including more files for reproduction, information on data and further topics, including my thesis:

https://osf.io/jxn5b/

**NOTE:** Unfortunately downloading additional dataset from this link is compulsory for
running the master file and receiving results. Download p4v2007 from order /data/polity/
and store it under exact same subfolder combination in your working directory. 

Aside from that only thing to change in the code to run it is changing the working directory.


### File overview

#### *climate_conflict_data_cleaning.R*

Master file for data cleaning. 

**NOTE:** My master file uses a different (more recent) version of the CRU climate data (4.03 compared to 2.1 in the original production).</br>
The reason for this is the version of CRU used by the authors of the paper is coded in ASCII and rather difficult to import in R, whereas CRU 4.03 is in a nice easy peasy NetCDF4 format.

#### *climate_conflict_analysis_code.R*

This script imports the data ultimately computed by the *climate_conflict_data_cleaning.R* script, and uses this data to compute all the outputs in my thesis. 
Regarding a complete reproduction of the paper I examined: As of now, no reproduction of Tables 2 and Figures 1 and 2 , because not relevant part of the reproduction excercise (do not focus on causal relationship estimation, but rather on prediction).
Tables S3 and S7 have not been replicated yet because of complexity.

**NOTE:** For producing Figure 1 in my thesis, the code imports the original replication folder provided by Burke et al. (downloadable at https://doi.org/10.7910/DVN/28109). 

#### *climate_conflict_data_appendix.R*

Script that computes the output of the data appendix, also provided in my OSF repository. 

### Further work to do 

- [ ] simplifying of master file 
  - improving modularity
  - removes duplicate code
  - adds to understandabilty of code - good code should be short and explain itself
- [ ] more on modularity: replace magic numbers by variables at the top of the file (or seperate script - something like master_input_....R ?) to show the input
- [ ] working on tables S3 and S7
- [ ] more robustness checks.
