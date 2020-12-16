# Imputing missing values in the Ethnographic Atlas

The aim is to produce a set of distances between societies based on their cultural traits.  The ethnographic atlas as made available in D-PLACE could do this, but there is a lot of missing data (about 25% in the whole dataset), which means that distances can't be computed easily.  One approach is to impute the missing data (guess their values based on existing data).  It's unlikely that any imputation method will be completely accurate, but for our purposes we don't need to be accurate, just *unbiased*.  That is, the imputed values should not bias the estimates of the distances between cultures.

In this case, we use multiple imputation: calculating many possible alternative imputations and taking the mean distances over all imputations.  Note that this can break ties between otherwise identical distances.

We use the imputation package `mice` for R.  We can test the imputation by taking the full Ethnographic Atlas data, creating some new missing values in random places for FAIR languages and re-imputing those missing values.  We can then asses how accurate the imputation was for those values.  Some informal testing compared various settings of the imputation method, and found that using classification and regression trees (CART) with the standard parameters produced the best results. 

CART works by building a decision tree: an optimal set of yes-no questions to ask about predictor variables in order to guess the value of a target variable.  The tree divides the data into partitions which look similar.  The algorithm works out which partition a missing data point would belong to, then samples the target variable distribution from that partition.  To account for historical relationships, we included language family according to Glottolog and geographic area according to Autotyp as additional factors on which the imputation process could draw.

We ran CART multiple imputation on the ethnographic atlas.  We excluded population size, one more variable that was coded for less than 33% of societies, and any societies that had fewer than 33% variables coded.  This left 92 variables for 962 languages with 16% missing data.

CART imputation guessed the correct value of missing data 74% of the time (average over 100 imputations) for the 15 FAIR languages where data was available.  This is reasonably good, considering that most variables have between 4 and 8 possible values (median = 6).  For example, this is 8.6 standard deviations better than choosing randomly (accuracy = 19%) and 5.6 standard deviations better than sampling from the known distribution of the target variable (accuracy = 37% on the same missing data).  This is not good enough to use in analyses that look at individual traits, but serves our purposes to estimate overall distances between 

We produced 100 imputation sets with the final settings.  These were then used to create distance matrices using gower distance between discrete traits (mean correlation between sets r = 0.94, estimates of distance vary by around 2% on average).

The same was done for sub-domains of the data.  The file `data/Concepticon_to_EA.csv` shows the mapping between Concepticon domains and Ethnographic Atlas domains (which are categorised in D-PLACE).

## Processing

The file `data/FAIR_langauges_glotto_xdid.csv` shows how the FAIR languages were mapped to Glottolog codes and society id codes from D-PLACE.

`processing/matchFAIRtoGlotto.R`: Match FAIR languages to glotto codes

`processing/imputeMissingData.R`: First run of imputation, and makes `preImputed.Rdat` data frame

`processing/imputeMissingData_*`: Most files choose missing values just from FAIR languages, but a few early ones select from the whole dataset.  The output is then used in:

`analysis/testImputationAccuracy.R`:  For different imputation settings, test the accuracy and performance above a baseline of random sampling from the existing distribution.

`processing/imputeMissingData_plusArea2.R`: Final imputation code used on the cluster computer to create 50 imputations.  Saves files in `data/EA_imputed/FullImputation*`.

`processing/getCompleteImputedDataframes.R`: Take the output of `mice` from above, and create full dataframes.  Saved to `data/EA_imputed/completeDataframes/*.csv`.

`processing/getDistanceMatrix.R`:  Create distance matrices from the imputed datasets for FAIR languages.  Also produces distances for sub-sets of the variables and creates some graphs (hierarchical clustering, not as good as the splitstrees below).  Stored in `results/EA_distances/*.csv`

`processing/combineCultAndLingDistances.R`: Combine the data from the cultural and linguistic data from different domains into a single long-form csv.  Outputs to `results/EA_distances/All_Domains.csv`.

`processing/makeSplitstreeNexusFile.R`:  This creates a nexus file format for the distance matrix which can be read by splitstree.  Outputs to `results/splitstree/CulturalDistances_Kinship_AllSocieties.nex`.



## Results

The main data file that lists the distances between languages in the FAIR sample is: 

[results/EA_distances/CulturalDistances_Long.csv](https://github.com/seannyD/ImputeEACulturalDifferences/blob/master/results/EA_distances/CulturalDistances_Long.csv)

The other files in `results/EA_distances` look at specific domains.

This file shows a Neighbour Net of the cultural distances:
[results/splitstree/CulturalDistances_NeighbourNet.pdf](https://github.com/seannyD/ImputeEACulturalDifferences/blob/master/results/splitstree/CulturalDistances_NeighbourNet.pdf).  Cultures cluster together reasonably sensibly.