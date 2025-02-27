# Season- and Trend-aware Symbolic Approximation for Accurate and Efficient Time Series Matching (Supporting Material)
Processing and analyzing time series datasets have become a central issue in many domains requiring data management systems to support time series as a data type natively. A crucial prerequisite of these systems is time series matching, yet it is a challenging problem. A time series is a high-dimensional data type, its representation is storage-, and its comparison is time-consuming. Among the representation techniques that tackle these challenges, the symbolic aggregate approximation (SAX) is of particular interest. This technique reduces a time series in a low-dimensional space by segmenting it and discretizing each segment into a small alphabet of symbols. However, SAX ignores the deterministic behavior of a time series such as its cyclical repeated season or its trend component affecting all segments and leading to a distortion of the symbolic distribution. In this paper, we present a season- and a trend-aware symbolic approximation. We show that they improve a representation's symbolic distribution and increase the representation accuracy without increasing the representation size. Most importantly, they enable a more efficient time series matching by providing a match up to three orders of magnitude faster than SAX.

## Scripts
The directory *Implementation* contains the scripts to carry out the evaluation:

 - *main.R*: bundles generation, distance calculation and evaluation scripts
 - *sources* (in logical order):
	 - *generate.R*: generation of synthetic time series datasets
	 - *euclidean_distance.R*: calculation of Euclidean distance on synthetic and real-world datasets
	 - *method.R*: construction of representation technique based on configuration and dataset parameters
	 - *represent.R*: transformation of a time series dataset into representation
	 - *distance.R*: calculation of representation distance
	 - *exact_search.R*: linear search (exact matching)
	 - *approximate_search.R*: linear search (approximate matching)
	 - *eval.R*: textual and visual evaluation of results
	 - *run.R*: helper methods for evaluation scripts
	 - *util.R*: helper methods for file management and data frames
 - *configs*: configuration parameters for representation techniques and dataset parameters
 - *c*: sources for efficiency evaluation

Required packages:

 - idxrepr (see below)
 - data.table
 - distr
 - parallel
 - R.utils
 - ggplot2
 - directlabels
 - latticeExtra
 - grDevices
 - extrafont
 - Rttf2pt1 and ghostscript 9.27 must be installed for plots with the correct fonts

## Datasets
The directory *Data* contains the datasets together with representations, intermediate and final results of the evaluation.

 - Synthetic time series datasets (*Season*, *Trend*) have been generated by the authors. The user may re-use them or generate other synthetic datasets with the scripts.
 - *Metering* dataset
	 - This dataset has to be included manually from [Irish Social Science Data Archive](http://www.ucd.ie/issda).
	 - It is the dataset "CER Smart Metering Project" of the Commission for Energy Regulation.
	 - Select the 5958 time series with 21840 values.
	 - Store the dataset in Data/issda_I_5958_T_21840_L_1_48/dataset.rds.
 - The *Economy* dataset is provided under Data/m4_I_6400_T_300/dataset.rds.
 - *Season (Large)* has to be generated.

## Representation Techniques

The *idxrepr* package contains the representation techniques: [https://github.com/lkegel/idxrepr](https://github.com/lkegel/idxrepr). Install this package to run the evaluation.

## Software Environment
The accuracy experiments have been tested under

 - Windows 10 with R 3.4.4 and Rtools 3.4.0.1964
 - Linux Ubuntu 14.04.6 LTS with R 3.4.4 and gcc 4.8.4

The effiency experiments are only runnable under Windows because they depend on Windows I/O functions.
