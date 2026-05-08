**Imbalanced Neural Newsvendor**
Official implementation of the research paper:"**Imbalanced neural newsvendor**" published in **Optimization and Engineering (2026)**.  
**Overview**: 
This repository provides a data-driven framework to address the imbalance problem in the newsvendor model. Traditional machine learning methodologies often presuppose that target variables are uniformly distributed, leading to the undervaluing of rare demand values. This can result in significant economic costs due to the asymmetric nature of over- and under-prediction costs.  We propose two major contributions to resolve these challenges:

(i) Relevance-Weighted (RW) Learning: A neural network training framework that utilizes a unified loss function to incorporate both demand rareness and asymmetric newsvendor costs.  
(ii) NECRA Metric: The Newsvendor Error Cost Relevance Area metric, which evaluates model performance across all possible thresholds of demand rareness.  

**Repository Structure**: The repository is organized as follows:
**R code**/: Contains core R and Python scripts for neural network model creation and the RW loss function implementation.
**Real Data**/: Includes the retail demand dataset and scripts used for external validation.
**Figures**/: Scripts to reproduce the performance comparisons and line plots of NECRA across varying experimental parameters.
**Results in Excel**/: Detailed numerical NECRA scores and summary tables for transparency and reproducibility.


**Requirements**: The implementation leverages an integrated R and Python workflow via the reticulate package. 
Key dependencies include:  
**R**: reticulate, ggplot2.  
**Python**: PyTorch, RMSprop. 
