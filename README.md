# Productivity Susceptibility Analsysis for Top 306 Marine Aquarium Trade Species
Code for running the Guassian Mixture Model using the Mclust package to cluster datapoints in the Productivity-Susceptibility Analysis into three vulnerability bins.

The data needs to be a csv file in the folloiwng format for the code to run:
3 columns for Productivity Score, Susceptibility Score, and Vulnerability Score with each row corresponding to a single species.
The data used for the paper related to this model can be found here: 10.5281/zenodo.17468588

The GMM model name "VVI" was chosen for this PSA as it clusters points diagnoally with unequal volumen and shape based on 3 data inputs (prodcutivity, susceptibility, and vulnerability in this application).

Methods for running the GMM, analyzing output, and plotting the PSA data overlaid with the GMM model is provided. 

Further all code and needed datasets for graphs in the paper Baillargeon et al., 2025 [In Review] are contained within as well. 

