# Productivity Susceptibility Analsysis for Top 306 Marine Aquarium Trade Species
Code for running the Guassian Mixture Model using the Mclust package to cluster datapoints in the Productivity-Susceptibility Analysis into three vulnerability bins.

The data needs to be a csv file in the folloiwng format for the code to run:
3 columns for Productivity Score, Susceptibility Score, and Vulnerability Score with each row corresponding to a single species.

The GMM model name "VVI" was chosen for this PSA as it clusters points diagnoally with unequal volumen and shape based on 3 data inputs (prodcutivity, susceptibility, and vulnerability in this application).

Methods for running the GMM, analyzing output, and plotting the PSA data overlaid with the GMM model is provided. 

