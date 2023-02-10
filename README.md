# Efficient and Effective Calibration of Numerical Model Outputs Using Hierarchical Dynamic Models

This Github page provides code for reproducing the results in the manuscript:``Efficient and Effective Calibration of Numerical Model Outputs Using Hierarchical Dynamic Model'' by Y. Chen, X. Chang, B. Zhang, and H. Huang. 

In this paper, we propose a Bayesian hierarchical dynamic model (HDCM) and develop an algorithm based on variational Bayes (VB) and ensemble Kalman smoother (EnKS) to accelerate the parameter estimation and calibration procedure. To further improve the scalability of the HDCM, a Laplace approximation and a space-partitioning-based procedure are embedded in VB and EnKS, respectively. The VB and space-partitioning-based EnKS (spEnKS) have been implemented via our an R package - [HDCM](https://github.com/ChenYW68/HDCM/tree/main/HDCMc/LoadPackages). 


# Datasets
The effectiveness and efficiency of the HDCM is demonstrated via two datasets: 
-	 Moderately large datasets: $PM_{2.5}$ concentrations are from the monitoring stations and the outputs of Community Multiscale Air
Quality (CMAQ, Byun and Schere, (2006)) system for China's Beijing-Tianjin-Hebei (BTH) region, and the sizes are $68 \times 92 = 6{,}256$ observations and $5{,}587 \times 92 = 514{,}004$ $PM_{2.5}$ outputs, respectively 
- Large datasets: $PM_{2.5}$ concentrations are from the reanalysis $PM_{2.5}$ outputs of the Nested Air Quality Prediction Modeling System (NAQPMS, Wang et al., (2006)) and from the raw $PM_{2.5}$ outputs of the CMAQ, and the sizes are $6{,}382 \times 30 = 191{,}460$ reanalysis $PM_{2.5}$ outputs and $16{,}093 \times 30 = 482{,}790$ $PM_{2.5}$ outputs, respectively 

# An illustration for the first dataset in the BTH region
<figure id="Figure1">
  <p align="center">
  <img src="./HDCMc/figure/Fig1.jpg" width=50% height=50%>
    </p>
  <figcaption
  <strong>Figure 1:</strong> (a) Map of China. (b) Zoomed-in map of the BTH region along with locations of $68$ monitoring stations (red dots) and centroids of $5{,}587$ 9-km CMAQ grids (gray dots).} (c)-(e) Scatter plots of CMAQ $PM_{2.5}$ forecasts versus actual observations at Zhangjiakou, Beijing, and Hengshui in different seasons, where ``Corr'' represents the Pearson correlation coefficient between CMAQ $PM_{2.5}$ outputs and observed $PM_{2.5}$ concentrations. Three reference lines with the slope k = 0.5, 1, and  2 are colored in gray.
  </figcaption>
</figure>


# An illustration for the second dataset


<figure id="Figure2">
    <p align="center">
  <img src="./HDCMc/figure/FigS2.jpg" width=50% height=50%>
  </p>
  <figcaption
  <strong>Figure 1:</strong> Maps of grid cells of the CMAQ and the NAQPMS. The symbols ``+'' represent the centroids of 16{,} 093 9km CMAQ grids. The symbols ``*'' denote the centroids of $6{,} 382$ 15km NAQPMS grids.
  </figcaption>
</figure>
