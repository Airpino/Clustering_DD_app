---
title: "HistDAWass application of regression on Ozone dataset"
author: "A. Irpino"
output:
  html_document:
    df_print: paged
    keep_md: true
---

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook showing an application of the two-components regression model on the `OzoneFull` dataset which is available in the `HistDAWass` package. We used the `HistDAWass` package and the therein build functions for the regression analysis.

# Install and load the package

For installing and loading the package in your environment launch the following code:


```r
## if not installed in your environment
#  install.packages("HistDAWass")
library(HistDAWass) #load the package
#other useful packages
library(tidyverse)
```

# Data description

The `OzoneFull` dataset is a `MatH` object, namely, a table of histogram-valued data, representing aggregate raw data downloaded from the Clean Air Status and Trends Network (CASTNET) ([\<http://java.epa.gov/castnet/\>](http://java.epa.gov/castnet/%7D,)), an air-quality monitoring network of the United States, designed to provide data to assess trends in air quality, atmospheric deposition and ecological effects due to changes in air pollutant emissions.

We selected data on the ozone concentration in 78 USA sites among those depicted in Fig. \\ref{FIG:EPA} for which the monitored data were complete (i.e., without missing values for each of the selected characteristics).

![CASTNET network sites map](Images/site_map_ozone.png)

Ozone is a gas that can cause respiratory diseases. In the literature, several studies reported evidence of the relation between the ozone concentration level and temperature, wind speed and the solar radiation (see, for example,\citep{Duenas2002}).

Given the distribution of \emph{temperature} ($X_{1}$) (degrees Celsius), the distribution of \emph{solar radiation} ($X_{2}$) (Watts per square meter) and the distribution of \emph{wind speed} ($X_{3}$) (meters per second), the main objective is to predict the distribution of \emph{ozone concentration} ($Y$) (Particles per billion) using a linear model. CASTNET collects hourly data and, as the period of observation, we chose the summer season of 2010 and the central hours of the days (10 a.m.--5 p.m.).

We collected the histograms of the values of each site observed for the four variables. The histograms were constructed using 100 equi-frequent bins, namely, we have bins of different widths but of constant frequency. The histogram representation of varying bin-width histograms in not always pleasant, we plot the data using only ten equi-frequent bins. We show the first 5 of 78 sites.




```r
plot(New_OZ[1:10,])+theme_bw()+xlab("")+ylab("")+theme(legend.position = "none")
```

![](Regression_application_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

Each cell of the data table contains a histogram. We see the first three rows of the matrix


```r
els<-list()
cc<-0
for (i in 1:3){
  for (j in 1:p){
    tmp<-tibble(
      Bin=paste(
        c(format(round(OzoneFull@M[i,j][[1]]@x[1:4],2),nsmall = 2),
                  "...",
          format(round(OzoneFull@M[i,j][[1]]@x[99:100],2),nsmall = 2)),
        c(format(round(OzoneFull@M[i,j][[1]]@x[2:5],2),nsmall = 2),
          "...",
          format(round(OzoneFull@M[i,j][[1]]@x[100:101],2),nsmall = 2)),
        sep="-"),
      p=rep(0.01,length(Bin))
    )
    cc<-cc+1
    els[[cc]]<-tmp
  }
  
}
fin_t<-tibble(ST_ID=c("I1","I2","I3"),
  V1=els[c(1,5,9)],
              V2=els[c(2,6,10)],
              V3=els[c(3,7,11)],
              V4=els[c(4,8,12)])
fin_t
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["ST_ID"],"name":[1],"type":["chr"],"align":["left"]},{"label":["V1"],"name":[2],"type":["list"],"align":["right"]},{"label":["V2"],"name":[3],"type":["list"],"align":["right"]},{"label":["V3"],"name":[4],"type":["list"],"align":["right"]},{"label":["V4"],"name":[5],"type":["list"],"align":["right"]}],"data":[{"1":"I1","2":"<tibble>","3":"<tibble>","4":"<tibble>","5":"<tibble>"},{"1":"I2","2":"<tibble>","3":"<tibble>","4":"<tibble>","5":"<tibble>"},{"1":"I3","2":"<tibble>","3":"<tibble>","4":"<tibble>","5":"<tibble>"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
xtable::xtable(unnest(fin_t))
```

```
## Warning: `cols` is now required when using unnest().
## Please use `cols = c(V1, V2, V3, V4)`
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["ST_ID"],"name":[1],"type":["chr"],"align":["left"]},{"label":["Bin"],"name":[2],"type":["chr"],"align":["left"]},{"label":["p"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Bin1"],"name":[4],"type":["chr"],"align":["left"]},{"label":["p1"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["Bin2"],"name":[6],"type":["chr"],"align":["left"]},{"label":["p2"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["Bin3"],"name":[8],"type":["chr"],"align":["left"]},{"label":["p3"],"name":[9],"type":["dbl"],"align":["right"]}],"data":[{"1":"I1","2":"8.77-16.62","3":"0.01","4":"8.45-11.65","5":"0.01","6":"25.29- 75.88","7":"0.01","8":"0.10-0.35","9":"0.01"},{"1":"I1","2":"16.62-17.54","3":"0.01","4":"11.65-13.06","5":"0.01","6":"75.88-108.27","7":"0.01","8":"0.35-0.41","9":"0.01"},{"1":"I1","2":"17.54-18.42","3":"0.01","4":"13.06-13.83","5":"0.01","6":"108.27-111.43","7":"0.01","8":"0.41-0.50","9":"0.01"},{"1":"I1","2":"18.42-18.90","3":"0.01","4":"13.83-14.12","5":"0.01","6":"111.43-114.09","7":"0.01","8":"0.50-0.55","9":"0.01"},{"1":"I1","2":"...-...","3":"0.01","4":"...-...","5":"0.01","6":"...-...","7":"0.01","8":"...-...","9":"0.01"},{"1":"I1","2":"65.68-67.78","3":"0.01","4":"28.87-29.23","5":"0.01","6":"914.12-933.30","7":"0.01","8":"3.52-3.79","9":"0.01"},{"1":"I1","2":"67.78-89.60","3":"0.01","4":"29.23-30.18","5":"0.01","6":"933.30-942.00","7":"0.01","8":"3.79-4.48","9":"0.01"},{"1":"I2","2":"9.00-15.00","3":"0.01","4":"9.50- 9.75","5":"0.01","6":"49.00- 56.16","7":"0.01","8":"0.10-0.55","9":"0.01"},{"1":"I2","2":"15.00-17.00","3":"0.01","4":"9.75-10.38","5":"0.01","6":"56.16- 71.50","7":"0.01","8":"0.55-0.80","9":"0.01"},{"1":"I2","2":"17.00-18.00","3":"0.01","4":"10.38-10.60","5":"0.01","6":"71.50-102.84","7":"0.01","8":"0.80-0.80","9":"0.01"},{"1":"I2","2":"18.00-19.00","3":"0.01","4":"10.60-11.24","5":"0.01","6":"102.84-133.40","7":"0.01","8":"0.80-0.90","9":"0.01"},{"1":"I2","2":"...-...","3":"0.01","4":"...-...","5":"0.01","6":"...-...","7":"0.01","8":"...-...","9":"0.01"},{"1":"I2","2":"54.24-58.00","3":"0.01","4":"29.02-29.60","5":"0.01","6":"910.00-916.84","7":"0.01","8":"7.52-8.37","9":"0.01"},{"1":"I2","2":"58.00-63.00","3":"0.01","4":"29.60-30.70","5":"0.01","6":"916.84-944.00","7":"0.01","8":"8.37-9.60","9":"0.01"},{"1":"I3","2":"9.25-17.99","3":"0.01","4":"17.57-20.13","5":"0.01","6":"52.57- 78.67","7":"0.01","8":"0.08-0.26","9":"0.01"},{"1":"I3","2":"17.99-20.31","3":"0.01","4":"20.13-20.63","5":"0.01","6":"78.67-105.48","7":"0.01","8":"0.26-0.38","9":"0.01"},{"1":"I3","2":"20.31-21.41","3":"0.01","4":"20.63-21.13","5":"0.01","6":"105.48-116.96","7":"0.01","8":"0.38-0.41","9":"0.01"},{"1":"I3","2":"21.41-22.20","3":"0.01","4":"21.13-21.61","5":"0.01","6":"116.96-140.19","7":"0.01","8":"0.41-0.45","9":"0.01"},{"1":"I3","2":"...-...","3":"0.01","4":"...-...","5":"0.01","6":"...-...","7":"0.01","8":"...-...","9":"0.01"},{"1":"I3","2":"62.38-64.11","3":"0.01","4":"36.10-36.42","5":"0.01","6":"979.18- 990.02","7":"0.01","8":"3.77-4.07","9":"0.01"},{"1":"I3","2":"64.11-69.45","3":"0.01","4":"36.42-37.07","5":"0.01","6":"990.02-1020.00","7":"0.01","8":"4.07-4.81","9":"0.01"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

## Basic Wasserstein-based statistics

### The Frechét mean distributions of the the four variables


```r
Oz_bar<-OzoneFull[,1]
```

The barycenters of each histogram variable are shown at the bottom of figure (the mean histogram according to \citep{Irpino20081648}).

```{=tex}
\begin{figure}
\includegraphics[width=0.9\textwidth]{OZO0101.eps}
\caption{Ozone dataset: first 26 sites.}\label{FIG:OZONE1}
\end{figure}
```
\% %\\begin{figure} %\includegraphics[width=0.9\textwidth]{OZO0102.eps} %

\caption{Ozone dataset: monitored sites from 27 to 52.}\label{FIG:OZONE2}

%\\end{figure} % %\\begin{figure} %\includegraphics[width=0.9\textwidth]{OZO0103.eps} %

\caption{Ozone dataset: monitored sites from 53 to 78.}\label{FIG:OZONE3}

%\\end{figure}

```{=tex}
\begin{table}
\caption{Ozone dataset: summary statistics}\label{TAB:OZO_summarystat}
\centering{}{\small }%
\resizebox{\textwidth}{!} {
\begin{tabular}{lcccc}
\hline
 & {\small Ozone Concentration } & {\small Temperature } & {\small Solar Radiation } & {\small Wind Speed}\\
 & {\small ($Y$ in Ppb)} & {\small ($X_{1}$ in deg. Celsius)} & {\small ($X_{2}$ $Watt/m^{2}$)} & {\small{} ($X_{3}$ m/s)}\\
\hline
{\small Mean (BD)} & {\small 41.2147} & {\small 23.2805} & {\small 645.3507} & {\small 2.3488}\\
{\small Barycenter mean (VI)} & {\small 41.2147} & {\small 23.2805} & {\small 645.3507} & {\small 2.3488}\\
{\small Barycenter std (VI)} & {\small 9.9680} & {\small 3.7641} & {\small 225.7818} & {\small 1.0987}\\
{\small Standard dev. (BD)} & {\small 13.790} & {\small 5.3787} & {\small 252.6736} & {\small 1.7125}\\
{\small Standard dev. (VI)} & {\small 9.5295} & {\small 3.8422} & {\small 113.4308} & {\small 1.1337}\\
\hline
\end{tabular}{\small }}\\
{\small }%
\begin{tabular}{cccccccc}
\multicolumn{8}{c}{{\small Correlations}}\\
\hline
 & \multicolumn{3}{c}{{\small Billard-Diday}} &  & \multicolumn{3}{c}{{\small Verde-Irpino}}\\
 & {\small $X_{1}$} & {\small $X_{2}$} & {\small $X_{3}$} &  & {\small $X_{1}$} & {\small $X_{2}$} & {\small $X_{3}$}\\
\hline
{\small $Y$} & {\small 0.2328} & {\small 0.4064} & {\small 0.2951} &  & {\small 0.2473} & {\small 0.6392} & {\small 0.4020}\\
{\small $X_{1}$} &  & {\small 0.2622} & {\small 0.0621} &  &  & {\small 0.4537} & {\small 0.1429}\\
{\small $X_{2}$} &  &  & {\small 0.3013} &  &  &  & {\small 0.4394}\\
\hline
\end{tabular}
\end{table}
```
In Table \ref{TAB:OZO_summarystat}, we report the main summary statistics for the four histogram variables, while in Fig. \ref{Fig: OZO_barycenters}, we provide the four barycenters of the 78 sites for each variable. We note, for example, the different skewness of the barycenters. In general, when the barycenter is skewed, the observed distributions are in general skewed in the same direction. This is not in general true for symmetric barycenters, which can be generated both from left- and right-skewed distributions.

```{=tex}
\begin{figure}
\centering{}\includegraphics[scale=0.5]{bary_ozone.eps}
\caption{Ozone dataset. Barycenters}
\label{Fig: OZO_barycenters}
\end{figure}
```
Using the full dataset, we estimated the three models and the associated goodness-of-fit indices and show the main results in Tables \ref{TAB:Ozone Bill Did}, \ref{TAB: Ozone Dias Brito} and \ref{TAB: Ozone Irpino Verde}.

```{=tex}
\begin{table}
\caption{Ozone dataset: BIllard-Diday model parameters estimated on the full
dataset and bootstrapping the dataset.}
\label{TAB:Ozone Bill Did}
\resizebox{\textwidth}{!} {
\begin{tabular}{cccccccr}
\multicolumn{8}{c}{Billard-Diday model:}\\
\multicolumn{8}{c}{ $\hat{y}_{i}=\hat{\beta_{0}}+\hat{\beta}_{1}x_{i1}+\hat{\beta}_{2}x_{i2}+\hat{\beta}_{3}x_{i3}$}\\
\cline{2-8}
 & \multicolumn{4}{c}{Model parameters} & \multicolumn{3}{c}{Goodness-of-fit }\\
\cline{2-8}
 & %
$\hat{\beta_{0}}$
 & %
$\hat{\beta}_{1}$
 & %
$\hat{\beta}_{2}$
 & %
$\hat{\beta}_{3}$
 & $\Omega$ & {\footnotesize $Pseudo-R^{2}$} & {\footnotesize $RMSE_{W}$}\\
\cline{2-8}
{\small Observed} & {\small 18.28} & {\small 0.357} & {\small 0.017} & {\small 1.550} & \emph{\footnotesize 0.203} & \emph{\footnotesize 0.024} & \emph{\footnotesize 9.413}\\
 & \multicolumn{7}{c}{{\small Bootstrap estimates}}\\
{\small Mean} & {\small 18.96} & {\small 0.323} & {\small 0.018} & {\small 1.463} & \emph{\footnotesize 0.215} & \emph{\footnotesize 0.103} & \emph{\footnotesize 9.274}\\
{\small Bias} & {\small 0.678} & {\small -0.034} & {\small 0.001} & {\small{} -0.087} &  &  & \\
{\small SE} & {\small 5.077} & {\small 0.182} & {\small 0.005 } & {\small 0.652} &  &  & \\
{\small 2.5\%} & {\small 9.52} & {\small -0.014} & {\small 0.007} & {\small 0.147} & \emph{\footnotesize 0.070} & \emph{\footnotesize 0.000} & \emph{\footnotesize 7.766}\\
{\small 97.5\%} & {\small 28.89} & {\small 0.697} & {\small 0.027} & {\small 2.836} & \emph{\footnotesize 0.350} & \emph{\footnotesize 0.372} & \emph{\footnotesize 10.967}\\
\cline{2-8}
\end{tabular}}
\end{table}
\begin{table}
\caption{Ozone dataset: Dias-Brito model parameters estimated on the full dataset
and bootstrapping the dataset.}
\label{TAB: Ozone Dias Brito}
\begin{centering}
\resizebox{\textwidth}{!} {
\begin{tabular}{ccccccccccc}
\multicolumn{11}{c}{Dias-Brito model:}\\
\multicolumn{11}{c}{ $\hat{Q}^y_{i}=\hat{\beta_{0}}+\hat{\beta}_{1}Q^x_{i1}+\hat{\beta}_{2}Q^x_{i2}+\hat{\beta}_{3}Q^x_{i3}+\hat{\tilde{\beta}}_{1}\tilde{Q}^x_{i1}(t)+\hat{\tilde{\beta}}_{2}\tilde{Q}^x_{i2}+\hat{\tilde{\beta}}_{3}\tilde{Q}^x_{i3}$}\\
\cline{2-11}
 & \multicolumn{7}{c}{Model parameters} & \multicolumn{3}{c}{Goodness-of-fit }\\
\cline{2-11}
 & $\hat{\beta_{0}}$ & $\hat{\beta}_{1}$ & $\hat{\beta}_{2}$ & $\hat{\beta}_{3}$ & $\hat{\tilde{\beta}}_{1}$ & $\hat{\tilde{\beta}}_{2}$ & $\hat{\tilde{\beta}}_{3}$ & $\Omega$ & {\footnotesize $Pseudo-R^{2}$} & {\footnotesize $RMSE_{W}$}\\
\cline{2-11}
{\small Observed} & {\small 13.32} & {\small 0.000} & {\small 0.037} & {\small 1.691} & {\small 0.000} & {\small 0.000} & {\small 0.000} & \emph{\footnotesize 0.670} & \emph{\footnotesize 0.371} & \emph{\footnotesize 7.557}\\
 & \multicolumn{10}{c}{{\small Bootstrap estimates}}\\
{\small Mean} & {\small 14.22} & {\small 0.117 } & {\small 0.034 } & {\small 1.709 } & {\small 0.080} & {\small 0.000} & {\small 0.002} & \emph{\footnotesize 0.712} & \emph{\footnotesize 0.358} & \emph{\footnotesize 7.368}\\
{\small Bias} & {\small 0.905} & {\small 0.117} & {\small -0.003} & {\small 0.018} & {\small 0.080} & {\small 0.000} & {\small{} 0.002} &  &  & \\
{\small SE} & {\small 4.760} & {\small 0.161} & {\small 0.004} & {\small 0.610} & {\small 0.112} & {\small 0.000} & {\small{} 0.025} &  &  & \\
{\small 2.5\%} & {\small 5.409} & {\small 0.000} & {\small 0.026} & {\small 0.602} & {\small 0.000} & {\small 0.000} & {\small 0.000} & \emph{\footnotesize 0.625} & \emph{\footnotesize 0.220} & \emph{\footnotesize 5.614}\\
{\small 97.5\%} & {\small 24.46} & {\small 0.540} & {\small 0.040} & {\small 3.070} & {\small 0.391} & {\small 0.000} & {\small 0.000} & \emph{\footnotesize 0.801} & \emph{\footnotesize 0.498} & \emph{\footnotesize 9.126}\\
\cline{2-11}
\end{tabular}}
\end{centering}
\end{table}
\begin{table}
\caption{Ozone dataset: Irpino-Verde model parameters estimated on the full
dataset and bootstrapping the dataset.}
\label{TAB: Ozone Irpino Verde}
\begin{centering}
\resizebox{\textwidth}{!} {
\begin{tabular}{ccccccccccc}
\multicolumn{11}{c}{Irpino-Verde model:}\\
\multicolumn{11}{c}{ $\hat{Q}^y_{i}=\hat{\beta_{0}}+\hat{\beta}_{1}\bar{x}_{i1}+\hat{\beta}_{2}\bar{x}_{i2}+\hat{\beta}_{3}\bar{x}_{i3}+\hat{\gamma}_{1}Q_{i1}^{xc}+\hat{\gamma}_{2}Q_{i2}^{xc}+\hat{\gamma}_{3}Q_{i3}^{xc}$}\\
\cline{2-11}
 & \multicolumn{7}{c}{Model parameters} & \multicolumn{3}{c}{Goodness-of-fit }\\
\cline{2-11}
 & %
$\hat{\beta_{0}}$
 & %
$\hat{\beta}_{1}$
 & %
$\hat{\beta}_{2}$
 & %
$\hat{\beta}_{3}$
 & %
$\hat{\gamma}_{1}$
 & %
$\hat{\gamma}_{2}$
 & %
$\hat{\gamma}_{3}$
 & $\Omega$ & {\footnotesize $Pseudo-R^{2}$} & {\footnotesize $RMSE_{W}$}\\
\cline{2-11}
{\small Observed} & 2.928 & -0.346 & 0.070 & 0.395 & 0.915 & 0.018 & 1.887 & \emph{\footnotesize 0.742} & \emph{\footnotesize 0.460} & \emph{\footnotesize 6.999}\\
 & \multicolumn{10}{c}{{\small Bootstrap estimates}}\\
{\small Mean} & 3.108 & -0.353 & 0.070 & 0.363 & 0.928 & 0.018 & 1.958 & \emph{\footnotesize 0.758} & \emph{\footnotesize 0.474} & \emph{\footnotesize 6.729}\\
{\small Bias} & 0.181 & -0.008 & 0.000 & -0.032  & 0.013  & 0.000 &  0.071 &  &  & \\
{\small SE} & 7.180 & 0.271 & 0.010 & 0.823 & 0.237 & 0.003  & 0.542 &  &  & \\
{\small 2.5\%} & -11.24 & -0.846 & 0.052 & -1.186 & 0.482 & 0.012 & 1.054 & \emph{\footnotesize 0.675} & \emph{\footnotesize 0.296} & \emph{\footnotesize 5.267}\\
{\small 97.5\%} & 18.87 & 0.173 & 0.090 & 2.030 & 1.377 & 0.024 & 3.115 & \emph{\footnotesize 0.829} & \emph{\footnotesize 0.625} & \emph{\footnotesize 8.298}\\
\cline{2-11}
\end{tabular}}
\par\end{centering}

\end{table}
```
Also in this case, we performed bootstrap estimates of the parameters and goodness-of-fit measures of the three models in Tables \ref{TAB:Ozone Bill Did}, \ref{TAB: Ozone Dias Brito} and \ref{TAB: Ozone Irpino Verde}. From the goodness-of-fit measures of the three models, we can conclude that the Irpino-Verde and the Dias-Brito models fit the linear regression relationship better than the Billard-Diday model and that the Irpino-Verde model is slightly more accurate than the Dias-Brito model. Also in this case, the Irpino-Verde model parameters give an easier interpretation. Reading the Irpino-Verde bootstrapped model, we may assert that the ozone concentration distribution of a site depends on the mean \emph{solar
radiation}, where for each $\Delta Watt/m^{2}$ a $0.070\;(ppb)$ variation of the \emph{ozone concentration} mean level is expected, while in general we cannot say that the mean levels of \emph{temperature} and \emph{wind
speed} induce a significant variation of the \emph{ozone concentration
level }($95\%$ bootstrap confidence intervals include the zero). Furthermore, the variability of the \emph{ozone concentration} is almost the same of the \emph{temperature }($0.928$); a unitary variation in the variability of the \emph{solar radiation} induces a variation of $0.018\;(ppb)$ and a variation in the variability of the \emph{wind
speed} causes an increase in the variability of $1.958\;(ppb)$. Similar conclusions can be derived from the Dias-Brito model estimation, even if it gives a different interpretation of the parameter associated with the symmetric histogram variables.