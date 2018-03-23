# E-ECG R Script Preparations

This document describes the required preparations to start using the R-scripts

## R

To use R and run R-scripts on a terminal, R needs to be installed on the environment you're running. If R is not pre-installed on your distro, execute the following commands on the terminal:

```BASH

sudo apt-get update
sudo apt-get install r-base r-base-dev
sudo apt-get install gdebi-core

```

When R is installed ```Rscript``` can be used in a Terminal window and in BASH scripts.

For your own convenience, it may be useful to install R-Studio. However, this is not necessary to run the scripts.

For more information on the installation of both R and R-Studio, see [[r-bloggers|https://www.r-bloggers.com/how-to-install-r-on-linux-ubuntu-16-04-xenial-xerus/]]. Note that the R-Studio version used in this blog is not the latest version. See [[rstudio.com|https://www.rstudio.com/products/rstudio/download/#download]] for the latest version.

## R-packages

```E-ECG.main.R``` includes ```E-ECG.functions.R``` which uses the following packages:
- zoo
- TTR
- ggplot2
- plyr
- XML
- reshape2
- forecast
- tsoutliers
- gridExtra
- dplyr

The function script attempts to install each package, if it is not installed yet and finally attempts to load the installed package with the ```library``` function.

**Note: R may encounter issues during  installation of some packages! Please, see the following paragraphs for the solution of known issues.**

### XML-package

In bash execute:

```bash

sudo apt-get install libxml2-dev

```

Then retry the ```install.packages``` command:

```R

install.packages("XML")

```

### tsoutliers-package

```bash

sudo apt-get install libgsl0-dev

```

Then retry the ```install.packages``` command: 
```R

install.packages("tsoutliers")

```
