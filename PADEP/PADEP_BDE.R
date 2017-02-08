#==============================================================================
#==============================================================================
# Title: PADEP Metric Sensitivity
# Author: Zachary M. Smith
# Organization: ICPRB
# Email: zsmith@icprb.org
# Date: 2/08/2017
# Purpose: This script written to allow D. Shull to test the Balanced
# Descrimination Efficiency method.
#==============================================================================
#==============================================================================
# Install the package devtools, which makes it easy to install the MMI
# package.
install.packages(c("devtools","curl", "httr"))
# Install the MMI package.
devtools::install_github("zsmith27/MMI", build_vignettes = TRUE)
# Load the MMI package.
library(MMI)
#==============================================================================
# Set working directory.
# This should refer to the file location of the tables you  want
# to import.
setwd("C:/Users/zsmith/Desktop/Large_River/Large_River_PADEP")
# Import a csv file.
metrics.df <- read.csv("MetricRun2.csv", stringsAsFactors = FALSE)
#==============================================================================
# Barabour et al. (1996) visual method of assessming metric sensitivity based
# on the amount of overlap between the two interquartile ranges.
# I automated their visual check by calclulating and comparing the
# interquartile range of reference samples vs. degraded samples.
sens.barbour <- sensitivity(metrics.df,
                       first.metric = "Richness",
                       condition.colname = "ConditionAbrev",
                       ref.cond = "LD",
                       deg.cond = "S",
                       method = "BARBOUR")
#==============================================================================
# This is the orginal Discrimination Efficiency (DE) test.
# If the metric decreases with disturbance, then I find the percentage of 
# degraded sites below the 25th percentile of the reference distribution.
sens.de <- sensitivity(metrics.df,
                   first.metric = "Richness",
                   condition.colname = "ConditionAbrev",
                   ref.cond = "LD",
                   deg.cond = "S",
                   method = "DE")
#==============================================================================
# This is the Balanced Discrimination Efficiency (BDE) test. This method tests
# each percentile of the reference distribution as a potential threshold for
# seperating the reference and degraded distributions.  I use the Classification
# Efficiency (CE) equation with a penalty factor to compare BDE sensitivity
# values. The penalty factor helps to reduce the difference between the 
# percentage of correctly identified reference (%Ref) and the percentage of
# correctly identified degraded (%Deg). We are aiming to find the most 
# BALANCED threshold.
# BDE = [(%Ref + %Deg) / 2] - |%Ref - % Deg|
# The actual sensitivity value reported is calculated using the CE equation.
# BDE = [(%Ref + %Deg) / 2]
# I don't think you will see major differences between the DE method and the 
# BDE method when comparing metric sensitivity.  I think the BDE method becomes
# beneficial when you apply the thresholds found using the BDE method to your
# scoring procedure.
sens.bde <- sensitivity(metrics.df,
                       first.metric = "Richness",
                       condition.colname = "ConditionAbrev",
                       ref.cond = "LD",
                       deg.cond = "S",
                       method = "BDE")
#==============================================================================
# This function runs all three sensitivity methods and combines the output into
# a single dataframe.
sens.all <- sensitivity(metrics.df,
                        first.metric = "Richness",
                        condition.colname = "ConditionAbrev",
                        ref.cond = "LD",
                        deg.cond = "S",
                        method = "ALL")



#==============================================================================
