# Exploring changes in interest on the basis of youths' momentary engagement in summer STEM enrichment programs

## Background

Authors: Joshua M. Rosenberg, Patrick N. Beymer, Thomas M. Houslay, and Jennifer A. Schmidt

### Purpose and Theoretical Framework

A strength of intensive data collection methods like the Experience Sampling Method (ESM; Hektner, Schmidt, & Csikszentmihalyi, 2007) is that they enable researchers to examine participants’ immediate experience and model changes in these experiences over time while accounting for multiple dependencies in the data. A current analytic challenge for researchers using this type of data involves examining how participants’ responses repeatedly measured throughout data collection cumulatively relate to some single, longer-term outcome. 

Recently, out-of-school-time programs focusing on STEM  have proliferated to combat declines in STEM interest during adolescence (National Academy of Engineering and National Research Council, 2014). Though many have argued that contexts for learning outside of school have an important role to play in youths’ development of interest (Hidi, Renninger, & Krapp, 2016), relatively little is known about whether and how youths’ interest develops in such contexts. Contemporary motivational theory suggests that interests emerge from the interactions of an individual in a particular environment, rather than residing entirely within the individual (Hidi et al., 2016). Thus it is essential to understand the ways that individuals engage with STEM-focused environments to know how STEM-related interests may emerge. 

### Method & Data Analysis

The purpose of this study was to explore the utility of a particular analytic method for testing the effects of sustained engagement in summer STEM programs on the development of youth’s interest over time. The participants for the study were 203 racially and ethnically diverse youth in the Northeast United States. To determine how youths’ in-the-moment engagement related to their individual interest in STEM, a multivariate model estimated with tools familiar to Bayesian methods (namely, Markov Chain Monte Carlo [MCMC]). The model was estimated using the MCMCglmm R package (Hadfield, 2010) and includes both youths’ in-the-moment engagement (measured via ESM) and their post-program interest in a single, multivariate model. This approach contrasts with a multilevel modeling approach, for which two separate models are specified, which can contribute to overconfident inferences about effects (Houslay & Wilson, 2017). Also, a feature of MCMC is that its use allows for the recognition of complex data structures, which can be challenging to do when using a latent variable modeling approach.

### Results & Significance

The analysis showed that youths’ in-the-moment engagement was a significant, positive predictor (effect size r = .27) of youths’ post-program interest in STEM, accounting for their initial interest in STEM, their gender, and the nesting and cross-classification of youths’ responses. This effect is more conservative than that found as part of analyses carried out with separate models (r = .34; Rosenberg, Beymer, & Schmidt, 2018). Future work can use an extension of the methodological approach to examine both the (between-youth) effects of engagement upon post-program interest and (within-youth) relations between engagement and its rate of change and the effects of both upon interest development. This study demonstrates how embracing MCMC and Bayesian methods may be a natural fit for analyses when a goal is to embrace and study multivariate motivational processes in real-world settings that lead to novel insights into learning and development. 

## Analysis / files

- See the `lme4-and-mcmcglmm-analysis.Rmd` file for the main analysis. See `process-raw-data` for the data that is used to create the processed data file.

- See `process-raw-data.R` for the script that processes the data. Right now, the raw data is not added to Git.
