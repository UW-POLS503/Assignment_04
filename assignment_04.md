# Assignment 04

```r
knitr::opts_chunk$set(cache = TRUE, autodep = TRUE)
```

## Instructions

1. [Fork this repository](https://help.github.com/articles/using-pull-requests/) to your GitHub account.
2. Write your solutions in R Markdown in a file named `solutions.Rmd`.
3. When you are ready to submit your assignment, [initiate a pull request](https://help.github.com/articles/using-pull-requests/#initiating-the-pull-request). Title your
pull request "Submission".

To update your fork from the upstream repository:

1. On your fork, e.g. `https://github.com/jrnold/Assignment_04` click on "New Pull reqest"
2. Set your fork `jrnold/Assignment_04` as the base fork on the left, and `UW-POLS503/Assignment_04` as the head fork on the right. In both cases the branch will be master. This means, compare any chanes in the head fork that are not in the base fork. You will see differences between the `US-POLS503` repo and your fork. Click on "Create Pull Request", and if there are no issues, "Click Merge" A quick way is to use this link, but change the `jrnold` to your own username: `https://github.com/jrnold/Assignment_04/compare/master...UW-POLS503:master`.


# Libraries used 


```r
library("pols503")
library("rio")
library("ggplot2")
library("dplyr")
library("broom")
```
If you do not have the **pols503** package installed, you can install it with,

```r
library("devtools")
install_github("UW-POLS503/r-pols503")
```

# Interactions

In the social sciences the effect of a variable $x$ on another variable $y$ often varies depending on the context or, in other words, another variable $z$. For example, let's say we are interested in studying the effect that `income inequality` ($x$) has on `political mobilization` ($y$) at the country level. Our basic argument is that as income inequality increases, the number of protests in a country also increase. However, not all countries are created equal, and while some have democratic institutions, others are authoritarian regimes. Do we expect income inequality to have the same effect on political mobilization independently of the political institutions in place? Probably not. If we take a democracy scale (e.g. Polity IV), we can theorize that in highly authoritarian regimes the effect of income inequality on political mobilization is practically null because people is afraid of repression. As countries are less authoritarian, income inequality is likely to have a larger impact on political mobilization; and maybe, in countries with very strong and representative democratic institutions, income inequality has again a smaller effect on political mobilization because citizens have effective formal channels to address the issue.

# Data

In this assignment we will use replication data for Kastner's (2007) ["When Do Conflicting Political Relations Affect International Trade"](http://jcr.sagepub.com/content/51/4/664.abstract). The replication dataset is from Berry, Golder, and Smith's (2012) ["Improving Tests of Theories Positing Interaction"](http://mattgolder.com/files/research/jop2.pdf).

Use the `import()` function of the `rio` package to import the STATA file `TradConflict.dta`. The file contains 74,415 rows and 43 columns/variables. Each row contains information about a country pair or dyad. The authors built the dataset using information from 76 countries from 1960 to 1992. 


```r
db <- import("TradeConflict.dta")
```



# Theory

Kastner (2007) argues that "conflicting political interests between countries can have a detrimental effect on their economic relations" but that the "effects of international political conflict on trade are less severe in cases where internationalist economic interests have relatively strong political clout domestically."

Dependent Variable:

  - **Trade** (`lnrtrade`): The log of the bilateral trade between the two countries $i$ and $j$ in year $t$ (constant 1992 dollars).

Independent Variables of interest:

  - **Conflict** (`logUNsun`): Whether two countries have similar voting patterns in the UN General Assembly. 1 (most similar) to 3 (most dissimilar) scale.
  
  - **Trade Barriers** (`avpctBCFE`): Trade Barriers as a proxy for the domestic political power of economic elites with internationalist economic interests. Logged form of the Hiscox and Kastner (2002) index that evaluates trade barriers. Higher numbers mean more closed trade policies, range: {1.61 , 60.53}.

# Model

$Trade = \beta_{0} + \beta_{c}Conflict + \beta_{b} TradeBarriers + \beta_{cb}(Conflict \times TradeBarriers) + \beta Controls + \epsilon$ 

This model is similar to their Model 1 in Table 1 (p. 676):

```r
mod1 <- lm(lnrtrade ~ lnrpciab + avremote + landlocked + island + 
              landratio + pciratio + jointdem + laglnrtrade +
              lnrgdpab + lndist + logUNsun * avpctBCFE, 
           data = db)
```



**A:** Create a new variable `avpctBCFEcat3` by splitting the variable `avpctBCFE` into 3 categories. 

**B:** Run a new version of `mod1` (`mod1b`) but in this case ignore the interaction effect between the variables `logUNsun` and `avpctBCFE`, and substitute the variable `avpctBCFE` for the new categorical you just created.

**C:** Plot the predicted values of the model `mod1b` against the covariate `logUNsun`. Draw a linear regression line on it.

**D:** If you used `geom_point()` in the previous plot, you probably saw that there are a lot of data points. Replicate the same plot using `stat_binhex()` instead of `geom_point()`. You can find the documentation [here](http://docs.ggplot2.org/0.9.3/stat_binhex.html).

**E:** Take a look at the plot and at the coefficient for `logUNsun` in `mod1b`. What can you say about the relationship betweeh this covariate and the outcome variable `lnrtrade`?

**F:** Replicate the same plot (`logUNsun` v. fitted values of `mod1b`) but in this case use again `geom_point()` and color the dots differently depending on their values for `avpctBCFEcat3`. Make sure you also plot 3 different lines describing the relationship between `logUNsun` and the predicted values of `lnrtrade` for each group of `avpctBCFEcat3`. What do you see? How would you interpret this new plot?

**G:** Run a new model (`mod3`) similar to `mod2` but in this case interact the variables `logUNsun` and `avpctBCFE`.

**H:** Keeping all the control variables at their means, calculate the predicted values for the following scenarios:

| `logUNsun`     |     `avpctBCFE` |
|:---------|:-------|
| 0     | low |
| 1     | low |
| 0     | medium |
| 1     | medium |
| 0     | high |
| 1     | high |

**I:** Calculate the following:

    - `dif1`: Difference between the predicted values of scenarios 2 and 1: (`logUNsun` == 1 & `avpctBCFE` == low) - (`logUNsun` == 0 & `avpctBCFE` == low).
    - `dif2`: Difference between the predicted values of scenarios 2 and 1: (`logUNsun` == 1 & `avpctBCFE` == medium) - (`logUNsun` == 0 & `avpctBCFE` == medium).
    - `dif3`: Difference between the predicted values of scenarios 2 and 1: (`logUNsun` == 1 & `avpctBCFE` == high) - (`logUNsun` == 0 & `avpctBCFE` == high).
    - `dif4`: Difference between the predicted values of scenarios 3 and 1: (`logUNsun` == 0 & `avpctBCFE` == medium) - (`logUNsun` == 0 & `avpctBCFE` == low).
    - `dif5`: Difference between the predicted values of scenarios 5 and 1: (`logUNsun` == 0 & `avpctBCFE` == high) - (`logUNsun` == 0 & `avpctBCFE` == low).
    - `dif6`: Difference between `dif2` and `dif1`.
    - `dif7`: Difference between `dif3` and `dif1`.
 
**J:** Explain in your own words what do all these differences represent.

**K:** Create a dataset with all these differences

**L:** Create and print a table showing the `mod1` coefficients, standard errors, t-statistic and p.value for only the `Intercept` and the covariates: `logUnsun`, `avpctBCFEcat3`, and their interactions. 
