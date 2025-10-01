
[![R-CMD-check](https://github.com/mlampros/elmNNRcpp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mlampros/elmNNRcpp/actions/workflows/R-CMD-check.yaml)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/elmNNRcpp)](http://cran.r-project.org/package=elmNNRcpp)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/elmNNRcpp?color=blue)](http://www.r-pkg.org/pkg/elmNNRcpp)
[![status](https://tinyverse.netlify.app/badge/elmNNRcpp)](https://CRAN.R-project.org/package=elmNNRcpp)


## elmNNRcpp ( Extreme Learning Machine )
<br>

The *elmNNRcpp* package is a reimplementation of *elmNN* using *RcppArmadillo* after the [*elmNN* package was archived](https://CRAN.R-project.org/package=elmNN). Based on the documentation of the *elmNN* it consists of,
*"Training and predict functions for SLFN ( Single Hidden-layer Feedforward Neural Networks ) using the ELM algorithm. The ELM algorithm differs from the traditional gradient-based algorithms for very short training times ( it doesn't need any iterative tuning, this makes learning time very fast ) and there is no need to set any other parameters like learning rate, momentum, epochs, etc."*. More details can be found in the package Documentation, Vignette and [blog-post](http://mlampros.github.io/2018/07/05/the_extreme_learning_machine_package/).
<br><br>

To install the package from CRAN use, 

```R

install.packages("elmNNRcpp")


```
<br>

or download the latest version from Github using the *pak* package,

<br>

```R

pak::pak('mlampros/elmNNRcpp')


```
<br>

Use the following link to report bugs/issues,
<br><br>

[https://github.com/mlampros/elmNNRcpp/issues](https://github.com/mlampros/elmNNRcpp/issues)

<br>

### **Citation:**

If you use the code of this repository in your paper or research please cite both **elmNNRcpp** and the **original articles / software** `https://CRAN.R-project.org/package=elmNNRcpp`:

<br>

```R
@Manual{,
  title = {{elmNNRcpp}: The Extreme Learning Machine Algorithm},
  author = {Lampros Mouselimis},
  year = {2025},
  note = {R package version 1.0.5},
  url = {https://CRAN.R-project.org/package=elmNNRcpp},
}
```

<br>

