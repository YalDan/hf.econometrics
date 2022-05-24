High Frequency Econometrics Package in R
================
Saef, Danial
11/05/2022

This library serves as a companion to the publication “Understanding
Jumps in High Frequency Digital Asset Markets”. However it can also be
used independently for clustering high dimensional datasets and fitting
an implied stochastic volatility model.

# 1 Methodology

This library contains implementations of a few recent publications in
the field of High Frequency Econometrics:

-   Lee & Mykland Jump Test (Lee and Mykland 2012)
-   Ait-Sahalia & Jacod Jump Test & Test for Jump Activity + variation
    estimation (Aït-Sahalia and Jacod 2012)
-   Ait-Sahalia, Jacod & Li Jump Test (Ait-Sahalia, Jacod, and Li 2012)
-   Pre-averaging approach ( Jacod et al. (2009), Jacod, Podolskij, and
    Vetter (2010) )

The methods can be used in a stand-alone fashion or when obtained from
the Blockchain Research Center with additional functionalities.

# 2 Usage

## 2.1 Installing

The usage is pretty simple. First, install the package with `devtools`.
Note that this library is still experimental, s.t. no proper unit
testing or object classes have been implemented yet. In case of bugs
please report them and I will work on fixing them.

``` r
library(devtools)
install_github("YalDan/hf.econometrics")
library(hf.econometrics)
```

## 2.2 Running the model

Now we can just load a suitable dataset and run the test statistics we
desire. A sample dataset is provided to illustrate the necessary file
structure.

``` r
# load the data
# currently, for this to work the data needs to be stored in "./data/raw/csv_dump/"
DT_list <- make_data("./data/raw/csv_dump/DT_sample.csv")
DT_split_list <-  list("impute" = split_by_id(DT_list),
                                "no_impute" =  split_by_id(DT_list))
```

Once the data is loaded we can calculate the jump test statistic:

``` r
## get LM result ##
DT_LM_result_id <- jump_test(DT_split_list$no_impute, which_test = "LM_JumpTest")

## get AJL result ##
DT_AJL_result_id <- jump_test(DT_split_list$impute, which_test = "AJL_JumpTest")
```

Finally, some processing can be made to denoise the jump statistic.

``` r
## Preprocess LM result ##
DT_jumps_crypto <- preprocess_jump_data(DT_LM_result_id, sign_level = 0.01)
```

# 3 References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-ait-sahalia_testing_2012" class="csl-entry">

Ait-Sahalia, Yacine, Jean Jacod, and Jia Li. 2012. “Testing for Jumps in
Noisy High Frequency Data.” *Journal of Econometrics* 168 (2): 207–22.
<https://econpapers.repec.org/article/eeeeconom/v_3a168_3ay_3a2012_3ai_3a2_3ap_3a207-222.htm>.

</div>

<div id="ref-ait-sahalia_analyzing_2012" class="csl-entry">

Aït-Sahalia, Yacine, and Jean Jacod. 2012. “Analyzing the Spectrum of
Asset Returns: Jump and Volatility Components in High Frequency Data.”
*Journal of Economic Literature* 50 (4): 1007–50.
<https://www.jstor.org/stable/23644910>.

</div>

<div id="ref-jacod_microstructure_2009" class="csl-entry">

Jacod, Jean, Yingying Li, Per A. Mykland, Mark Podolskij, and Mathias
Vetter. 2009. “Microstructure Noise in the Continuous Case: The
Pre-Averaging Approach.” *Stochastic Processes and Their Applications*
119 (7): 2249–76. <https://doi.org/10.1016/j.spa.2008.11.004>.

</div>

<div id="ref-jacod_limit_2010" class="csl-entry">

Jacod, Jean, Mark Podolskij, and Mathias Vetter. 2010. “Limit Theorems
for Moving Averages of Discretized Processes Plus Noise.” *The Annals of
Statistics* 38 (3): 1478–1545. <https://doi.org/10.1214/09-AOS756>.

</div>

<div id="ref-lee_jumps_2012" class="csl-entry">

Lee, Suzanne S., and Per A. Mykland. 2012. “Jumps in Equilibrium Prices
and Market Microstructure Noise.” {SSRN} {Scholarly} {Paper} 1693644.
Rochester, NY: Social Science Research Network.
<https://papers.ssrn.com/abstract=1693644>.

</div>

</div>
