<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/nutterb/pixiedust.svg?branch=master)](https://travis-ci.org/nutterb/pixiedust) ![](http://cranlogs.r-pkg.org/badges/grand-total/pixiedust) [![Coverage Status](https://coveralls.io/repos/nutterb/pixiedust/badge.svg?branch=master&service=github)](https://coveralls.io/github/nutterb/pixiedust?branch=master)

pixiedust
=========

After tidying up your analyses with the `broom` package, go ahead and grab the `pixiedust`. Customize your table output and write it to markdown, HTML, LaTeX, or even just the console. `pixiedust` makes it easy to customize the appearance of your tables in all of these formats by adding any number of "sprinkles", much in the same way you can add layers to a `ggplot`.

``` r
fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars)
library(pixiedust)
#> Additional documentation is being constructed at http://nutterb.github.io/pixiedust/index.html
dust(fit) %>% 
  sprinkle(col = 2:4, round = 3) %>% 
  sprinkle(col = 5, fn = quote(pvalString(value))) %>% 
  sprinkle_colnames(term = "Term", 
                    estimate = "Estimate", 
                     std.error = "SE",
                     statistic = "T-statistic", 
                     p.value = "P-value") %>%
  sprinkle_print_method("console")
#>            Term Estimate    SE T-statistic P-value
#> 1   (Intercept)    9.365 8.373       1.118    0.27
#> 2          qsec    1.245 0.383       3.252   0.003
#> 3   factor(am)1    3.151 1.941       1.624    0.12
#> 4            wt   -3.926 0.743      -5.286 < 0.001
#> 5 factor(gear)4   -0.268 1.655      -0.162    0.87
#> 6 factor(gear)5    -0.27 2.063      -0.131     0.9
```

### Customizing with Sprinkles

Tables can be customized by row, column, or even by a single cell by adding sprinkles to the `dust` object. The table below shows the currently planned and implemented sprinkles. In the "implemented" column, an 'x' indicates a customization that has been implemented, while a blank cell suggests that the customization is planned but has not yet been implemented. In the remaining columns, an 'x' indicates that the sprinkle is already implemented for the output format; an 'o' indicates that implementation is planned but not yet completed; and a blank cell indicates that the sprinkle will not be implemented (usually because the output format doesn't support the option).

| sprinkle          | implemented | console | markdown | html | latex |
|:------------------|:------------|:--------|:---------|:-----|:------|
| bg                | x           |         |          | x    | x     |
| bg\_pattern       | x           |         |          | x    | x     |
| bg\_pattern\_by   | x           |         |          | x    | x     |
| bold              | x           | x       | x        | x    | x     |
| bookdown          | x           |         |          |      | x     |
| border\_collapse  | x           |         |          | x    | x     |
| border            | x           |         |          | x    | x     |
| border\_thickness | x           |         |          | x    | x     |
| border\_units     | x           |         |          | x    | x     |
| border\_style     | x           |         |          | x    | x     |
| border\_color     | x           |         |          | x    | x     |
| caption           | x           | x       | x        | x    | x     |
| colnames          | x           | x       | x        | x    | x     |
| discrete          | x           |         |          | x    | x     |
| discrete\_colors  | x           |         |          | x    | x     |
| float             | x           |         |          |      | x     |
| fn                | x           | x       | x        | x    | x     |
| font\_color       | x           |         |          | x    | x     |
| font\_family      | x           |         |          | x    |       |
| font\_size        | x           |         |          | x    | x     |
| font\_size\_units | x           |         |          | x    | x     |
| gradient          | x           |         |          | x    | x     |
| gradient\_colors  | x           |         |          | x    | x     |
| gradient\_cut     | x           |         |          | x    | x     |
| gradient\_n       | x           |         |          | x    | x     |
| gradient\_na      | x           |         |          | x    | x     |
| halign            | x           |         |          | x    | x     |
| height            | x           |         |          | x    | x     |
| height\_units     | x           |         |          | x    | x     |
| hhline            | x           |         |          |      | x     |
| italic            | x           | x       | x        | x    | x     |
| justify           | x           |         |          | x    | x     |
| label             | x           |         |          | x    | x     |
| longtable         | x           | x       | x        | x    | x     |
| merge             | x           | x       | x        | x    | x     |
| na\_string        | x           | x       | x        | x    | x     |
| padding           | x           |         |          | x    |       |
| replace           | x           | x       | x        | x    | x     |
| round             | x           | x       | x        | x    | x     |
| rotate\_degree    | x           |         |          | x    | x     |
| sanitize          |             |         |          |      | x     |
| sanitize\_args    |             |         |          |      | x     |
| tabcolsep         |             |         |          |      | x     |
| valign            | x           |         |          | x    | x     |
| width             | x           |         |          | x    | x     |
| width\_units      | x           |         |          | x    | x     |

### A Brief Example

To demonstrate, let's look at a simple linear model. We build the model and generate the standard summary.

``` r
fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars)

summary(fit)
#> 
#> Call:
#> lm(formula = mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -3.5064 -1.5220 -0.7517  1.3841  4.6345 
#> 
#> Coefficients:
#>               Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)     9.3650     8.3730   1.118  0.27359    
#> qsec            1.2449     0.3828   3.252  0.00317 ** 
#> factor(am)1     3.1505     1.9405   1.624  0.11654    
#> wt             -3.9263     0.7428  -5.286 1.58e-05 ***
#> factor(gear)4  -0.2682     1.6555  -0.162  0.87257    
#> factor(gear)5  -0.2697     2.0632  -0.131  0.89698    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 2.55 on 26 degrees of freedom
#> Multiple R-squared:  0.8498, Adjusted R-squared:  0.8209 
#> F-statistic: 29.43 on 5 and 26 DF,  p-value: 6.379e-10
```

While the summary is informative and useful, it is full of "stats-speak" and isn't necessarily in a format that is suitable for publication or submission to a client. The `broom` package provides the summary in tidy format that, serendipitously, it a lot closer to what we would want for formal reports.

``` r
library(broom)
tidy(fit)
#>            term   estimate std.error  statistic      p.value
#> 1   (Intercept)  9.3650443 8.3730161  1.1184792 2.735903e-01
#> 2          qsec  1.2449212 0.3828479  3.2517387 3.168128e-03
#> 3   factor(am)1  3.1505178 1.9405171  1.6235455 1.165367e-01
#> 4            wt -3.9263022 0.7427562 -5.2861251 1.581735e-05
#> 5 factor(gear)4 -0.2681630 1.6554617 -0.1619868 8.725685e-01
#> 6 factor(gear)5 -0.2697468 2.0631829 -0.1307430 8.969850e-01
```

It has been observed by some, however, that even this summary isn't quite ready for publication. There are too many decimal places, the p-value employ scientific notation, and column titles like "statistic" don't specify what type of statistic. These kinds of details aren't the purview of `broom`, however, as `broom` is focused on tidying the results of a model for further analysis (particularly with respect to comparing slightly varying models).

The `pixiedust` package diverts from `broom`'s mission here and provides the ability to customize the `broom` output for presentation. The initial `dust` object returns a table that is similar to the `broom` output.

``` r
library(pixiedust)
dust(fit) %>%
  sprinkle_print_method("console")
#>            term   estimate std.error  statistic   p.value
#> 1   (Intercept)  9.3650443 8.3730161  1.1184792 0.2735903
#> 2          qsec  1.2449212 0.3828479  3.2517387 0.0031681
#> 3   factor(am)1  3.1505178 1.9405171  1.6235455 0.1165367
#> 4            wt -3.9263022 0.7427562 -5.2861251  1.58e-05
#> 5 factor(gear)4  -0.268163 1.6554617 -0.1619868 0.8725685
#> 6 factor(gear)5 -0.2697468 2.0631829  -0.130743  0.896985
```

Where `pixiedust` shows its strength is the ease of which these tables can be customized. The code below rounds the columns `estimate`, `std.error`, and `statistic` to three decimal places each, and then formats the `p.value` into a format that happens to be one that I like.

``` r
x <- dust(fit) %>% 
  sprinkle(col = 2:4, round = 3) %>% 
  sprinkle(col = 5, fn = quote(pvalString(value))) %>%
  sprinkle_print_method("console")
x
#>            term estimate std.error statistic p.value
#> 1   (Intercept)    9.365     8.373     1.118    0.27
#> 2          qsec    1.245     0.383     3.252   0.003
#> 3   factor(am)1    3.151     1.941     1.624    0.12
#> 4            wt   -3.926     0.743    -5.286 < 0.001
#> 5 factor(gear)4   -0.268     1.655    -0.162    0.87
#> 6 factor(gear)5    -0.27     2.063    -0.131     0.9
```

Now we're almost there! Let's change up the column names, and while we're add it, let's add some "bold" markers to the statistically significant terms in order to make them stand out some (I say "bold" because the console output doesn't show up in bold, but with the markdown tags for bold text. In a rendered table, the text would actually be rendered in bold).

``` r
x <- x %>% 
  sprinkle(col = c("estimate", "p.value"), 
           row = c(2, 4), 
           bold = TRUE) %>% 
  sprinkle_colnames(term = "Term", 
                estimate = "Estimate", 
                std.error = "SE",
                statistic = "T-statistic", 
                p.value = "P-value") %>%
  sprinkle_print_method("console")

x
#>            Term   Estimate    SE T-statistic     P-value
#> 1   (Intercept)    9.365   8.373       1.118      0.27  
#> 2          qsec  **1.245** 0.383       3.252   **0.003**
#> 3   factor(am)1    3.151   1.941       1.624      0.12  
#> 4            wt **-3.926** 0.743      -5.286 **< 0.001**
#> 5 factor(gear)4   -0.268   1.655      -0.162      0.87  
#> 6 factor(gear)5    -0.27   2.063      -0.131       0.9
```

A cool, free tip!
-----------------

The markdown output from `pixiedust` is somewhat limited due to the limitations of `Rmarkdown` itself. If/when more features become available for `Rmarkdown` output, I'll be sure to include them. But what can you do if you *really* want all of the flexibility of the HTML tables but need the MS Word document?

With a little help from the `Gmisc` package, you can have the best of both worlds. `Gmisc` isn't available on CRAN yet, but if you're willing to install it from GitHub, you can render a `docx` file. Install `Gmisc` with

`install.packages("Gmisc")`

Then use in your YAML header

    ---
    output: Gmisc::docx_document
    ---

When you knit your document, it knits as an HTML file, but I've had no problems with the rendering when I right-click the file and open with MS Word.

Read more at <http://gforge.se/2014/07/fast-track-publishing-using-rmarkdown/> (but note that this blog post was written about the `Grmd` package before it was moved into the `Gmisc` package).
