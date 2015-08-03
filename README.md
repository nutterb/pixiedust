[![Travis-CI Build Status](https://travis-ci.org/nutterb/pixiedust.svg?branch=master)](https://travis-ci.org/nutterb/pixiedust)

# pixiedust
After tidying up your analyses with the `broom` package, go ahead and grab the `pixiedust`.  Customize your table output and write it to markdown, HTML, LaTeX, or even just the console.  `pixiedust` makes it easy to customize the appearance of your tables in all of these formats by adding any number of "sprinkles", much in the same way you can add layers to a `ggplot`.

```r
> fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars)
> library(pixiedust)
> dust(fit) + 
+   sprinkle(col = 2:4, round = 3) + 
+   sprinkle(col = 5, fn = quote(pvalString(value))) + 
+   sprinkle_colnames(term = "Term", 
+                     estimate = "Estimate", 
+                      std.error = "SE",
+                      statistic = "T-statistic", 
+                      p.value = "P-value")
```
```
           Term Estimate    SE T-statistic P-value
1   (Intercept)    9.365 8.373       1.118    0.27
2          qsec    1.245 0.383       3.252   0.003
3   factor(am)1    3.151 1.941       1.624    0.12
4            wt   -3.926 0.743      -5.286 < 0.001
5 factor(gear)4   -0.268 1.655      -0.162    0.87
6 factor(gear)5    -0.27 2.063      -0.131     0.9
```

### Customizing with Sprinkles

Tables can be customized by row, column, or even by a single cell by adding sprinkles to the `dust` object.  The table below shows the currently planned and implemented sprinkles.  In the "implemented" column, an 'x' indicates a customization that has been implemented, while a blank cell suggests that the customization is planned but has not yet been implemented.  In the remaining columns, an 'x' indicates that the sprinkle is already implemented for the output format; an 'o' indicates that implementation is planned but not yet completed; and a blank cell indicates that the sprinkle will not be implemented (usually because the output format doesn't support the option).

|dustbunny        | implemented | console | markdown | html | latex |
|:----------------|:-----------:|:-------:|:--------:|:----:|:-----:|
|`bg` (background)  |      x      |         |          |  x   |   o   |
|`bg_pattern`       |      x      |         |          |  x   |   o   |
|`bg_pattern_by`    |      x      |         |          |  x   |   o   |
|`bold`             |      x      |    x    |    x     |  x   |   o   |
|`border_collapse`  |      x      |         |          |  x   |       |     
|`border`           |      x      |         |          |  x   |   o   |
|`border_thickness` |      x      |         |          |  x   |       |
|`border_units`     |      x      |         |          |  x   |       |
|`border_style`     |      x      |         |          |  x   |       |
|`border_color`     |      x      |         |          |  x   |       |
|`colnames`         |      x      |    x    |    x     |  x   |   o   |
|`fn`               |      x      |    x    |    x     |  x   |   o   |
|`font_color`       |      x      |         |          |  x   |   o   |
|`font_size`        |      x      |         |          |  x   |   o   |
|`font_size_units`  |      x      |         |          |  x   |   o   |
|`halign`           |      x      |         |          |  x   |   o   |
|`height`           |      x      |         |          |  x   |   o   |
|`height_units`     |      x      |         |          |  x   |   o   |
|`italic`           |      x      |    x    |    x     |  x   |   o   |
|`pad`              |             |         |          |  o   |       |
|`round`            |      x      |    x    |    x     |  x   |   o   |
|`rotate_degree`    |             |         |          |  o   |   o   |
|`valign`           |      x      |         |          |  x   |   o   |
|`width`            |      x      |         |          |  x   |   o   |
|`width_units`      |      x      |         |          |  x   |   o   |


### A Brief Example

To demonstrate, let's look at a simple linear model.  We build the model and generate the standard summary.  

```r
> fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars)
> 
> summary(fit)
```
```
Call:
lm(formula = mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars)

Residuals:
    Min      1Q  Median      3Q     Max 
-3.5064 -1.5220 -0.7517  1.3841  4.6345 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)     9.3650     8.3730   1.118  0.27359    
qsec            1.2449     0.3828   3.252  0.00317 ** 
factor(am)1     3.1505     1.9405   1.624  0.11654    
wt             -3.9263     0.7428  -5.286 1.58e-05 ***
factor(gear)4  -0.2682     1.6555  -0.162  0.87257    
factor(gear)5  -0.2697     2.0632  -0.131  0.89698    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 2.55 on 26 degrees of freedom
Multiple R-squared:  0.8498,	Adjusted R-squared:  0.8209 
F-statistic: 29.43 on 5 and 26 DF,  p-value: 6.379e-10
```

While the summary is informative and useful, it is full of "stats-speak" and isn't necessarily in a format that is suitable for publication or submission to a client.  The `broom` package provides the summary in tidy format that, serendipitously, it a lot closer to what we would want for formal reports.

```r
> library(broom)
> tidy(fit)
```

```
           term   estimate std.error  statistic      p.value
1   (Intercept)  9.3650443 8.3730161  1.1184792 2.735903e-01
2          qsec  1.2449212 0.3828479  3.2517387 3.168128e-03
3   factor(am)1  3.1505178 1.9405171  1.6235455 1.165367e-01
4            wt -3.9263022 0.7427562 -5.2861251 1.581735e-05
5 factor(gear)4 -0.2681630 1.6554617 -0.1619868 8.725685e-01
6 factor(gear)5 -0.2697468 2.0631829 -0.1307430 8.969850e-01
```

It has been observed by some, however, that even this summary isn't quite ready for publication.  There are too many decimal places, the p-value employ scientific notation, and column titles like "statistic" don't specify what type of statistic.  These kinds of details aren't the purview of `broom`, however, as `broom` is focused on tidying the results of a model for further analysis (particularly with respect to comparing slightly varying models).

The `pixiedust` package diverts from `broom`'s mission here and provides the ability to customize the `broom` output for presentation.  The initial `dust` object returns a table that is largely similar to the `broom` output.  Truthfully, it may be less desirable because it has converted all of those numerical values into character strings.  This has the consequence of losing the numerical formatting employed by printing a data frame.

```r
> library(pixiedust)
> dust(fit)
```

```
           term           estimate         std.error          statistic              p.value
1   (Intercept)   9.36504430865836  8.37301612033658   1.11847919245161    0.273590282784449
2          qsec   1.24492121340088 0.382847869162145    3.2517386504602  0.00316812765022556
3   factor(am)1   3.15051775932893  1.94051711270669   1.62354546563854    0.116536745986852
4            wt   -3.9263021501002 0.742756198609962   -5.2861250534807 1.58173505907644e-05
5 factor(gear)4 -0.268163000929796  1.65546166120695 -0.161986838604456    0.872568516561885
6 factor(gear)5 -0.269746805223248   2.0631829212229 -0.130743039043461    0.896984955536724
```

Where `pixiedust` shows its strength is the ease of which these tables can be customized.  The code below rounds the columns `estimate`, `std.error`, and `statistic` to three decimal places each, and then formats the `p.value` into a format that happens to be one that I like.

```r
> x <- dust(fit) + 
+   sprinkle(col = 2:4, round = 3) + 
+   sprinkle(col = 5, fn = quote(pvalString(value)))
> 
> x
```

```
           term estimate std.error statistic p.value
1   (Intercept)    9.365     8.373     1.118    0.27
2          qsec    1.245     0.383     3.252   0.003
3   factor(am)1    3.151     1.941     1.624    0.12
4            wt   -3.926     0.743    -5.286 < 0.001
5 factor(gear)4   -0.268     1.655    -0.162    0.87
6 factor(gear)5    -0.27     2.063    -0.131     0.9
```

Now we're almost there!  Let's change up the column names, and while we're add it, let's add some "bold" markers to the statistically significant terms in order to make them stand out some (I say "bold" because the console output doesn't show up in bold, but with the markdown tags for bold text.  In a rendered table, the text would actually be rendered in bold).

```r
> x <- x + 
+   sprinkle(col = c("estimate", "p.value"), 
+            row = c(2, 4), 
+            bold = TRUE) + 
+   dust_colnames(term = "Term", 
+                 estimate = "Estimate", 
+                 std.error = "SE",
+                 statistic = "T-statistic", 
+                 p.value = "P-value")
> 
> x
```
```
           Term   Estimate    SE T-statistic     P-value
1   (Intercept)    9.365   8.373       1.118      0.27  
2          qsec  **1.245** 0.383       3.252   **0.003**
3   factor(am)1    3.151   1.941       1.624      0.12  
4            wt **-3.926** 0.743      -5.286 **< 0.001**
5 factor(gear)4   -0.268   1.655      -0.162      0.87  
6 factor(gear)5    -0.27   2.063      -0.131       0.9  
```

### Upcoming Developments

We're just getting started!  While there are a number of customizations already available or planned, here are some other ideas that I hope to implement in the future.

1. Support for multi-row table headings
2. Support for `longtable`s in LaTeX (and a similar rendering in markdown and HTML).  
3. An option to use `broom`'s `glance` output in the table foot.
4. Multicolumn and multirow support
5. Perhaps an `htmlTables` engine
6. Option to add variable labels to `broom` output (if labels were given using the `Hmisc::label` functions
7. Option to add factor levels as a separate column to `broom` output

Are there other features you want or need?  Please submit an issue, or contribute functionality yourself.  

### Development Schedule
| Version | Release Description                                | Target Date | Acutal Date |
|--------:|:---------------------------------------------------|------------:|-------------|
|**0.1.0**| Console, markdown and HTML output for simple table | 1 Aug 2015  | |
| 0.2.0   | Multirow table headers; footers; multipage tables  | 20 Aug 2015 | |
| 0.3.0   | Multicolumn and multirow cells in HTML             | 15 Sep 2015 | |
| 0.4.0   | Glance statistics in table footer                  | 1 Oct 2015  | |
|**0.5.0**| LaTeX output for simple table                      | 15 Oct 2015 | |
| 0.6.0   | Borders and backgrounds for LaTeX tables           | 1 Nov 2015  | |
| 0.7.0   | Multicolumn and multirow support for LaTeX tables  | 1 Dec 2015  | |
|**0.8.0**| Longtable support for LaTeX tables                 |   Jan 2016  | |
| 0.9.0   | Adjustable cell heights and widths in LaTeX tables |   Feb 2016  | |
| 0.10.0  | Rotated text for LaTeX tables                      |   Mar 2016  | |
| 0.11.0  | Add variable labels and levels to `broom` output   |   Apr 2016  | |
| 0.12.0  | Add themes (cobwebs?) for batch customizations     |   Apr 2016  | |
|**1.0.0**| Release of basic, stable package                   |   Jun 2016  | |

**bold version numbers** indicate a planned release to CRAN.
