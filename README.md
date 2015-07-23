# dustpan
Publication ready tables using the broom package.

## Package Requirements

### General 

1. Add global option `options(dustpan_format = "[option]")` which may be one of `console`, `html`, `latex`, or `markdown`
2. Base object will be a data frame on which attributes may be added.
3. Attributes, such as cell coloring, bold text, italics, and borders may be added by cell, by row, or by column
4. Use a theme object like `ggplot2` to allow complex themes to be applied with ease
5. Additional attributes are not added until the object is printed. This will allow the objec to be printed 
  in any of the formats by simply changing an option.
6. Arguments for custom column names
7. Methods for all of the broom tidiers plus `matrix`, `data_frame`, `tbl_df`, and `data.table`
8. `print` method relies on `options(dustpan_format)`, but may be overridden manual by using `print(x, format = [format])` 
  (or should it be `obj + theme(format = [format])`?)

### Console Output

1. Effectively a reprint of the `broom` output, but with report-ready column names
2. Allow an option for `glance` statistics to be printed under the table.
3. Ignores cell coloring, borders
4. bold and italics are represented by `**text**` and `_text_`, respectively
5. Glance statistics are shown in a separate table under the output table

### Markdown Output

1. Accept bold text, italics.
2. Return a message that cell coloring and borders are not supported by markdown?
3. Glance statistics are shown in a separate table under the output table

### HTML Output

1. Accept bold text, italics, cell coloring, and borders.
2. Borders may accept thickness and style arguments
3. Accept a CSS name in place of a theme
4. Allow multicolumn cells. Cells that are displaced by a multicolumn are represented as NA
5. Glance statistics are shown in multicol cells under the table

### LaTeX Output

1. Use `longtable` by default?
2. Accept bold text, italics, cell coloring, and borders.
3. Allow multicolumn cells. Cells that are displaced by a multicolumn are represented as NA
4. Glance statistics are shown in multicol cells under the table


## Object Components
* cell_attributes: a tidy data frame with columns `x`, `y`, `bold`, `italic`, `bg`, `left_border`, `right_border`, 
  `bottom_border`, `top_border`, `round`.  Each of the columns after `x` and `y` is logical (except for `round`)
* table_attributes: CSS specifications?  captions? What else goes here?
* glance_attributes: a data frame with columns `glance_stat` (character), `label` (character), `display` (logical), 
  `round` (numeric)
* header_attributes: similar to cell attributes, but applies only to column headings
* longtable attributes: for LaTeX tables, defines `endfoot`, `endlastfoot`, and where to find `endhead`.  
  For HTML, markdown, and console, also defines how many rows before a table break.
