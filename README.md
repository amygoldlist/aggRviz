
[![Build Status](https://travis-ci.org/amygoldlist/aggRviz.svg?branch=master)](https://travis-ci.org/amygoldlist/aggRviz)

<h1 align="center">aggRviz</h1>


<h4 align="center">A tool to work with and visualize data aggregated data</a>.</h4>

<h5 align="center">
Created by</a></h5>

<h4 align="center">

[Amy Goldlist](https://github.com/amygoldlist) &nbsp;&middot;&nbsp;
[Susan Fung](https://github.com/susan-fung) &nbsp;&middot;&nbsp;
[Fand Yang](https://github.com/fyang95)
</a></h4>


<br>
<h4 align="center">

[![Build Status](https://travis-ci.org/amygoldlist/aggRviz.svg?branch=master)](https://travis-ci.org/amygoldlist/aggRviz)
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
[![GitHub forks](https://img.shields.io/github/forks/amygoldlist/aggRviz.svg?style=social)](https://github.com/amygoldlist/aggRviz/network)&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
[![GitHub issues](https://img.shields.io/github/issues/amygoldlist/aggRviz.svg?style=social)](https://github.com/amygoldlist/aggRviz/issues)&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://github.com/amygoldlist/aggRviz/LICENSE)
&nbsp;&nbsp;&nbsp;&nbsp;
[![contributions welcome](https://img.shields.io/badge/contributions-welcome-brightgreen.svg?style=flat)](https://github.com/amygoldlist/aggRvviz/CONTRIBUTING.md)
</a></h4>
### To Install:

`devtools::install_github("amygoldlist/aggRviz")`

and to load:  `library(aggRviz)`

### Overview

The `aggRviz` package is used for analysis of aggregated data. Privacy concerns mean that much of the data we need to analyze is already aggregated and anonymized.  Often this means it has been aggregated in different ways, and all of them are included in a dataset.  In this case, and individual may show up in several rows: for example, all the males who live in California, and all of the males who own pets.  This package contains several tools to take data in aggregated form, and return a tidy dataframe, where each individual datapoint lies in exactly one row.


This package includes several functions:

* `aggR_possible(data,number = NULL,features = names(data), keep = TRUE, all_symbol = "")`: each sublist contains a set of features that can be filtered out.

	*Argument:*

  `data` - a data frame

  `number` - the number of dimensions, default = Null = the largest number of dimensions

  `keep` - Checks through a dataframe and a vector of features that can be kept or deleted

	*Value:*

  Returns a list of all combinations of dismensions.



* `aggRviz_filter2(data,col_2_delete = NULL, col_2_keep = NULL, features = NULL, all_symbol = "", fix_place = TRUE, places = c("State.or.Province", "Region", "Country"))`:

  *Arguments:*

    `data` - a data frame

    `col_2_delete = NULL` - This function filters out any row, stratified by those columns.

    `col_2_keep = NULL` - This function filters out any row, stratified by those columns.

    `features = NULL` - select the dimensions you want to keep or delete.

  *Value:*

    Return a data frame that filter out any unstratified rows from the other features.



* `filter_blanks(data, features = NULL, all_symbol = "")`:

  *Argument:*

    `data` - a dataframe

    `features` - features you selected

  *Value:*

    Create a filtered dataframe with no blanks


* `identify_measures(data, key = c("measure", "rate")`:

  *Argument:*

    `data` - a dataframe

    `key` - terms that key metrics includes

  *Value:*

    Return a vector of key metrics.


* `read_all_csv_skip_n((path,n=2, pattern = "*.csv"))`:

  *Argument:*

    `path` - a folder path

    `n` - the number of row you want to skip

    `pattern` - what kind of files you want to read

  *Value:*

    Return a list of all datasets.


## License

 [MIT License](LICENSE)

## Contributing

 Interested in contributing?
 See our [Contributing Guidelines](contributing.md) and [Code of Conduct](conduct.md).

 ---
 <h6 align="center">
 Created by

 [Amy Goldlist](https://github.com/amygoldlist) &nbsp;&middot;&nbsp;
 [Susan Fung](https://github.com/susan-fung) &nbsp;&middot;&nbsp;
 [Fang Yang](https://github.com/fyang95)
 </a></h4>
