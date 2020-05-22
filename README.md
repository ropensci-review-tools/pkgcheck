# pkgreport

<!-- badges: start -->

[![R build
status](https://github.com/mpadge/pkgreport/workflows/R-CMD-check/badge.svg)](https://github.com/mpadge/pkgreport/actions?query=workflow%3AR-CMD-check)
[![gitlab
push](https://github.com/mpadge/pkgreport/workflows/push-to-gitlab/badge.svg)](https://github.com/mpadge/pkgreport/actions?query=workflow%3Apush-to-gitlab)
[![Travis build
status](https://travis-ci.org/mpadge/pkgreport.svg?branch=master)](https://travis-ci.org/mpadge/pkgreport)
[![Project Status:
Concept](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
<!-- badges: end -->

Plumber API to report on package structure and function. Uses
functionality provided by the packages
[`r-lib/pkgapi`](https://github.com/r-lib/pkgapi) and
[`packgraph`](https://github.com/mpadge/packgraph) which are not on CRAN
and must first be installed with

``` r
source("https://install-github.me/r-lib/pkgapi") #or `remotes::install_github()`
remotes::install_github("mpadge/packgraph")
```

The package can then be loaded the usual way:

``` r
library (pkgreport)
```

Also uses the [github GraphQL API](https://developer.github.com/v4)
which requires a local github token to be stored with an unambiguous
name including `GITHUB` and maybe `QL`, if alternative `GITHIB` tokens
already exist. This can be obtained from github (via your user
settings), and stored using

``` r
Sys.setenv ("GITHUB_QL" = "<my_token>")
```

This can also be set permanently by putting this line in your
`~/.Renviron` file (or creating this if it does not yet exist).

The package also works by locally caching previously analysed packages,
in a `pkgreport` subdirectory of the location determined by

``` r
rappdirs::user_cache_dir()
```

You may manually erase the contents of this subdirectory at any time at
no risk.

## Usage

Set up a local host at specified port, currently only configured to
serve on `localhost`:

``` r
port <- 8000
ps <- serve_api (port = port)
```

The returned object, `ps`, is a
[`processx`](https://github.com/r-lib/processx) object identifying a
background R process hosting the local API. This process must be
manually killed once finished, as demonstrated below.

A report on the structure of a specified software repository can then be
generated with the `pr_report()` function:

``` r
u <- "https://github.com/ropensci/osmdata"
res <- pr_report (u, port = port)
res
```

    ##  [1] "## osmdata"                                                                                                                                                                     
    ##  [2] ""                                                                                                                                                                               
    ##  [3] "The  package has 26 exported functions, and 57 non-exported funtions. The exported functions are structured into the following 2 primary clusters containing 16 and 5 functions"
    ##  [4] ""                                                                                                                                                                               
    ##  [5] "| cluster|  n|name             | num_params| num_doc_words| num_doc_lines| num_example_lines| centrality|"                                                                      
    ##  [6] "|-------:|--:|:----------------|----------:|-------------:|-------------:|-----------------:|----------:|"                                                                      
    ##  [7] "|       1|  1|bbox_to_string   |          1|            71|            47|                 3|         31|"                                                                      
    ##  [8] "|       1|  2|getbb            |          9|           512|             3|                22|         24|"                                                                      
    ##  [9] "|       1|  3|overpass_status  |          1|            35|            10|                 0|         14|"                                                                      
    ## [10] "|       1|  4|opq              |          5|           328|             0|                14|          8|"                                                                      
    ## [11] "|       1|  5|add_osm_feature  |          7|           464|             0|                16|         NA|"                                                                      
    ## [12] "|       1|  6|get_overpass_url |          0|           103|             0|                 0|         NA|"                                                                      
    ## [13] "|       1|  7|opq_osm_id       |          3|           323|             1|                13|         NA|"                                                                      
    ## [14] "|       1|  8|opq_string       |          1|           102|             3|                 2|         NA|"                                                                      
    ## [15] "|       1|  9|osm_poly2line    |          1|           407|             0|                10|         NA|"                                                                      
    ## [16] "|       1| 10|osmdata          |          8|           658|             0|                 0|         NA|"                                                                      
    ## [17] "|       1| 11|osmdata_sc       |          3|           223|            17|                 5|         NA|"                                                                      
    ## [18] "|       1| 12|osmdata_sf       |          4|            76|             6|                 5|         NA|"                                                                      
    ## [19] "|       1| 13|osmdata_sp       |          3|            76|             0|                 5|         NA|"                                                                      
    ## [20] "|       1| 14|osmdata_xml      |          4|           246|            75|                 5|         NA|"                                                                      
    ## [21] "|       1| 15|set_overpass_url |          1|           565|             0|                 0|         NA|"                                                                      
    ## [22] "|       1| 16|trim_osmdata     |          3|           159|             9|                14|         NA|"                                                                      
    ## [23] ""                                                                                                                                                                               
    ## [24] "| cluster|  n|name              | num_params| num_doc_words| num_doc_lines| num_example_lines| centrality|"                                                                     
    ## [25] "|-------:|--:|:-----------------|----------:|-------------:|-------------:|-----------------:|----------:|"                                                                     
    ## [26] "|       2|  1|osm_lines         |          2|           223|             1|                14|         NA|"                                                                     
    ## [27] "|       2|  2|osm_multilines    |          2|           289|             0|                12|         NA|"                                                                     
    ## [28] "|       2|  3|osm_multipolygons |          2|           200|            13|                11|         NA|"                                                                     
    ## [29] "|       2|  4|osm_points        |          2|            52|             7|                 6|         NA|"                                                                     
    ## [30] "|       2|  5|osm_polygons      |          2|           235|             0|                 8|         NA|"                                                                     
    ## [31] ""                                                                                                                                                                               
    ## [32] "There are also 5 isolated functions:"                                                                                                                                           
    ## [33] "|  n|name               | loc|"                                                                                                                                                 
    ## [34] "|--:|:------------------|---:|"                                                                                                                                                 
    ## [35] "|  1|available_features |  13|"                                                                                                                                                 
    ## [36] "|  2|available_tags     |  21|"                                                                                                                                                 
    ## [37] "|  3|osm_elevation      |  21|"                                                                                                                                                 
    ## [38] "|  4|unique_osmdata     |  23|"                                                                                                                                                 
    ## [39] "|  5|unname_osmdata_sf  |  12|"                                                                                                                                                 
    ## [40] ""                                                                                                                                                                               
    ## [41] "### Documentation of non-exported functions:"                                                                                                                                   
    ## [42] ""                                                                                                                                                                               
    ## [43] "|value  | doclines| cmtlines|"                                                                                                                                                  
    ## [44] "|:------|--------:|--------:|"                                                                                                                                                  
    ## [45] "|mean   |      9.6|      2.7|"                                                                                                                                                  
    ## [46] "|median |      6.0|      0.0|"                                                                                                                                                  
    ## [47] ""

Finally, kill the `processx` process hosting the local API:

``` r
ps
```

    ## PROCESS 'R', running, pid 3200.

``` r
chk <- ps$kill()
ps
```

    ## PROCESS 'R', finished.
