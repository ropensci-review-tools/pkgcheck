# pkgreport

<!-- badges: start -->

[![R build
status](https://github.com/mpadge/pkgreport/workflows/R-CMD-check/badge.svg)](https://github.com/mpadge/pkgreport/actions)
[![Travis build
status](https://travis-ci.org/mpadge/pkgreport?branch=master)](https://travis-ci.org/mpadge/pkgreport)
[![Project Status:
Concept](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
<!-- badges: end -->

Plumber API to report on package structure and function. Uses
functionality provided by the [`r-lib/pkgapi`
package](https://github.com/r-lib/pkgapi), which is not on CRAN and must
first be installed with

``` r
source("https://install-github.me/r-lib/pkgapi")
```

(or `remotes::install_github()` will also work).

    ## Loading pkgreport

``` r
library (pkgreport)
```

## Usage

Set up a local host at specified port:

``` r
port <- 8000
ps <- serve_api (port = port)
```

The returned object, `ps`, is a
[`processx`](https://github.com/r-lib/processx) object identifying a
background R process hosting the local API. This process must be
manually killed once finished, as demonstrated below.

A specified repository can then be locally downloaded with the `pr_dl()`
function, and a corresponding report on the software generated with
`pr_report()`:

``` r
dl <- pr_dl ("https://github.com/ropensci/osmdata", port = port)
res <- pr_report (dl, port = port)
res
```

    ##  [1] "## osmdata"                                                                                                                                                                                                
    ##  [2] ""                                                                                                                                                                                                          
    ##  [3] "The  package has 26 exported functions, and 57\n                           non-exported funtions. The exported functions are structured into the following 2primary clusters containing 16 and 5 functions"
    ##  [4] ""                                                                                                                                                                                                          
    ##  [5] "| cluster|  n|name             | centrality|"                                                                                                                                                              
    ##  [6] "|-------:|--:|:----------------|----------:|"                                                                                                                                                              
    ##  [7] "|       1|  1|bbox_to_string   |         31|"                                                                                                                                                              
    ##  [8] "|       1|  2|getbb            |         24|"                                                                                                                                                              
    ##  [9] "|       1|  3|overpass_status  |         14|"                                                                                                                                                              
    ## [10] "|       1|  4|opq              |          8|"                                                                                                                                                              
    ## [11] "|       1|  5|add_osm_feature  |         NA|"                                                                                                                                                              
    ## [12] "|       1|  6|get_overpass_url |         NA|"                                                                                                                                                              
    ## [13] "|       1|  7|opq_osm_id       |         NA|"                                                                                                                                                              
    ## [14] "|       1|  8|opq_string       |         NA|"                                                                                                                                                              
    ## [15] "|       1|  9|osm_poly2line    |         NA|"                                                                                                                                                              
    ## [16] "|       1| 10|osmdata          |         NA|"                                                                                                                                                              
    ## [17] "|       1| 11|osmdata_sc       |         NA|"                                                                                                                                                              
    ## [18] "|       1| 12|osmdata_sf       |         NA|"                                                                                                                                                              
    ## [19] "|       1| 13|osmdata_sp       |         NA|"                                                                                                                                                              
    ## [20] "|       1| 14|osmdata_xml      |         NA|"                                                                                                                                                              
    ## [21] "|       1| 15|set_overpass_url |         NA|"                                                                                                                                                              
    ## [22] "|       1| 16|trim_osmdata     |         NA|"                                                                                                                                                              
    ## [23] ""                                                                                                                                                                                                          
    ## [24] "| cluster|  n|name              | centrality|"                                                                                                                                                             
    ## [25] "|-------:|--:|:-----------------|----------:|"                                                                                                                                                             
    ## [26] "|       2|  1|osm_lines         |         NA|"                                                                                                                                                             
    ## [27] "|       2|  2|osm_multilines    |         NA|"                                                                                                                                                             
    ## [28] "|       2|  3|osm_multipolygons |         NA|"                                                                                                                                                             
    ## [29] "|       2|  4|osm_points        |         NA|"                                                                                                                                                             
    ## [30] "|       2|  5|osm_polygons      |         NA|"                                                                                                                                                             
    ## [31] ""                                                                                                                                                                                                          
    ## [32] "There are also 5 isolated functions:"                                                                                                                                                                      
    ## [33] "|  n|name               | loc|"                                                                                                                                                                            
    ## [34] "|--:|:------------------|---:|"                                                                                                                                                                            
    ## [35] "|  1|available_features |  13|"                                                                                                                                                                            
    ## [36] "|  2|available_tags     |  21|"                                                                                                                                                                            
    ## [37] "|  3|osm_elevation      |  21|"                                                                                                                                                                            
    ## [38] "|  4|unique_osmdata     |  23|"                                                                                                                                                                            
    ## [39] "|  5|unname_osmdata_sf  |  12|"

Finally, kill the `processx` process hosting the local API:

``` r
ps
```

    ## PROCESS 'R', running, pid 27247.

``` r
chk <- ps$kill()
ps
```

    ## PROCESS 'R', finished.
