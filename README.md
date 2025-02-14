# whatswrapped

<!-- badges: start -->
<!-- badges: end -->

This repo currently contains two R scripts for WhatsApp chat analysis in R, and a sample txt file in WhatsApp exported format.

## rwa.R

This script contains code to read and clean WhatsApp exported chat data, and some visualizations for it. For reading data, `rwhatsapp` package is used.

## read.R

This script contains a function that is alternative of `rwhatsapp` package with some additional cleaning functions. However, it currently lacks the emoji lookup that `rwhatsapp` offer.  

## Download

You can download the script files in R with

``` r
download.file("https://raw.githubusercontent.com/aiktamseel/whatswrapped/refs/heads/main/read.R", destfile = "read.R") 
download.file("https://raw.githubusercontent.com/aiktamseel/whatswrapped/refs/heads/main/rwa.R", destfile = "rwa.R") 
```
