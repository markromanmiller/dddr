vrmvrm
================

# About

The R package `vrmvrm` (pronouced *vroom-vroom*) is a tidyverse-style
toolset for working with spatial data. Unlike most packages working with
spatial data in R, we have focused on small-scale data rather than
geographic data. This is because the package creator has a background in
behavioral data collected virtual and augmented reality studies.

# Quick View

``` r
library(tidyverse)
library(gganimate)
library(vrmvrm)
```

``` r
spiral <- tibble(i = seq(0, 10*pi, 0.05)) %>%
  mutate(
    # vector3s are created using three numeric vector arguments,
    # and thanks to dplyr, can refer to other columns in the dataframe
    circular_part = vector3(x=cos(i), y=sin(i), z=0),
    forward_part = vector3(x=0, y=0, z=i/15),
    # vector3s can be added together and multiplied by numerics 
    spiral_part = circular_part * i / 30 + forward_part
  )

spiral %>% head(10)
```

    ## # A tibble: 10 x 4
    ##        i                              circular_part                forward_part
    ##    <dbl>                                  <vector3>                   <vector3>
    ##  1  0                                     (1, 0, 0)                   (0, 0, 0)
    ##  2  0.05 (0.998750260394966, 0.0499791692706783, 0) (0, 0, 0.00333333333333333)
    ##  3  0.1  (0.995004165278026, 0.0998334166468282, 0) (0, 0, 0.00666666666666667)
    ##  4  0.15  (0.988771077936042, 0.149438132473599, 0)                (0, 0, 0.01)
    ##  5  0.2   (0.980066577841242, 0.198669330795061, 0)  (0, 0, 0.0133333333333333)
    ##  6  0.25  (0.968912421710645, 0.247403959254523, 0)  (0, 0, 0.0166666666666667)
    ##  7  0.3    (0.955336489125606, 0.29552020666134, 0)                (0, 0, 0.02)
    ##  8  0.35  (0.939372712847379, 0.342897807455451, 0)  (0, 0, 0.0233333333333333)
    ##  9  0.4   (0.921060994002885, 0.389418342308651, 0)  (0, 0, 0.0266666666666667)
    ## 10  0.45   (0.900447102352677, 0.43496553411123, 0)                (0, 0, 0.03)
    ## # … with 1 more variable: spiral_part <vector3>

``` r
spiral %>%
  # field access uses the $ operator
  ggplot(aes(x=spiral_part$x, y=spiral_part$y)) +
  geom_point() +
  geom_path() +
  coord_equal()
```

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
spiral %>%
  mutate(
    # rotations can be specified using quaternions, axis / angle, or even from / to vectors
    spiral_rotated = rotate(spiral_part, axis=c(0, 1, 0), angle=pi/4)
  ) %>% 
  ggplot(aes(x=spiral_rotated$x, y=spiral_rotated$y)) +
  geom_point() +
  geom_path() +
  coord_equal()
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

# Contributions

Contributions are welcome\! The interest is not sufficiently large to be
a bandwidth issue for me, so the issue tracker can be used for bug
reports, feature requests, and questions - whatever you might need.
