Tidy Data
================

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

## Pivot Longer

Load the PULSE data

``` r
pulse_data = 
  haven::read_sas("./data_import_examples/public_pulse_data.sas7bdat") %>% 
  janitor::clean_names()
```

### Wide format to long format

``` r
pulse_tidy_df = 
  pivot_longer(
    pulse_data, 
    bdi_score_bl:bdi_score_12m,
    names_to = "visit", 
    names_prefix = "bdi_score",
    values_to = "bdi")
pulse_tidy_df
```

    ## # A tibble: 4,348 × 5
    ##       id   age sex   visit   bdi
    ##    <dbl> <dbl> <chr> <chr> <dbl>
    ##  1 10003  48.0 male  _bl       7
    ##  2 10003  48.0 male  _01m      1
    ##  3 10003  48.0 male  _06m      2
    ##  4 10003  48.0 male  _12m      0
    ##  5 10015  72.5 male  _bl       6
    ##  6 10015  72.5 male  _01m     NA
    ##  7 10015  72.5 male  _06m     NA
    ##  8 10015  72.5 male  _12m     NA
    ##  9 10022  58.5 male  _bl      14
    ## 10 10022  58.5 male  _01m      3
    ## # ℹ 4,338 more rows

rewrite, combine, and mutate

``` r
pulse_df = 
  haven::read_sas("./data_import_examples/public_pulse_data.sas7bdat") |>
  janitor::clean_names() |>
  pivot_longer(
    bdi_score_bl:bdi_score_12m,
    names_to = "visit", 
    names_prefix = "bdi_score_",
    values_to = "bdi") |>
  mutate(
    visit = replace(visit, visit == "bl", "00m"),
    visit = factor(visit)) 
```

# Binding Rows using LOTR data

First Step: Import each data table

``` r
fellowship_ring = 
  readxl::read_excel("./data_import_examples/LotR_Words.xlsx", range = "B3:D6") |>
  mutate(movie = "fellowship_ring")
two_towers = 
  readxl::read_excel("./data_import_examples/LotR_Words.xlsx", range = "F3:H6") |>
  mutate(movie = "two_towers")
return_king = 
  readxl::read_excel("./data_import_examples/LotR_Words.xlsx", range = "J3:L6") |>
  mutate(movie = "return_king")
```

Bind all the rows together

``` r
lotr_tidy = 
  bind_rows(fellowship_ring, two_towers, return_king) |>
  janitor::clean_names() |>
  relocate(movie)|>
  pivot_longer(
    female:male,
    names_to = "gender", 
    values_to = "words") |>
  mutate(race = str_to_lower(race)) |> 
  select(movie, everything()) 

lotr_tidy
```

    ## # A tibble: 18 × 4
    ##    movie           race   gender words
    ##    <chr>           <chr>  <chr>  <dbl>
    ##  1 fellowship_ring elf    female  1229
    ##  2 fellowship_ring elf    male     971
    ##  3 fellowship_ring hobbit female    14
    ##  4 fellowship_ring hobbit male    3644
    ##  5 fellowship_ring man    female     0
    ##  6 fellowship_ring man    male    1995
    ##  7 two_towers      elf    female   331
    ##  8 two_towers      elf    male     513
    ##  9 two_towers      hobbit female     0
    ## 10 two_towers      hobbit male    2463
    ## 11 two_towers      man    female   401
    ## 12 two_towers      man    male    3589
    ## 13 return_king     elf    female   183
    ## 14 return_king     elf    male     510
    ## 15 return_king     hobbit female     2
    ## 16 return_king     hobbit male    2673
    ## 17 return_king     man    female   268
    ## 18 return_king     man    male    2459

## Joining Data sets

``` r
pup_df = 
  read_csv(
    "./data_import_examples/FAS_pups.csv",
    na = c("NA", "", "."), show_col_types = FALSE) |>
  janitor::clean_names() |>
  mutate(
    sex = 
      case_match(
        sex, 
        1 ~ "male", 
        2 ~ "female"),
    sex = as.factor(sex)) 
pup_df
```

    ## # A tibble: 313 × 6
    ##    litter_number sex   pd_ears pd_eyes pd_pivot pd_walk
    ##    <chr>         <fct>   <dbl>   <dbl>    <dbl>   <dbl>
    ##  1 #85           male        4      13        7      11
    ##  2 #85           male        4      13        7      12
    ##  3 #1/2/95/2     male        5      13        7       9
    ##  4 #1/2/95/2     male        5      13        8      10
    ##  5 #5/5/3/83/3-3 male        5      13        8      10
    ##  6 #5/5/3/83/3-3 male        5      14        6       9
    ##  7 #5/4/2/95/2   male       NA      14        5       9
    ##  8 #4/2/95/3-3   male        4      13        6       8
    ##  9 #4/2/95/3-3   male        4      13        7       9
    ## 10 #2/2/95/3-2   male        4      NA        8      10
    ## # ℹ 303 more rows

``` r
litter_df = 
  read_csv(
    "./data_import_examples/FAS_litters.csv",
    na = c("NA", ".", ""),  show_col_types = FALSE) |>
  janitor::clean_names() |>
  separate(group, into = c("dose", "day_of_tx"), sep = 3) |>
  relocate(litter_number) |>
  mutate(
    wt_gain = gd18_weight - gd0_weight,
    dose = str_to_lower(dose))
litter_df
```

    ## # A tibble: 49 × 10
    ##    litter_number   dose  day_of_tx gd0_weight gd18_weight gd_of_birth
    ##    <chr>           <chr> <chr>          <dbl>       <dbl>       <dbl>
    ##  1 #85             con   7               19.7        34.7          20
    ##  2 #1/2/95/2       con   7               27          42            19
    ##  3 #5/5/3/83/3-3   con   7               26          41.4          19
    ##  4 #5/4/2/95/2     con   7               28.5        44.1          19
    ##  5 #4/2/95/3-3     con   7               NA          NA            20
    ##  6 #2/2/95/3-2     con   7               NA          NA            20
    ##  7 #1/5/3/83/3-3/2 con   7               NA          NA            20
    ##  8 #3/83/3-3       con   8               NA          NA            20
    ##  9 #2/95/3         con   8               NA          NA            20
    ## 10 #3/5/2/2/95     con   8               28.5        NA            20
    ## # ℹ 39 more rows
    ## # ℹ 4 more variables: pups_born_alive <dbl>, pups_dead_birth <dbl>,
    ## #   pups_survive <dbl>, wt_gain <dbl>

``` r
fas_df = 
  left_join(pup_df, litter_df, by = "litter_number") %>% 
  arrange(litter_number) %>% 
  relocate(litter_number, dose, day_of_tx)
fas_df
```

    ## # A tibble: 313 × 15
    ##    litter_number   dose  day_of_tx sex    pd_ears pd_eyes pd_pivot pd_walk
    ##    <chr>           <chr> <chr>     <fct>    <dbl>   <dbl>    <dbl>   <dbl>
    ##  1 #1/2/95/2       con   7         male         5      13        7       9
    ##  2 #1/2/95/2       con   7         male         5      13        8      10
    ##  3 #1/2/95/2       con   7         female       4      13        7       9
    ##  4 #1/2/95/2       con   7         female       4      13        7      10
    ##  5 #1/2/95/2       con   7         female       5      13        8      10
    ##  6 #1/2/95/2       con   7         female       5      13        8      10
    ##  7 #1/2/95/2       con   7         female       5      13        6      10
    ##  8 #1/5/3/83/3-3/2 con   7         male         4      NA       NA       9
    ##  9 #1/5/3/83/3-3/2 con   7         male         4      NA        7       9
    ## 10 #1/5/3/83/3-3/2 con   7         male         4      NA        7       9
    ## # ℹ 303 more rows
    ## # ℹ 7 more variables: gd0_weight <dbl>, gd18_weight <dbl>, gd_of_birth <dbl>,
    ## #   pups_born_alive <dbl>, pups_dead_birth <dbl>, pups_survive <dbl>,
    ## #   wt_gain <dbl>
