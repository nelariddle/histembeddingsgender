Tran_Riddle_Historical Embeddings
================
Nela Riddle
December 3, 2024

First, load in the pre-written group and word lists to be used in
analyses:

``` r
## Load in data ----
## Set WD to word stimuli
setwd("wordstim")

# Function to read and process lists
read_list <- function(file, col_name) {
  list_data <- read.delim(file, header = FALSE)
  colnames(list_data) <- col_name
  return(as.vector(list_data[[col_name]]))
}

# Specific groups (men, women)
groupwrds <- read.csv("groupstimlists.csv", header = FALSE)
groupwrds <- as.data.frame(t(groupwrds))
colnames(groupwrds) <- as.character(groupwrds[1, ])
groupwrds <- groupwrds[-1, ]

# Read lists using the function
agentic <- read_list("agentic.txt", "agentic")
communal <- read_list("communal.txt", "communal")
trait <- read_list("traitlist.txt", "trait")
job <- read_list("joblist.txt", "job")
fruit <- read_list("fruit.txt", "fruit")
noun <- read_list("nouns.txt", "noun")
common <- read_list("common.txt", "common")
```

The agentic and communal lists were borrowed from
<https://onlinelibrary.wiley.com/doi/10.1002/ejsp.2561>; here are some
examples

``` r
head(agentic)
```

    ## [1] "able"           "accomplish"     "accomplishment" "accuracy"       "accurate"       "achieve"

``` r
head(communal)
```

    ## [1] "accept"        "acceptable"    "acceptance"    "accommodate"   "accommodation" "accompany"

The group word lists were taken from
<https://pubmed.ncbi.nlm.nih.gov/35787033/>, as well as the trait list:

``` r
head(groupwrds$men)
```

    ## [1] "men"         "man"         "male"        "males"       "masculine"   "masculinity"

``` r
head(groupwrds$women)
```

    ## [1] "women"      "woman"      "female"     "females"    "feminine"   "femininity"

``` r
head(trait)
```

    ## [1] "able"          "abrupt"        "absentminded"  "abusive"       "accommodating" "accurate"

The job titles were scraped off this site:
<https://spotterful.com/blog/job-description-template/job-titles-list-a-z>,
and expanded through nearest neighbors

    ## [1] "accompanist"   "accountant"    "actuary"       "actor"         "acupuncturist" "adjudicator"

The workhorse function; it iterates over each decade, computing the MAC
score between each word and each group, then finds the Pearson
correlation of the resulting lists (demonstrated visually later)

``` r
print(mac(
  as.matrix(wordvecs.dat[[20]]),
  S = c("cat", "puppy", "canine", "happy", "weird", "car"),
  A = c("dog")
)$P)
```

    ##        cat      puppy     canine      happy      weird        car 
    ## 0.52902236 0.47944586 0.36709169 0.08328442 0.15497696 0.21841119

``` r
print(mac(
  as.matrix(wordvecs.dat[[20]]),
  S = c("god", "bible", "lord", "banana", "french", "blond"),
  A = c("jesus")
)$P)
```

    ##         god       bible        lord      banana      french       blond 
    ##  0.55230813  0.32821543  0.50016478 -0.06574848  0.03215507  0.03632459

    ## Time Series:
    ## Start = 1820 
    ## End = 2010 
    ## Frequency = 0.1 
    ##  [1] 0.5666162 0.4309299 0.1850314 0.2648075 0.3305753 0.4125082 0.2793425 0.3600384 0.3158208 0.5707101
    ## [11] 0.4633638 0.3282957 0.6107289 0.5777284 0.5585068 0.6213059 0.6853386 0.6157834 0.7589619 0.6207805
    ## attr(,"group1index")
    ## [1] nonhuman
    ## attr(,"group2index")
    ## [1] women
    ## attr(,"wordterms")
    ## [1] noun
    ## attr(,"corpus")
    ## [1] coha

    ##                 grp1ef       grp2ef    trait
    ## apple    -0.0175141881  0.012216518    apple
    ## bridge    0.0219305368 -0.020148409   bridge
    ## car       0.0220909291 -0.005440041      car
    ## dream     0.0667966968  0.081342743    dream
    ## elephant  0.2312207941  0.213757766 elephant
    ## forest   -0.0078725570  0.006766126   forest
    ## garden   -0.0530657606  0.054511087   garden
    ## house    -0.0192488666  0.024736192    house
    ## island    0.0008567316  0.042285972   island
    ## jacket    0.1052850273  0.063152146   jacket
    ## key      -0.0159210243 -0.025462033      key
    ## lamp      0.0184488993  0.054662164     lamp
    ## mountain  0.0389751978  0.031631657 mountain
    ## notebook  0.1672584803  0.168067874 notebook
    ## ocean    -0.0280244380 -0.012584872    ocean
    ## pencil    0.1914772436  0.194880300   pencil
    ## queen     0.1241449175  0.258921486    queen
    ## river     0.0190086921 -0.001780772    river
    ## sun      -0.0147225168  0.008250374      sun
    ## tree      0.0238232460  0.036017161     tree
    ## umbrella  0.1758348031  0.203902068 umbrella
    ## village   0.0391640480  0.093909121  village
    ## window    0.0233733214  0.030247392   window
    ## yacht     0.1970494170  0.162671996    yacht
    ## zebra              NaN          NaN    zebra
    ## arrow     0.2020074942  0.148782030    arrow
    ## balloon   0.1860049711  0.176078672  balloon
    ## castle    0.1051304568  0.112649808   castle
    ## diamond   0.0594390996  0.103904600  diamond
    ## eagle     0.2213715984  0.190353974    eagle
    ## feather   0.2099160151  0.217230310  feather
    ## guitar    0.2002663570  0.168767772   guitar
    ## hat       0.1270004640  0.099805039      hat
    ## iceberg            NaN          NaN  iceberg
    ## jungle    0.1979612549  0.166727272   jungle
    ## kite               NaN          NaN     kite
    ## ladder    0.1857369778  0.125624475   ladder
    ## mirror    0.0226273606  0.079464282   mirror
    ## necklace  0.1965937643  0.257320401 necklace
    ## octopus            NaN          NaN  octopus
    ## painting  0.0298548327  0.072060449 painting
    ## quilt     0.1575433847  0.229595442    quilt
    ## rocket    0.1926537562  0.126032587   rocket
    ## statue    0.2201962058  0.213073632   statue
    ## treasure  0.1971641660  0.205583065 treasure
    ## universe -0.0204368589 -0.004092586 universe
    ## violin    0.2284506788  0.206341322   violin
    ## whistle   0.2188643517  0.170772593  whistle
    ## yogurt    0.1476598723  0.150609206   yogurt
    ## zipper    0.2140006263  0.199501709   zipper

``` r
plot_one_decade(get_decade("men", "women", "agentic", "coha", 1820))
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](histembeddingsGender_files/figure-gfm/plottingDecade-1.png)<!-- -->

``` r
plot_one_decade(get_decade("men", "women", "communal", "engall", 1990))
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](histembeddingsGender_files/figure-gfm/plottingDecade-2.png)<!-- -->

``` r
plot_one_decade(get_decade("men", "women", "common", "engall", 1800))
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](histembeddingsGender_files/figure-gfm/plottingDecade-3.png)<!-- -->

``` r
head(get_decade("men", "women", "agentic", "engall", 1990))
```

    ##                     grp1ef       grp2ef          trait
    ## able            0.03830098 -0.019302234           able
    ## accomplish     -0.02177715 -0.053832454     accomplish
    ## accomplishment  0.02045993  0.005052507 accomplishment
    ## accuracy       -0.05605307 -0.069085822       accuracy
    ## accurate       -0.03337303 -0.060975881       accurate
    ## achieve        -0.04780008 -0.064694855        achieve

``` r
# Calculate the number of missing group words by decade
groupmiss_coha <-
  as.data.frame(sapply(1:dim(groupwrds)[2], function(j) {
    sapply(1:length(wordvecs.dat_coha), function(i) {
      sum(groupwrds[, j] %in% unavwords_coha[[i]])
    })
  }))
colnames(groupmiss_coha) <- colnames(groupwrds)
groupmiss_coha[21,] <- colSums(groupwrds != "", na.rm = TRUE)
groupmiss2_coha <-
  as.data.frame(sapply(1:dim(groupmiss_coha)[2], function(j) {
    1 - groupmiss_coha[1:20, j] / groupmiss_coha[21, j]
  }))
colnames(groupmiss2_coha) <- colnames(groupwrds)

# Add the year column
groupmiss2_coha$year <- seq(1820, 2010, by = 10)
groupmiss2_coha
```

    ##          men     women     human  nonhuman year
    ## 1  0.4242424 0.3714286 0.5000000 0.1111111 1820
    ## 2  0.5757576 0.5714286 0.6428571 0.4444444 1830
    ## 3  0.6969697 0.6571429 0.6428571 0.6111111 1840
    ## 4  0.6666667 0.6857143 0.6428571 0.6111111 1850
    ## 5  0.6969697 0.7142857 0.6428571 0.6666667 1860
    ## 6  0.6969697 0.7142857 0.7142857 0.6666667 1870
    ## 7  0.6969697 0.6857143 0.8571429 0.6111111 1880
    ## 8  0.7272727 0.7142857 0.9285714 0.7222222 1890
    ## 9  0.7575758 0.6857143 0.9285714 0.6111111 1900
    ## 10 0.7272727 0.7428571 0.9285714 0.7222222 1910
    ## 11 0.7878788 0.7428571 0.9285714 0.6111111 1920
    ## 12 0.8181818 0.6857143 1.0000000 0.7222222 1930
    ## 13 0.8181818 0.7142857 0.9285714 0.7222222 1940
    ## 14 0.8181818 0.7142857 0.9285714 0.7222222 1950
    ## 15 0.7575758 0.7142857 1.0000000 0.8333333 1960
    ## 16 0.7575758 0.6857143 1.0000000 0.8888889 1970
    ## 17 0.7878788 0.6571429 1.0000000 0.8888889 1980
    ## 18 0.7272727 0.6857143 1.0000000 0.8333333 1990
    ## 19 0.6969697 0.7142857 1.0000000 0.8888889 2000
    ## 20 0.7575758 0.7142857 1.0000000 0.8333333 2010

``` r
# Create the plot
ggplot(groupmiss2_coha, aes(x = year)) +
  geom_line(aes(y = men, color = "Men")) +
  geom_line(aes(y = women, color = "Women")) +
  scale_color_manual(values = c("Men" = "blue", "Women" = "red")) +
  labs(title = "prop. group words available over time",
       x = "Years",
       y = "Values",
       color = "Legend") +
  theme_minimal()
```

![](histembeddingsGender_files/figure-gfm/plotMissing-1.png)<!-- -->

``` r
find_outliers <- function(column, year) {
  is_outlier <-
    abs(column - mean(column, na.rm = TRUE)) > 3 * sd(column, na.rm = TRUE)
  data.frame(Decade = year[is_outlier], Value = column[is_outlier])
}

# Find outliers for men and women
outliers_men <-
  find_outliers(groupmiss2_coha$men, groupmiss2_coha$year)
outliers_women <-
  find_outliers(groupmiss2_coha$women, groupmiss2_coha$year)

# Print results
outliers_men
```

    ##   Decade     Value
    ## 1   1820 0.4242424

``` r
outliers_women
```

    ##   Decade     Value
    ## 1   1820 0.3714286

``` r
# Initialize an empty data frame to store results
results_df <- data.frame()

# Define the parameters to iterate over
categories <- c("agentic", "communal")
corpora <- c("coha", "engall")
years <- seq(1800, 2010, by = 10)

# Loop through each year
for (year in years) {
  # Create a temporary vector to store the values for the current year
  temp_row <- c(year)
  
  # Loop through each combination of trait and corpus
  for (category in categories) {
    for (corpus in corpora) {
      if ((year >= 1820 & corpus == "coha") | (year <= 1990 & corpus == "engall")) {
        df <- get_decade("men", "women", category, corpus, year)
        mean_men <- mean(df$grp1ef, na.rm = TRUE)
        mean_women <- mean(df$grp2ef, na.rm = TRUE)
        temp_row <- c(temp_row, mean_men, mean_women)
      } else{
        temp_row <- c(temp_row, NaN, NaN)
      }
    }
  }
  results_df <- rbind(results_df, temp_row)
}

# Set the column names for the results data frame
colnames(results_df) <- c(
  "year",
  "agentic_coha_men", "agentic_coha_women",
  "agentic_engall_men", "agentic_engall_women",
  "communal_coha_men", "communal_coha_women",
  "communal_engall_men", "communal_engall_women"
)

# Display the results
generate_trait_corpus_plot <- function(data, trait, corpus) {
  # Dynamically generate column names
  y_men <- paste0(trait, "_", corpus, "_men")
  y_women <- paste0(trait, "_", corpus, "_women")
  
  # Generate title
  title <- paste(trait, corpus, sep = ", ")
  
  # Create the plot
  ggplot(data, aes(x = year)) +
    geom_line(aes_string(y = y_men, color = "'Men'")) +
    geom_line(aes_string(y = y_women, color = "'Women'")) +
    labs(
      title = title,
      x = "Year",
      y = "Mean Values",
      color = "Group"
    ) +
    theme_minimal() +
    scale_color_manual(values = c("Men" = "blue", "Women" = "red"))
}

generate_trait_corpus_plot(results_df, "communal", "coha")
```

    ## Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
    ## ℹ Please use tidy evaluation idioms with `aes()`.
    ## ℹ See also `vignette("ggplot2-in-packages")` for more information.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.

    ## Warning: Removed 2 rows containing missing values (`geom_line()`).
    ## Removed 2 rows containing missing values (`geom_line()`).

![](histembeddingsGender_files/figure-gfm/getMagnitudes-1.png)<!-- -->

``` r
generate_trait_corpus_plot(results_df, "agentic", "coha")
```

    ## Warning: Removed 2 rows containing missing values (`geom_line()`).
    ## Removed 2 rows containing missing values (`geom_line()`).

![](histembeddingsGender_files/figure-gfm/getMagnitudes-2.png)<!-- -->

``` r
generate_trait_corpus_plot(results_df, "communal", "engall")
```

    ## Warning: Removed 2 rows containing missing values (`geom_line()`).
    ## Removed 2 rows containing missing values (`geom_line()`).

![](histembeddingsGender_files/figure-gfm/getMagnitudes-3.png)<!-- -->

``` r
generate_trait_corpus_plot(results_df, "agentic", "engall")
```

    ## Warning: Removed 2 rows containing missing values (`geom_line()`).
    ## Removed 2 rows containing missing values (`geom_line()`).

![](histembeddingsGender_files/figure-gfm/getMagnitudes-4.png)<!-- -->

``` r
plot_one_ts(get_ts("nonhuman", "women", "agentic", "coha"))
```

![](histembeddingsGender_files/figure-gfm/plottingTsHumanNonhuman-1.png)<!-- -->

``` r
plot_one_ts(get_ts("nonhuman", "men", "communal", "coha"))
```

![](histembeddingsGender_files/figure-gfm/plottingTsHumanNonhuman-2.png)<!-- -->

``` r
human_nonhuman_ts<-list(get_ts("nonhuman", "women", "trait", "coha"),get_ts("nonhuman", "men", "trait", "coha"), get_ts("nonhuman", "women", "trait", "engall"),get_ts("nonhuman", "men", "trait", "engall"),get_ts("men", "women", "trait", "engall"),get_ts("men", "women", "trait", "coha"))
plot_multiple_ts(human_nonhuman_ts)
```

![](histembeddingsGender_files/figure-gfm/plottingTsHumanNonhuman-3.png)<!-- -->

``` r
plot_one_decade(get_decade("nonhuman", "women", "trait", "coha", 2010))
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 244 rows containing non-finite values (`stat_smooth()`).

    ## Warning: Removed 244 rows containing missing values (`geom_point()`).

    ## Warning: Removed 244 rows containing missing values (`geom_text()`).

![](histembeddingsGender_files/figure-gfm/plottingTsHumanNonhuman-4.png)<!-- -->

``` r
plot_one_ts(get_ts("men", "women", "trait", "coha"))
```

![](histembeddingsGender_files/figure-gfm/plottingTsMenWomen-1.png)<!-- -->

``` r
plot_one_ts(get_ts("men", "women", "job", "coha"))
```

![](histembeddingsGender_files/figure-gfm/plottingTsMenWomen-2.png)<!-- -->

``` r
men_women_trait_job_ts<-list(get_ts("men", "women", "trait", "coha"), get_ts("men", "women", "job", "coha"), get_ts("men", "women", "trait", "engall"),get_ts("men", "women", "job", "engall"))
plot_multiple_ts(men_women_trait_job_ts)
```

![](histembeddingsGender_files/figure-gfm/plottingTsMenWomen-3.png)<!-- -->

``` r
men_women_trait_job_ts<-list(get_ts("men", "women", "agentic", "coha"),get_ts("men", "women", "communal", "coha"), get_ts("men", "women", "agentic", "engall"),get_ts("men", "women", "communal", "engall"),get_ts("men", "women", "common", "engall"),get_ts("men", "women", "common", "coha"))
plot_multiple_ts(men_women_trait_job_ts)
```

![](histembeddingsGender_files/figure-gfm/plottingTsMenWomen-4.png)<!-- -->

``` r
# plot_one_decade(get_decade("nonhuman", "women", "trait", "engall", 1990))

plot_one_decade(get_decade("nonhuman", "women", "trait", "coha", 1850))
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 280 rows containing non-finite values (`stat_smooth()`).

    ## Warning: Removed 280 rows containing missing values (`geom_point()`).

    ## Warning: Removed 280 rows containing missing values (`geom_text()`).

![](histembeddingsGender_files/figure-gfm/plottingTsMenWomen-5.png)<!-- -->
