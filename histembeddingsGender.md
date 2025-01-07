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
# job <- read_list("joblistDOT.txt", "job")
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

    ## [1] "able"           "accomplish"     "accomplishment" "accuracy"      
    ## [5] "accurate"       "achieve"

``` r
head(communal)
```

    ## [1] "accept"        "acceptable"    "acceptance"    "accommodate"  
    ## [5] "accommodation" "accompany"

The group word lists were taken from
<https://pubmed.ncbi.nlm.nih.gov/35787033/>, as well as the trait list:

``` r
head(groupwrds$men)
```

    ## [1] "men"         "man"         "male"        "males"       "masculine"  
    ## [6] "masculinity"

``` r
head(groupwrds$women)
```

    ## [1] "women"      "woman"      "female"     "females"    "feminine"  
    ## [6] "femininity"

``` r
head(trait)
```

    ## [1] "able"          "abrupt"        "absentminded"  "abusive"      
    ## [5] "accommodating" "accurate"

The job titles were scraped off this site:
<https://spotterful.com/blog/job-description-template/job-titles-list-a-z>,
and expanded through nearest neighbors

    ## [1] "accompanist"   "accountant"    "actuary"       "actor"        
    ## [5] "acupuncturist" "adjudicator"

The workhorse function; it iterates over each decade, computing the MAC
score between each word and each group, then finds the Pearson
correlation of the resulting lists (demonstrated visually later)

``` r
grpwrdassoc_rel <-
  function(group1index,
           group2index,
           wordterms,
           wordvecs.dat = wordvecs.dat,
           unavwords = unavwords,
           corpus) {
    
    start_year <- if (corpus=="coha") 1820 else 1800
    end_year <- if (corpus=="coha") 2010 else 1990
    
    # Create lists of the group's available words
    availwrds_decade_group1 <-
      lapply(1:length(wordvecs.dat), function(i) {
        groupwrds[, group1index][!groupwrds[, group1index] %in% unavwords[[i]]]
      })
    availwrds_decade_group2 <-
      lapply(1:length(wordvecs.dat), function(i) {
        groupwrds[, group2index][!groupwrds[, group2index] %in% unavwords[[i]]]
      })
    
    
    # Now compute MAC from available words for each decade
    wordvecs.mat <- list()
    mac_group1_2list <- list()
    cor_group1_2 <- list()
    cor_group1_2ts <- vector()
    for (i in 1:length(wordvecs.dat)) {
      wordvecs.mat[[i]] <- as.matrix(wordvecs.dat[[i]])
      mac_group1_2list[[i]] <-
        data.frame(
          grp1ef = mac(wordvecs.mat[[i]], S = wordterms, A = availwrds_decade_group1[[i]])$P,
          grp2ef = mac(wordvecs.mat[[i]], S = wordterms, A = availwrds_decade_group2[[i]])$P,
          trait = names(
            mac(wordvecs.mat[[i]], S = wordterms, A = availwrds_decade_group1[[i]])$P
          )
        )
      cor_group1_2[[i]] <-
        cor.test(mac_group1_2list[[i]]$grp2ef, mac_group1_2list[[i]]$grp1ef)
      cor_group1_2ts[i] <- cor_group1_2[[i]]$estimate
      cor_group1_2ts <-
        ts(
          cor_group1_2ts,
          start = start_year,
          end = end_year,
          frequency = 1 / 10
        )
      print(i)
    }
    output_rel <- list(mac_group1_2list,
                       cor_group1_2ts)
    return(output_rel)
  }
```

An example of how the mac function works (using engall 1990); here we
compute the mean average correlation of each word in the first list to
the list of animals. It makes sense that the animals in the first list
had the highest mac score.

``` r
print(mac(
  as.matrix(wordvecs.dat[[20]]),
  S = c("elephant", "horse", "tiger", "happy", "weird", "car"),
  A = c("dog", "cat", "turtle", "fish", "monkey")
)$P)
```

    ##   elephant      horse      tiger      happy      weird        car 
    ## 0.29825993 0.26152604 0.32429946 0.01836855 0.12177856 0.09239695

You can compute the cosine similarity of any two words by replacing the
lists with single words.

``` r
print(mac(
  as.matrix(wordvecs.dat[[20]]),
  S = "happy",
  A = "sad"
)$P)
```

    ##     happy 
    ## 0.4395598

We can plot the mac scores for two different groups against each other
like so:

``` r
plot_one_decade(get_decade("men", "women", "agentic", "coha", 1820))
```

![](histembeddingsGender_files/figure-gfm/plottingDecade-1.png)<!-- -->

``` r
plot_one_decade(get_decade("men", "women", "agentic", "engall", 1800))
```

![](histembeddingsGender_files/figure-gfm/plottingDecade-2.png)<!-- -->

``` r
plot_one_decade(get_decade("men", "women", "communal", "engall", 1990))
```

![](histembeddingsGender_files/figure-gfm/plottingDecade-3.png)<!-- -->

``` r
plot_one_decade(get_decade("men", "women", "job", "engall", 1990))
```

![](histembeddingsGender_files/figure-gfm/plottingDecade-4.png)<!-- -->

``` r
plot_one_decade(get_decade("men", "women", "job", "engall", 1800))
```

![](histembeddingsGender_files/figure-gfm/plottingDecade-5.png)<!-- -->

``` r
plot_one_decade(get_decade("men", "women", "job", "coha", 1990))
```

![](histembeddingsGender_files/figure-gfm/plottingDecade-6.png)<!-- -->

The titles of the plots contain the Pearson coefficient, which is what
we use to measure the similarity of the two groups.

Noticing that the 1820 coha plot had an odd correlation, let’s check the
proportion of gender words that were available, as this could be skewing
certain words.

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
rownames(groupmiss2_coha) <- seq(1820, 2010, by = 10)
groupmiss2_coha$year <- seq(1820, 2010, by = 10)
groupmiss2_coha
```

    ##            men     women     human  nonhuman year
    ## 1820 0.4242424 0.3714286 0.5000000 0.1111111 1820
    ## 1830 0.5757576 0.5714286 0.6428571 0.4444444 1830
    ## 1840 0.6969697 0.6571429 0.6428571 0.6111111 1840
    ## 1850 0.6666667 0.6857143 0.6428571 0.6111111 1850
    ## 1860 0.6969697 0.7142857 0.6428571 0.6666667 1860
    ## 1870 0.6969697 0.7142857 0.7142857 0.6666667 1870
    ## 1880 0.6969697 0.6857143 0.8571429 0.6111111 1880
    ## 1890 0.7272727 0.7142857 0.9285714 0.7222222 1890
    ## 1900 0.7575758 0.6857143 0.9285714 0.6111111 1900
    ## 1910 0.7272727 0.7428571 0.9285714 0.7222222 1910
    ## 1920 0.7878788 0.7428571 0.9285714 0.6111111 1920
    ## 1930 0.8181818 0.6857143 1.0000000 0.7222222 1930
    ## 1940 0.8181818 0.7142857 0.9285714 0.7222222 1940
    ## 1950 0.8181818 0.7142857 0.9285714 0.7222222 1950
    ## 1960 0.7575758 0.7142857 1.0000000 0.8333333 1960
    ## 1970 0.7575758 0.6857143 1.0000000 0.8888889 1970
    ## 1980 0.7878788 0.6571429 1.0000000 0.8888889 1980
    ## 1990 0.7272727 0.6857143 1.0000000 0.8333333 1990
    ## 2000 0.6969697 0.7142857 1.0000000 0.8888889 2000
    ## 2010 0.7575758 0.7142857 1.0000000 0.8333333 2010

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
Clearly many fewer words were available in that first decade; let’s
check for statistical outliers.

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

Since 1820 was an outlier for both men and women, we can safely ignore
that decade from COHA.

    ##  [1] 0.084520244 0.036986220 0.017284895 0.001474853 0.018157545 0.001449928
    ##  [7] 0.022906022 0.023492385 0.029276455 0.045164065 0.047460166 0.048764028
    ## [13] 0.050883157 0.055722354 0.041074543 0.040904315 0.069172537 0.077888404
    ## [19] 0.065539308 0.113000601
    ## attr(,"group1index")
    ## [1] "men"
    ## attr(,"wordterms")
    ## [1] "agentic"
    ## attr(,"corpus")
    ## [1] "coha"

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
men_women_trait_job_ts <-
  list(
    get_ts("men", "women", "trait", "coha"),
    get_ts("men", "women", "job", "coha"),
    get_ts("men", "women", "trait", "engall"),
    get_ts("men", "women", "job", "engall")
  )
plot_multiple_ts(men_women_trait_job_ts)
```

![](histembeddingsGender_files/figure-gfm/plottingTsMenWomen-3.png)<!-- -->

``` r
men_women_trait_job_ts <-
  list(
    get_ts("men", "women", "agentic", "coha"),
    get_ts("men", "women", "communal", "coha"),
    get_ts("men", "women", "agentic", "engall"),
    get_ts("men", "women", "communal", "engall"),
    get_ts("men", "women", "common", "engall"),
    get_ts("men", "women", "common", "coha")
  )
plot_multiple_ts(men_women_trait_job_ts)
```

![](histembeddingsGender_files/figure-gfm/plottingTsMenWomen-4.png)<!-- -->

``` r
# plot_one_decade(get_decade("nonhuman", "women", "trait", "engall", 1990))

plot_one_decade(get_decade("nonhuman", "women", "trait", "engall", 1850))
```

    ## Warning: Removed 131 rows containing non-finite values (`stat_smooth()`).

    ## Warning: Removed 131 rows containing missing values (`geom_point()`).

    ## Warning: Removed 131 rows containing missing values (`geom_text()`).

![](histembeddingsGender_files/figure-gfm/plottingTsMenWomen-5.png)<!-- -->
