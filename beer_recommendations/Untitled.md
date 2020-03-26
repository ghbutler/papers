Exploring the BeerAdvocate Dataset
================
Gabriel Butler

``` r
library(tidyverse)
library(GGally)
library(ggpubr)
library(lindia)

beer_reviews <- read.csv('beer_reviews.csv')
```

### Introduction

This project was inspired by a webinar on the website DataCamp called
“How to Hire and Test for Data Skills: A One-Size-Fits-All Interview
Kit”. In particular, I will attempt to answer the four questions on
slide 26 of the presentation:

> 1.  Which brewery produces the strongest beers by ABV%?
> 2.  If you had to pick 3 beers to recommend using only this data,
>     which would you pick?
> 3.  Which of the factors (aroma, taste, appearance, palette) are most
>     important in determining the overall quality of a beer?
> 4.  Lastly, if I typically enjoy a beer due to its aroma and
>     appearance, which beer style should I try?

I do not consume alcohol, but I am passionate about statistical
programming and I believe that the questions above are interesting and
worth answering.

Links to the original webinar and its slides (which may be inaccessible
to those without a DataCamp subscription) as well as the original freely
available dataset are listed below.

Webinar:
<https://www.datacamp.com/resources/webinars/how-to-hire-and-test-for-data-skills-a-one-size-fits-all-kit>

Slides:
<https://docs.google.com/presentation/d/1O85_HAAozQ-hWEWm-kfTCc_0jJXthOPXnht3tRBh7cs/edit?usp=sharing>

Dataset: <https://data.world/socialmediadata/beeradvocate>

### Question 1: Which brewery produces the strongest beers by ABV%?

One way to answer this question is by searching for the breweries in
this dataset with the 10 highest average measurements for percentage of
alcohol by volume (ABV%). Judging from the result below, the
Schorschbräu brewery ranks highest with an astounding average ABV of
19.2%.

``` r
strongest_beers <- beer_reviews %>% 
  group_by(brewery_name) %>% 
  summarize(avg_abv = mean(beer_abv)) %>% 
  arrange(desc(avg_abv)) %>% 
  slice(1:10)

strongest_beers
```

    ## # A tibble: 10 x 2
    ##    brewery_name                        avg_abv
    ##    <fct>                                 <dbl>
    ##  1 "Schorschbräu"                         19.2
    ##  2 "Shoes Brewery"                        15.2
    ##  3 "Rascal Creek Brewing Co."             13  
    ##  4 "Monks Porter House"                   12.5
    ##  5 "Rinku\u009aki&#371; Aluas Darykla"    12  
    ##  6 "United Brands Company"                12  
    ##  7 "Brauerei Schloss Eggenberg"           11.8
    ##  8 "Nasu Kogen Beer Co. Ltd."             11.5
    ##  9 "Brasserie Dubuisson Frères sprl"      11.4
    ## 10 "Wibblers"                             11

This is an interesting preliminary result, but we should dig deeper to
make sure our findings so far are meaningful.

Another question worth exploring regarding these rankings is how many
reviews of beers from each of the above companies are in this dataset.
Below we find that the top five all have more than 10 reviews here, but
the bottom six all have three reviews or less. And the brands with the
most and second most reviews in the dataset have over 1,000 and close to
2,000 reviews in the set respectively. At least now we know that
Schorschbräu doesn’t have just one review in this dataset with an
exceptionally high ABV measurement.

``` r
beer_reviews %>% 
  filter(brewery_name %in% strongest_beers$brewery_name) %>% 
  group_by(brewery_name) %>% 
  count() %>% 
  arrange(desc(n))
```

    ## # A tibble: 10 x 2
    ## # Groups:   brewery_name [10]
    ##    brewery_name                            n
    ##    <fct>                               <int>
    ##  1 "Brauerei Schloss Eggenberg"         1821
    ##  2 "Brasserie Dubuisson Frères sprl"    1078
    ##  3 "Schorschbräu"                         34
    ##  4 "United Brands Company"                26
    ##  5 "Rinku\u009aki&#371; Aluas Darykla"    11
    ##  6 "Monks Porter House"                    3
    ##  7 "Nasu Kogen Beer Co. Ltd."              2
    ##  8 "Shoes Brewery"                         2
    ##  9 "Rascal Creek Brewing Co."              1
    ## 10 "Wibblers"                              1

Let’s have a closer look at Schorschbräu. The histogram below plots the
distribution of ABV measurements for this brewery in this dataset. As we
can see, a large majority of their beers their beers that were reviewed
are below 20% ABV. The average ABV measurement, marked by the red dashed
line, is near 20%. The distribution is a bit skewed due to the presence
of a few outliers.

Still, it looks like almost every review of a beer from this brand in
this dataset has an ABV measurement of well above 10%, which is indeed
extraordinary by the standards of beer.

``` r
schor_dat <- beer_reviews %>% 
  filter(brewery_name == 'Schorschbräu') %>% 
  select(beer_abv, beer_name)

schor_dat %>%
  ggplot() +
  geom_histogram(aes(beer_abv), 
                 color = 'black',
                 binwidth = 1) +
  geom_vline(xintercept = mean(schor_dat$beer_abv),
             color = 'red',
             linetype = 'dashed',
             size = 1) +
  theme_bw() +
  labs(x = 'ABV (%)',
       y = 'Number of beers',
       title = 'Distribution of ABV(%) measurements for Schorschbräu') +
  scale_x_continuous(breaks = seq(0, 60, 10))
```

![](https://github.com/ghbutler/papers/blob/master/beer_recommendations/unnamed-chunk-4-1.png?raw=true)<!-- -->

Now let’s have a quick look at the different beers from Schorschbräu. It
appears that there are 10 different beers from this brand in this
dataset and the most popular ones, at least in terms of number of
reviews, are between 13 and 16 percent ABV.

``` r
schor_dat %>% 
  group_by(beer_name) %>% 
  summarize(mean_abv = mean(beer_abv), 
            n = n()) %>%
  arrange(desc(mean_abv))
```

    ## # A tibble: 10 x 3
    ##    beer_name                     mean_abv     n
    ##    <fct>                            <dbl> <int>
    ##  1 Schorschbräu Schorschbock 57%     57.7     1
    ##  2 Schorschbräu Schorschbock 43%     43       2
    ##  3 Schorschbräu Schorschbock 40%     39.4     3
    ##  4 Schorschbräu Schorschbock 31%     30.9     1
    ##  5 Schorsch Weizen 16%               16       1
    ##  6 Schorschbock                      16       5
    ##  7 Schorschbock Ice 13               13       1
    ##  8 Schorschbräu Donner Bock          13       9
    ##  9 Schorschbräu Donner Weizen        13      10
    ## 10 Schorschbräu Dunkles               4.9     1

### Question 2: If you had to pick 3 beers to recommend using only this data, which would you pick?

This is an open-ended question. In my opinion it’s much easier to
attempt to answer it if one has some experience as a beer drinker.
Therefore I will base my answer on my own previous preferences.

I quit drinking almost six years ago. But while I still drank, I found
through extensive independent research that my favorite kind of beer was
the India Pale Ale, also known as the IPA. These beers are known for
being rather dark (compared to a typical lager at least), having
relatively high alcohol content (ABV%) and most of all for their strong,
“hoppy” taste. In order of importance, I drank them for their taste,
their alcohol content and their aroma.

In order to begin my search for the three best IPAs, I should summarize
my search criteria. To be eligible for recommendation as one of the top
three beers in this dataset, a beer must:

1.  Have at least 100 reviews in this dataset
2.  Be an IPA

The code below filters the `beer_reviews` dataset for beers with at
least 100 reviews that also contain “IPA” in the name of the style of
the beer.

``` r
enough_reviews <- beer_reviews %>% 
  group_by(beer_name) %>% 
  count() %>% filter(n >= 100)

popular_beers <- beer_reviews %>% 
  filter(beer_name %in% enough_reviews$beer_name)

ipas <- popular_beers %>% 
  filter(str_detect(popular_beers$beer_style, "IPA") == TRUE)
```

Beers which fit the above criteria will be filtered and ranked by these
factors in descending order of importance:

1.  Taste (measured by average user rating for this characteristic)
2.  Average ABV (measured by average of the measurement for the beer in
    this dataset)
3.  Aroma (measured by average user rating for this characteristic)

And the winners are…

``` r
ipas %>% 
  select(beer_name, 
         brewery_name,
         review_taste, 
         review_aroma, 
         beer_abv) %>% 
  group_by(beer_name) %>% 
  summarize(mean_taste = mean(review_taste),
            mean_aroma = mean(review_aroma),
            mean_abv = mean(beer_abv)) %>% 
  arrange(desc(mean_taste, mean_abv, mean_aroma)) %>%
  slice(1:3)
```

    ## # A tibble: 3 x 4
    ##   beer_name         mean_taste mean_aroma mean_abv
    ##   <fct>                  <dbl>      <dbl>    <dbl>
    ## 1 Pliny The Younger       4.72       4.72       11
    ## 2 Pliny The Elder         4.63       4.61        8
    ## 3 Heady Topper            4.61       4.66        8

I have heard of the first two beers on this short list. They are
expensive and extremely difficult to come by. But if I were to start
drinking again and I were to go on Amazon to search for the three best
IPAs to drink based on my personal criteria as measured by variables in
this dataset, these are the three beers that I would pick.

Of course there are limitations to these recommendations. How much do
these beers cost? One thing I forgot to mention about IPAs above is that
they’re also known for being expensive. Affordability is also a very
important consideration for beer drinkers regardless of their
preferences. Unfortunately this dataset does not contain any information
about prices.

We should also be suspicious of people who consider themselves to be
alcohol connoisseurs. A considerable amount of research has shown that
wine critics can’t tell the difference between cheap and expensive wine
in blind taste tests. (Links to a couple of articles about this are
pasted below.) Unfortunately it does not look like similar research has
been conducted for beer drinkers, perhaps because beer prices tend to
vary much less than wine prices. Still, I would not be surprised if beer
drinkers are unable to tell the difference between an IPA that goes for
$7.99 for a six pack and one that goes for $18.99. This means that
accepting recommendations based on data like the kind we’re working with
may not lead to the best possible choices in the
end.

<https://www.theguardian.com/lifeandstyle/2013/jun/23/wine-tasting-junk-science-analysis>

<https://www.dailymail.co.uk/sciencetech/article-1376686/Britons-tell-difference-fine-wine-plonk.html>

### Question 3: Which of the factors (aroma, taste, appearance, palette) are most important in determining the overall quality of a beer?

To answer this question I will construct a linear regression model based
on aggregated data for the most popular beers (i.e., the ones with at
least 100 reviews). The data will be aggregated by calculating average
measurements for each of the variables mentioned in the question. And we
will use the mean overall score for a beer as the measure of its overall
quality. I will also consider one more variable that I believe is an
important factor in determining overall quality ratings: average ABV.
The data is aggregated below.

``` r
popular_agg <- popular_beers %>% 
  group_by(beer_name) %>% 
  summarize(mean_aroma = mean(review_aroma),
            mean_taste = mean(review_taste),
            mean_appearance = mean(review_appearance),
            mean_overall = mean(review_overall),
            mean_palate = mean(review_palate),
            mean_abv = mean(beer_abv))
```

Before we consider the full regression model we must check to see how
correlated the variables we are considering are with each other. To do
this we will use a correlation matrix.

We can see from the correlation matrix below that all of the variables
of interest (with the exception of mean ABV) are highly correlated. The
one that is most strongly correlated with our dependent variable is
`mean_taste`.

``` r
popular_agg %>% select(mean_aroma,
                       mean_appearance,
                       mean_overall,
                       mean_palate,
                       mean_taste,
                       mean_abv) %>%
  ggcorr(label = TRUE,
         label_round = 3) +
  labs(title = 'Correlation matrix for popular beers')
```

![](https://github.com/ghbutler/papers/blob/master/beer_recommendations/unnamed-chunk-9-1.png?raw=true)<!-- -->

We can see from the plots of the univariate regressions below that all
of these variables are linearly related to `mean_overall`, our dependent
variable and that the relationships are quite strong as one would expect
from strong correlation measurements. One exception is `mean_abv`, which
requires a natural logarithm transformation to maximize linearity.

``` r
univariate_plot <- function(expl_variable, data){
  
  popular_agg %>% ggplot(aes_string(expl_variable, 'mean_overall')) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = 'lm', se = FALSE) +
    theme_bw() +
    scale_x_continuous(limits = c(0, 5)) +
    scale_y_continuous(limits = c(0, 5)) +
    labs(title = paste(expl_variable, "vs. \nmean_overall"))
  
}

mean_abv_plot <- popular_agg %>% ggplot(aes(log(mean_abv), mean_overall)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = 'lm', se = FALSE) +
  theme_bw() +
  scale_x_continuous(limits = c(0, max(popular_agg$mean_abv))) +
  scale_y_continuous(limits = c(0, 5)) +
  labs(title = paste("log(mean_abv), vs. \nmean_overall"))

ggarrange(univariate_plot('mean_taste', popular_agg),
          univariate_plot('mean_aroma', popular_agg),
          univariate_plot('mean_appearance', popular_agg),
          univariate_plot('mean_palate', popular_agg),
          mean_abv_plot)
```

![](https://github.com/ghbutler/papers/blob/master/beer_recommendations/unnamed-chunk-10-1.png?raw=true)<!-- -->

Now we can consider the full model. It does not appear to work in the
way we’d expect from the plots above. In particular, it appears that
when all of these variables are in the model together, this causes the
relationship between `mean_overall` and `mean_aroma` to invert, because
in the full model the latter variable has a negative coefficient even
though it is positively correlated with `mean_overall`. The same is true
for `mean_appearance`.This is likely due to the strong multicolinearity
we observed between all the variables in our correlation matrix.

``` r
summary(lm(mean_overall ~ mean_aroma + 
             mean_taste + 
             mean_appearance + 
             mean_palate +
             log(mean_abv), 
           data = popular_agg))
```

    ## 
    ## Call:
    ## lm(formula = mean_overall ~ mean_aroma + mean_taste + mean_appearance + 
    ##     mean_palate + log(mean_abv), data = popular_agg)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.74917 -0.04240  0.00498  0.04869  0.30614 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      0.859460   0.017607  48.813   <2e-16 ***
    ## mean_aroma      -0.299823   0.014700 -20.396   <2e-16 ***
    ## mean_taste       0.924728   0.023905  38.684   <2e-16 ***
    ## mean_appearance -0.011694   0.011944  -0.979    0.328    
    ## mean_palate      0.300130   0.024155  12.425   <2e-16 ***
    ## log(mean_abv)   -0.266437   0.005969 -44.640   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.07993 on 2935 degrees of freedom
    ##   (181 observations deleted due to missingness)
    ## Multiple R-squared:  0.9489, Adjusted R-squared:  0.9488 
    ## F-statistic: 1.09e+04 on 5 and 2935 DF,  p-value: < 2.2e-16

Through backwards elimination we find that our final model has two
independent variables: `mean_taste` and `log(mean_abv)`. For every point
that `mean_taste` increases, we can expect to see `mean_overall`
increase by an average of about 0.88 points. Surprisingly, higher
`mean_abv` measurements appear to negatively affect overall beer ratings
in this dataset, although the coefficient shows that this effect is
quite small, with a typical decrease of about -0.284 points in
`mean_overall` for every one point increase in `log(mean_abv)`.

``` r
final_model <- lm(mean_overall ~ mean_taste + 
                    log(mean_abv), 
                  data = popular_agg)

summary(final_model)
```

    ## 
    ## Call:
    ## lm(formula = mean_overall ~ mean_taste + log(mean_abv), data = popular_agg)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.82485 -0.04453  0.00578  0.05407  0.31520 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    0.998063   0.014741   67.71   <2e-16 ***
    ## mean_taste     0.885769   0.004494  197.10   <2e-16 ***
    ## log(mean_abv) -0.283960   0.006237  -45.53   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.08868 on 2938 degrees of freedom
    ##   (181 observations deleted due to missingness)
    ## Multiple R-squared:  0.9371, Adjusted R-squared:  0.937 
    ## F-statistic: 2.187e+04 on 2 and 2938 DF,  p-value: < 2.2e-16

Finally, we can see that our normalized residuals of our final model
appear to be normally distributed and that there does not appear to be a
pattern in the plot of the predicted values versus the residuals.

``` r
reshist <- gg_reshist(final_model) + 
  theme_bw()

resfitted <- gg_resfitted(final_model) + 
  theme_bw() +
  scale_x_continuous(limits = c(0, 5))

ggarrange(reshist, resfitted)
```

![](https://github.com/ghbutler/papers/blob/master/beer_recommendations/unnamed-chunk-13-1.png?raw=true)<!-- -->

### Question 4: Lastly, if I typically enjoy a beer due to its aroma and appearance, which beer style should I try?

For this question I will assume that aroma and appearance are ranked in
order of importance and I will again limit my search to popular beers as
I previously defined them (i.e., ones with at least 100 reviews in the
dataset).

Based on the calculations below, the best style of beer to go with is
Eisbock. I have never heard of this type of beer, so perhaps it’s hard
to find in some parts of the world. That is why I have included 9 other
styles of beer which are comparable to Eisbock in terms of the given
criteria. Some are familiar to me, especially the “American Double /
Imperial IPA”.

``` r
popular_beers %>% 
  group_by(beer_style) %>% 
  summarize(mean_aroma = mean(review_aroma), 
            mean_appearance = mean(review_appearance)) %>% 
  arrange(desc(mean_aroma, mean_appearance)) %>%
  slice(1:10)
```

    ## # A tibble: 10 x 3
    ##    beer_style                       mean_aroma mean_appearance
    ##    <fct>                                 <dbl>           <dbl>
    ##  1 Eisbock                                4.22            3.97
    ##  2 American Double / Imperial Stout       4.19            4.19
    ##  3 American Wild Ale                      4.18            4.06
    ##  4 Quadrupel (Quad)                       4.15            4.14
    ##  5 Lambic - Unblended                     4.14            3.92
    ##  6 American Double / Imperial IPA         4.13            4.10
    ##  7 Gueuze                                 4.12            4.05
    ##  8 Weizenbock                             4.10            4.06
    ##  9 Russian Imperial Stout                 4.09            4.23
    ## 10 Flanders Red Ale                       4.08            4.04

### Conclusion

By digging through this data we were able to learn some interesting
things about beer. In particular, there are some breweries that seem to
specialize in producing very strong beers in terms of their alcohol
content, in particular the Schorschbräu brewery. I was also able to make
data driven beer recommendations to myself in case I decide to start
drinking alcohol again. (But I won’t.) We did the same for beer styles
for people who value aroma and appearance (in that order) above other
characteristics.

Are the conclusions we’ve drawn perfect? Of course not. We lack
information about prices and other characteristics that influence beer
preferences. But we have considered most of the most important ones, and
I am confident that beer drinkers with the preferences considered in
this analysis would be pleased with these recommendations regardless.
