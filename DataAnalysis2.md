---
title: "exampleRMD"
author: "Sarah Poulin"
date: "December 3, 2018"
output: 
  html_document:
    keep_md: TRUE
  
---
# My Example RMD File


## Read in the Data


```r
# Read in data
# positional data about the RV Kahuna
# kahuna <- read_csv('../../../presentations/2018/2018-12-03_DUML-RepResearch/data/2018-11-26_2017-Cape-Hatteras-BRS-kahuna-CEE.csv') 
kahuna <- read_csv('C:/Users/sarah/myrepo/data/2018-11-26_2017-Cape-Hatteras-BRS-kahuna-CEE.csv')
```

```
## Parsed with column specification:
## cols(
##   date = col_character(),
##   time = col_time(format = ""),
##   longitude = col_double(),
##   latitude = col_double(),
##   ship = col_character(),
##   status = col_character()
## )
```

```r
kStart <- kahuna %>% 
  filter(status == 'start')

# Read in Gm182 Data: 100 estimated positions of Gm182, augmented with focal follow data
gm182UP <- read_csv('C:/Users/sarah/myrepo/data/2018-11-27_Gm182-UserPoints-Start-CEE-Locations-Kahuna.csv') %>% 
  mutate(status = 'userPoints')
```

```
## Parsed with column specification:
## cols(
##   trackNum = col_integer(),
##   time = col_datetime(format = ""),
##   dfOrig.x = col_double(),
##   dfOrig.y = col_double()
## )
```

```r
# Read in Gm182 Data: 100 estimated positions of Gm182
gm182 <- read_csv('C:/Users/sarah/myrepo/data/2018-11-27_Gm182-Start-CEE-Locations-Kahuna.csv') %>% 
  mutate(status = 'noUserPoints')
```

```
## Parsed with column specification:
## cols(
##   trackNum = col_integer(),
##   time = col_datetime(format = ""),
##   dfOrig.x = col_double(),
##   dfOrig.y = col_double()
## )
```

## Perform some Data wrangling


```r
# Minimal Wrangling of the data
gmpts <- bind_rows(gm182, gm182UP)
colnames(gmpts) <- c('trackNum', 'time', 'longitude', 'latitude', 'status')
```

## Plot the KDE 


```r
# Plot the points out:
g <- ggplot(gmpts, aes(longitude, latitude, group = status))+
  scale_fill_gradient(low = "grey70", high = "grey30", guide = "none") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_minimal()+
  facet_grid(~ status, labeller = label_value) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", color = "black", size = 0.2, alpha = 0.5) +
  geom_point(color = "navy", size = .5, alpha = .4)

plot(g)
```

![](DataAnalysis2_files/figure-html/plotData-1.png)<!-- -->

## Data Analysis

### Calculate Distance

```r
# Analysis Section
# Calculate distance to ship at the start of the CEE
gmpts$d2ship <- rdist.earth.vec(cbind(kStart$longitude, kStart$latitude), 
                                cbind(gmpts$longitude, gmpts$latitude))

gmpts %>% 
  group_by(status) %>% 
  summarize(mean = mean(d2ship, na.rm = TRUE))
```

```
## # A tibble: 2 x 2
##   status        mean
##   <chr>        <dbl>
## 1 noUserPoints 4.43 
## 2 userPoints   0.962
```

### Run Regression 

```r
# Test the distance
gmpts.fit <- with(gmpts, lmer(d2ship ~ status + (1 | trackNum)))
gmpts.fit
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: d2ship ~ status + (1 | trackNum)
## REML criterion at convergence: 793.1435
## Random effects:
##  Groups   Name        Std.Dev.
##  trackNum (Intercept) 0.09385 
##  Residual             1.74937 
## Number of obs: 200, groups:  trackNum, 100
## Fixed Effects:
##      (Intercept)  statususerPoints  
##            4.427            -3.465
```

```r
summary(gmpts.fit)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: d2ship ~ status + (1 | trackNum)
## 
## REML criterion at convergence: 793.1
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.0117 -0.1996 -0.0319  0.1487  4.4918 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  trackNum (Intercept) 0.008808 0.09385 
##  Residual             3.060296 1.74937 
## Number of obs: 200, groups:  trackNum, 100
## 
## Fixed effects:
##                  Estimate Std. Error t value
## (Intercept)        4.4275     0.1752   25.27
## statususerPoints  -3.4652     0.2474  -14.01
## 
## Correlation of Fixed Effects:
##             (Intr)
## statssrPnts -0.706
```

