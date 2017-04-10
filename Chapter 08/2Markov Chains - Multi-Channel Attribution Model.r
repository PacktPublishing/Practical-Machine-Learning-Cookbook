R version 3.2.2 (2015-08-14) -- "Fire Safety"
Copyright (C) 2015 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64 (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

[Workspace loaded from ~/.RData]
 install.packages("dplyr")
 install.packages("reshape2")
 install.packages("ggplot2")
 install.packages("ChannelAttribution")
 install.packages("markovchain")
 library(dplyr)
 library(reshape2)
 library(ggplot2)
 library(ChannelAttribution)
 library(markovchain)
 
 
 datafrm1 <- data.frame(path = c('c1 > c2 > c3', 'c1', 'c2 > c3'), conv = c(1, 0, 0), conv_null = c(0, 1, 1))
 datafrm1
          path conv conv_null
1 c1 > c2 > c3    1         0
2           c1    0         1
3      c2 > c3    0         1
 
 model1 <- markov_model(datafrm1, var_path = 'path', var_conv = 'conv', var_null = 'conv_null', out_more = TRUE)
 
 model1

$result
  channel_name total_conversions
1           c1         0.1991463
2           c2         0.4004269
3           c3         0.4004269

$transition_matrix
  channel_from   channel_to transition_probability
1      (start)           c1              0.6666667
2      (start)           c2              0.3333333
3           c1           c2              0.5000000
4           c1       (null)              0.5000000
5           c2           c3              1.0000000
6           c3 (conversion)              0.5000000
7           c3       (null)              0.5000000

$removal_effects
  channel_name removal_effects
1           c1        0.497335
2           c2        1.000000
3           c3        1.000000

 datafrmresult1 <- model1$result
 
 datafrmtransmatrix1 <- model1$transition_matrix
 

 datafrmtransmatrix <- dcast(datafrmtransmatrix1, channel_from ~ channel_to, value.var = 'transition_probability')
 

 datafrmtransmatrix
  channel_from (start) (conversion) (null)        c1        c2 c3
1      (start)       0           NA     NA 0.6666667 0.3333333 NA
2 (conversion)      NA          1.0     NA        NA        NA NA
3       (null)      NA           NA    1.0        NA        NA NA
4           c1      NA           NA    0.5        NA 0.5000000 NA
5           c2      NA           NA     NA        NA        NA  1
6           c3      NA          0.5    0.5        NA        NA NA


 datafrmtransmatrix <- model1$transition_matrix
 
 datafrmtransmatrix
  channel_from   channel_to transition_probability
1      (start)           c1              0.6666667
2      (start)           c2              0.3333333
3           c1           c2              0.5000000
4           c1       (null)              0.5000000
5           c2           c3              1.0000000
6           c3 (conversion)              0.5000000
7           c3       (null)              0.5000000


 datafrmdummy <- data.frame(channel_from = c('(start)', '(conversion)', '(null)'), channel_to = c('(start)', '(conversion)', '(null)'), transition_probability = c(0, 1, 1))
 
 
 datafrmdummy
  channel_from   channel_to transition_probability
1      (start)      (start)                      0
2 (conversion) (conversion)                      1
3       (null)       (null)                      1
 
 
 datafrmtransmatrix <- rbind(datafrmtransmatrix, datafrmdummy)
 
 datafrmtransmatrix
   channel_from   channel_to transition_probability
1       (start)           c1              0.6666667
2       (start)           c2              0.3333333
3            c1           c2              0.5000000
4            c1       (null)              0.5000000
5            c2           c3              1.0000000
6            c3 (conversion)              0.5000000
7            c3       (null)              0.5000000
8       (start)      (start)              0.0000000
9  (conversion) (conversion)              1.0000000
10       (null)       (null)              1.0000000
 
 
 datafrmtransmatrix$channel_from <- factor(datafrmtransmatrix$channel_from, levels = c('(start)', '(conversion)', '(null)', 'c1', 'c2', 'c3'))
 
 datafrmtransmatrix$channel_from
 [1] (start)      (start)      c1           c1           c2           c3           c3           (start)      (conversion)
[10] (null)      
Levels: (start) (conversion) (null) c1 c2 c3
 

 datafrmtransmatrix$channel_to <- factor(datafrmtransmatrix$channel_to, levels = c('(start)', '(conversion)', '(null)', 'c1', 'c2', 'c3'))

 datafrmtransmatrix$channel_to
 [1] c1           c2           c2           (null)       c3           (conversion) (null)       (start)     
 [9] (conversion) (null)      
Levels: (start) (conversion) (null) c1 c2 c3
 
 datafrmtransmatrix <- dcast(datafrmtransmatrix, channel_from ~ channel_to, value.var = 'transition_probability')
 
 datafrmtransmatrix
  channel_from (start) (conversion) (null)        c1        c2 c3
1      (start)       0           NA     NA 0.6666667 0.3333333 NA
2 (conversion)      NA          1.0     NA        NA        NA NA
3       (null)      NA           NA    1.0        NA        NA NA
4           c1      NA           NA    0.5        NA 0.5000000 NA
5           c2      NA           NA     NA        NA        NA  1
6           c3      NA          0.5    0.5        NA        NA NA
 

 
 
 transitionmatrix <- matrix(data = as.matrix(datafrmtransmatrix[, -1]), nrow = nrow(datafrmtransmatrix[, -1]), ncol = ncol(datafrmtransmatrix[, -1]), dimnames = list(c(as.character(datafrmtransmatrix[, 1])), c(colnames(datafrmtransmatrix[, -1]))))

 transitionmatrix
             (start) (conversion) (null)        c1        c2 c3
(start)            0           NA     NA 0.6666667 0.3333333 NA
(conversion)      NA          1.0     NA        NA        NA NA
(null)            NA           NA    1.0        NA        NA NA
c1                NA           NA    0.5        NA 0.5000000 NA
c2                NA           NA     NA        NA        NA  1
c3                NA          0.5    0.5        NA        NA NA
 

 
 transitionmatrix[is.na(transitionmatrix)] <- 0

 
 transitionmatrix1 <- new("markovchain", transitionMatrix = transitionmatrix)
 
 transitionmatrix1
Unnamed Markov chain 
 A  6 - dimensional discrete Markov Chain defined by the following states: 
 (start), (conversion), (null), c1, c2, c3 
 The transition matrix  (by rows)  is defined as follows: 
             (start) (conversion) (null)        c1        c2 c3
(start)            0          0.0    0.0 0.6666667 0.3333333  0
(conversion)       0          1.0    0.0 0.0000000 0.0000000  0
(null)             0          0.0    1.0 0.0000000 0.0000000  0
c1                 0          0.0    0.5 0.0000000 0.5000000  0
c2                 0          0.0    0.0 0.0000000 0.0000000  1
c3                 0          0.5    0.5 0.0000000 0.0000000  0
 

 plot(transitionmatrix1, edge.arrow.size = 0.5, main = "Markov Graph Transition Matrix - transitionmatrix1")


 set.seed(354)

 datafrm2 <- data.frame(client_id = sample(c(1:1000), 5000, replace = TRUE), date = sample(c(1:32), 5000, replace = TRUE), channel = sample(c(0:9), 5000, replace = TRUE, prob = c(0.1, 0.15, 0.05, 0.07, 0.11, 0.07, 0.13, 0.1, 0.06, 0.16)))



 head(datafrm2)
  client_id date channel
1       411    4       5
2       761   30       6
3       509    8       6
4       541   32       8
5       156    3       9
6       934   25       8
 

 datafrm2$date <- as.Date(datafrm2$date, origin = "2016-01-01")
 
 head(datafrm2)
  client_id       date channel
1       411 2016-01-05       5
2       761 2016-01-31       6
3       509 2016-01-09       6
4       541 2016-02-02       8
5       156 2016-01-04       9
6       934 2016-01-26       8
 
 datafrm2$channel <- paste0('channel_', datafrm2$channel)
 
 head(datafrm2)
  client_id       date   channel
1       411 2016-01-05 channel_5
2       761 2016-01-31 channel_6
3       509 2016-01-09 channel_6
4       541 2016-02-02 channel_8
5       156 2016-01-04 channel_9
6       934 2016-01-26 channel_8
 
 datafrm2 <- datafrm2 %>% group_by(client_id) %>% summarise(path = paste(channel, collapse = ' > '), conv = 1, conv_null = 0) %>% ungroup()
 
 datafrm2
# A tibble: 990 × 4
   client_id
       <int>
1          1
2          2
3          3
4          4
5          5
6          6
7          7
8          8
9          9
10        10
# ... with 980 more rows, and 3 more variables: path <chr>, conv <dbl>, conv_null <dbl>


 model2 <- markov_model(datafrm2, var_path = 'path', var_conv = 'conv', var_null = 'conv_null', out_more = TRUE)


 datafrmheuristic <- datafrm2 %>% mutate(channel_name_ft = sub('>.*', '', path), channel_name_ft = sub(' ', '', channel_name_ft), channel_name_lt = sub('.*>', '', path), channel_name_lt = sub(' ', '', channel_name_lt))

 
 datafrmheuristic
# A tibble: 990 × 6
   client_id                                                                                                      path  conv conv_null
       <int>                                                                                                     <chr> <dbl>     <dbl>
1          1                                                                                                 channel_3     1         0
2          2                         channel_4 > channel_9 > channel_9 > channel_6 > channel_2 > channel_1 > channel_0     1         0
3          3 channel_7 > channel_9 > channel_9 > channel_8 > channel_8 > channel_9 > channel_5 > channel_0 > channel_0     1         0
4          4                                                             channel_1 > channel_9 > channel_0 > channel_6     1         0
5          5                                     channel_4 > channel_6 > channel_4 > channel_9 > channel_2 > channel_7     1         0
6          6                                                 channel_1 > channel_1 > channel_4 > channel_7 > channel_6     1         0
7          7                         channel_6 > channel_5 > channel_6 > channel_6 > channel_7 > channel_7 > channel_6     1         0
8          8 channel_4 > channel_5 > channel_5 > channel_3 > channel_5 > channel_6 > channel_8 > channel_4 > channel_1     1         0
9          9                                                                                     channel_4 > channel_8     1         0
10        10                                                             channel_0 > channel_4 > channel_1 > channel_1     1         0
# ... with 980 more rows, and 2 more variables: channel_name_ft <chr>, channel_name_lt <chr>
 
 
 
 datafrmfirsttouch <- datafrmheuristic %>% group_by(channel_name_ft) %>% summarise(first_touch_conversions = sum(conv)) %>% ungroup()
 
 
 datafrmfirsttouch
# A tibble: 10 × 2
   channel_name_ft first_touch_conversions
             <chr>                   <dbl>
1        channel_0                      82
2        channel_1                     159
3        channel_2                      60
4        channel_3                      71
5        channel_4                     102
6        channel_5                      75
7        channel_6                     142
8        channel_7                      83
9        channel_8                      50
10       channel_9                     166

 
 datafrmlasttouch <- datafrmheuristic %>% group_by(channel_name_lt) %>% summarise(last_touch_conversions = sum(conv)) %>% ungroup()
 
 datafrmlasttouch
# A tibble: 10 × 2
   channel_name_lt last_touch_conversions
             <chr>                  <dbl>
1        channel_0                     92
2        channel_1                    166
3        channel_2                     50
4        channel_3                     71
5        channel_4                    114
6        channel_5                     64
7        channel_6                    139
8        channel_7                     88
9        channel_8                     46
10       channel_9                    160
 
 
 
 heuristicmodel2 <- merge(datafrmfirsttouch, datafrmlasttouch, by.x = 'channel_name_ft', by.y = 'channel_name_lt')
 
 heuristicmodel2
   channel_name_ft first_touch_conversions last_touch_conversions
1        channel_0                      82                     92
2        channel_1                     159                    166
3        channel_2                      60                     50
4        channel_3                      71                     71
5        channel_4                     102                    114
6        channel_5                      75                     64
7        channel_6                     142                    139
8        channel_7                      83                     88
9        channel_8                      50                     46
10       channel_9                     166                    160

 allmodels <- merge(heuristicmodel2, model2$result, by.x = 'channel_name_ft', by.y = 'channel_name')
 
 allmodels
   channel_name_ft first_touch_conversions last_touch_conversions total_conversions
1        channel_0                      82                     92          97.59677
2        channel_1                     159                    166         139.07908
3        channel_2                      60                     50          57.98764
4        channel_3                      71                     71          73.53247
5        channel_4                     102                    114         110.94456
6        channel_5                      75                     64          82.25067
7        channel_6                     142                    139         126.11034
8        channel_7                      83                     88          98.86641
9        channel_8                      50                     46          65.69749
10       channel_9                     166                    160         137.93456

 
 colnames(allmodels)[c(1, 4)] <- c('channel_name', 'attrib_model_conversions')

 datafrmplottransition <- model2$transition_matrix
 
 cols <- c("#e7f0fa", "#c9e2f6", "#95cbee", "#0099dc", "#4ab04a", "#ffd73e", "#eec73a", "#e29421", "#e29421", "#f05336", "#ce472e")
 
 
 t <- max(datafrmplottransition$transition_probability)
 
 t
[1] 0.2391931
 
 
 
 ggplot(datafrmplottransition, aes(y = channel_from, x = channel_to, fill = transition_probability)) +
     theme_minimal() + geom_tile(colour = "white", width = .9, height = .9) + scale_fill_gradientn(colours = cols, limits = c(0, t), breaks = seq(0, t, by = t/4), labels = c("0", round(t/4*1, 2), round(t/4*2, 2), round(t/4*3, 2), round(t/4*4, 2)), guide = guide_colourbar(ticks = T, nbin = 50, barheight = .5, label = T, barwidth = 10)) + geom_text(aes(label = round(transition_probability, 2)), fontface = "bold", size = 4) +
     theme(legend.position = 'bottom',
           legend.direction = "horizontal",
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           plot.title = element_text(size = 20, face = "bold", vjust = 2, color = 'black', lineheight = 0.8),
           axis.title.x = element_text(size = 24, face = "bold"),
           axis.title.y = element_text(size = 24, face = "bold"),
           axis.text.y = element_text(size = 8, face = "bold", color = 'black'),
           axis.text.x = element_text(size = 8, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain")) +
     ggtitle("Heatmap - Transition Matrix ")


