---
title: "Using Machine Learning to Predict the Outcome of a Play in the National Football League"
date: 2023-07-01 00:00:00 + 0000
categories: [NFL PBP]
tags: [NFL,PBP,Regression, Machine Learning]
---

I will be using supervised learning algorithms to predict the
categorical outcome of a play in the National Football League using 2009
to 2023 play-by-play NFL data provided by nflfastR. I see the future of
the NFL to have data scientists with fast computing machines to predict
real time the next play. Would this ruin the game? Maybe. But I am
confident that football, a sport that is dear to my heart, will become
more exciting as time goes on.

### Load libraries
``` r
library(sjlabelled)
library(labelled)
library(janitor)
library(nflfastR)
library(tidyverse)
library(gtsummary)
```

### Load Data

``` r
pbp <- load_pbp(c(2020:2022)) 
```

``` r
data <- pbp %>%
  select(game_id, desc, home_team, away_team,season_type, play_type,ydstogo, qtr, down, game_seconds_remaining,
         yardline_100, yrdln, drive, season, season_type, away_score, home_score, rush_attempt, pass_attempt) %>%
  filter(rush_attempt == 1 | pass_attempt == 1) %>%
  filter(play_type == "pass" | play_type == "run") %>% # onyl want to predict pass or run 
  mutate(game_state = case_when(away_score > home_score ~ 0,
                                away_score < home_score ~ 1,
                                away_score == home_score ~2 )) %>%
  set_value_labels(game_state = c("Away Team Up" = 0,
                                     "Home Team Up" = 1,
                                     "Tie" = 2))
```

### Factor categorical variables

``` r
data_logreg <- data %>%
  mutate(play_type_factor = recode(play_type,
    "pass" = 1,
    "run" = 0
  )) %>%
   mutate(play_type_factor = as.factor(play_type_factor), #outcome variable
         qtr_factor = as.factor(qtr),
         down_factor = as.factor(down)
  ) 
```

Training Data

``` r
library(caret)
Train <- createDataPartition(data_logreg$play_type_factor, p=.6, list = FALSE)
training <- data_logreg[Train,]
testing <- data_logreg[-Train,]
```

Logistic Regression Model

``` r
mylogit <- glm(play_type_factor ~ qtr_factor + down_factor+ drive + ydstogo + game_seconds_remaining,
               family = "binomial",
               data = data_logreg)
```

All my variables seem to be statistically significant. Am I overfitting
or did my literature review and NFL background pay off? Should run more
model diagnostics to confirm.

``` r
tbl_regression(mylogit, exponentiate = TRUE)
```

<div id="mddzrlgkhv" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#mddzrlgkhv table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#mddzrlgkhv thead, #mddzrlgkhv tbody, #mddzrlgkhv tfoot, #mddzrlgkhv tr, #mddzrlgkhv td, #mddzrlgkhv th {
  border-style: none;
}
&#10;#mddzrlgkhv p {
  margin: 0;
  padding: 0;
}
&#10;#mddzrlgkhv .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#mddzrlgkhv .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#mddzrlgkhv .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#mddzrlgkhv .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#mddzrlgkhv .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#mddzrlgkhv .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#mddzrlgkhv .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#mddzrlgkhv .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#mddzrlgkhv .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#mddzrlgkhv .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#mddzrlgkhv .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#mddzrlgkhv .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#mddzrlgkhv .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#mddzrlgkhv .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#mddzrlgkhv .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#mddzrlgkhv .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#mddzrlgkhv .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#mddzrlgkhv .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#mddzrlgkhv .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mddzrlgkhv .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#mddzrlgkhv .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#mddzrlgkhv .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#mddzrlgkhv .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mddzrlgkhv .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#mddzrlgkhv .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#mddzrlgkhv .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#mddzrlgkhv .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mddzrlgkhv .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#mddzrlgkhv .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#mddzrlgkhv .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#mddzrlgkhv .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#mddzrlgkhv .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#mddzrlgkhv .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mddzrlgkhv .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#mddzrlgkhv .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mddzrlgkhv .gt_left {
  text-align: left;
}
&#10;#mddzrlgkhv .gt_center {
  text-align: center;
}
&#10;#mddzrlgkhv .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#mddzrlgkhv .gt_font_normal {
  font-weight: normal;
}
&#10;#mddzrlgkhv .gt_font_bold {
  font-weight: bold;
}
&#10;#mddzrlgkhv .gt_font_italic {
  font-style: italic;
}
&#10;#mddzrlgkhv .gt_super {
  font-size: 65%;
}
&#10;#mddzrlgkhv .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#mddzrlgkhv .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#mddzrlgkhv .gt_indent_1 {
  text-indent: 5px;
}
&#10;#mddzrlgkhv .gt_indent_2 {
  text-indent: 10px;
}
&#10;#mddzrlgkhv .gt_indent_3 {
  text-indent: 15px;
}
&#10;#mddzrlgkhv .gt_indent_4 {
  text-indent: 20px;
}
&#10;#mddzrlgkhv .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;OR&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>OR</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;95% CI&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>95% CI</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">qtr_factor</td>
<td headers="estimate" class="gt_row gt_center"></td>
<td headers="ci" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    1</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="ci" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    2</td>
<td headers="estimate" class="gt_row gt_center">0.79</td>
<td headers="ci" class="gt_row gt_center">0.75, 0.84</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    3</td>
<td headers="estimate" class="gt_row gt_center">0.39</td>
<td headers="ci" class="gt_row gt_center">0.35, 0.43</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    4</td>
<td headers="estimate" class="gt_row gt_center">0.27</td>
<td headers="ci" class="gt_row gt_center">0.24, 0.31</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    5</td>
<td headers="estimate" class="gt_row gt_center">0.25</td>
<td headers="ci" class="gt_row gt_center">0.21, 0.31</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">down_factor</td>
<td headers="estimate" class="gt_row gt_center"></td>
<td headers="ci" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    1</td>
<td headers="estimate" class="gt_row gt_center">—</td>
<td headers="ci" class="gt_row gt_center">—</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    2</td>
<td headers="estimate" class="gt_row gt_center">2.21</td>
<td headers="ci" class="gt_row gt_center">2.14, 2.28</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    3</td>
<td headers="estimate" class="gt_row gt_center">5.85</td>
<td headers="ci" class="gt_row gt_center">5.61, 6.11</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    4</td>
<td headers="estimate" class="gt_row gt_center">4.04</td>
<td headers="ci" class="gt_row gt_center">3.67, 4.44</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">drive</td>
<td headers="estimate" class="gt_row gt_center">1.01</td>
<td headers="ci" class="gt_row gt_center">1.00, 1.02</td>
<td headers="p.value" class="gt_row gt_center">0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">ydstogo</td>
<td headers="estimate" class="gt_row gt_center">1.16</td>
<td headers="ci" class="gt_row gt_center">1.16, 1.17</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">game_seconds_remaining</td>
<td headers="estimate" class="gt_row gt_center">1.00</td>
<td headers="ci" class="gt_row gt_center">1.00, 1.00</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> OR = Odds Ratio, CI = Confidence Interval</td>
    </tr>
  </tfoot>
</table>
</div>

3rd quarter makes it more likely to rush (which makes sense because
usually teams will only have a few yards to go after 2 downs)

Let’s take a look at the confidence intervals

``` r
confint(mylogit)
```

    ## Waiting for profiling to be done...

    ##                                2.5 %        97.5 %
    ## (Intercept)            -0.3543595709  0.0473492971
    ## qtr_factor2            -0.2883994179 -0.1711276965
    ## qtr_factor3            -1.0391956494 -0.8503129287
    ## qtr_factor4            -1.4338814036 -1.1621043799
    ## qtr_factor5            -1.5741253818 -1.1600884641
    ## down_factor2            0.7613492223  0.8243904797
    ## down_factor3            1.7240887414  1.8101421944
    ## down_factor4            1.3014239271  1.4910800640
    ## drive                   0.0040716346  0.0164015779
    ## ydstogo                 0.1443445387  0.1533391839
    ## game_seconds_remaining -0.0005346189 -0.0004187792

Predicted probabilities and graph them with their standard errors to
produce a confidence interval

``` r
newdata <- predict(mylogit, 
                        newdata = testing,
                        type = "link",
                        se = TRUE)

#i get an error. so now i need to make the make them a dataframe
df1<- data.frame(matrix(unlist(newdata$fit), ncol = 1 , byrow = TRUE))
df2 <- data.frame(matrix(unlist(newdata$se.fit), ncol = 1 , byrow = TRUE))
df3 <- data.frame(matrix(unlist(newdata$residual.scale), ncol = 1 , byrow = TRUE))
df4 <- bind_cols(df1,df2,df3)

colnames(df4)[1] = "fit"
colnames(df4)[2] = "se.fit"
colnames(df4)[3] = "residual.scale"

newdata3 <- cbind(testing, df4)
newdata3 <- within(newdata3, {
    PredictedProb <- plogis(fit)
    LL <- plogis(fit - (1.96 * se.fit))
    UL <- plogis(fit + (1.96 * se.fit))
})
```

``` r
head(newdata3, 5)
```

    ##           game_id
    ## 1: 2020_01_ARI_SF
    ## 2: 2020_01_ARI_SF
    ## 3: 2020_01_ARI_SF
    ## 4: 2020_01_ARI_SF
    ## 5: 2020_01_ARI_SF
    ##                                                                                                                                                                                                          desc
    ## 1: (15:00) (Shotgun) 10-J.Garoppolo pass short right to 85-G.Kittle to SF 30 for 5 yards (48-I.Simmons). PENALTY on ARI-48-I.Simmons, Horse Collar Tackle, 15 yards, enforced at SF 30. Caught at SF29. 1-YAC
    ## 2:                                                                                                                              (13:21) (Shotgun) 31-R.Mostert right end to ARI 45 for -6 yards (58-J.Hicks).
    ## 3:                                                                                                         (11:50) (Shotgun) 1-K.Murray pass short left to 10-D.Hopkins to ARI 28 for 3 yards (41-E.Moseley).
    ## 4:                                                                                   (9:53) (Shotgun) 1-K.Murray pass short left to 10-D.Hopkins to ARI 26 for 1 yard (25-R.Sherman). caught at ARI 21, 5 YAC
    ## 5:                                                                                                                            (8:43) (No Huddle, Shotgun) 1-K.Murray pass incomplete deep right to 13-C.Kirk.
    ##    home_team away_team season_type play_type ydstogo qtr down
    ## 1:        SF       ARI         REG      pass      10   1    1
    ## 2:        SF       ARI         REG       run       8   1    2
    ## 3:        SF       ARI         REG      pass      10   1    1
    ## 4:        SF       ARI         REG      pass      10   1    1
    ## 5:        SF       ARI         REG      pass       7   1    3
    ##    game_seconds_remaining yardline_100  yrdln drive season away_score
    ## 1:                   3600           75  SF 25     1   2020         24
    ## 2:                   3501           39 ARI 39     1   2020         24
    ## 3:                   3410           75 ARI 25     2   2020         24
    ## 4:                   3293           75 ARI 25     4   2020         24
    ## 5:                   3223           72 ARI 28     4   2020         24
    ##    home_score rush_attempt pass_attempt game_state play_type_factor qtr_factor
    ## 1:         20            0            1          0                1          1
    ## 2:         20            1            0          0                0          1
    ## 3:         20            0            1          0                1          1
    ## 4:         20            0            1          0                1          1
    ## 5:         20            0            1          0                1          1
    ##    down_factor        fit     se.fit residual.scale        UL        LL
    ## 1:           1 -0.3710314 0.01855846              1 0.4171080 0.3995343
    ## 2:           2  0.1713414 0.01847960              1 0.5517049 0.5337290
    ## 3:           1 -0.2702256 0.01636019              1 0.4407399 0.4249974
    ## 4:           1 -0.1939827 0.01630168              1 0.4595805 0.4437556
    ## 5:           3  1.1599136 0.02131109              1 0.7688241 0.7536443
    ##    PredictedProb
    ## 1:     0.4082918
    ## 2:     0.5427309
    ## 3:     0.4328517
    ## 4:     0.4516558
    ## 5:     0.7613170

Looks like the predicted probability goes down as the yards to go
increases The predicted probability is highest with the first down

``` r
newdata3 %>%
  drop_na(down_factor) %>%
ggplot(aes(x = ydstogo , y = PredictedProb)) + 
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = down_factor), alpha = 0.2) + 
  geom_line(aes(colour = down_factor), 
            size = 1)
```


![](https://raw.githubusercontent.com/sbudataanlyst/nbaproject/main/000020.png)<!-- -->
Confusion Matrix

``` r
library(caret)
pred <- predict(mylogit,
                     testing,
                     type ="response")
# If p exceeds threshold of 0.5, 1 else 0
play_type_pred <- ifelse(pred > 0.5, 1, 0)

# Convert to factor: p_class
p_class <- factor(play_type_pred, levels = levels(testing[["play_type_factor"]]))



accuracy <- table(p_class, testing$play_type_factor)
sum(diag(accuracy))/sum(accuracy)
```

    ## [1] 0.6287572

``` r
confusionMatrix(data = p_class,  #predicted classes
                reference = testing$play_type_factor) #true results ) 
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction     0     1
    ##          0  7980  6079
    ##          1  9508 18419
    ##                                           
    ##                Accuracy : 0.6288          
    ##                  95% CI : (0.6241, 0.6334)
    ##     No Information Rate : 0.5835          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.2142          
    ##                                           
    ##  Mcnemar's Test P-Value : < 2.2e-16       
    ##                                           
    ##             Sensitivity : 0.4563          
    ##             Specificity : 0.7519          
    ##          Pos Pred Value : 0.5676          
    ##          Neg Pred Value : 0.6595          
    ##              Prevalence : 0.4165          
    ##          Detection Rate : 0.1901          
    ##    Detection Prevalence : 0.3348          
    ##       Balanced Accuracy : 0.6041          
    ##                                           
    ##        'Positive' Class : 0               
    ## 

The accuracy of our predicted model is 63% ! Not bad but we can do
better. Let’s try Naive Bayes, KNN, Decision Tree, Random Forest, &
Kernal Support Vector Machine next.
