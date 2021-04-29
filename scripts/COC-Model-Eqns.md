COC Model Equations
================
Eva Dusek Jennings
April 27, 2021

# Copper Models

``` r
load(here("Copper Models.RData"))

Cu.coc2 <- coc2
Cu.r1X <- r1X
Cu.vf1X <- vf1X
Cu.Form4 <- Form4
Cu.Form5 <- Form5

Cu.M1 <- gls(data=Cu.coc2, result~1, method="REML")  #here, we use the actual concentration, rather than the transformed one
Cu.M3 <- lme(data=Cu.coc2, result~landuse+rain, random = Cu.r1X, method="REML", weights=Cu.vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
Cu.M4 <- lme(data=Cu.coc2, Cu.Form4, random = Cu.r1X, method="REML", weights=Cu.vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))
Cu.M5 <- lme(data=Cu.coc2, Cu.Form5, random = Cu.r1X, method="REML", weights=Cu.vf1X, control = lmeControl(maxIter = 1e8, msMaxIter = 1e8))

Cu_models <- list(Cu.M1,Cu.M3,Cu.M4,Cu.M5)

htmlreg(Cu_models)
```

    ## <table class="texreg" style="margin: 10px auto;border-collapse: collapse;border-spacing: 0px;caption-side: bottom;color: #000000;border-top: 2px solid #000000;">
    ## <caption>Statistical models</caption>
    ## <thead>
    ## <tr>
    ## <th style="padding-left: 5px;padding-right: 5px;">&nbsp;</th>
    ## <th style="padding-left: 5px;padding-right: 5px;">Model 1</th>
    ## <th style="padding-left: 5px;padding-right: 5px;">Model 2</th>
    ## <th style="padding-left: 5px;padding-right: 5px;">Model 3</th>
    ## <th style="padding-left: 5px;padding-right: 5px;">Model 4</th>
    ## </tr>
    ## </thead>
    ## <tbody>
    ## <tr style="border-top: 1px solid #000000;">
    ## <td style="padding-left: 5px;padding-right: 5px;">(Intercept)</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">2.17<sup>***</sup></td>
    ## <td style="padding-left: 5px;padding-right: 5px;">1.01<sup>**</sup></td>
    ## <td style="padding-left: 5px;padding-right: 5px;">1.87<sup>***</sup></td>
    ## <td style="padding-left: 5px;padding-right: 5px;">0.99<sup>*</sup></td>
    ## </tr>
    ## <tr>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">(0.05)</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">(0.34)</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">(0.41)</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">(0.48)</td>
    ## </tr>
    ## <tr>
    ## <td style="padding-left: 5px;padding-right: 5px;">landuse2.HDR</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">0.61<sup>***</sup></td>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">0.59<sup>***</sup></td>
    ## </tr>
    ## <tr>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">(0.10)</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">(0.10)</td>
    ## </tr>
    ## <tr>
    ## <td style="padding-left: 5px;padding-right: 5px;">landuse3.COM</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">1.72<sup>***</sup></td>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">1.54<sup>***</sup></td>
    ## </tr>
    ## <tr>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">(0.10)</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">(0.16)</td>
    ## </tr>
    ## <tr>
    ## <td style="padding-left: 5px;padding-right: 5px;">landuse4.IND</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">0.94<sup>***</sup></td>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">0.87<sup>***</sup></td>
    ## </tr>
    ## <tr>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">(0.11)</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">(0.14)</td>
    ## </tr>
    ## <tr>
    ## <td style="padding-left: 5px;padding-right: 5px;">rain</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">-0.18<sup>***</sup></td>
    ## <td style="padding-left: 5px;padding-right: 5px;">-0.17<sup>***</sup></td>
    ## <td style="padding-left: 5px;padding-right: 5px;">-0.19<sup>***</sup></td>
    ## </tr>
    ## <tr>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">(0.03)</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">(0.03)</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">(0.03)</td>
    ## </tr>
    ## <tr>
    ## <td style="padding-left: 5px;padding-right: 5px;">paved</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">0.39<sup>***</sup></td>
    ## <td style="padding-left: 5px;padding-right: 5px;">0.26<sup>**</sup></td>
    ## </tr>
    ## <tr>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">(0.06)</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">(0.09)</td>
    ## </tr>
    ## <tr>
    ## <td style="padding-left: 5px;padding-right: 5px;">trees</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">-0.49<sup>***</sup></td>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## </tr>
    ## <tr>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">(0.05)</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## </tr>
    ## <tr>
    ## <td style="padding-left: 5px;padding-right: 5px;">rain:paved</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">-0.07<sup>*</sup></td>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## </tr>
    ## <tr>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">(0.03)</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## </tr>
    ## <tr>
    ## <td style="padding-left: 5px;padding-right: 5px;">traffic</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">-0.27<sup>**</sup></td>
    ## </tr>
    ## <tr>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">(0.08)</td>
    ## </tr>
    ## <tr style="border-top: 1px solid #000000;">
    ## <td style="padding-left: 5px;padding-right: 5px;">AIC</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">1624.69</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">955.93</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">971.75</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">954.74</td>
    ## </tr>
    ## <tr>
    ## <td style="padding-left: 5px;padding-right: 5px;">BIC</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">1633.17</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">1015.18</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">1031.00</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">1022.40</td>
    ## </tr>
    ## <tr>
    ## <td style="padding-left: 5px;padding-right: 5px;">Log Likelihood</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">-810.34</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">-463.96</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">-471.88</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">-461.37</td>
    ## </tr>
    ## <tr>
    ## <td style="padding-left: 5px;padding-right: 5px;">Num. obs.</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">514</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">514</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">514</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">514</td>
    ## </tr>
    ## <tr style="border-bottom: 2px solid #000000;">
    ## <td style="padding-left: 5px;padding-right: 5px;">Num. groups: agency</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">&nbsp;</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">6</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">6</td>
    ## <td style="padding-left: 5px;padding-right: 5px;">6</td>
    ## </tr>
    ## </tbody>
    ## <tfoot>
    ## <tr>
    ## <td style="font-size: 0.8em;" colspan="5"><sup>***</sup>p &lt; 0.001; <sup>**</sup>p &lt; 0.01; <sup>*</sup>p &lt; 0.05</td>
    ## </tr>
    ## </tfoot>
    ## </table>

\#—————\# \# Zinc Models \# \#—————\#

\#————–\# \# TSS Models \# \#————–\#

\#————————–\# \# Nitrite-Nitrate Models \# \#————————–\#
