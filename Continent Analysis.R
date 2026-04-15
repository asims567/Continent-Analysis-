install.packages("gapminder")
library(gapminder)
data("gapminder")

country   continent  year lifeExp    pop
   <fct>     <fct>     <int>   <dbl>  <int>
 1 Afghanis… Asia       1952    28.8 8.43e6
 2 Afghanis… Asia       1957    30.3 9.24e6
 3 Afghanis… Asia       1962    32.0 1.03e7
 4 Afghanis… Asia       1967    34.0 1.15e7
 5 Afghanis… Asia       1972    36.1 1.31e7
 6 Afghanis… Asia       1977    38.4 1.49e7
 7 Afghanis… Asia       1982    39.9 1.29e7
 8 Afghanis… Asia       1987    40.8 1.39e7
 9 Afghanis… Asia       1992    41.7 1.63e7
10 Afghanis… Asia       1997    41.8 2.22e7
# ℹ 1,694 more rows
# ℹ 1 more variable: gdpPercap <dbl>
# ℹ Use `print(n = ...)` to see more rows

summary(gapminder)
Country        continent  
 Afghanistan:  12   Africa  :624  
 Albania    :  12   Americas:300  
 Algeria    :  12   Asia    :396  
 Angola     :  12   Europe  :360  
 Argentina  :  12   Oceania : 24  
 Australia  :  12                 
 (Other)    :1632                 
      year         lifeExp     
 Min.   :1952   Min.   :23.60  
 1st Qu.:1966   1st Qu.:48.20  
 Median :1980   Median :60.71  
 Mean   :1980   Mean   :59.47  
 3rd Qu.:1993   3rd Qu.:70.85  
 Max.   :2007   Max.   :82.60  
                               
      pop              gdpPercap       
 Min.   :6.001e+04   Min.   :   241.2  
 1st Qu.:2.794e+06   1st Qu.:  1202.1  
 Median :7.024e+06   Median :  3531.8  
 Mean   :2.960e+07   Mean   :  7215.3  
 3rd Qu.:1.959e+07   3rd Qu.:  9325.5  
 Max.   :1.319e+09   Max.   :113523.1 


mean(gapminder$gdpPercap)
[1] 7215.327

attach(gapminder)
median(pop)
hist(lifeExp)
hist(log(pop))
boxplot(lifeExp ~ continent)
plot(lifeExp ~ log(gdpPercap))

install.packages("dyplr")
library(dplyr)

df1 <- gapminder %>% 
  select(country, lifeExp) %>% 
  filter(country == "South Africa" | 
           country == "Ireland") 

t.test(data = df1, lifeExp ~ country)

Welch Two-Sample t-test

data:  lifeExp by country
t = 10.067, df = 19.109, p-value =
4.466e-09
Null hypothesis: no true difference in means between group Ireland and group South Africa is equal to 0.
Alternative hypothesis: true difference in means between group Ireland and group South Africa is not equal to 0
95 percent confidence interval:
 15.07022 22.97794
sample estimates:
     mean in group Ireland 
                  73.01725 
mean in group South Africa 
                  53.99317 


library(ggplot2)

gapminder %>% 
  filter(gdpPercap < 5000) %>% 
  ggplot(aes(x=gdpPercap, y=lifeExp, col = year, size=pop)) +
  geom_point(alpha=0.3)+
  geom_smooth(method =lm)+
  facet_wrap(~continent)

lm(lifeExp ~ gdpPercap)

Call:
lm(formula = lifeExp ~ gdpPercap)

Coefficients:
(Intercept)    gdpPercap  
  5.396e+01    7.649e-04  

summary(lm(lifeExp ~ gdpPercap))

Call:
lm(formula = lifeExp ~ gdpPercap)

Residuals:
    Min      1Q  Median      3Q 
-82.754  -7.758   2.176   8.225 
    Max 
 18.426 

Coefficients:
             Estimate Std. Error
(Intercept) 5.396e+01  3.150e-01
gdpPercap   7.649e-04  2.579e-05
            t value Pr(>|t|)    
(Intercept)  171.29   <2e-16 ***
gdpPercap     29.66   <2e-16 ***
---
Signif. codes:  
  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’
  0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 10.49 on 1702 degrees of freedom
Multiple R-squared:  0.3407,	Adjusted R-squared:  0.3403 
F-statistic: 879.6 on 1 and 1702 DF,  p-value: < 2.2e-16


summary(lm(lifeExp ~ gdpPercap+pop))

Call:
lm(formula = lifeExp ~ gdpPercap + pop)

Residuals:
    Min      1Q  Median      3Q 
-82.754  -7.745   2.055   8.212 
    Max 
 18.534 

Coefficients:
             Estimate Std. Error
(Intercept) 5.365e+01  3.225e-01
gdpPercap   7.676e-04  2.568e-05
pop         9.728e-09  2.385e-09
            t value Pr(>|t|)    
(Intercept)  166.36  < 2e-16 ***
gdpPercap     29.89  < 2e-16 ***
pop            4.08 4.72e-05 ***
---
Signif. codes:  
  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’
  0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 10.44 on 1701 degrees of freedom
Multiple R-squared:  0.3471,	Adjusted R-squared:  0.3463 
F-statistic: 452.2 on 2 and 1701 DF,  p-value: < 2.2e-16

