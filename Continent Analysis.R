install.packages("gapminder")
library(gapminder)
data("gapminder")

summary(gapminder)

x <- mean(gapminder$gdpPercap)

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

library(ggplot2)

gapminder %>% 
  filter(gdpPercap < 5000) %>% 
  ggplot(aes(x=gdpPercap, y=lifeExp, col = year, size=pop)) +
  geom_point(alpha=0.3)+
  geom_smooth(method =lm)+
  facet_wrap(~continent)

summary(lm(lifeExp ~ gdpPercap+pop))
