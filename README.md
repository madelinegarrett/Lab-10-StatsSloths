# Lab-10-StatsSloths
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(data.table)
library(tidyverse)
```
## Team Section

```{r}
answers <- read_csv("Answers_trunc.csv")
questions <- read_csv("Questions_trunc.csv")

merged <- answers %>%
  left_join(questions, c('ParentId' = 'Id')) %>%
  mutate(total_time = CreationDate.x - CreationDate.y) %>%
  filter(total_time>0)

ggplot(data = (merged)) + 
  geom_jitter(mapping = aes(x = total_time, y = Score.x))
```
```{r}
answers <- read_csv("Answers_trunc.csv") %>%
  separate(CreationDate, into = c("Date", "Time"), sep = " ", convert = TRUE)%>%
  separate(Date, into = c("Year", "Month", "Day"), sep = "-", convert = TRUE)%>%
  separate(Time, into = c("Hour", "Minute", "Second"), sep = ":", convert=TRUE)
questions <- read.csv("Questions_trunc.csv") %>%
  separate(CreationDate, into = c("Date", "Time"), sep = "T", convert = TRUE)%>%
  separate(Date, into = c("Year", "Month", "Day"), sep = "-", convert = TRUE)%>%
  separate(Time, into = c("Hour", "Minute", "Second"), sep = ":", convert=TRUE)%>%
  separate(Second, into = c("Second", "Z"), sep = "Z", convert=TRUE)
questions$Z <- NULL
```
## Individual Sections
### Kevin's Section: 
* Feature Question: Does adding a link that describes what they're talking about affect score?

```{r}
questions <- (read_csv("Questions_trunc.csv")) %>%
  select(-7)
answers <- (read_csv("Answers_trunc.csv")) %>%
  select(-7)
a <- questions %>%
  summarise(quantile(Score, 0.90))
b <- answers %>%
  summarise(quantile(Score, 0.90))

q_ind <- questions %>%
  mutate(link = str_detect(Body, "http:")) %>%
  mutate(s = case_when((Score>=41)~"high", ((Score<41)~"low")))
ggplot(data = q_ind) +
  geom_bar(mapping = aes(s)) +
  facet_wrap(~ link)

a_ind <- answers %>%
  mutate(link = str_detect(Body, "http:")) %>%
  mutate(s = case_when((Score>=19)~"high", ((Score<19)~"low")))
ggplot(data = a_ind) +
  geom_bar(mapping = aes(s)) +
  facet_wrap(~ link)
```
