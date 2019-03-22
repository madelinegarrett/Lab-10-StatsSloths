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
* Feature Question: Does adding a link that describes what is being asked or answered affect score?
* Why: Whenever I am looking for answers online, the first answer I see does not always make sense to me so I find it helpful when another explanation or link is provided. This makes me want to know if this is true for all people.

```{r}
questions <- (read_csv("Questions_trunc.csv")) %>%
  select(-7)
answers <- (read_csv("Answers_trunc.csv")) %>%
  select(-7)
a <- questions %>%
  summarise(quantile(Score, 0.90))
#a = 41
b <- answers %>%
  summarise(quantile(Score, 0.90))
#b = 19

q_ind <- questions %>%
  mutate(link = str_detect(Body, "http:")) %>%
  mutate(s = case_when((Score>=41)~"high", ((Score<41)~"low")))
ggplot(data = q_ind) +
  geom_bar(mapping = aes(s, fill = s)) +
  facet_wrap(~ link) +
  labs(title = "Effect of Link on Score for Questions", x = "Score", y = "Count")

a_ind <- answers %>%
  mutate(link = str_detect(Body, "http:")) %>%
  mutate(s = case_when((Score>=19)~"high", ((Score<19)~"low")))
ggplot(data = a_ind) +
  geom_bar(mapping = aes(s, fill = s)) +
  facet_wrap(~ link) +
  labs(title = "Effect of Link on Score for Answers", x = "Score", y = "Count")
```

* Findings: For both questions and answers, if a link providing further explanation of the topic is included, the ratio of high scores to low scores increases. This means that there is a higher chance of a question or answer with a link to earn a higher score.
* Explanation: This finding can be explained by the fact that including a link provides the reader with a second interpretation of what is being described. This means it is more likely the reader will understand what is being said, thus leading to them giving an upvote.

## Team Report:
* I, Kevin Luth, looked into whether the inclusion of a link to a source further explaining the topic of discussion leads to higher scores. I first found the score representing the 90th percentile in both the questions and answers dataset using the summarise and quantile function and then mutated a column indicating if the score was high or low based on the 90th percentile number. Next I used str_detect in  both the questions and answers to find any posts that include "http:" which signifies a link. I plotted my results using a bar graph that was color coded to show if the post received a high or low score. I also used a facet_wrap by whether or not the post had a link.
