library(tidyverse)
library(rvest)

url <-  "http://loterias.caixa.gov.br/wps/portal/loterias/landing/megasena/!ut/p/a1/04_Sj9CPykssy0xPLMnMz0vMAfGjzOLNDH0MPAzcDbwMPI0sDBxNXAOMwrzCjA0sjIEKIoEKnN0dPUzMfQwMDEwsjAw8XZw8XMwtfQ0MPM2I02-AAzgaENIfrh-FqsQ9wNnUwNHfxcnSwBgIDUyhCvA5EawAjxsKckMjDDI9FQE-F4ca/dl5/d5/L2dBISEvZ0FBIS9nQSEh/pw/Z7_HGK818G0K8DBC0QPVN93KQ10G1/res/id=historicoHTML/c=cacheLevelPage/=/"

megasena <- read_html(url)

dez1 <- html_text(megasena %>% html_elements("tr :nth-child(4)")) %>% as_tibble()
dez2 <- html_text(megasena %>% html_elements("tr :nth-child(5)")) %>% as_tibble()
dez3 <- html_text(megasena %>% html_elements("tr :nth-child(6)")) %>% as_tibble()
dez4 <- html_text(megasena %>% html_elements("tr :nth-child(7)")) %>% as_tibble()
dez5 <- html_text(megasena %>% html_elements("tr :nth-child(8)")) %>% as_tibble()
dez6 <- html_text(megasena %>% html_elements("tr :nth-child(9)")) %>% as_tibble()

dez1$value <- as.numeric(dez1$value)
dez1 <- dez1 %>% drop_na()

dez2$value <- as.numeric(dez2$value)
dez2 <- dez2 %>% drop_na()

dez3$value <- as.numeric(dez3$value)
dez3 <- dez3 %>% drop_na()

dez4$value <- as.numeric(dez4$value)
dez4 <- dez4 %>% drop_na()

dez5$value <- as.numeric(dez5$value)
dez5 <- dez5 %>% drop_na()

dez6$value <- as.numeric(dez6$value)
dez6 <- dez6 %>% drop_na()

tabela <- cbind(dez1, dez2, dez3, dez4, dez5, dez6)

colnames(tabela) <- c("dez1","dez2","dez3","dez4","dez5","dez6")

tabela <- tabela %>%
  mutate(sorteio = 1:length(dez1))

tabela2 <- rbind(dez1, dez2, dez3, dez4, dez5, dez6)

hist(tabela$value)

# Números que mais sairam
mais_sairam <- tabela2 %>% 
  group_by(value) %>% 
  summarise(vezes = n()) %>% 
  arrange(desc(vezes)) %>% 
  top_n(6, wt = vezes) %>% 
  print()

# Números que menos sairam
menos_sairam <- tabela2 %>% 
  group_by(value) %>% 
  summarise(vezes = n()) %>% 
  arrange(desc(-vezes)) %>% 
  top_n(-6, wt = vezes) %>% 
  print()

# Números que não saem há mais tempo

