# Laster ned nødvendige pakker 
library(WDI)
library(tidyverse)

# søk på "imports"
imports <- WDIsearch('imports') %>% as_tibble()
imports

# lagrer tabellen
df_import <- WDI(indicator = "NE.IMP.GNFS.CD", country = "all")
head(df_import)

# ser på importen til Norge
df_import %>%
  filter(country=="Norway") %>% 
  rename(import=NE.IMP.GNFS.CD,
         år=year) %>%
  mutate(import=import/1e9) %>% 
  ggplot(aes(x=år, y=import)) +
  geom_line(col="dark blue") +
  labs(title="Norsk import av varer og tjenester \n (nominelle tall)",
       x =" ",
       y = "milliarder US$") +
  theme_bw()

# Oppgave 1
df_import %>% 
  filter(country=="Norway") %>% 
  rename(import=NE.IMP.GNFS.CD,
         år=year) %>%
  mutate(import=import/1e9) %>% 
  mutate("prosentvis endring" = 100*(import - lag(import))/lag(import)) %>% 
  head()

# lager plot som viser endringen
df_import %>% 
  filter(country=="Norway") %>% 
  rename(import=NE.IMP.GNFS.CD,
         år=year) %>%
  mutate(import=import/1e9) %>% 
  mutate(prosendring = 100*(import - lag(import))/lag(import)) %>% 
  ggplot(aes(x=år, y=prosendring)) +
  geom_line(col="green") +
  theme_bw() +
  xlab("ÅR") +
  ylab("Prosntvis endring") +
  ggtitle("Prosntvis endring i Norges import")

# Søk på "Eksport"
exports <- WDIsearch('exports') %>% as_tibble()
exports

# Lagrer tabellen
df_export <- WDI(indicator = "NE.EXP.GNFS.CD", country = "all")
head(df_export)

# Ser på eksporten til Norge
df_export %>%
  filter(country=="Norway") %>% 
  rename(eksport=NE.EXP.GNFS.CD,
         år=year) %>%
  mutate(eksport=eksport/1e9) %>% 
  ggplot(aes(x=år, y=eksport)) +
  geom_line(col="dark red") +
  labs(title="Norsk eksport av varer og tjenester \n (nominelle tall)",
       x =" ",
       y = "milliarder US$") +
  theme_bw()

# Oppgave 2

df_export %>%
  filter(country=="Norway") %>% 
  rename(eksport=NE.EXP.GNFS.CD,
         år=year) %>%
  mutate(eksport=eksport/1e9) %>%
  mutate("prosentvis endring" = 100*(eksport - lag(eksport))/lag(eksport)) %>% 
  head()

# lager et pott som viser den prosentvise endringen 
df_export %>%
  filter(country=="Norway") %>% 
  rename(eksport=NE.EXP.GNFS.CD,
         år=year) %>%
  mutate(eksport=eksport/1e9) %>%
  mutate(prosend = 100*(eksport - lag(eksport))/lag(eksport)) %>%
  ggplot(aes(x=år, y=prosend)) + geom_line(col="blue") +
  theme_bw() +
  xlab("ÅR") +
  ylab("Prosntvis endring") +
  ggtitle("Prosntvis endring i Norges eksport")

# Slå sammen datasettene
dframe <- left_join(df_import, df_export, by = c("iso2c", "country", "year"))
head(dframe)

# ser på Norge
dframe %>%
  filter(country=="Norway") %>% 
  rename(import=NE.IMP.GNFS.CD,
         eksport=NE.EXP.GNFS.CD,
         år=year) %>%
  mutate(import=import/1e9,
         eksport=eksport/1e9) %>% 
  select(år, import, eksport) %>% 
  pivot_longer(-år, names_to="aktivitet", values_to="verdi") %>% 
  ggplot(aes(x=år, y=verdi, col=aktivitet)) +
  geom_line() +
  scale_color_manual(values=c("dark red", "dark blue")) +
  labs(title="Norsk eksport og import av varer og tjenester \n (nominelle tall)",
       x =" ",
       y = "milliarder US$") +
  theme_bw()

# sammenligner med sverige 
df_export %>%
  filter(country %in% c("Norway","Sweden")) %>% 
  rename(eksport=NE.EXP.GNFS.CD,
         land=country,
         år=year) %>%
  mutate(eksport=eksport/1e9) %>% 
  ggplot(aes(x=år, y=eksport, col=land)) +
  geom_line() +
  labs(title="Eksport av varer og tjenester \n (nominelle tall)",
       x =" ",
       y = "milliarder US$") +
  theme_bw()

df_import %>%
  filter(country %in% c("Norway","Sweden")) %>% 
  rename(import=NE.IMP.GNFS.CD,
         land=country,
         år=year) %>%
  mutate(import=import/1e9) %>% 
  ggplot(aes(x=år, y=import, col=land)) +
  geom_line() +
  labs(title="Import av varer og tjenester \n (nominelle tall)",
       x =" ",
       y = "milliarder US$") +
  theme_bw()

# Oppgave 3
df_export %>%
  filter(country %in% c("Norway","Sweden")) %>% 
  rename(eksport=NE.EXP.GNFS.CD,
         land=country,
         år=year) %>%
  mutate(eksport=eksport/1e9) %>% 
  mutate(land=recode(land, "Norway"="Norge", "Sweden"="Sverige")) %>% 
  ggplot(aes(x=år, y=eksport, col=land)) +
  geom_line() +
  labs(title="Eksport av varer og tjenester \n (nominelle tall)",
       x =" ",
       y = "milliarder US$") +
  theme_bw() 

# Oppagev 4
df_export %>% 
filter(country %in% c("Norway", "Sweden")) %>% 
  rename(eksport=NE.EXP.GNFS.CD,
         år=year) %>%
  mutate(eksport=eksport/1e9) %>%
  mutate(prosend = 100*(eksport - lag(eksport))/lag(eksport)) %>% 
  mutate(kumprosend = (prosend + lag(prosend))) %>% 
  ggplot(aes(x=år, y=kumprosend, col=country)) + geom_line() +
  xlab("År") +
  ylab("Kumulative prosentvise endring") +
  ggtitle("Kumulative prosentvise endring for eksport Norge og Sverige") +
  theme_bw()

          

         