## ---- message=FALSE, warning=FALSE, results="hide"-------------------------------
library(readxl)     # Excel-Dateien lesen
library(tidyverse)  # https://www.tidyverse.org/packages/

daten <- read_excel("data/corona_bez_regression_v1.xlsx", sheet = "ex")


## --------------------------------------------------------------------------------
head(daten)


## --------------------------------------------------------------------------------
daten <- daten %>%
  mutate(bld = floor(bez_id/100))


## --------------------------------------------------------------------------------
daten <- daten %>%
  mutate(bld = factor(bld, labels = c("Bgld.", "Ktn.", "NÖ", "OÖ",
                                      "Sbg.", "Stmk.", "T", "Vbg.", "W")))
head(daten$bld)


## --------------------------------------------------------------------------------
sel_daten <- daten %>%
  filter(bez_id <= 900 & bez_id != 709) %>%
  select(bez_id, bez_txt, bld, anteil_immun, tote_100k, bev_anteil_65plus,
         anteil_noaut, bildung_anteil_hochschule, nrw19_anteil_fpoe)


## ----Levels bld------------------------------------------------------------------
levels(sel_daten$bld)


## ----Auto-Dummy-Codes anzeigen---------------------------------------------------
model.matrix(~ bld, sel_daten) %>%  # Dummy-Coding liefert ...
  .[,-1] %>%        # ... eine Matrix ...
  as_tibble() %>%   # ... die ein einen Tibble überführt wird ...
  bind_cols(sel_daten[c("bez_txt", "bld")], .) %>%  # ... der noch einleitende Spalten bekommt
  head()


## ----Vars standardisieren--------------------------------------------------------
sel_daten_trans <- sel_daten %>%
  mutate(across(c("anteil_immun", "tote_100k", "bev_anteil_65plus",
         "anteil_noaut", "bildung_anteil_hochschule", "nrw19_anteil_fpoe"),scale))


## ----lm0 ohne bld----------------------------------------------------------------
lm0 <- lm(anteil_immun ~ tote_100k + bev_anteil_65plus + anteil_noaut 
          + bildung_anteil_hochschule, data = sel_daten_trans)
summary(lm0)


## ----lm1 mit bld und noSZ--------------------------------------------------------
lm1 <- lm(anteil_immun ~ tote_100k + bev_anteil_65plus + anteil_noaut 
          + bildung_anteil_hochschule + bld, data = sel_daten_trans)
summary(lm1)


## ----vergleichRquad--------------------------------------------------------------
cbind(c("lm0", "lm1"), c(summary(lm0)$adj.r.squared, summary(lm1)$adj.r.squared))


## ----coeffPlot-------------------------------------------------------------------
coeff_lm1 <- as_tibble(summary(lm1)$coefficients, rownames = "erklaerende") %>%
  janitor::clean_names() %>%    # Spaltennamen bereinigen
  mutate(erklaerende = factor(erklaerende),   # in Faktor überführen für Diagramm
         erklaerende = forcats::fct_reorder(erklaerende, estimate),  #Sortieren für Diagramm
         vis_sig = cut(pr_t, breaks = c(0, 0.05, 1), 
                       labels = c("signifikant (p<=0,05)", "nicht signifikant (p>0,05)")))  # Signifikanz als Faktor für Diagramm

ggplot(coeff_lm1, aes(x = erklaerende, y = estimate, fill = vis_sig)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Erklärende Variablen\n",
       y = "\nRegressionskoeffizienten",
       fill = "Signifikanz")


## ----lm2 ohne noAut--------------------------------------------------------------
lm2 <- lm(anteil_immun ~ tote_100k + bev_anteil_65plus 
          + bildung_anteil_hochschule + bld, data = sel_daten_trans)
summary(lm2)



## ----lm3 ohne tote_100k----------------------------------------------------------
lm3 <- lm(anteil_immun ~ bev_anteil_65plus + bildung_anteil_hochschule + bld, 
          data = sel_daten_trans)
summary(lm3)


## --------------------------------------------------------------------------------
plot(lm3, 1, labels.id = sel_daten_trans$bez_txt)


## --------------------------------------------------------------------------------
plot(lm3, 2, labels.id = sel_daten_trans$bez_txt)


## --------------------------------------------------------------------------------
sum(residuals(lm3))


## ----message=FALSE, warning=FALSE------------------------------------------------
plot(lm3, 3, labels.id = sel_daten_trans$bez_txt)


## --------------------------------------------------------------------------------
ggplot(sel_daten_trans, aes(x=(1:length(bez_id)), y=residuals(lm3))) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x, se=F, color="red") +
  labs(x = "\nBeobachtungen", y = "Residuen\n") +
  theme_gray(base_size = 18)

