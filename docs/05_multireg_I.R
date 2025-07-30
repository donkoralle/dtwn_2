## ----Datenimport, message=FALSE, warning=FALSE, results="hide"-------------------
library(readxl)     # Excel-Dateien lesen
library(tidyverse)  # https://www.tidyverse.org/packages/

daten <- read_excel("data/corona_bez_regression_v1.xlsx", sheet = "ex")


## --------------------------------------------------------------------------------
head(daten)


## --------------------------------------------------------------------------------
  str(daten)


## ----Missings_2------------------------------------------------------------------
colSums(is.na(daten)) %>%
  knitr::kable()


## ----Check NA mit dplyr, message=FALSE, warning=FALSE----------------------------
daten %>%
  summarise(across(where(is.numeric), ~ sum(is.na(.)))) %>%
  t() %>%
  knitr::kable()


## ----fig.width=9, fig.height=4.75------------------------------------------------
library(sf)
library(tmap)
bez <- read_sf("data/bez/STATISTIK_AUSTRIA_POLBEZ_20210101.shp")
bez$id <- as.integer(bez$id)
tm_shape(bez) +
  tm_polygons()


## --------------------------------------------------------------------------------
bez %>%
  filter(id >= 900)


## ----fig.width=9, fig.height=4.75------------------------------------------------
bez.sel <- bez %>%
  filter(id <= 900)
tm_shape(bez.sel) +
  tm_polygons()


## ----fig.width=9, fig.height=3.75------------------------------------------------
joined_bez.sel <- left_join(bez.sel, daten,
                            by = c("id" = "bez_id"))
str(joined_bez.sel)


## ----choropleth1, fig.width=9, fig.height=4.75-----------------------------------
tmap::qtm(joined_bez.sel, fill = "anteil_immun")


## ----choropleth2, fig.width=9, fig.height=3.75-----------------------------------
mymap <- tm_shape(joined_bez.sel) +
  tm_polygons("anteil_immun",
              title = "Anteil \nImmunisierte [%]",
              palette = "YlOrRd",
              legend.hist = TRUE) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_legend(outside = TRUE,
            legend.outside.size = 0.15,
            hist.width = 1,
            outer.margins = 0)
mymap


## ----message=FALSE, warning=FALSE------------------------------------------------
tmap_save(mymap, filename = "output/impfquoten_bez_2021.png",
          units = "px", dpi = 300,
          width = 2000)


## ----Überblick Variablen---------------------------------------------------------
cbind(colnames(daten))


## --------------------------------------------------------------------------------
sel.daten <- daten %>%
  filter(bez_id <= 900) %>%
  select(bez_id, bez_txt, anteil_immun, tote_100k, bev_anteil_65plus,
         anteil_noaut, bildung_anteil_hochschule, nrw19_anteil_fpoe)


## ----Streuung_Variablen----------------------------------------------------------
sel.daten %>%
  select(anteil_immun:nrw19_anteil_fpoe) %>%
  pivot_longer(cols = anteil_immun:nrw19_anteil_fpoe, names_to = "Variable", 
               values_to = "Messwert") %>%
  ggplot(., aes(x = Variable, y = Messwert)) +
  geom_boxplot() +
  geom_jitter(width=0.1,alpha=0.2, color="red") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1))


## ----Standardisierung------------------------------------------------------------
sel.daten.trans <- sel.daten %>%
  mutate(across(c("anteil_immun", "tote_100k", "bev_anteil_65plus",
         "anteil_noaut", "bildung_anteil_hochschule", "nrw19_anteil_fpoe"),scale))


## ----KontrolleStandardisierung---------------------------------------------------
# zur Kontrolle: Mittelwert = 0
sel.daten.trans %>%
  summarise(across(c("anteil_immun", "tote_100k", "bev_anteil_65plus",
         "anteil_noaut", "bildung_anteil_hochschule", "nrw19_anteil_fpoe"),mean)) %>%
  t() %>%
  knitr::kable()


## ----Vergleich Streuungen, message=FALSE, warning=FALSE--------------------------
# Streuung der nicht-standardisierten Variablen abbilden
vis.data.1 <- sel.daten %>%
  select(anteil_immun:nrw19_anteil_fpoe) %>%
  pivot_longer(cols = anteil_immun:nrw19_anteil_fpoe, names_to = "Variable", 
               values_to = "Messwert")

p1 <- ggplot(vis.data.1, aes(x = Variable, y = Messwert)) +
  geom_boxplot() +
  labs(x = "Variablen\n", y = "\nbeobachtete Werte") +
  theme_gray(base_size = 16) +
  coord_flip()

# Streuung der standardisierten Variablen darstellen
vis.data.2 <- sel.daten.trans %>%
  select(anteil_immun:nrw19_anteil_fpoe) %>%
  pivot_longer(cols = anteil_immun:nrw19_anteil_fpoe, names_to = "Variable", 
               values_to = "Messwert")

p2 <- ggplot(vis.data.2, aes(x = Variable, y = Messwert)) +
  geom_boxplot() +
  labs(y = "\nz-transf. Werte") +
  theme_gray(base_size = 16) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  coord_flip()

# beide Plots kombinieren damit der Vergleich leichter fällt mit dem grid_extra package
library(gridExtra)
grid.arrange(p1, p2, nrow = 1, widths = c(2, 1))


## ----Korrelationen1, message=FALSE-----------------------------------------------
# old school: numerisch
library(Hmisc)
rcorr(as.matrix(sel.daten[,4:8]))


## ----Korrelationen2, message=FALSE, warning=FALSE--------------------------------
# etwas hübscher: graphisch mittels GGally
library(GGally)
ggpairs(sel.daten, columns = 4:8)


## ----Regression V1 standardisiert------------------------------------------------
# options(scipen = 999)
# options(scipen = 0)

lm.v1.trans <- lm(anteil_immun ~ tote_100k + bev_anteil_65plus + anteil_noaut + bildung_anteil_hochschule + nrw19_anteil_fpoe,
                  data = sel.daten.trans)
summary(lm.v1.trans)



## ----Regression V1a--------------------------------------------------------------
lm.v1.trans <- lm(anteil_immun ~ tote_100k + bev_anteil_65plus + anteil_noaut + bildung_anteil_hochschule,
                  data = sel.daten.trans)
summary(lm.v1.trans)


## ----Regression V2 standardisiert------------------------------------------------

# Schwaz aus Modellbildung ausschließen
lmv2noSchwaz <- sel.daten.trans %>%
  filter(bez_id != 709)

lm.v2.trans <- lm(anteil_immun ~ tote_100k + bev_anteil_65plus + anteil_noaut + bildung_anteil_hochschule, 
                  data = lmv2noSchwaz)
summary(lm.v2.trans)


## ----paarweiser Zusammenhang, message=FALSE--------------------------------------
ggpairs(sel.daten, c(3:8))


## ----fitted zu residuals---------------------------------------------------------
plot(lm.v2.trans, 1, labels.id = sel.daten$bez_txt)


## ----paarweise Korreltationen zwischen erklärenden Variablen, message=FALSE------
ggpairs(sel.daten, columns = 4:8)


## ----Normalverteilung Residuen---------------------------------------------------
plot(lm.v2.trans, 2, labels.id = sel.daten$bez_txt)


## ----Mittelwert Residuen---------------------------------------------------------
mean(residuals(lm.v2.trans))


## ----Test Homoskedastizität------------------------------------------------------
plot(lm.v2.trans, 3, labels.id = sel.daten$bez_txt)


## ----Regression v2 getrimmt------------------------------------------------------
trimmed.data <- lmv2noSchwaz %>%
  filter(bildung_anteil_hochschule < quantile(bildung_anteil_hochschule, 0.975))

# Vergleich der Streuungen vor und nach der Trimmung
vis.min <- min(lmv2noSchwaz$bildung_anteil_hochschule)
vis.max <- max(lmv2noSchwaz$bildung_anteil_hochschule)

p3 <- ggplot(lmv2noSchwaz, aes(x = bildung_anteil_hochschule, y = "ungetrimmt")) +
  geom_boxplot() +
  labs(x = "Ausprägungen bildung_anteil_hochschule\n", y = "") +
  xlim(vis.min, vis.max) +
  theme_gray() +
  coord_flip()
p4 <- ggplot(trimmed.data, aes(x = bildung_anteil_hochschule, y = "getrimmt (97,5%)")) +
  geom_boxplot() +
  xlim(vis.min, vis.max) +
  labs(x = "", y = "") +
  coord_flip()
# beide Plots kombinieren damit der Vergleich leichter fällt mit dem grid_extra package
grid.arrange(p3, p4, nrow = 1)

# Regressionsmodell V2 mit getrimmten Ausgangswerten berechnen
lm.v2.trans.trimmed <- lm(anteil_immun ~ tote_100k + bev_anteil_65plus + anteil_noaut + bildung_anteil_hochschule, 
                  data = trimmed.data)
summary(lm.v2.trans.trimmed)
summary(lm.v2.trans)


## ----Cook------------------------------------------------------------------------
plot(lm.v2.trans, 4, labels.id = sel.daten$bez_txt)


## ----Leverage--------------------------------------------------------------------
plot(lm.v2.trans, 5, labels.id = sel.daten$bez_txt)


## ----Test Autokorrelation--------------------------------------------------------
ggplot(lmv2noSchwaz, aes(x=(1:length(bez_id)), y=residuals(lm.v2.trans))) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x, se=F, color="red") +
  labs(x = "\nBeobachtungen", y = "Residuen\n") +
  theme_gray(base_size = 18)

