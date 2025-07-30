## ----DatenLaden, message=FALSE, warning=FALSE------------------------------------
library(readxl)     # Excel-Dateien lesen
library(tidyverse)  # https://www.tidyverse.org/packages/

daten <- read_excel("data/corona_bez_regression_v1.xlsx", sheet = "ex")


## ----DatenFiltern----------------------------------------------------------------
sel_daten <- daten %>%
  filter(bez_id <= 900) %>%
  select(bez_id, bez_txt, anteil_immun, tote_100k, bev_anteil_65plus, bildung_anteil_hochschule)


## ----ClusterVariablen------------------------------------------------------------
myVars <- c("anteil_immun", "tote_100k", "bev_anteil_65plus", "bildung_anteil_hochschule")


## ----KorrClustervars, message=FALSE----------------------------------------------
library(GGally)
ggpairs(sel_daten, columns = 3:6)


## ----DeskriptionClustervariablen1------------------------------------------------
streuungClustervars <- sel_daten %>%
  select(all_of(myVars)) %>%
  pivot_longer(cols = anteil_immun:bildung_anteil_hochschule, 
               names_to = "Variable", values_to = "Messwert")
ggplot(streuungClustervars, aes(x = Variable, y = Messwert)) +
  geom_boxplot() +
  geom_jitter(width=0.1,alpha=0.2, color="red") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1))


## ----DeskriptionClustervariablen2------------------------------------------------
sel_daten %>%
  select(all_of(myVars)) %>%
  summary()


## ----Ztransform------------------------------------------------------------------
sel_daten_trans <- sel_daten %>%
  mutate(across(all_of(myVars),scale))


## ----DeskriptionClustervariablen3------------------------------------------------
streuungClustervars <- sel_daten_trans %>%
  select(all_of(myVars)) %>%
  pivot_longer(cols = anteil_immun:bildung_anteil_hochschule, 
               names_to = "Variable", values_to = "Messwert")
ggplot(streuungClustervars, aes(x = Variable, y = Messwert)) +
  geom_boxplot() +
  geom_jitter(width=0.1,alpha=0.2, color="red") +
  theme(axis.text.x=element_text(angle = 45, hjust = 1))


## ----SingleLinkageClusterung-----------------------------------------------------
d0 <- dist(sel_daten_trans[myVars], method = "euclidean")
fit0 <- hclust(d0, method="single")


## ----DendroSingleLinkage---------------------------------------------------------
plot(fit0, labels = sel_daten_trans$bez_id, cex = 0.75,
     main = "Single Linkage Clusterung")


## ----BezirkRustFiltern-----------------------------------------------------------
sel_daten_trans_2 <- sel_daten_trans %>%
  filter(bez_id != "102")


## ----WardClusterung--------------------------------------------------------------
d1 <- dist(sel_daten_trans_2[myVars], method = "euclidean")
fit1 <- hclust(d1, method="ward.D2")
plot(fit1, labels = sel_daten_trans_2$bez_id, cex = 0.75,
     main = "Ward Clusterung")


## ---- fig.width=7.5, fig.height=4------------------------------------------------
# Heterogenität der letzten 10 Schritte holen
height <- sort(fit1$height)
Schritt <- c(10:1)
height <- height[(length(height)-9):length(height)]
screeplot_data_1 <- data.frame(Schritt, height)
# plotten
ggplot(screeplot_data_1, aes(x=Schritt, y=height)) + 
  geom_line(size=1) +
  scale_x_continuous(breaks=Schritt) +
  labs(x = "Anzhal Cluster") +
  geom_vline(xintercept=4, color = "red") +
  geom_vline(xintercept=5, linetype="dashed", color = "red") +
  geom_vline(xintercept=9, linetype="solid", color = "red")


## ---- fig.width=7.5, fig.height=10, class.output = "videoframe"------------------
library(cluster)
par(mfrow=c(3,2))
clusplot(sel_daten_trans_2[myVars], cutree(fit1, k = 4),
         labels=4, color=TRUE, shade=FALSE, lines = FALSE, col.p = cutree(fit1, k = 4),
         main = "Bivariater Clusterplot n = 4")
clusplot(sel_daten_trans_2[myVars], cutree(fit1, k = 5),
         labels=4, color=TRUE, shade=FALSE, lines = FALSE, col.p = cutree(fit1, k = 5),
         main = "Bivariater Clusterplot n = 5")
clusplot(sel_daten_trans_2[myVars], cutree(fit1, k = 6),
         labels=4, color=TRUE, shade=FALSE, lines = FALSE, col.p = cutree(fit1, k = 6),
         main = "Bivariater Clusterplot n = 6")
clusplot(sel_daten_trans_2[myVars], cutree(fit1, k = 7),
         labels=4, color=TRUE, shade=FALSE, lines = FALSE, col.p = cutree(fit1, k = 7),
         main = "Bivariater Clusterplot n = 7")
clusplot(sel_daten_trans_2[myVars], cutree(fit1, k = 8),
         labels=4, color=TRUE, shade=FALSE, lines = FALSE, col.p = cutree(fit1, k = 8),
         main = "Bivariater Clusterplot n = 8")
clusplot(sel_daten_trans_2[myVars], cutree(fit1, k = 9),
         labels=4, color=TRUE, shade=FALSE, lines = FALSE, col.p = cutree(fit1, k = 9),
         main = "Bivariater Clusterplot n = 9")
par(mfrow=c(1,1))


## --------------------------------------------------------------------------------
# Anzahl Cluster setzen
nCluster <- 5


## --------------------------------------------------------------------------------
plot(fit1, labels = sel_daten_trans_2$bez_id, cex = 0.75,
     main = "Ward Clusterung")
rect.hclust(fit1, k = nCluster, border="red")


## --------------------------------------------------------------------------------
table(cutree(fit1, k = nCluster))


## ----assignClusters--------------------------------------------------------------
# transformierte Daten
sel_daten_trans_2$fit1_cl5 <- as_factor(cutree(fit1, k = nCluster))
# Rohdaten: Rust zuerst entfernen
sel_daten_2 <- sel_daten %>%
  filter(bez_id != "102")
sel_daten_2$fit1_cl5 <- as_factor(cutree(fit1, k = nCluster))


## ----assignAusreißerCluster------------------------------------------------------
### transformierte Daten
sel_daten_trans <- sel_daten_trans_2 %>%
  select(bez_id, fit1_cl5) %>%
  left_join(sel_daten_trans, ., by = "bez_id")
# Rust manuell auf "Ausreißer Rust" setzen
sel_daten_trans <- sel_daten_trans %>%
  mutate(fit1_cl5 = fct_explicit_na(fit1_cl5, na_level = "Ausreißer Rust"))

### absolute Daten
sel_daten <- sel_daten_2 %>%
  select(bez_id, fit1_cl5) %>%
  left_join(sel_daten, ., by = "bez_id")
# Rust manuell auf "Ausreißer Rust" setzen
sel_daten <- sel_daten %>%
  mutate(fit1_cl5 = fct_explicit_na(fit1_cl5, na_level = "Ausreißer Rust"))


## ----MedianClusterDesc-----------------------------------------------------------
sel_daten %>%
  group_by(fit1_cl5) %>%
  summarise(across(anteil_immun:bildung_anteil_hochschule, median)) %>%
  kable()


## ----barClusterDescAbs-----------------------------------------------------------
sel_daten %>%
  group_by(fit1_cl5) %>%
  summarise(across(all_of(myVars), mean), .groups="keep") %>%
  pivot_longer(all_of(myVars), names_to = "variable", values_to ="wert") %>%
  ggplot(., aes(x=variable, y=wert, fill=variable)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~ fit1_cl5)


## ----barClusterDescTrans---------------------------------------------------------
sel_daten_trans %>%
  group_by(fit1_cl5) %>%
  summarise(across(all_of(myVars), mean), .groups="keep") %>%
  pivot_longer(all_of(myVars), names_to = "variable", values_to ="wert") %>%
  ggplot(., aes(x=variable, y=wert, fill=variable)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~ fit1_cl5)


## --------------------------------------------------------------------------------
ggplot(sel_daten, aes(x = fit1_cl5, y = anteil_immun, fill = fit1_cl5)) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "Anteil Immunisierte", x = "Cluster") +
  geom_jitter(width=0.2,alpha=0.25, color="black") +
  theme(legend.position = "none")

ggplot(sel_daten, aes(x = fit1_cl5, y = tote_100k, fill = fit1_cl5)) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "Mortalität", x = "Cluster") +
  geom_jitter(width=0.2,alpha=0.25, color="black") +
  theme(legend.position = "none")

ggplot(sel_daten, aes(x = fit1_cl5, y = bev_anteil_65plus, fill = fit1_cl5)) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "Anteil 65+", x = "Cluster") +
  geom_jitter(width=0.2,alpha=0.25, color="black") +
  theme(legend.position = "none")

ggplot(sel_daten, aes(x = fit1_cl5, y = bildung_anteil_hochschule, fill = fit1_cl5)) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "Anteil Hochschule", x = "Cluster") +
  geom_jitter(width=0.2,alpha=0.25, color="black") +
  theme(legend.position = "none")


## ----mapCleanWien----------------------------------------------------------------
library(sf)
library(tmap)
bez <- read_sf("data/bez/STATISTIK_AUSTRIA_POLBEZ_20210101.shp")
bez$id <- as.integer(bez$id)
bez_sel <- bez %>%
  filter(id <= 900)


## ----mapJoinCluster--------------------------------------------------------------
joined_bez_sel <- left_join(bez_sel, sel_daten_trans,
                            by = c("id" = "bez_id"))


## ----mapCl5, fig.width=9, fig.height=3.75----------------------------------------
mapCl5 <- tm_shape(joined_bez_sel) +
  tm_polygons(col = "fit1_cl5",
              title = "5-er Cluster-\nLösung",
              palette = "Set3",
              # palette = wes_palette("Zissou1", 5),
              legend.hist = TRUE) +
  tm_text("id", size = 0.45, alpha = 0.5, remove.overlap = TRUE) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_legend(outside = TRUE,
            legend.outside.size = 0.15,
            hist.width = 1,
            outer.margins = 0)
mapCl5

