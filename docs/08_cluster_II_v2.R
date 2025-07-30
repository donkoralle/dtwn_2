## --------------------------------------------------------------------------------
library(readxl)     # Excel-Dateien lesen
library(tidyverse)  # https://www.tidyverse.org/packages/

daten <- read_excel("data/corona_gem_cluster_v2.xlsx", sheet = "ex")

# qual. Variablen setzen
daten <- daten %>%
  mutate(ur_metatypus = factor(ur_metatypus, labels = c("Urbane Räume",
                                                "Regionale Zentren",
                                                "Ländliches Umland",
                                                "Ländlicher Raum")),
         ur_typus = factor(ur_typus, labels = c("Urbane Großzentren",
                                                "Urbane Mittelzentren",
                                                "Urbane Kleinzentren",
                                                "Regionale Zentren, zentral",
                                                "Regionale Zentren, intermediär",
                                                "Ländlicher Raum im Umland von Zentren, zentral",
                                                "Ländlicher Raum im Umland von Zentren, intermediär",
                                                "Ländlicher Raum im Umland von Zentren, peripher",
                                                "Ländlicher Raum, zentral",
                                                "Ländlicher Raum, intermediär",
                                                "Ländlicher Raum, peripher")),
         degurba_21 = factor(degurba_21))



## --------------------------------------------------------------------------------
colSums(is.na(daten)) %>%
  knitr::kable()


## --------------------------------------------------------------------------------
myQuantVars = c("immun_100k", "anteil_fpoe")
myQualVars = c("ur_metatypus")
myVars = c(myQuantVars, myQualVars)


## --------------------------------------------------------------------------------
daten %>%
  select(all_of(myVars)) %>%
  summary()


## --------------------------------------------------------------------------------
daten %>%
  select(all_of(myQuantVars)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Messwert") %>%
  ggplot(., aes(x = "", y = Messwert)) +
  geom_boxplot() +
  labs(x = "Variablen", y = "Messwerte\n") +
  facet_wrap(~ Variable, scales = "free_y")

daten %>%
  group_by(ur_metatypus) %>%
  summarise(n = n(), .groups = "drop") %>%
  ggplot(. , aes(x = "", y = n, fill = ur_metatypus)) +
  geom_bar(stat = "identity", position = 'fill') +
  labs(x = "ur_typus", y = "Anteile [%]")


## --------------------------------------------------------------------------------
daten_trans <- daten %>%
  select(gem_id, gem_txt, all_of(myVars)) %>%
  mutate(across(all_of(myQuantVars), scale)) %>%
  mutate_if(is.matrix, as.numeric)  # scale liefert eine Matrix und keinen numeric > wieder zurückstellen


## --------------------------------------------------------------------------------
library(GGally)
ggpairs(daten, myQuantVars)


## --------------------------------------------------------------------------------
daten_trans_fit0 <- daten_trans %>%
  mutate(gem_id_rows = gem_id) %>%
  tibble::column_to_rownames("gem_id_rows")


## --------------------------------------------------------------------------------
library(cluster)
gower_dist_fit0 <- daisy(daten_trans_fit0[myVars], metric = "gower")
summary(gower_dist_fit0)


## --------------------------------------------------------------------------------
fit0 <- hclust(gower_dist_fit0, method = "single")
plot(fit0, main = "Single Linkage Clusterung", cex = 0.65)


## --------------------------------------------------------------------------------
get.agglo <- function(clustermodell){
  data.frame(row.names=paste0("Cluster",seq_along(clustermodell$height)),
             height=clustermodell$height,
             components=ifelse(clustermodell$merge<0, clustermodell$labels[abs(clustermodell$merge)],
                               paste0("Cluster",clustermodell$merge)),
             stringsAsFactors=FALSE)
}

tail(get.agglo(fit0), 10)


## --------------------------------------------------------------------------------
fit1 <- hclust(gower_dist_fit0, method="ward.D2")
plot(fit1, main = "Ward-Clusterung", cex = 0.5)


## --------------------------------------------------------------------------------
# Heterogenität der letzten 10 Schritte holen
height <- sort(fit1$height)
Schritt <- c(10:1)
height <- height[(length(height)-9):length(height)]
screeplot_data_1 <- data.frame(Schritt, height)
# plotten
ggplot(screeplot_data_1, aes(x=Schritt, y=height)) +
  geom_line(size=1) +
  scale_x_continuous(breaks=Schritt) +
  labs(title = "Elbow-Diagramm", x = "Anzhal Cluster") +
  geom_vline(xintercept=6, color = "red")



## --------------------------------------------------------------------------------
nCluster <- 6
# Dendorgramm erstellen
plot(fit1, cex = 0.75, main = "Ward Clusterung")
rect.hclust(fit1, k = nCluster, border="red")


## --------------------------------------------------------------------------------
table(cutree(fit1, k = nCluster))


## --------------------------------------------------------------------------------
# zunächst in den zur Berechnung genutzten Datenframe
daten_trans_fit0$fit1_cl6 <- as_factor(cutree(fit1, k = nCluster))
# dann per join (sicherer bei Ausreißern) im transformierten Ausgangsdatensatz
daten_trans <- daten_trans_fit0 %>%
  select(gem_id, fit1_cl6) %>%
  left_join(daten_trans, ., by = "gem_id")
# ... und noch auf die nicht-transformierten Daten
daten <- daten_trans %>%
  select(gem_id, fit1_cl6) %>%
  left_join(daten, ., by = "gem_id")


## ----message=FALSE, warning=FALSE------------------------------------------------
library(Rtsne)
set.seed(1976)

tsne_obj <- Rtsne(gower_dist_fit0, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = daten_trans_fit0$fit1_cl6,
         name = daten_trans_fit0$gem_id)

ggplot(tsne_data, aes(x = X, y = Y)) +
  geom_point(aes(color = cluster))


## --------------------------------------------------------------------------------
daten %>%
  group_by(fit1_cl6) %>%
  summarise(across(all_of(myQuantVars), median), .groups = "keep")


## --------------------------------------------------------------------------------
daten_trans %>%
  group_by(fit1_cl6) %>%
  summarise(across(all_of(myQuantVars), mean), .groups="keep") %>%
  pivot_longer(all_of(myQuantVars), names_to = "variable", values_to ="wert") %>%
  ggplot(., aes(x=variable, y=wert, fill=variable)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~ fit1_cl6)


## --------------------------------------------------------------------------------
ggplot(daten, aes(x = fit1_cl6, y = anteil_fpoe, fill = fit1_cl6)) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "anteil_fpoe", x = "Cluster") +
  geom_jitter(width=0.2,alpha=0.15, color="black") +
  theme(legend.position = "none")


## --------------------------------------------------------------------------------
ggplot(daten, aes(x = fit1_cl6, y = immun_100k, fill = fit1_cl6)) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "immun_100k", x = "Cluster") +
  geom_jitter(width=0.2,alpha=0.15, color="black") +
  theme(legend.position = "none")


## --------------------------------------------------------------------------------
table(daten$fit1_cl6, daten$ur_metatypus)


## --------------------------------------------------------------------------------
ggplot(daten, aes(x = fit1_cl6, fill = ur_typus)) +
  geom_bar(position = "fill")


## --------------------------------------------------------------------------------
gem <- read_sf("data/gem/STATISTIK_AUSTRIA_GEM_20210101.shp")
gem$id <- as.integer(gem$id)


## --------------------------------------------------------------------------------
tail(gem, 30)


## --------------------------------------------------------------------------------
gem_clean <- gem %>%
  mutate(id = ifelse(id > 90000, 90001, id))


## --------------------------------------------------------------------------------
gem_clean <- gem_clean %>%
  group_by(id) %>%
  summarise(geometry = st_union(geometry))


## ----eval=FALSE------------------------------------------------------------------
## st_write(gem_clean, "data/gem/gem_clean.shp")


## ----fig.width=9, fig.height=4.75------------------------------------------------
tmap::qtm(gem_clean)


## --------------------------------------------------------------------------------
gem_clean_join <- left_join(gem_clean, daten,
                            by = c("id" = "gem_id"))


## ----fig.width=9, fig.height=4.75------------------------------------------------
map_Cl6 <- tm_shape(gem_clean_join) +
  tm_polygons(col = "fit1_cl6",
              title = "6-er Cluster-\nLösung",
              palette = "Set3",
              # palette = wes_palette("Zissou1", 5),
              legend.hist = TRUE) +
  # tm_text("id", size = 0.45, alpha = 0.5, remove.overlap = TRUE) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_legend(outside = TRUE,
            legend.outside.size = 0.15,
            hist.width = 1,
            outer.margins = 0)
map_Cl6


## ----message=FALSE, warning=FALSE------------------------------------------------
tmap_save(map_Cl6, filename = "output/map_Cl6.png",
          units = "px", dpi = 300,
          width = 2000)

