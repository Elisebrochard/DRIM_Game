# =============================================================================
# DRIM Game 2026 - Etape 2 : analyse descriptive (volet 1)
# Entree  : data/drim_clean.rds   (cree par l'etape 1)
# Sorties : output/*.csv (tableaux)  et  output/graphiques/*.png
#
# Contenu
#   1. Profil du portefeuille par millesime (FICO, LTV, DTI, taux, montant...)
#   2. Issues brutes par millesime (et pourquoi elles trompent)
#   3. Probabilite cumulee de prepayment / defaut par age du pret (comparable)
#   4. Timing du prepayment dans la vie du pret (hazard par age)
#   5. Lien taux d'origine <-> prepayment
#   6. Premier regard sur les autres facteurs de risque
# =============================================================================

library(tidyverse)
library(lubridate)
library(scales)

df <- readRDS("data/drim_clean.rds")
dir.create("output/graphiques", recursive = TRUE, showWarnings = FALSE)
theme_set(theme_minimal(base_size = 12))

FIN_OBS <- as.Date("2026-03-01")
AGE_MAX <- max(df$duree_mois)

sauver <- function(p, nom, w = 9, h = 5.5) {
  print(p)
  ggsave(file.path("output/graphiques", paste0(nom, ".png")),
         p, width = w, height = h, dpi = 150, bg = "white")
}

# Variables utiles pour la suite
mois_entre <- function(d_fin, d_debut) {
  (year(d_fin) - year(d_debut)) * 12L + (month(d_fin) - month(d_debut))
}

df <- df %>%
  mutate(
    # age maximal que le pret pouvait atteindre avant la fin d'observation
    age_possible = pmax(mois_entre(FIN_OBS, date_1er_paiement) + 1L, 1L),
    periode = cut(annee_orig, c(2009, 2012, 2016, 2019, 2021, 2026),
                  labels = c("2010-2012", "2013-2016", "2017-2019",
                             "2020-2021", "2022-2026"))
  )


# =============================================================================
# 1. PROFIL DU PORTEFEUILLE PAR MILLESIME
# =============================================================================
# Rappel : l'echantillon contient ~12 500 prets par trimestre. Les effectifs
# ne refletent donc PAS les volumes reels d'origination : on compare des
# profils (moyennes, parts), jamais des volumes.

profil <- df %>%
  group_by(annee_orig) %>%
  summarise(
    n                    = n(),
    fico_moy             = mean(fico, na.rm = TRUE),
    ltv_med              = median(ltv, na.rm = TRUE),
    dti_moy              = mean(dti, na.rm = TRUE),
    taux_moy             = mean(taux_orig),
    upb_moy              = mean(upb_orig),
    upb_med              = median(upb_orig),
    pct_refi             = 100 * mean(finalite != "Achat", na.rm = TRUE),
    pct_cashout          = 100 * mean(finalite == "Refi cash-out", na.rm = TRUE),
    pct_investisseur     = 100 * mean(occupation == "Investissement", na.rm = TRUE),
    pct_premier_acheteur = 100 * mean(premier_acheteur == "Oui", na.rm = TRUE),
    pct_maison_indiv     = 100 * mean(type_bien == "Maison individuelle", na.rm = TRUE),
    pct_dti_manquant     = 100 * mean(dti_manquant),
    .groups = "drop"
  ) %>%
  mutate(across(-c(annee_orig, n), ~ round(.x, 1)))

print(profil, width = Inf)
write_csv(profil, "output/profil_par_millesime.csv")

g1 <- profil %>%
  select(annee_orig, fico_moy, ltv_med, dti_moy, taux_moy, upb_med, pct_refi) %>%
  pivot_longer(-annee_orig) %>%
  mutate(name = factor(name,
                       levels = c("fico_moy", "ltv_med", "dti_moy", "taux_moy", "upb_med", "pct_refi"),
                       labels = c("FICO moyen", "LTV médian (%)", "DTI moyen (%)",
                                  "Taux d'origine moyen (%)", "Montant médian ($)",
                                  "Part de refinancements (%)"))) %>%
  ggplot(aes(annee_orig, value)) +
  geom_line(color = "#1D9E75", linewidth = 0.8) +
  geom_point(color = "#1D9E75", size = 1.8) +
  facet_wrap(~ name, scales = "free_y", ncol = 3) +
  labs(title = "Profil du portefeuille par millésime d'origination",
       x = "Année d'origination", y = NULL)
sauver(g1, "01_profil_millesime", h = 6)

g2 <- df %>%
  count(annee_orig, finalite) %>%
  group_by(annee_orig) %>% mutate(part = n / sum(n)) %>% ungroup() %>%
  ggplot(aes(annee_orig, part, fill = finalite)) +
  geom_col() +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c("#1D9E75", "#7F77DD", "#D85A30"), name = "Finalité") +
  labs(title = "Finalité des prêts par millésime", x = "Année d'origination", y = NULL)
sauver(g2, "02_finalite_millesime")


# =============================================================================
# 2. ISSUES BRUTES PAR MILLESIME (a presenter... avec un avertissement)
# =============================================================================
issues <- df %>%
  count(annee_orig, type_evt) %>%
  group_by(annee_orig) %>%
  mutate(part = n / sum(n)) %>%
  ungroup()

g3 <- ggplot(issues, aes(annee_orig, part, fill = type_evt)) +
  geom_col() +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c(actif = "#B4B2A9", prepay = "#1D9E75",
                               defaut = "#D85A30", censure_tech = "#EF9F27",
                               maturite = "#7F77DD"), name = "Issue") +
  labs(title = "Issue finale des prêts par millésime (part brute)",
       subtitle = "Attention : les millésimes récents ont eu moins de temps pour être remboursés",
       x = "Année d'origination", y = NULL)
sauver(g3, "03_issues_brutes")


# =============================================================================
# 3. PROBABILITE CUMULEE PAR AGE DU PRET (table de survie, risques concurrents)
# =============================================================================
# Pour chaque groupe et chaque age t (en mois) :
#   n_risque = prets encore la au debut du mois t (ni sortis, ni censures)
#   h1 = prepayments en t / n_risque ;  h2 = defauts en t / n_risque
#   S(t) = prod(1 - h1 - h2)  = probabilite d'etre toujours actif apres t
#   CIF_prepay(t) = somme[ S(t-1) * h1(t) ]  = proba cumulee d'avoir prepaye
# C'est l'estimateur d'Aalen-Johansen en temps discret : il tient compte des
# prets censures (encore actifs) ET du defaut comme risque concurrent.
#
# age_lim : on ne montre les resultats que jusqu'a l'age ou au moins 75 % des
# prets du groupe pouvaient encore etre observes (evite de se baser sur
# quelques prets anciens d'un millesime recent).

table_vie <- function(data, ...) {
  grp <- enquos(...)
  
  lim <- data %>%
    group_by(!!!grp) %>%
    summarise(N = n(), age_lim = quantile(age_possible, 0.25, names = FALSE),
              .groups = "drop")
  cles <- setdiff(names(lim), c("N", "age_lim"))
  
  data %>%
    group_by(!!!grp, duree_mois) %>%
    summarise(d1 = sum(statut_cr == 1), d2 = sum(statut_cr == 2),
              c0 = sum(statut_cr == 0), .groups = "drop") %>%
    group_by(!!!grp) %>%
    complete(duree_mois = seq(1, AGE_MAX), fill = list(d1 = 0, d2 = 0, c0 = 0)) %>%
    group_by(!!!grp) %>%
    arrange(duree_mois, .by_group = TRUE) %>%
    mutate(
      sortis     = d1 + d2 + c0,
      n_risque   = sum(sortis) - (cumsum(sortis) - sortis),
      h1         = if_else(n_risque > 0, d1 / n_risque, 0),
      h2         = if_else(n_risque > 0, d2 / n_risque, 0),
      surv       = cumprod(1 - h1 - h2),
      surv_prec  = lag(surv, default = 1),
      cif_prepay = cumsum(surv_prec * h1),
      cif_defaut = cumsum(surv_prec * h2)
    ) %>%
    ungroup() %>%
    left_join(lim, by = cles)
}

cif_annee <- table_vie(df, annee_orig)

g4 <- cif_annee %>%
  filter(duree_mois <= pmin(age_lim, 120)) %>%
  ggplot(aes(duree_mois, cif_prepay, color = factor(annee_orig), group = annee_orig)) +
  geom_line(linewidth = 0.7) +
  scale_color_viridis_d(name = "Millésime") +
  scale_y_continuous(labels = percent) +
  labs(title = "Probabilité cumulée de remboursement anticipé selon l'âge du prêt",
       subtitle = "Estimateur de survie avec défaut en risque concurrent",
       x = "Âge du prêt (mois)", y = "Probabilité cumulée de prepayment")
sauver(g4, "04_cif_prepay_millesime")

g5 <- cif_annee %>%
  filter(duree_mois <= pmin(age_lim, 120)) %>%
  ggplot(aes(duree_mois, cif_defaut, color = factor(annee_orig), group = annee_orig)) +
  geom_line(linewidth = 0.7) +
  scale_color_viridis_d(name = "Millésime") +
  scale_y_continuous(labels = percent) +
  labs(title = "Probabilité cumulée de défaut selon l'âge du prêt",
       x = "Âge du prêt (mois)", y = "Probabilité cumulée de défaut")
sauver(g5, "05_cif_defaut_millesime")

# Tableau comparable : probabilite cumulee (%) a 12, 24, 36 et 60 mois
cif_fixe <- cif_annee %>%
  filter(duree_mois %in% c(12, 24, 36, 60), duree_mois <= age_lim) %>%
  select(annee_orig, duree_mois, cif_prepay, cif_defaut) %>%
  mutate(across(starts_with("cif"), ~ round(100 * .x, 1))) %>%
  pivot_wider(names_from = duree_mois, values_from = c(cif_prepay, cif_defaut),
              names_glue = "{.value}_{duree_mois}m")
print(cif_fixe, n = Inf, width = Inf)
write_csv(cif_fixe, "output/cif_par_millesime.csv")


# =============================================================================
# 4. TIMING DU PREPAYMENT DANS LA VIE DU PRET
# =============================================================================
# Hazard = probabilite de prepayer le mois t SACHANT qu'on n'a pas encore
# prepaye ni fait defaut. On ne trace que les ages avec >= 2 000 prets a risque.
haz <- table_vie(df, periode)

g6 <- haz %>%
  filter(duree_mois <= 96, n_risque >= 2000) %>%
  ggplot(aes(duree_mois, 100 * h1, color = periode)) +
  geom_line(linewidth = 0.7) +
  scale_color_viridis_d(name = "Période\nd'origination") +
  labs(title = "Taux mensuel de remboursement anticipé selon l'âge du prêt",
       x = "Âge du prêt (mois)", y = "Hazard mensuel de prepayment (%)")
sauver(g6, "06_hazard_age_periode")


# =============================================================================
# 5. LIEN TAUX D'ORIGINE <-> PREPAYMENT
# =============================================================================
# 5a. Heatmap : proba cumulee de prepayment a 24 mois, par millesime et classe
#     de taux. On se limite aux millesimes <= 2023 (24 mois observables) et aux
#     cellules d'au moins 500 prets.
cell24 <- df %>%
  filter(annee_orig <= 2023) %>%
  table_vie(annee_orig, taux_classe) %>%
  filter(duree_mois == 24, 24 <= age_lim, N >= 500)

g7 <- ggplot(cell24, aes(factor(annee_orig), taux_classe, fill = cif_prepay)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(100 * cif_prepay)), size = 3, color = "white") +
  scale_fill_viridis_c(labels = percent, name = "Prepay\nà 24 mois") +
  labs(title = "Remboursement anticipé à 24 mois selon le taux d'origine",
       subtitle = "Cellules d'au moins 500 prêts ; valeurs en %",
       x = "Millésime", y = "Taux d'origine")
sauver(g7, "07_heatmap_taux_millesime")

# 5b. Au niveau du millesime : taux moyen vs prepayment a 24 mois
nuage <- cif_annee %>%
  filter(duree_mois == 24, 24 <= age_lim) %>%
  select(annee_orig, cif_prepay) %>%
  left_join(select(profil, annee_orig, taux_moy), by = "annee_orig")

g8 <- ggplot(nuage, aes(taux_moy, cif_prepay, label = annee_orig)) +
  geom_point(color = "#7F77DD", size = 2.5) +
  geom_text(vjust = -0.8, size = 3.2) +
  scale_y_continuous(labels = percent) +
  labs(title = "Taux moyen du millésime vs prepayment à 24 mois",
       x = "Taux d'origine moyen du millésime (%)",
       y = "Probabilité cumulée de prepayment à 24 mois")
sauver(g8, "08_taux_vs_prepay_millesime")


# =============================================================================
# 6. PREMIER REGARD SUR LES AUTRES FACTEURS DE RISQUE
# =============================================================================
# Prepayment / defaut a 24 mois par modalite, millesimes 2010-2023 melanges.
# Attention : c'est une vue brute, les millesimes n'ont pas le meme profil
# (ex. beaucoup de refinancements en 2020-2021). Les modeles des volets 2 et 3
# permettront de controler ces effets.
vars_risque <- c("fico_classe", "ltv_classe", "dti_classe", "finalite",
                 "occupation", "canal", "premier_acheteur", "type_bien")
noms_var <- c(fico_classe = "FICO", ltv_classe = "LTV (%)", dti_classe = "DTI (%)",
              finalite = "Finalité", occupation = "Occupation", canal = "Canal",
              premier_acheteur = "Premier achat", type_bien = "Type de bien")

cif_risque <- map_dfr(vars_risque, function(v) {
  df %>%
    filter(annee_orig <= 2023, !is.na(.data[[v]])) %>%
    mutate(modalite = as.character(.data[[v]])) %>%
    table_vie(modalite) %>%
    filter(duree_mois == 24) %>%
    mutate(variable = v)
}) %>%
  filter(N >= 1000)

ordre <- map_dfr(vars_risque, ~ tibble(variable = .x, modalite = levels(df[[.x]]))) %>%
  mutate(cle = paste(variable, modalite, sep = "|"))

cif_risque <- cif_risque %>%
  mutate(cle = factor(paste(variable, modalite, sep = "|"), levels = ordre$cle))

write_csv(cif_risque %>% select(variable, modalite, N, cif_prepay, cif_defaut) %>%
            mutate(across(starts_with("cif"), ~ round(100 * .x, 2))),
          "output/cif24_par_facteur.csv")

graph_risque <- function(col, titre, couleur) {
  ggplot(cif_risque, aes(cle, {{ col }})) +
    geom_col(fill = couleur) +
    facet_wrap(~ variable, scales = "free_x", labeller = labeller(variable = noms_var)) +
    scale_x_discrete(labels = function(x) sub(".*\\|", "", x)) +
    scale_y_continuous(labels = percent) +
    labs(title = titre, subtitle = "Millésimes 2010-2023, vue brute (non ajustée)",
         x = NULL, y = NULL) +
    theme(axis.text.x = element_text(angle = 35, hjust = 1))
}

sauver(graph_risque(cif_prepay, "Prepayment cumulé à 24 mois selon les caractéristiques", "#1D9E75"),
       "09_prepay24_facteurs", w = 11, h = 7)
sauver(graph_risque(cif_defaut, "Défaut cumulé à 24 mois selon les caractéristiques", "#D85A30"),
       "10_defaut24_facteurs", w = 11, h = 7)

cat("\nTermine. Tableaux dans output/, graphiques dans output/graphiques/\n")