# 0. Identification ---------------------------------------------------

# Title: LCA for Merit-scale code of EDUMERCO data
# Institution: JUSMER
# Responsible: Andreas Laffert

# Executive Summary: This script contains the code for run an LCA for merit scale in EDUMERCO data
# Date: March 5, 2026

# 1. Packages  -----------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               sjmisc, 
               here,
               lavaan,
               psych,
               corrplot,
               ggdist,
               patchwork,
               sjlabelled,
               gtools,
               RColorBrewer,
               skimr,
               naniar,
               poLCA,
               reshape)


options(scipen=999)
rm(list = ls())

# 2. Data -----------------------------------------------------------------

load(here("input/data/proc/db_proc.RData"))

glimpse(db)

# 3. Analysis -------------------------------------------------------------

# 3.1 descriptive ----

t1 <- db %>% 
  dplyr::select(1:8) %>% 
  skim() %>% 
  yank("numeric") %>% 
  as_tibble() %>% 
  mutate(range = paste0("(",p0,"-",p100,")")) %>% 
  mutate_if(.predicate = is.numeric, .funs = ~ round(.,2)) %>% 
  dplyr::select("Variable" = skim_variable,"Mean"= mean, "SD"=sd, "Range" = range, "Histogram"=hist) 

t1 %>% 
  kableExtra::kable(format = "markdown")

theme_set(theme_ggdist())
colors <- RColorBrewer::brewer.pal(n = 4, name = "RdBu")

a <- db %>% 
  dplyr::select(perc_effort, 
                perc_talent,
                perc_rich_parents,
                perc_contact) %>% 
  sjPlot::plot_likert(geom.colors = colors,
                      title = c("a. Percepciones"),
                      geom.size = 0.8,
                      axis.labels = c("Esfuerzo", "Talento", "Padres ricos", "Contactos"),
                      catcount = 4,
                      values  =  "sum.outside",
                      reverse.colors = F,
                      reverse.scale = T,
                      show.n = FALSE,
                      show.prc.sign = T
  ) +
  ggplot2::theme(legend.position = "none")

b <- db %>% 
  dplyr::select(pref_effort, 
                pref_talent,
                pref_rich_parents,
                pref_contact) %>% 
  sjPlot::plot_likert(geom.colors = colors,
                      title = c("b. Preferencias"),
                      geom.size = 0.8,
                      axis.labels = c("Esfuerzo", "Talento", "Padres ricos", "Contactos"),
                      catcount = 4,
                      values  =  "sum.outside",
                      reverse.colors = F,
                      reverse.scale = T,
                      show.n = FALSE,
                      show.prc.sign = T
  ) +
  ggplot2::theme(legend.position = "bottom")

likerplot <- a / b + plot_annotation(caption = paste0("Fuente: Elaboración propia en base a Encuesta EDUMERCO"," (n = ",dim(db)[1],")"
))

likerplot

# 3.2 correlations ----

M <- psych::polychoric(db[c(1:8)])

diag(M$rho) <- NA

rownames(M$rho) <- c("A. Percepción Esfuerzo",
                     "B. Percepción Talento",
                     "C. Percepción Padres Ricos",
                     "D. Percepción Contactos",
                     "E. Preferencias Esfuerzo",
                     "F. Preferencias Talento",
                     "G. Preferencias Padres Ricos",
                     "H. Preferencias Contactos")

#set Column names of the matrix
colnames(M$rho) <-c("(A)", "(B)","(C)","(D)","(E)","(F)","(G)",
                    "(H)")

testp <- cor.mtest(M$rho, conf.level = 0.95)

#Plot the matrix using corrplot
corrplot::corrplot(M$rho,
                   method = "color",
                   addCoef.col = "black",
                   type = "upper",
                   tl.col = "black",
                   col = colorRampPalette(c("#E16462", "white", "#0D0887"))(12),
                   bg = "white",
                   na.label = "-") 

# 3.3 LCA: dichotomized items ----

a1 <- db$perc_effort_d
a2 <- db$perc_talent_d
a3 <- db$perc_rich_parents_d
a4 <- db$perc_contact_d
a5 <- db$pref_effort_d
a6 <- db$pref_talent_d 
a7 <- db$pref_rich_parents_d
a8 <- db$pref_contact_d

db_lca <- data.frame(a1, a2, a3, a4, a5, a6, a7, a8) %>% na.omit()

db_lca <- db_lca %>% 
  mutate_all(~as.factor(.))

f <-cbind(a1, a2, a3, a4, a5, a6, a7, a8)~1

fit_lca <- function(K, data, formula, nrep = 50, maxiter = 3000, seed = 123) {
  set.seed(seed)
  poLCA(formula, data = data, nclass = K,
        nrep = nrep, maxiter = maxiter,
        verbose = FALSE, graphs = FALSE)
}

Ks <- 1:5
fits <- lapply(Ks, fit_lca, data = db_lca, formula = f, nrep = 30)

fit_tbl <- data.frame(
  K    = Ks,
  logLik = sapply(fits, `[[`, "llik"),
  AIC  = sapply(fits, `[[`, "aic"),
  BIC  = sapply(fits, `[[`, "bic"),
  Gsq  = sapply(fits, `[[`, "Gsq"),
  Chisq = sapply(fits, `[[`, "Chisq")
)

fit_tbl

# 3.4 Fit measures -----

# --- helpers -------------------------------------------------------------

get_K <- function(fit) {
  # robust: number of classes = columns of posterior
  if (!is.null(fit$posterior)) return(as.integer(ncol(fit$posterior)))
  # fallback: rows of probs for first item
  if (!is.null(fit$probs) && length(fit$probs) > 0) return(as.integer(nrow(fit$probs[[1]])))
  stop("Cannot determine K from poLCA object (no posterior/probs).")
}

get_npar <- function(fit) {
  if (!is.null(fit$npar)) return(as.numeric(fit$npar))
  # fallback if npar missing (rare): compute from probs + class shares
  K <- get_K(fit)
  Rj <- vapply(fit$probs, function(m) if (is.matrix(m)) ncol(m) else NA_integer_, integer(1))
  if (anyNA(Rj)) stop("Cannot compute npar: missing category counts in fit$probs.")
  as.numeric(sum(K * (Rj - 1)) + (K - 1))
}

get_n_patterns_obs <- function(fit) {
  # poLCA usually stores predicted cell counts for observed patterns as a vector
  if (!is.null(fit$predcell)) return(as.integer(length(fit$predcell)))
  # fallback: sometimes observed pattern table exists
  if (!is.null(fit$observed) && is.matrix(fit$observed)) return(as.integer(nrow(fit$observed)))
  NA_integer_
}

n_patterns_from_data <- function(data_indicators) {
  # data_indicators = db_lca with only indicators (your a1..a8), already NA-omitted
  as.integer(nrow(dplyr::distinct(data_indicators)))
}


entropy_stats <- function(fit) {
  P <- fit$posterior
  P <- pmax(P, 1e-12)
  N <- nrow(P); K <- ncol(P)
  
  # relative entropy (0..1): 1 = best separation
  rel_entropy <- 1 - (-sum(P * log(P)) / (N * log(K)))
  
  class_hat <- max.col(P)
  max_p <- apply(P, 1, max)
  
  avg_diag <- vapply(1:K, function(k) {
    idx <- which(class_hat == k)
    if (length(idx) == 0) NA_real_ else mean(P[idx, k])
  }, numeric(1))
  
  tibble(
    entropy = rel_entropy,
    mean_max_p = mean(max_p),
    p10_max_p = unname(quantile(max_p, 0.10)),
    avg_diag_min = suppressWarnings(min(avg_diag, na.rm = TRUE)),
    avg_diag_mean = mean(avg_diag, na.rm = TRUE)
  )
}

min_class_share <- function(fit) {
  Ppost <- fit$posterior
  N <- nrow(Ppost); K <- ncol(Ppost)
  class_hat <- max.col(Ppost)
  
  tibble(
    min_class_prior = min(fit$P),
    min_class_map   = min(tabulate(class_hat, nbins = K)) / N
  )
}

# ---- build table (no list-cols) -----------------------------------------

N_used <- nrow(db_lca)

m_patterns <- n_patterns_from_data(db_lca)  # scalar

m_patterns

# Robust extractors (avoid list-columns)
get_K <- function(fit) as.integer(ncol(fit$posterior))  # classes = cols of posterior
get_ll  <- function(fit) as.numeric(fit$llik)
get_aic <- function(fit) as.numeric(fit$aic)
get_bic <- function(fit) as.numeric(fit$bic)
get_npar <- function(fit) {
  if (!is.null(fit$npar)) return(as.numeric(fit$npar))
  # fallback: compute from probs
  K <- get_K(fit)
  Rj <- vapply(fit$probs, function(m) ncol(m), integer(1))
  as.numeric(sum(K * (Rj - 1)) + (K - 1))
}

# Table of fit indices
fit_tbl <- tibble(
  K     = map_int(fits, get_K),
  N     = nrow(db_lca),
  logLik= map_dbl(fits, get_ll),
  npar  = map_dbl(fits, get_npar),
  AIC   = map_dbl(fits, get_aic),
  BIC   = map_dbl(fits, get_bic)
) %>%
  arrange(K) %>%
  mutate(
    # Differences relative to preceding (K-1) model
    dAIC = AIC - lag(AIC),
    dBIC = BIC - lag(BIC),
    
    # Likelihood-ratio test (K vs K-1)
    # Test statistic: 2*(LL_K - LL_{K-1}) ~ Chi-square with df = npar_K - npar_{K-1}
    # Note: in LCA, models are not strictly nested in the regular sense;
    # this LRT is widely used as a heuristic, but interpret with caution.
    LRT = 2 * (logLik - lag(logLik)),
    df_LRT = npar - lag(npar),
    p_LRT = ifelse(!is.na(LRT) & df_LRT > 0, pchisq(LRT, df = df_LRT, lower.tail = FALSE), NA_real_)
  )

fit_tbl

fit_tbl_report <- fit_tbl %>%
  transmute(
    K, N,
    logLik = round(logLik, 1),
    npar = npar,
    AIC = round(AIC, 1),
    dAIC = round(dAIC, 1),
    BIC = round(BIC, 1),
    dBIC = round(dBIC, 1),
    LRT = round(LRT, 1),
    df_LRT = df_LRT,
    p_LRT = signif(p_LRT, 3)
  )

fit_tbl_report

# Visual: AIC/BIC by K (lower is better)
fit_long <- fit_tbl %>%
  dplyr::select(K, AIC, BIC) %>%
  pivot_longer(-K, names_to = "criterion", values_to = "value")

ggplot(fit_long, aes(x = K, y = value, group = criterion)) +
  geom_line() +
  geom_point() +
  facet_wrap(~criterion, scales = "free_y") +
  labs(x = "Number of classes (K)", y = "Value") +
  theme_minimal()

# Visual: ΔAIC and ΔBIC (negative = improvement over K-1)
delta_long <- fit_tbl %>%
  dplyr::select(K, dAIC, dBIC) %>%
  pivot_longer(-K, names_to = "delta", values_to = "value")

ggplot(delta_long, aes(x = K, y = value, group = delta)) +
  geom_hline(yintercept = 0) +
  geom_line() +
  geom_point() +
  facet_wrap(~delta, scales = "free_y") +
  labs(x = "K", y = "Change vs K-1 (negative = better)") +
  theme_minimal()


# Ajuste

fits[[1]]$predcell
fits[[2]]$predcell
fits[[3]]$predcell
fits[[4]]$predcell
fits[[5]]$predcell

p1 <- (1-pchisq(fits[[1]]$Chisq, fits[[1]]$resid.df))
p2 <- (1-pchisq(fits[[2]]$Chisq, fits[[2]]$resid.df))
p3 <- (1-pchisq(fits[[3]]$Chisq, fits[[3]]$resid.df))
p4 <- (1-pchisq(fits[[4]]$Chisq, fits[[4]]$resid.df))
p5 <- (1-pchisq(fits[[5]]$Chisq, fits[[5]]$resid.df))

AjusteM_acum<-data.frame(c("M1", "M2", "M3", "M4", "M5"),
                         c(fits[[1]]$llik, fits[[2]]$llik, fits[[3]]$llik, fits[[4]]$llik, fits[[5]]$llik),
                         c(fits[[1]]$Chisq, fits[[2]]$Chisq, fits[[3]]$Chisq, fits[[4]]$Chisq, fits[[5]]$Chisq),
                         c(fits[[1]]$Gsq, fits[[2]]$Gsq, fits[[3]]$Gsq, fits[[4]]$Gsq, fits[[5]]$Gsq),
                         c(fits[[1]]$npar, fits[[2]]$npar, fits[[3]]$npar, fits[[4]]$npar, fits[[5]]$npar),
                         c(fits[[1]]$aic, fits[[2]]$aic, fits[[3]]$aic, fits[[4]]$aic, fits[[5]]$aic),
                         c(fits[[1]]$bic, fits[[2]]$bic, fits[[3]]$bic, fits[[4]]$bic, fits[[5]]$bic),
                         c(p1, p2, p3, p4, p5))
colnames(AjusteM_acum)<-c("Modelo", "Loglike", "X2", "G2", "DF", "AIC", "BIC", "P-value")

View(AjusteM_acum)

# Gráfico acumulado

# Con Pr(1)
plotdatos <- melt(fits[[4]]$probs) 
plotdatos2 <- plotdatos[plotdatos$X2=="1",]

ggplot(plotdatos2, aes(x=L1, y = value, group = X1, colour = X1)) +
  geom_point() + geom_line() + 
  labs(x="Variables",
       y = "Valores",
       caption = "Fuente: Elaboración propia con base en datos de huelgas laborales OHL-COES (1979-2018)") + 
  theme(legend.position = "bottom",plot.title = element_text(size=12),
        plot.subtitle = element_text(size=10),
        plot.caption = element_text(size=8)) 


# 6. Visualize and report latent classes (poLCA) --------------------------

fit4 <- fits[[4]]

# 1) Class prevalence (prior class proportions)
class_prev <- tibble(
  class = 1:length(fit4$P),
  pi = as.numeric(fit4$P)
) %>%
  mutate(pi_pct = 100 * pi)

class_prev

# 2) Extract conditional probabilities per item and class
# fit4$probs is a list: each element is K x Rj matrix.
# For dichotomous items: K x 2 (categories = colnames or "1","2")
probs_long <- bind_rows(lapply(names(fit4$probs), function(item) {
  mat <- fit4$probs[[item]]
  df <- as.data.frame(mat)
  df$class <- 1:nrow(mat)
  df$item <- item
  df
})) %>%
  pivot_longer(cols = -c(class, item), names_to = "category", values_to = "prob") %>%
  mutate(
    class = as.integer(class),
    prob = as.numeric(prob)
  )

probs_long %>% arrange(item, class, category)

# 3) Decide which category is the "endorsement"/"included" category
# Because you did mutate_all(as.factor), the columns are often named "1","2" etc.
# Check the levels of one indicator in db_lca:
levels(db_lca$a1)

# If levels are c("0","1") or c("1","2") you'll want the "higher"/"yes" one.
# Here I will assume endorsement is the LAST level (most common after dichotomization).
endorsement_level <- tail(levels(db_lca$a1), 1)
endorsement_level

# 4) Build a clean table: P(endorsement | class) for each item
p_endorse <- probs_long %>%
  filter(category == endorsement_level) %>%
  dplyr::select(item, class, p = prob) %>%
  mutate(item = factor(item, levels = names(fit4$probs)))

p_endorse

# 5) Plot: profile lines (one line per class)
ggplot(p_endorse, aes(x = item, y = p, group = factor(class))) +
  geom_line() +
  geom_point() +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Item", y = paste0("P(Y = ", endorsement_level, " | class)"), group = "Class") +
  theme_minimal()

# 6) Plot: heatmap (good for papers)
ggplot(p_endorse, aes(x = item, y = factor(class), fill = p)) +
  geom_tile() +
  coord_cartesian(xlim = NULL) +
  labs(x = "Item", y = "Class", fill = paste0("P(Y=", endorsement_level, ")")) +
  theme_minimal()

# 7) Report table (wide): items x classes with probabilities
report_probs <- p_endorse %>%
  mutate(class = paste0("Class_", class)) %>%
  pivot_wider(names_from = class, values_from = p) %>%
  mutate(across(starts_with("Class_"), ~ round(.x, 3)))

report_probs

# 8) Optional: add class prevalence to the column names (for reporting)
prev_lab <- class_prev %>%
  transmute(class = paste0("Class_", class),
            lab = paste0(class, " (", round(pi_pct, 1), "%)"))

report_probs2 <- report_probs
for (i in seq_len(nrow(prev_lab))) {
  old <- prev_lab$class[i]
  new <- prev_lab$lab[i]
  names(report_probs2)[names(report_probs2) == old] <- new
}

report_probs2

# Compare K=3 vs K=4 (poLCA): fit, sizes/separation, item profiles --------------

fit3 <- fits[[3]]
fit4 <- fits[[4]]

# ---- 1) Fit comparison (already in your fit_tbl_report, but make it explicit)

fit_comp <- tibble(
  K = c(3, 4),
  N = nrow(db_lca),
  logLik = c(fit3$llik, fit4$llik),
  npar   = c(fit3$npar, fit4$npar),
  AIC    = c(fit3$aic,  fit4$aic),
  BIC    = c(fit3$bic,  fit4$bic)
) %>%
  arrange(K) %>%
  mutate(
    dAIC = AIC - lag(AIC),
    dBIC = BIC - lag(BIC),
    LRT  = 2 * (logLik - lag(logLik)),
    df_LRT = npar - lag(npar),
    p_LRT  = ifelse(df_LRT > 0, pchisq(LRT, df=df_LRT, lower.tail=FALSE), NA_real_)
  )

fit_comp


# ---- 2) Class size + separation diagnostics

sep_diag <- function(fit) {
  P <- fit$posterior
  K <- ncol(P)
  class_hat <- max.col(P)
  max_p <- apply(P, 1, max)
  
  avg_diag <- vapply(1:K, function(k) {
    idx <- which(class_hat == k)
    if (length(idx) == 0) NA_real_ else mean(P[idx, k])
  }, numeric(1))
  
  tibble(
    K = K,
    min_class_prior = min(fit$P),
    class_prior = paste0(round(100 * fit$P, 1), collapse = " / "),
    mean_max_p = mean(max_p),
    p10_max_p  = unname(quantile(max_p, 0.10)),
    avg_diag_min  = suppressWarnings(min(avg_diag, na.rm = TRUE)),
    avg_diag_mean = mean(avg_diag, na.rm = TRUE)
  )
}

bind_rows(sep_diag(fit3), sep_diag(fit4))

# ---- 3) Item profile comparison: P(endorsement | class) for each K

# determine endorsement category from your factors
endorsement_level <- tail(levels(db_lca$a1), 1)

get_p_endorse <- function(fit, endorsement_level) {
  items <- names(fit$probs)
  
  probs_long <- bind_rows(lapply(items, function(item) {
    mat <- fit$probs[[item]]              # K x 2 (dichotomous)
    df <- as.data.frame(mat)
    df$class <- 1:nrow(mat)
    df$item <- item
    df
  })) %>%
    pivot_longer(cols = -c(class, item), names_to = "category", values_to = "prob") %>%
    mutate(class = as.integer(class), prob = as.numeric(prob)) %>%
    filter(category == endorsement_level) %>%
    transmute(item, class, p = prob)
  
  probs_long
}

p3 <- get_p_endorse(fit3, endorsement_level) %>% mutate(K = 3)
p4 <- get_p_endorse(fit4, endorsement_level) %>% mutate(K = 4)

# Plot profiles for each solution
ggplot(bind_rows(p3, p4),
       aes(x = item, y = p, group = factor(class))) +
  geom_line() +
  geom_point() +
  coord_cartesian(ylim = c(0,1)) +
  facet_wrap(~K, ncol = 1) +
  labs(x = "Item", y = paste0("P(Y=", endorsement_level, " | class)"), group = "Class") +
  theme_minimal()

# Heatmap for each solution
ggplot(bind_rows(p3, p4),
       aes(x = item, y = factor(class), fill = p)) +
  geom_tile() +
  facet_wrap(~K, ncol = 1) +
  labs(x = "Item", y = "Class", fill = "P(endorse)") +
  theme_minimal()


# ---- 4) "Marginal value" of the 4th class: does K=4 split an existing class?

# Compare class-mean profiles by looking at within-solution dispersion across classes for each item.
# If K=4 increases dispersion for some items, it adds differentiation.

disp_by_item <- function(p_df) {
  p_df %>%
    group_by(item) %>%
    summarise(sd_across_classes = sd(p), range_across_classes = max(p) - min(p), .groups="drop")
}

disp3 <- disp_by_item(p3) %>% dplyr::rename(sd3 = sd_across_classes, range3 = range_across_classes)
disp4 <- disp_by_item(p4) %>% dplyr::rename(sd4 = sd_across_classes, range4 = range_across_classes)

disp_comp <- disp3 %>%
  left_join(disp4, by="item") %>%
  mutate(
    d_sd = sd4 - sd3,
    d_range = range4 - range3
  ) %>%
  arrange(desc(d_range))

disp_comp

# Quick plot: which items gain most differentiation when moving 3 -> 4 classes?
ggplot(disp_comp, aes(x = reorder(item, d_range), y = d_range)) +
  geom_point() +
  coord_flip() +
  labs(x = "Item", y = "Increase in range across classes (K=4 minus K=3)") +
  theme_minimal()
