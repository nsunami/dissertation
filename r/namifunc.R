# Project-Wide Functions for the Dissertation by Nami Sunami
# Thanks for this blog post: https://kdestasio.github.io/post/r_best_practices/
# Apa stats not on CRAN
# install.packages("remotes")
# remotes::install_github('achetverikov/apastats',subdir='apastats')
library(english)
library(wordcloud)
library(car) # Type III ANOVA
library(emmeans)
library(tidytext)
library(scales)
library(lme4)
library(GGally)
library(ggpubr)
library(ggmosaic)
library(here)
library(broom)
library(codebook)
library(ggtext)
library(apastats)
library(kableExtra)
library(effectsize) # for equivalence tests and effect size estimation

# Project-Specific Parameters
## SESOI for ROPE in the equivalence test
Cohen_d_target <- 0.35
ROPE_d <- c(-Cohen_d_target, Cohen_d_target)
SESOI_r <- .17
ROPE_r <- c(-SESOI_r, SESOI_r)
# Dodge width
my_dodge_width <- 0.5


# Summary Statistics ------------------------------------------------------------

## Convert to 90% CI string
to_CI_str <- function(conf_pct = 95, lower, upper, digits = 2) {
  lower <- lower %>% f.round(digits)
  upper <- upper %>% f.round(digits)
  sprintf(
    "%s%%CI [%s, %s]",
    conf_pct, lower, upper
  )
}

## Descriptive Tables by Factor
describe_by_factor <- function(df, ..., vars) {
  # .... are groups
  # vars are target variables
  .group <- enquos(...)
  df %>%
    dplyr::select(!!!.group, {{ vars }}) %>%
    dplyr::group_by(!!!.group) %>%
    dplyr::summarise(across(
      everything(),
      list(mean = ~ mean(., na.rm = TRUE), sd = ~ sd(., na.rm = TRUE))
    ))
}

## Get database (long format)
get_desc_db <- function(describe_by_factor_df,
                        keys) {
  # Get descriptives table by factor and produce a query-able data frame
  descriptives_long <- describe_by_factor_df %>%
    pivot_longer(c(ends_with("mean"), ends_with("sd"))) %>%
    dplyr::mutate(
      variable = str_match(name, "^[^_].+(?=_)") %>% as.vector(),
      stat = str_match(name, "(?:[^_](?!_))+$") %>% as.vector()
    )
  db <- descriptives_long %>% data.table::setDT(key = c("essay_condition", "variable", "stat"))
  return(db)
}
# Get APA formatted statics from the describe_by_factor object
get_APA_from_msd <- function(describe_by_factor_df,
                             group_cols) {
  # Get descriptives table by factor and produce a query-able data frame
  descriptives_long <- describe_by_factor_df %>%
    pivot_longer(c(ends_with("mean"), ends_with("sd"))) %>%
    # Parse with underscores
    dplyr::mutate(
      variable = str_match(name, "^[^_].+(?=_)") %>% as.vector(),
      stat = str_match(name, "(?:[^_](?!_))+$") %>% as.vector()
    )
  db <- descriptives_long %>%
    pivot_wider(-name, values_from = value, names_from = stat) %>%
    dplyr::mutate(APA = to_msd(mean %>% f.round(), sd %>% f.round()))
  return(db)
}

## Function to summarise Means, SD's, and N's given the df of target variables for correlation analysis
summarise_descriptives <- function(df) {
  df %>%
    # create a summary row with _n, _nmean, _sd
    summarise(across(
      where(is.numeric),
      list(
        n = ~ sum(!is.na(.x)),
        mean = ~ mean(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE)
      )
    )) %>%
    # Pivot to the long format (1 column)
    pivot_longer(everything()) %>%
    # Label types using regex (get characters after "_")
    dplyr::mutate(type = str_extract(name, "[^_]*$")) %>%
    # Repeat the scale names (since we are dealing with n, mean, sd, rep is 3)
    dplyr::mutate(scale = rep(names(df), each = 3)) %>%
    # pivot to wider form. Each row is a scale, and each columns are n, mean, sd
    pivot_wider(
      names_from = type,
      id_cols = scale
    ) %>%
    # Mutate to number the variables (for correlation)
    dplyr::mutate(scale = paste0(dplyr::row_number(), ". ", scale))
}

## Get Bivariate Correlations with 90 and 95% CI's
get_correlations <- function(target_df,
                             vars, outcome = Heart) {
  vars <- enquo(vars)
  outcome <- enquo(outcome)
  # conf level for CIs
  conf.level1 <- 0.95
  conf.level2 <- 0.90
  # Calculate correlations via cor.test()
  df <- target_df %>%
    summarise(across(
      !!vars,
      ~ list(
        model95 = cor.test(pull(cur_data(), !!outcome), .x, conf.level = conf.level1),
        model90 = cor.test(pull(cur_data(), !!outcome), .x, conf.level = conf.level2)
      )
    ))
  model_names <- df %>%
    pull(1) %>%
    names()
  df <- df %>%
    dplyr::mutate(Conf_Level = model_names) %>%
    pivot_longer(-Conf_Level, values_to = "model", names_to = c("label"))
  # Extract numbers from the calculated model using map()
  df <- df %>%
    dplyr::mutate(
      model_df = map(model, ~ broom::glance(.)),
      # Create an APA paragraph with
      APA = map_chr(model, ~ describe.r(., .add_CI = TRUE))
    ) %>%
    # Add Confidence Intervals XX%[XX, XX] to APA string
    # dplyr::mutate(APA = paste0(APA, ",", to_CI_str(Conf_Level, lower = conf.low, upper = conf.high))) %>%
    unnest(model_df) %>%
    # name parameter to df
dplyr::rename(df = "parameter")
  # Wider to create separate columns for 90 & 95% CIs
  df <- df %>%
    pivot_wider(
      id_cols = c("label", "estimate", "df"), names_from = c("Conf_Level"),
      values_from = c("model", "conf.low", "conf.high", "APA", "p.value")
    )
  # add confidence intervals for APA
  return(df)
}

## Tidy with APA
summarize_Anova_with_APA <- function(lm, .sumtype = 3) {
  Anova_obj <- lm %>% Anova(type = .sumtype)
  output_tibble <- broom::tidy(Anova_obj) %>%
    dplyr::mutate(APA = describe.Anova(Anova_obj))
  return(output_tibble)
}

## Deframe and aslist
deframe_as_list <- function(x) {
  x %>%
    tibble::deframe() %>%
    as.list()
}


# GGplot-Related Functions --------------------------------------------------------
## Dodge positions for plots
dodge_pos <- position_dodge(width = my_dodge_width)
## ggplot theme
ggplot2::theme_set(ggplot2::theme_minimal())
## Default width for the violin plot
default_violin_width <- 0.4
## Default width for the errorbar
default_errorbar_width <- 0.1
## Default color pallette for color
default_color_continuous <- scale_color_continuous("Set2")
## Default errorbar and the means
default_violin <- list(
  # violin
  geom_violin(width = default_violin_width),
  # Plot data points
  geom_point(position = position_jitter(width = .3), size = .5),
  geom_errorbar(
    fun.data = mean_cl_normal,
    stat = "summary",
    width = .1,
    position = dodge_pos,
    color = "black", alpha = 1
  ),
  # Plot means
  geom_point(
    stat = "summary", fun.data = mean_cl_normal,
    position = dodge_pos,
    shape = 21, fill = "white",
    color = "black"
  ),
  # color
  scale_color_brewer(palette = "Set2"),
  # Enable Markdown for the Caption
  theme(plot.caption = element_markdown(lineheight = 1.2))
)

## Add SESOI
SESOIgeom <- geom_vline(xintercept = c(-SESOI_r, SESOI_r), color = "grey", linetype = "dashed")
## Add vertical lines for SESOI and zero
geom_zeroSESOI <- list(
  # Line for SESOI
  SESOIgeom,
  # Line for Zero
  geom_vline(xintercept = 0, color = "black", linetype = "dashed")
)

## SESOI in D
SESOIgeom_d <- geom_vline(xintercept = c(-Cohen_d_target, Cohen_d_target), color = "grey", linetype = "dashed")
geom_zeroSEOI_d <- list(
  # Line for SESOI
  SESOIgeom_d,
  # Line for Zero
  geom_vline(xintercept = 0, color = "black", linetype = "dashed")
)

# Violin Plot for Study 3
s3_draw_mc_plot <- function(df = s3_df, y, ylab,
                            x = parasocial,
                            color = parasocial,
                            xlab = "Parasocial Condition",
                            facet_by = social_world,
                            caption = "") {
  df %>%
    # Exclude those failed attention check
    dplyr::filter(attention_all_correct == TRUE) %>%
    ggplot(aes(x = {{ x }}, y = {{ y }}, color = {{ color }})) +
    # Default violin, errorbar plot
    default_violin +
    # Remove legend
    guides(color = FALSE) +
    # Labels
    ylab(ylab) +
    xlab(xlab) +
    # Condition value labels to title case
    scale_x_discrete(labels = str_to_title) +
    # Facet by Social World
    facet_grid(cols = vars({{ facet_by }})) +
    labs(caption = caption)
}

# Across-Time Plot for Study 1e
s1e_plot_across_time <- function(df_long, y, ylab = "", caption = "", title = "") {
  df_long %>%
    dplyr::mutate(across(c(rejection, confederate_desire), to_factor)) %>%
    ggplot(aes(
      x = time, y = {{ y }}, color = paste(rejection, confederate_desire),
      group = paste(rejection, confederate_desire)
    )) +
    # Plot Data Points
    geom_point(
      position = position_jitterdodge(
        jitter.height = 1,
        jitter.width = .05, dodge.width = my_dodge_width
      ),
      size = .2
    ) +
    # Add line to connect the groups
    geom_line(stat = "summary", fun.data = mean_cl_normal, position = dodge_pos) +
    geom_errorbar(
      fun.data = mean_cl_normal, stat = "summary", width = .3,
      position = dodge_pos, color = "black"
    ) +
    # Plot means
    geom_point(
      stat = "summary", fun.data = mean_cl_normal,
      shape = 21, aes(fill = paste(rejection, confederate_desire)), color = "black",
      position = dodge_pos,
      size = 1
    ) +
    # Labels
    ggtitle(title) +
    xlab("Time") +
    ylab(ylab) +
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Set2", name = "Condition") +
    guides(fill = FALSE) +
    labs(caption = caption) +
    # Scale for Tme
    scale_x_discrete(limits = c("3", "5"))
}

# Forest Plot Caption
s1_caption_errorbars9095 <- "Errorbars represent 90% (inner tick) and 95% (outer tick) confidence intervals"
s1_caption_SESOI_r <- sprintf("The vertical dashed lines represent the smallest effect size of interest (SESOI, |r| = %s)", SESOI_r)

# R Notebook Rendering-Related Functions --------------------------------------------
## Render a Kable Table
render_APA_kable <- function(df,
                             colnames = c(
                               "Variable", "$n$", "$M$", "$SD$",
                               1:(ncol(df) - 4)
                             ),
                             align = c(
                               "l", "c", "c", "c",
                               rep("c", nrow(df))
                             ),
                             digits = c(
                               NA, 0, 2, 2,
                               rep(2, nrow(df))
                             ),
                             title = NA,
                             footnote = NA,
                             col1_width = "1.2in") {
  out_kbl <- df %>% kbl(
    booktabs = TRUE, escape = FALSE,
    col.names = colnames, align = align,
    digits = digits, caption = title
  )

  if (!is.na(footnote)) {
    out_kbl <- out_kbl %>%
      footnote(
        general_title = "Note.",
        general = footnote,
        threeparttable = FALSE,
        footnote_as_chunk = TRUE,
        escape = FALSE
      )
  }

  out_kbl <- out_kbl %>%
    row_spec(row = 0, align = "c") %>%
    column_spec(column = 1, width = col1_width)
}

# Styling for correlation tables
kable_styling_cortable <- function(kbl_obj) {
  kbl_obj %>%
    kable_paper("hover") %>%
    # Scrollbox for html
    scroll_box(width = "100%", box_css = "border: 0px;") %>%
    # Landscale for Latex
    kableExtra::landscape() %>%
    kable_styling(
      bootstrap_options = c("condensed", "responsive"),
      latex_options = "scale_down"
    )
}

# Function to render study  cable
s1_render_kable <- function(df, studykey = " ") {
  # First check if packing is needed
  # get the named list of packing
  # Filter so that only the label rows will be returned (intervals will be the length of packing)
  pack_named_index <- df %>%
    dplyr::filter(!is.na(intervals)) %>%
    dplyr::select(labels, intervals) %>%
    tibble::deframe()
  # Get the names of the named list
  pack_named_index_names <- names(pack_named_index) %>% str_trim()
  # Check if Needs packing
  needs_packing <- (sum(pack_named_index_names == "") != length(pack_named_index_names))
  # Create Kable
  returned_kable <- df %>%
    dplyr::select(Measure, Time, Construct, Validity, Citation) %>%
    kbl(
      caption = paste0("Summary of Measures for Study", " 1", studykey),
      booktabs = TRUE
    ) %>%
    column_spec(1, width = "5cm") %>%
    column_spec(2, width = "2cm") %>%
    # column_spec(3, width = "4cm") %>%
    column_spec(4, width = "2cm") %>%
    column_spec(5, width = "3cm") %>%
    row_spec(0, bold = T) %>%
    kable_classic(full_width = F, html_font = "Cambria") %>%
    footnote(
      general_title = "Note.",
      footnote_as_chunk = TRUE,
      general = "Con. = Convergent Validity. Dis. = Discriminant Validity. (R) = Reverse association. "
    )
  # If packign is needed add the pack rows
  if (needs_packing) {
    returned_kable <- returned_kable %>%
      pack_rows(
        index = pack_named_index,
        bold = FALSE, hline_after = FALSE,
        # White out the line
        label_row_css = "background-color: white; border-bottom:white"
      )
  }

  # Style for pdf
  returned_kable <- returned_kable %>% kable_styling(latex_options = "scale_down")
  return(returned_kable)
}

## Describe effectsize::cohens_d to Markdown Format
describe_d_ci <- function(x) {
  sprintf(
    "_d_ = %s, %s%%CI [%s, %s]",
    x$Cohens_d %>% f.round(2),
    x$CI * 100, # convert to %
    x$CI_low %>% f.round(2),
    x$CI_high %>% f.round(2)
  )
}

## Function to create a markdown string of mean and sd in the APA format
to_msd <- function(mean, sd) {
  mean <- f.round(mean)
  sd <- f.round(sd)
  sprintf("_M_ = %s, _SD_ = %s", mean, sd)
}

## Get a chi-squrea APA formated from the chi-square htest object
describe_chi_htest <- function(x) {
  chistat <- x$statistic
  df <- x$parameter
  n <- x$observed %>% sum()
  pstring <- x$p.value %>% round.p()
  apastring <- sprintf(
    "X<sup>2</sup>(%i, _N_ = %s) = %.2f, \\emph{p} %s",
    df, n, chistat, pstring
  )
  apastring <- format.results(apastring)
  return(apastring)
}

## Describe rstatix::anova_test()
describe_anova_test <- function(x) {
  x %>%
    dplyr::mutate(APA = sprintf(
      "_F_(%s, %s) = %s, _p_ %s, $\\eta^2_{G}$ %s",
      DFn, DFd, F %>% f.round(),
      p %>% round.p(),
      round.p(ges)
    ))
}


# # Possible update to apastats::f.round()
f.round <- function(x, digits = 2, strip.lead.zeros = F) {
  values_string <- stringr::str_trim(format(round(as.numeric(x), digits), nsmall = digits))
  if (strip.lead.zeros) {
    values_string <- sub("^0", "", values_string)
    values_string <- sub("^-0", "-", values_string)
  }
  return(values_string)
}

## Possible Update to apastats::describe.r

describe.r <- function(rc, ..., .add_CI = FALSE) {
  if (.add_CI == FALSE) {
    format.results(sprintf(
      "\\emph{r}(%.0f) = %.2f, \\emph{p} %s",
      rc$parameter, rc$estimate, round.p(rc$p.value)
    ), ...)
  }
  if (.add_CI == TRUE) {
    conf.level <- attr(rc$conf.int, "conf.level")
    conf.lower <- rc$conf.int[1] %>% f.round(2)
    conf.upper <- rc$conf.int[2] %>% f.round(2)
    format.results(sprintf(
      "\\emph{r}(%.0f) = %.2f, \\emph{p} %s, %s%%CI [%s, %s]",
      rc$parameter, rc$estimate, round.p(rc$p.value),
      conf.level * 100, conf.lower, conf.upper
    ), ...)
  }
}

# Get alphas from a df that was created by codebook
get_cronbach_alphas <- function(reliabilities) {
  # Get the list of cronbach's alpha
  cronbach <- map(
    reliabilities,
    function(x) {
      if (is.null(x)) {
        NA
      }
      x$cronbach.alpha
    }
  )
  return(cronbach)
}

# Get the reliabiliyt output from reliabilities object for smaller chaching
get_reliability_output <- function(reliability) {
  reliability %>% map(~ .$internal_consistency$scaleReliability$output)
}

# Get equivalence
add_eqtest <- function(lm_df) {
  outdf <- lm_df %>%
    # use ci = .95 for 95% overall confidence (calculate 90% CI for estimates)
    dplyr::mutate(eq_test = map(model, ~ equivalence_test(., range = ROPE_r, ci = .95, rule = "classic"))) %>%
    # Extract the results for the predictor
    dplyr::mutate(eq_pred = map_chr(eq_test, ~ .$ROPE_Equivalence[[2]])) %>%
    # Add labels for plot annotation
    dplyr::mutate(eq_label = dplyr::case_when(
      eq_pred == "Rejected" ~ "",
      eq_pred == "Accepted" ~ "*",
      # Unicode
      eq_pred == "Undecided" ~ "\U2020"
    ))
  return(outdf)
}

# Equivalence logic
evaluate_equivalence <- function(lower, upper, ROPE) {
  ROPE_low <- min(ROPE)
  ROPE_upper <- max(ROPE)
  lower_in_ROPE <- lower >= ROPE_low & lower <= ROPE_upper
  upper_in_ROPE <- upper >= ROPE_low & upper <= ROPE_upper

  if (lower_in_ROPE & upper_in_ROPE) {
    return("Accepted")
  }
  if (!lower_in_ROPE & !upper_in_ROPE) {
    return("Rejected")
  } else {
    return("Undecided")
  }
}


# Get n and percent by specified group and convert it to list for formatting
get_n_pct <- function(df, ...) {
  df %>%
    dplyr::group_by(...) %>%
    summarise(n =dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(pct = round(n / sum(n) * 100, 2)) %>%
    pivot_longer(cols = c(n, pct)) %>%
    dplyr::nest_by(...) %>%
    dplyr::ungroup() %>% # Ungroup to avoid cannot be recycled one error
    dplyr::mutate(data = map(data, function(x) x %>% deframe_as_list())) %>%
    unite("label", ...) %>%
    dplyr::select(label, data) %>%
    deframe_as_list()
}

# Plot moderation results for Study 2
plot_s2_moderation <- function(model) {
  # Check if the model includes the grouping factor
  terms <- terms(model)
  terms_attr <- attributes(terms)$term.labels
  cur_mod <- terms_attr[[1]]
  varnames <- s2_var_nv %>% names()
  newnames <- varnames %>% paste0("_c")
  varnames[2:7] <- newnames[-1]
  names(s2_var_nv) <- varnames
  cur_mod_label <- s2_var_nv[cur_mod]
  # Labels function
  s2mod_relabel <- function(x) {
    x %>%
      stringr::str_replace("parasocial_MC_group2-1", "Group [2-1]") %>%
      stringr::str_replace("parasocial_MC_group3-2", "Group [3-2]") %>%
      stringr::str_replace(cur_mod, cur_mod_label) %>%
      stringr::str_replace("essay_condition1", "Essay [Surrogacy]") %>%
      stringr::str_replace("Time1", "Time [1]") %>%
      str_replace_all(":", " x ")
  }

  if ("parasocial_MC_group" %in% terms_attr) {
    # Geoms for the parasocial MC
    geom_now <- list(
      scale_x_discrete(labels = s2mod_relabel),
      labs(
        caption = paste("Errorbars represent 95% confidence intervals.",
          "The reference group for the Parasocial Relationship Group is Formed Parasocial Relationships.",
          sep = "\n"
        ),
        title = "Parasocial Relationships Group"
      )
    )
  } else {
    # GEOMS for the other plots
    geom_now <- list(
      scale_x_discrete(labels = s2mod_relabel),
      labs(
        caption = "Errorbars represent 95% confidence intervals.",
        title = cur_mod_label
      )
    )
  }
  model %>% sjPlot::plot_model(
    show.values = TRUE, colors = "Set2",
    value.offset = .5, value.size = 2.5
  ) +
    geom_now
}


# Plot Moderation for Study 3
# User function
plot_s3_mod_bymc <- function(model) {
  # Check if the model includes the grouping factor
  cur_terms <- terms(model)
  terms_attr <- attributes(cur_terms)$term.labels
  current_moderator <- terms_attr[[1]]

  terms_labels <- c(
    "parasocial_MC_group" = "parasocial_MC_group",
    "IOS" = "IOS",
    "PSI" = "PSI",
    "Single_Immersion" = "Immersion",
    "OTF_Social_World" = "OTF Social World",
    "Enjoyment" = "Enjoyment",
    "PC_Identification" = "PCID"
  )
  terms_names <- names(terms_labels)
  terms_names[2:7] <- paste0(terms_names[2:7], "_c")
  names(terms_labels) <- terms_names
  # Get the label
  current_moderator_label <- terms_labels[current_moderator]

  # Function to relabel the variabels
  s3mod_relabel <- function(x) {
    x %>%
      stringr::str_replace("parasocial_MC_group1", "Group [1]") %>%
      stringr::str_replace("parasocial_MC_group2", "Group [2]") %>%
      stringr::str_replace(current_moderator, current_moderator_label) %>%
      stringr::str_replace("social_world1", "Social World [High]") %>%
      stringr::str_replace("parasocial1", "Parasocial [High]") %>%
      stringr::str_replace("time1", "Time [1]") %>%
      str_replace_all(":", " x ")
  }

  if ("parasocial_MC_group" == current_moderator_label) {
    # Geoms for the parasocial MC
    geom_now <- list(
      scale_x_discrete(labels = s3mod_relabel),
      labs(
        caption = paste("The reference group for the Parasocial Group is No Parasocial Relationship with NPC",
          "Group [1] = Formed Parasocial Relationship with NPC",
          "Group [2] = No Interaction with NPC",
          sep = "\n"
        ),
        title = "Parasocial Relationships Group"
      )
    )
  } else {
    # geoms for the other plots
    geom_now <- list(
      scale_x_discrete(labels = s3mod_relabel),
      labs(title = current_moderator_label)
    )
  }
  p <- model %>%
    sjPlot::plot_model(
      show.values = TRUE, colors = "Set2",
      value.offset = .5, value.size = 2.5
    ) +
    geom_now
  return(p)
}



# Get a matrix of correlations with ** for statistical significance
# The code was in a document from Fred
corstarsl <- function(x) {
  # df as input
  x <- as.matrix(x)
  R <- Hmisc::rcorr(x)$r
  p <- Hmisc::rcorr(x)$P

  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "*", ifelse(p < .01, "*", ifelse(p < .05, "*", " ")))

  ## trunctuate the matrix that holds the correlations to two decimal
  R <- R %>%
    f.round(strip.lead.zeros = TRUE) %>%
    as.matrix(ncol = ncol(x))

  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep = ""), ncol = ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep = "")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep = "")

  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew)

  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew) - 1])
  return(Rnew)
}


# get NMSD COR table
get_nmsd_cor_table <- function(target_vars_df) {
  if (!is_tibble(target_vars_df)) {
    target_vars_df <- target_vars_df %>% as_tibble()
  }
  # N, Mean, SD Table
  nmsd_table <- target_vars_df %>%
    summarise_descriptives()
  # Correlation Table
  cor_table <- target_vars_df %>% corstarsl()
  # Combine the tables
  nmsd_cor_table <- nmsd_table %>% cbind(cor_table)
  return(nmsd_cor_table)
}


# Get b =
get_slope_APA <- function(b, SE, df, ci.lower, ci.upper, b.digits = 2) {
  sprintf(
    paste0(
      "\\emph{B} = %.",
      b.digits, "f, \\emph{SE} = %.",
      b.digits, "f, 95%%CI[%.2f, %.2f]"
    ),
    b, SE, ci.lower, ci.upper
  ) %>% format.results()
}


# Get the APA-formatted t-test
describe_t_val <- function(t_stat, df,
                           p_value, ...) {
  res_str <- sprintf(
    "\\emph{t}(%.1f) = %.2f, \\emph{p} %s",
    df, t_stat, round.p(p_value)
  )
  format.results(res_str, ...)
}


# Get APA from Em trends
get_APA_from_emm <- function(emm) {
  # if length is one, no contrasts
  if (length(emm) != 1) {
    emm <- emm$emtrends
  }

  stats_df <- emm %>%
    as_tibble() %>%
    dplyr::select(ends_with(".trend"):last_col())
  labels_df <- emm %>%
    as_tibble() %>%
    dplyr::select(1:ends_with(".trend")) %>%
    dplyr::select(-last_col())
  # Combine the labels as the first row
  labels_df <- labels_df %>%
    unite(col = "label")

  # Stats df
  stats_df <- stats_df %>%
    dplyr::mutate(APA = get_slope_APA(
      b = .[[1]],
      SE = SE,
      ci.lower = lower.CL, ci.upper = upper.CL
    ))

  # Combine two (assuming the order does not change)
  combined_df <- labels_df %>%
    bind_cols(stats_df)

  # Deframe as list
  APA_output <- combined_df %>%
    dplyr::select(1, APA) %>%
    deframe_as_list()
}
