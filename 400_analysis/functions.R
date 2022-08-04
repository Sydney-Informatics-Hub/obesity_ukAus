get_wc <- function(corpuslabel, sourcetypelabel){
  metadata |>
    filter(corpus == corpuslabel & source_type == sourcetypelabel) |>
    pull(no_words_in_text)
}

get_article_ids <- function(df, language_type){
  df |>
    filter(dataset == language_type) |>
    pull(article_id)
}

generate_conting_tab_sumhits <- function(df) {
  tmp <- df |>
    group_by(corpus, source_type) |>
    summarize(sum = sum(no_hits_in_text)) |>
    pivot_wider(names_from = corpus, values_from = sum) |>
    as.data.frame()
  rownames(tmp) <- tmp$source_type
  tmp |>
    select(-source_type) |>
    ungroup()
}

generate_conting_tab_noarticles <- function(df) {
  tmp <- df |>
    group_by(corpus, source_type) |>
    summarize(count = n()) |>
    pivot_wider(names_from = corpus, values_from = count) |>
    as.data.frame()
  rownames(tmp) <- tmp$source_type
  tmp |>
    select(-source_type) |>
    ungroup()
}

generate_wc <- function(df) {
  tmp <- df |>
    group_by(corpus, source_type) |>
    summarise(total_words = sum(no_words_in_text)) |>
    pivot_wider(names_from = corpus, values_from = total_words) |>
    as.data.frame()
  rownames(tmp) <- tmp$source_type
  tmp |>
    select(-source_type) |>
    ungroup()
}

generate_article_count <- function(df) {
  tmp <- df |>
    group_by(corpus, source_type) |>
    summarise(total_articles = n()) |>
    pivot_wider(names_from = corpus, values_from = total_articles) |>
    as.data.frame()
  rownames(tmp) <- tmp$source_type
  tmp |>
    select(-source_type) |>
    ungroup()
}

get_cramers_v <- function(mymatrix, chi) {
  # Find degrees of freedom - min row or col - 1
  n = sum(mymatrix)
  # always 1 for all of the comparisons in this study
  df <- 1
  #df <- min(dim(mymatrix)) - 1
  sqrt((chi)/(n * df))
}


get_chisq_tabloid_broadsheet <- function(matrix, col_no, type) {

  normalisation_matrix <- if (type == "wc") {
    generate_wc(metadata)
  } else {
    generate_article_count(metadata)
  }

  # needing to subset the matrix with [,col_no]
  # is why we have a diff funct for Aus vs UK
  chisq_result <- chisq.test(
    matrix[,col_no],
    p = normalisation_matrix[,col_no],
    rescale.p = TRUE
  )

  cbind({data.frame(t(chisq_result$observed)) |>
      rename_with(.fn = ~ paste0(.x, "_observed"))},
      {data.frame(t(chisq_result$expected)) |>
          rename_with(.fn = ~ paste0(.x, "_expected"))},
      broom::tidy(chisq_result)) |>
    mutate(effect_size = get_cramers_v(mymatrix = matrix[,col_no],
                                       chi = chisq_result$statistic)) |>
    select(method,
           starts_with("broadsheet"),
           starts_with("tabloid"),
           everything()) |>
    t()
}

# get_chisq_tabloid_broadsheet(obes_4chisq_matrix_sumhits, 1, type = "wc")

get_chisq_aus_uk <- function(matrix, row_no, type) {
  normalisation_matrix <- if (type == "wc") {
    generate_wc(metadata)
  } else {
    generate_article_count(metadata)
  }
  # needing to subset the matrix with [row_no,]
  # is why we have a diff funct for tab vs broad
  chisq_result <- chisq.test(
    matrix[row_no,],
    # this takes word count into consideration,
    # so need dif funct for article_count
    p = normalisation_matrix[row_no,],
    rescale.p = TRUE
  )

  cbind({data.frame(t(chisq_result$observed)) |>
      rename_with(.fn = ~ paste0(.x, "_observed"))},
      {data.frame(chisq_result$expected) |>
          rename_with(.fn = ~ paste0(.x, "_expected"))},
      broom::tidy(chisq_result)) |>
    mutate(effect_size = get_cramers_v(mymatrix = matrix[row_no,],
                                       chi = chisq_result$statistic)) |>
    select(method,
           starts_with("AUS"),
           starts_with("UK"),
           everything()) |>
    t()
}

ttest_broad_vs_tabl <- function(freq_table,
                                corpus_label,
                                variable) {
  broadsheet <- freq_table |>
    filter(corpus == corpus_label & source_type == "broadsheet") |>
    pull(variable)
  tabloid <- freq_table |>
    filter(corpus == corpus_label & source_type == "tabloid") |>
    pull(variable)
  report::report(t.test(broadsheet, tabloid))
}

mydistribution <- approximate(nresample = 10000,
                              parallel = "multicore",
                              ncpus = 8)

wmw_test_broad_vs_tabl <- function(
    freq_table = frequency_table,
    corpus_label = "AUS",
    dataset = "cond",
    myformula = formula(cond_freq ~ source_type),
    dist = mydistribution) {
  df <- freq_table |>
    filter(corpus == corpus_label) |>
    select(source_type, paste0(dataset, "_freq")) |>
    mutate(source_type =as.factor(source_type))
  wilcox_test(myformula,
              df,
              distribution = dist,
              conf.int = TRUE )
}

wmw_test_aus_vs_uk <- function(
    freq_table = frequency_table,
    source_label = "broadsheet",
    dataset = "cond",
    myformula = formula(cond_freq ~ corpus),
    dist = mydistribution) {
  df <- freq_table |>
    filter(source_type == source_label) |>
    select(corpus, paste0(dataset, "_freq")) |>
    mutate(corpus =as.factor(corpus))
  wilcox_test(myformula,
              df,
              distribution = dist,
              conf.int = TRUE )
}

fp_test_broad_vs_tabl <- function(
    freq_table = frequency_table,
    corpus_label = "AUS",
    dataset = "cond",
    myformula = formula(cond_freq ~ source_type),
    dist = mydistribution) {
  df <- freq_table |>
    filter(corpus == corpus_label) |>
    select(source_type, paste0(dataset, "_freq")) |>
    mutate(source_type =as.factor(source_type))
  coin::oneway_test(myformula,
                    df,
                    distribution = dist,
                    conf.int = TRUE )
}

fp_test_aus_vs_uk <- function(
    freq_table = frequency_table,
    source_label = "broadsheet",
    dataset = "cond",
    myformula = formula(cond_freq ~ corpus),
    dist = mydistribution) {
  df <- freq_table |>
    filter(source_type == source_label) |>
    select(corpus, paste0(dataset, "_freq")) |>
    mutate(corpus =as.factor(corpus))
  coin::oneway_test(myformula,
                    df,
                    distribution = dist,
                    conf.int = TRUE )
}

fp_test_wc <- function(
    wc1 = au_broadsheet_wordcounts,
    wc2 = au_tabloid_wordcounts,
    label1 = "broadsheet",
    label2 = "tabloid",
    dist = mydistribution) {
  df <-
    rbind(data.frame(wc = wc1,
                     label = label1),
          data.frame(wc = wc2,
                     label = label2))|>
    mutate(label = as.factor(label))

  coin::oneway_test(formula(wc ~ label),
                    df,
                    distribution = dist,
                    conf.int = TRUE )
}

wmw_test_wc <- function(
    wc1 = au_broadsheet_wordcounts,
    wc2 = au_tabloid_wordcounts,
    label1 = "broadsheet",
    label2 = "tabloid",
    dist = mydistribution) {
  df <-
    rbind(data.frame(wc = wc1,
                     label = label1),
          data.frame(wc = wc2,
                     label = label2))|>
    mutate(label = as.factor(label))

  coin::wilcox_test(formula(wc ~ label),
                    df,
                    distribution = dist,
                    conf.int = TRUE )
}

ttest_aus_vs_uk <- function(freq_table,
                            source_type_label,
                            variable) {
  aus <- freq_table |>
    filter(corpus == "AUS" & source_type == source_type_label) |>
    pull(variable)
  uk <- freq_table |>
    filter(corpus == "UK" & source_type == source_type_label) |>
    pull(variable)
  report::report(t.test(aus, uk))
}

ttest_broad_vs_tabl_non0 <- function(freq_table,
                                     corpus_label,
                                     variable) {
  broadsheet <- freq_table |>
    filter(corpus == corpus_label & source_type == "broadsheet") |>
    pull(variable)
  tabloid <- freq_table |>
    filter(corpus == corpus_label & source_type == "tabloid") |>
    pull(variable)
  broadsheet <- broadsheet[broadsheet != 0]
  tabloid <- tabloid[tabloid != 0]
  report::report(t.test(broadsheet, tabloid))
}

ttest_aus_vs_uk_non0 <- function(freq_table,
                                 source_type_label,
                                 variable) {
  aus <- freq_table |>
    filter(corpus == "AUS" & source_type == source_type_label) |>
    pull(variable)
  uk <- freq_table |>
    filter(corpus == "UK" & source_type == source_type_label) |>
    pull(variable)
  aus <- aus[aus != 0]
  uk <- uk[uk != 0]
  report::report(t.test(aus, uk))
}

histogram_pairwise <- function(
    # for word counts
  wc1 = au_broadsheet_wordcounts,
  wc2 = au_tabloid_wordcounts,
  label1 = "broadsheet",
  label2 = "tabloid") {
  rbind(
    data.frame(wc = wc1,
               corpus = label1),
    data.frame(wc = wc2,
               corpus = label2)
  ) |>
    ggplot(aes(x = wc, fill = corpus)) +
    geom_histogram(bins = 1000) +
    labs(x = "Word count, CQP web",
         y = "Number of articles")

}
