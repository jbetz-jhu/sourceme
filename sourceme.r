# categorical.summary - tabulate frequency/percentage, and cumulative 
# frequency/percenteage.
categorical_summary <-
  function(x){
    my.table <- data.frame(table(x, useNA = 'ifany'))
    colnames(my.table) <- c(deparse(substitute(x)), "Frequency")
    my.table$Percent <- with(my.table, 100*Frequency/sum(Frequency))
    my.table$`Cumulative Frequency` <- cumsum(my.table$Frequency)
    my.table$`Cumulative Percent` <- cumsum(my.table$Percent)
    
    return(my.table)
  }

# continuous.summary - plot boxplot/violin plot, eCDF, and histogram if there
# are enough (minimum.n) non-missing values (those not in exclude.values). If
# there are too few, summarize using categorical.summary.
continuous_summary <-
  function(x,
           exclude.values=NA,
           minimum.n = 10) {
    if(!NA %in% exclude.values) exclude.values <- c(exclude.values, NA)
    non.missing <- subset(x, !x %in% exclude.values)
    var.name <- deparse(substitute(x))
    
    if(length(non.missing) > minimum.n){
      p1 <- 
        ggplot(data = data.frame(x = non.missing),
               aes(y = x, x = 1)) +
        geom_violin() +
        geom_boxplot(fill = NA) +
        geom_rug() +
        ylab(var.name) +
        theme_bw() +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())
      
      p2 <- 
        ggplot(data = data.frame(x = non.missing),
               aes(x = x)) +
        stat_ecdf(geom = "step") +
        geom_rug() +
        xlab(var.name) +
        ylab("Cumulative Proportion") +
        theme_bw()
      
      p3 <-
        ggplot(data = data.frame(x = non.missing),
               aes(x = x)) +
        geom_histogram(bins = nclass.FD(non.missing)) +
        geom_rug() +
        xlab(var.name) +
        theme_bw()
      
      exclude.counts <-
        sapply(exclude.values, function(y) sum(x %in% y, na.rm = TRUE))
      
      plot.label <-
        paste0(var.name, ": N=", length(non.missing), "; ",
               paste(paste0("N[", exclude.values, "]=", exclude.counts, ";"),
                     collapse = " "))
      
      return(grid.arrange(p1, p2, p3, nrow = 1,
                          top = plot.label))
    } else {
      my.table <- categorical.summary(x)
      colnames(my.table)[1] <- var.name
      kable(x = my.table,
            digits = c(0, 0, 2, 0, 2))
    }
  }




### format_number ##############################################################
# Format a number to a given number of decimals (digits).
format_number <- function(x, digits = 1) {
  result <- sprintf(fmt = paste0("%1.", digits, "f"), x)
  nonzero_zeros <-
    (x > 0) & sapply(X = suppressWarnings(as.numeric(result)),
                     function(x) identical(x, 0))
  
  if(sum(nonzero_zeros) > 0){
    result[which(nonzero_zeros)] <-
      paste0("< ",
             sprintf(fmt = paste0("%1.", max(digits, 0), "f"),
                     10^(-digits)))
  }
  result
}




### proportion #################################################################
# Takes a numerator (n) and denominator (d), produces a string: "n/d (p%)" where
# p is the percentage (n/d) x 100 to a specified number of decimals.
proportion <-
  function(n, d, digits = 1, output = c("n", "p", "nd", "np", "ndp")[5]){
    pct <- format_number(x = 100*(n/d), digits = digits)
    
    switch(
      output,
      "n" = n,
      "p" = paste0(pct, "%"),
      "nd" = paste0(n, "/", d),
      "np" = paste0(n, " (", pct, "%)"),
      "ndp" = paste0(n, "/", d, " (", pct, "%)"),
      stop(paste("Invalid value for `output`:", output))
    )
  }




### n_d_percent ################################################################
# Alias for proportion with output == "ndp" for compatibility
n_d_percent <-
  function(n, d, digits = 1) {
    proportion(n = n, d = d, digits = digits, output = "ndp")
  }




### create_table ###############################################################
create_table <-
  function(
    formula,
    data,
    proportion_output = c("n", "p", "nd", "np", "ndp")[5],
    numeric_output = c("Mean (SD)", "Median [IQR]", "Range"),
    digits = 1
  ) {
    
    # Create any terms necessary
    formula_terms <-
      terms(x = formula, data = data)
    
    vars <- attr(x = formula_terms, which = "variables")
    
    rhs_terms <- attr(x = formula_terms, which = "term.labels")
    lhs_terms <-
      paste(all.vars(expr = update(formula, . ~ 0)), collapse = ":")
    
    interactions <-
      c(lhs_terms, rhs_terms)[grep(pattern = ":", x = c(lhs_terms, rhs_terms))]
    
    interaction_vars <-
      unlist(strsplit(x = interactions, split = ":"))
    
    # Convert non-numeric to factors, with NA as a level
    # otherwise a:b is always NA if is.na(a)|is.na(b)
    data <-
      data %>% 
      dplyr::mutate(
        across(
          .cols = where(function(x) !is.numeric(x)) | all_of(interaction_vars),
          .fns = function(x) factor(x = x, exclude = NULL)
        )
      )
    
    terms_string <-
      paste0("data.frame(", 
             paste(c(lhs_terms, rhs_terms), collapse = ","),
             ", check.names = FALSE)", collapse = "")
    
    data <-
      with(get_all_vars(formula = formula, data = data),
           eval(parse(text = terms_string)))
    
    
    data[,lhs_terms] <- as.character(data[,lhs_terms])
    data[which(data[,lhs_terms] == ""), lhs_terms] <- "''"
    
    rhs_terms <-
      setdiff(x = colnames(data), lhs_terms)
    
    # Get numeric & non-numeric terms
    rhs_non_numeric <-
      intersect(
        rhs_terms,
        data %>% 
          dplyr::select(
            where(function(x) !is.numeric(x))
          ) %>% 
          names
      )
    
    rhs_numeric <-
      intersect(
        rhs_terms,
        data %>% 
          dplyr::select(
            where(is.numeric)
          ) %>% 
          names
      )
    
    data <-
      data %>% 
      dplyr::mutate(
        across(
          .cols = all_of(rhs_non_numeric),
          .fns = as.character)
      )
    
    # Get group sizes - Add to column names
    group_sizes <-
      data %>% 
      dplyr::select(
        all_of(x = lhs_terms)
      ) %>% 
      dplyr::mutate(
        across(
          .cols = everything(),
          .fns = as.character
        )
      ) %>% 
      tidyr::pivot_longer(
        cols = all_of(x = lhs_terms),
        names_to = "Variable",
        values_to = ".group"
      ) %>% 
      dplyr::mutate(
        .group = replace_na(data = .group, replace = "NA")
      ) %>% 
      dplyr::group_by(
        .group
      ) %>% 
      dplyr::count(
        name = ".group_size"
      ) %>% 
      dplyr::ungroup()
    
    if(length(rhs_non_numeric) > 0){
      
      aggregate_summary <-
        data %>% 
        dplyr::select(
          all_of(rhs_non_numeric)
        ) %>% 
        tidyr::pivot_longer(
          cols = all_of(x = rhs_non_numeric),
          names_to = ".variable",
          values_to = ".value"
        ) %>%
        dplyr::mutate(
          .summary =
            switch(
              proportion_output,
              "n" = "N",
              "p" = "%",
              "nd" = "N",
              "np" = "N (%)",
              "ndp" = "N (%)"
            )
        ) %>% 
        dplyr::group_by(.variable, .value, .summary) %>% 
        dplyr::count(
          name = ".all"
        ) %>% 
        dplyr::group_by(
          .variable
        ) %>% 
        dplyr::mutate(
          .all =
            proportion(
              n = .all,
              d = sum(.all),
              output = proportion_output
            )
        )
      
      
      grouped_summary <-
        data %>% 
        dplyr::select(
          all_of(x = c(rhs_non_numeric, lhs_terms))
        ) %>% 
        tidyr::pivot_longer(
          cols = all_of(x = rhs_non_numeric),
          names_to = ".variable",
          values_to = ".value"
        ) %>%
        dplyr::mutate(
          .summary =
            switch(
              proportion_output,
              "n" = "N",
              "p" = "%",
              "nd" = "N",
              "np" = "N (%)",
              "ndp" = "N (%)"
            )
        ) %>% 
        dplyr::group_by(
          across(.cols = all_of(x = c(".variable", ".value", lhs_terms)))
        ) %>% 
        dplyr::count(
          name = ".frequency",
        ) %>% 
        dplyr::group_by(
          across(.cols = all_of(x = c(".variable", lhs_terms)))
        ) %>% 
        dplyr::mutate(
          .frequency =
            proportion(
              n = .frequency,
              d = sum(.frequency),
              output = proportion_output
            )
        ) %>% 
        dplyr::ungroup() %>% 
        tidyr::pivot_wider(
          names_from = lhs_terms,
          values_from = ".frequency"
        ) %>% 
        tidyr::pivot_longer(
          cols = !all_of(x = c(".variable", ".value")),
          names_to = ".group",
          values_to = ".frequency"
        )
      
      # Fill in empty cells
      grouped_summary <-
        dplyr::full_join(
          x = grouped_summary,
          y = group_sizes,
          by = ".group"
        ) %>% 
        dplyr::mutate(
          .frequency =
            case_when(
              is.na(.frequency) ~ 
                proportion(
                  n = 0,
                  d = .group_size,
                  output = proportion_output
                ),
              !is.na(.frequency) ~ .frequency
            ),
          .group = paste0(.group, " (N=", .group_size, ")"),
          .group_size = NULL
        ) %>% 
        tidyr::pivot_wider(
          names_from = .group,
          values_from = .frequency
        ) %>% 
        dplyr::select(
          all_of(
            x = c(".variable", ".value")
          ), 
          sort(
            tidyselect::peek_vars()
          )
        )
      
      # Combine aggregate & grouped summaries
      non_numeric_summary <-
        dplyr::full_join(
          x = aggregate_summary,
          y = grouped_summary,
          by = c(".variable", ".value")
        )
      
      names(non_numeric_summary)[which(names(non_numeric_summary) == ".all")] <-
        paste0("All (N=", nrow(data), ")")
    } else {
      non_numeric_summary <- NULL
    }
    
    
    
    if(length(rhs_numeric) > 0){
      
      numeric_data <-
        data %>% 
        dplyr::select(
          all_of(x = c(lhs_terms, rhs_numeric))
        ) %>% 
        tidyr::pivot_longer(
          cols = all_of(x = rhs_numeric),
          names_to = ".variable",
          values_to = ".value"
        )
      
      
      aggregate_summary <-
        dplyr::bind_rows(
          numeric_data %>% 
            dplyr::select(-all_of(x = lhs_terms)) %>% 
            dplyr::group_by(.variable) %>%
            dplyr::summarize(
              .observed = sum(!is.na(.value)),
              .total = n()
            ) %>%
            dplyr::mutate(
              .value = "Completeness",
              .summary = 
                switch(
                  proportion_output,
                  "n" = "N",
                  "p" = "%",
                  "nd" = "N",
                  "np" = "N (%)",
                  "ndp" = "N (%)"
                ),
              .complete = .observed == .total,
              .all = 
                proportion(
                  n = .observed,
                  d = .total,
                  output = proportion_output
                )
            ) %>% 
            dplyr::filter(
              !.complete
            ) %>% 
            dplyr::select(
              -all_of(x = c(".observed", ".total", ".complete"))
            ),
          
          if("Mean (SD)" %in% numeric_output){
            numeric_data %>% 
              dplyr::select(-all_of(x = lhs_terms)) %>% 
              dplyr::group_by(.variable) %>% 
              dplyr::summarize(
                .summary = "Mean (SD)",
                .all = 
                  paste0(
                    format_number(x = mean(.value, na.rm = TRUE), digits = digits),
                    " (",
                    format_number(x = sd(.value, na.rm = TRUE), digits = digits),
                    ")")
              )
            
          } else{NULL},
          
          if("Median [IQR]" %in% numeric_output){
            numeric_data %>% 
              dplyr::select(-all_of(x = lhs_terms)) %>% 
              dplyr::group_by(.variable) %>%
              dplyr::summarize(
                .summary = "Median [IQR]",
                .all = 
                  paste0(
                    format_number(
                      x = median(.value, na.rm = TRUE),
                      digits = digits
                    ),
                    " [",
                    format_number(
                      x = quantile(.value, p = 0.25, na.rm = TRUE),
                      digits = digits
                    ), ", ",
                    format_number(
                      x = quantile(.value, p = 0.75, na.rm = TRUE),
                      digits = digits
                    ), "]")
              )
          } else{NULL},
          
          if("Range" %in% numeric_output){
            numeric_data %>% 
              dplyr::select(-all_of(x = lhs_terms)) %>% 
              dplyr::group_by(.variable) %>% 
              dplyr::summarize(
                .summary = "Range",
                .all = 
                  paste0(
                    "[",
                    format_number(
                      x = suppressWarnings(min(.value, na.rm = TRUE)),
                      digits = digits
                    ), ", ",
                    format_number(
                      x = suppressWarnings(max(.value, na.rm = TRUE)),
                      digits = digits
                    ), "]"
                  )
              )
          } else{NULL}
        ) %>% 
        dplyr::arrange(
          .variable,
          .summary
        )
      
      
      
      grouped_summary <-
        dplyr::bind_rows(
          numeric_data %>% 
            dplyr::group_by(
              across(.cols = all_of(x = c(".variable", lhs_terms)))
            ) %>% 
            dplyr::summarize(
              .observed = sum(!is.na(.value)),
              .total = n()
            ) %>% 
            dplyr::ungroup() %>% 
            dplyr::mutate(
              .value = "Completeness",
              .summary = 
                switch(
                  proportion_output,
                  "n" = "N",
                  "p" = "%",
                  "nd" = "N",
                  "np" = "N (%)",
                  "ndp" = "N (%)"
                ),
              .complete = .observed == .total,
              .completion = 
                proportion(
                  n = .observed,
                  d = .total,
                  output = proportion_output
                )
            ) %>% 
            dplyr::select(
              -all_of(x = c(".observed", ".total", ".complete"))
            ) %>% 
            tidyr::pivot_wider(
              names_from = lhs_terms,
              values_from = .completion
            ),
          
          if("Mean (SD)" %in% numeric_output){
            numeric_data %>% 
              dplyr::group_by(
                across(.cols = all_of(x = c(".variable", lhs_terms)))
              ) %>% 
              dplyr::summarize(
                .summary = "Mean (SD)",
                .meansd = 
                  paste0(
                    format_number(
                      x = mean(.value, na.rm = TRUE),
                      digits = digits
                    ), " (", 
                    format_number(
                      x = sd(.value, na.rm = TRUE),
                      digits = digits
                    ), ")"
                  )
              ) %>% 
              dplyr::ungroup() %>% 
              tidyr::pivot_wider(
                names_from = lhs_terms,
                values_from = .meansd
              )
            
          } else{NULL},
          
          if("Median [IQR]" %in% numeric_output){
            numeric_data %>% 
              dplyr::group_by(
                across(.cols = all_of(x = c(".variable", lhs_terms)))
              ) %>% 
              dplyr::summarize(
                .summary = "Median [IQR]",
                .medianiqr = 
                  paste0(
                    format_number(
                      x = median(.value, na.rm = TRUE),
                      digits = digits
                    ), " [", 
                    format_number(
                      x = quantile(.value, p = 0.25, na.rm = TRUE),
                      digits = digits
                    ), ", ",
                    format_number(
                      x = quantile(.value, p = 0.75, na.rm = TRUE),
                      digits = digits
                    ), "]"
                  )
              ) %>% 
              dplyr::ungroup() %>% 
              tidyr::pivot_wider(
                names_from = lhs_terms,
                values_from = .medianiqr
              )
          } else{NULL},
          
          if("Range" %in% numeric_output){
            numeric_data %>% 
              dplyr::group_by(
                across(.cols = all_of(x = c(".variable", lhs_terms)))
              ) %>%
              dplyr::summarize(
                .summary = "Range",
                .range = 
                  paste0(
                    "[",
                    format_number(
                      x = suppressWarnings(min(.value, na.rm = TRUE)),
                      digits = digits
                    ), ", ",
                    format_number(
                      x = suppressWarnings(max(.value, na.rm = TRUE)),
                      digits = digits
                    ), "]"
                  )
              ) %>% 
              dplyr::ungroup() %>% 
              tidyr::pivot_wider(
                names_from = lhs_terms,
                values_from = .range
              )
          } else{NULL}
        ) %>% 
        dplyr::arrange(
          .variable,
          .summary
        )
      
      grouped_summary <-
        dplyr::full_join(
          x = grouped_summary %>% 
            tidyr::pivot_longer(
              cols = !all_of(x = c(".variable", ".value", ".summary")),
              names_to = ".group",
              values_to = ".statistic"
            ) ,
          y = group_sizes,
          by = ".group"
        ) %>% 
        dplyr::mutate(
          .group = paste0(.group, " (N=", .group_size, ")"),
          .group_size = NULL
        ) %>% 
        tidyr::pivot_wider(
          names_from = .group,
          values_from = .statistic
        ) %>% 
        dplyr::select(
          all_of(
            x = c(".variable", ".value")
          ), 
          sort(
            tidyselect::peek_vars()
          )
        )
      
      
      # Combine aggregate & grouped summaries
      numeric_summary <-
        dplyr::left_join(
          x = aggregate_summary,
          y = grouped_summary,
          by = c(".variable", ".value", ".summary")
        )
      
      names(numeric_summary)[which(names(numeric_summary) == ".all")] <-
        paste0("All (N=", nrow(data), ")")
    } else{
      numeric_summary <- NULL
    }
    
    
    
    # Construct final table
    dplyr::bind_rows(
      numeric_summary,
      non_numeric_summary
    ) %>% 
      dplyr::rename(
        Variable = .variable,
        Value = .value,
        Summary = .summary
      ) %>% 
      dplyr::mutate(
        Variable  = 
          factor(
            x = Variable ,
            levels = rhs_terms
          )
      ) %>% 
      dplyr::arrange(
        Variable, Summary, Value
      ) %>% 
      dplyr::mutate(
        across(.fns = as.character)
      )
  }
