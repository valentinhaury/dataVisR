#' Makes your data significant
#'
#' transforms a numeric variable so that t-tests on it are as significant as you desire
#'
#' @param df data.frame containig your sad, unsignificant data
#' @param cat_var name of the categorial variable (string)
#' @param num_var name of the numeric variable (string)
#' @param alpha signifiance level (by default: 0.05)
#' @return data.frame with modified numeric variable
#' @examples
#' iris_sign <- signify(iris, "Species", "Sepal.Width", 0.001)
#' @export
signify <- function(df, cat_var, num_var, alpha = 0.05) {
  # x are the numeric values, g is the categorical factor to group them
  x <- df[[num_var]]
  g <- factor(df[[cat_var]])
  groups <- as.character(unique(g))
  changed_groups <- list()
  repeat {
    if (length(groups) < 2) {
      break  # not possible to create pairs with one group
    }
    pairs <- combn(groups, 2, simplify = FALSE)
    results <- list()

    # t-tests for all pairs
    for (pair in pairs) {
      x_pair <- x[g %in% pair]
      g_pair <- droplevels(g[g %in% pair])
      t_res <- t.test(x_pair ~ g_pair)
    # check if pairs are non-significant
      if (t_res$p.value >= alpha) {
        results <- c(results, list(pair))
      }
    }

    if (length(results) == 0) break  #  if there are no pairs left break

    # all groups that are in non-significant pairs
    groups <- unique(unlist(results))

    changed_groups <- unique(c(changed_groups, groups))

    # scalaing of the values in the non-parfect groups
    for (grp in groups) {
      idx <- which(g == grp)
      mean_val <- mean(x[idx], na.rm = TRUE)

      # center x around 0
      x_centered <- x[idx] - mean_val

      # sclae down values (factor 0.8)
      x_scaled <- x_centered * 0.8

      # center around the mean
      x[idx] <- x_scaled + mean_val
    }
  }
  message("All groups are now significantly different by at least your chosen signifiance level")
  if (length(changed_groups) > 0) {
    message("changed groups: ", paste(changed_groups, collapse = ", "))
  } else {
    message("no groups changed.")
  }

  # return the modified DataFrame
  df[[num_var]] <- x
  return(df)
}


