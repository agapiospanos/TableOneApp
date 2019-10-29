#' @param import.col.names (Character) the column names as specifed in the imported data.frame or excel file.
#' @param output.var.names (Character) the variable names as they will be displayed in the output. If not specified we will use the excel column names.
#' @param dichotomous (Character) a vector of character values that specifies which of the excel columns contain dichotomous data.
#' @param ordinal (Character) a vector of character values that specifies which of the excel columns contain ordinal data.
#' @param median.iqr (Character) a vector of character values that specifies for which continuous excel columns we should calculate median and iqr instead of mean and sd.
#' @param group.col.name (Character) the column name that specifies the group (control or treatment).
#' @param control.value (Character) the value that specifies the control group. (e.g. 0 or 'control').
#' @param treatment.value (Character) the value that specifies the treatment group (e.g. 1 or 'treatment').
#' @param data (data.frame) (optional) (default: NULL) the data in a data.frame format that will be loaded. If you do not provide data, a pop up window will be displayed asking for an excel file to load data from.
#' @param excel.path (Character) (optional) the path that the excel file can be read from. By default it is set to NULL so that a pop up window will ask for the path.
#' @param export.path (Character) (optional) the path that the Word Document will be exported to.
#' @param sheet (Character) (optional) (default: NULL) the sheet inside excel file that the data are stored. By default it gets the first one.
#' @param tableone.col.names (Character) (optional) a vector for the column names of the exported table. Default are: c('Variable', 'Treatment Group', 'Control Group', 'p-value', 'Mean Difference', 'OR', 'Test Stat.')
#' @param export.filename (Character) (optional) the name of the file that will be exported. Do not include the .docx extension. (default filename is TableOne.docx)
#' @param show.stats (Character) (optional) a vector of characters that specifies which statistics will be displayed in the produced table One. By default it displays the MD, OR and Test Stat. value as additional statistics except from p-value
#' @param export.word (Boolean) (optional) (default: TRUE) specify if you want to have a Word Document that contains the table one exported. Whether you export the table to Word or not, you will also get the results in a data.frame format after running the package.

tableone <- function(import.col.names, output.var.names, dichotomous = c(), ordinal = c(), median.iqr = c(), group.col.name, control.value, treatment.value, data = NULL, excel.path = NULL, export.path = NULL, sheet = NULL, tableone.col.names = NULL, export.filename = NULL, show.stats = NULL, export.word = TRUE) {
  
  # checking if the user has supplied the correct amount of names for all variables
  if (length(output.var.names) != length(import.col.names)) {
    stop("The length of the variables 'output.var.names' and 'import.col.names' is not equal. Please make sure you have entered the same number of items in these vectors.")
  }
  
  # if the user did not specify any output.var.names we will use the excel column names
  if (is.null(output.var.names) | length(output.var.names) == 0) {
    output.var.names <- import.col.names
  }
  
  # imported_data <- data
  
  # initializing final subset which includes only the desired columns from the excel.
  subset <- data.frame(matrix(nrow = nrow(data), ncol = length(output.var.names)))
  # setting column names to the final data data.frame
  names(subset) <- import.col.names
  
  # adding the desired data to subset data.frame
  for (i in 1:length(import.col.names)) {
    subset[i] <- data[import.col.names[i]]
  }
  
  # keeping only the selected columns in the imported data
  imported_data <- subset
  
  # initialize a grouping column
  group <- data.frame(matrix(nrow = nrow(data), ncol = 1))
  
  # get values for the column that groups
  group <- data[group.col.name]
  
  # checking if the user specified which statistics will be displayed
  if (is.null(show.stats)) {
    stats <- c("MD", "OR", "test-value")
  } else {
    stats <- show.stats
  }
  
  # calculating the number of columns according to the user's chosen stats. By default we have 4 columns + chosen stats.
  dataframe.ncols <- length(stats) + 4
  
  # initializing the data.frame that will be exported to Word
  table.to.export <- data.frame( matrix( ncol = dataframe.ncols, nrow = length(output.var.names) ) )
  
  # keeping treatment and control group values in a separate var
  treatment <- imported_data[which(group == treatment.value),]
  control <- imported_data[which(group == control.value),]
  
  # number of participants in the treatment group
  n.t <- nrow(treatment)
  
  # number of participants in the control group
  n.c <- nrow(control)
  
  # variable to check if continuity correction is applied to add note
  continuity_correction <- F
  
  # keep a vector of the vars for which we applied continuity correction
  continuity_correction_vars <- c()
  
  # checking if the user specified custom column names for the exported table
  if (is.null(tableone.col.names)) {
    tableone.col.names <- c('Variable', 'Treatment Group', 'Control Group', 'p-value')
    colname.ind <- 5
    if ("MD" %in% stats) {
      tableone.col.names[colname.ind] <- 'Mean Difference'
      colname.ind <- colname.ind + 1
    }
    if ("OR" %in% stats) {
      tableone.col.names[colname.ind] <- 'OR'
      colname.ind <- colname.ind + 1
    }
    if ("test-value" %in% stats) {
      tableone.col.names[colname.ind] <- 'Test Stat.'
    }
  } else {
    if (length(tableone.col.names) < 4 + dataframe.ncols) {
      stop(paste('you must provide a vector with', 4 + dataframe.ncols, 'column names for the argument tableone.col.names. Use " " inside the vector to keep empty column names'))
    } else {
      tableone.col.names[which(tableone.col.names == '' | is.na(tableone.col.names))] <- ' '
    }
  }
  
  # adding the number of participants in each group
  tableone.col.names[2] <- paste0(tableone.col.names[2], ' (n=', n.t, ')')
  tableone.col.names[3] <- paste0(tableone.col.names[3], ' (n=', n.c, ')')
  
  # adding the names in table that will be exported
  names(table.to.export) <- tableone.col.names
  
  # we must use a padding variable to compensate for the rows added by the ordinal data. The padding will only be used in the table.to.export variable
  # for each row of ordinal data we will increase the padding by 1
  padding <- 0
  
  # tabledata to keep all the results separately
  tabledata <- data.frame( matrix( ncol = 28, nrow = length(output.var.names) ) )

  # column names for tabledata
  names(tabledata) <- c('name.display', 'name.var', 'type', 't.percent', 't.mean', 't.sd', 't.median', 't.q1', 't.q3', 't.min', 't.max', 'c.percent', 'c.mean', 'c.sd', 'c.median', 'c.q1', 'c.q3', 'c.min', 'c.max', 'p.value', 'md', 'md.ci.low', 'md.ci.upper', 'or', 'or.ci.low', 'or.ci.high', 'test.stat', 'ordinal.levels')
  
  # generating the table that will be exported
  for (i in 1:length(import.col.names)) {
    
    tabledata[padding + i,2]  <- import.col.names[i]
    
    if (import.col.names[i] %in% dichotomous) {
      
      # get data as factors for treatment group
      t.factors <- table(treatment[i])
      t.f1name <- names(t.factors[1])
      t.f1count <- ifelse(is.na(t.factors[1]), 0, t.factors[1])
      t.f2name <- names(t.factors[2])
      t.f2count <- ifelse(is.na(t.factors[2]), 0, t.factors[2])
      # t.f2name <- names(t.factors[2]) TODO remove
      t.totalcount <- t.f1count + t.f2count
      # t.f1percent <- t.f1count / t.totalcount TODO remove
      t.f2percent <- t.f2count / t.totalcount
      
      # get data as factors for control group
      c.factors <- table(control[i])
      c.f1name <- names(c.factors[1])
      c.f1count <- ifelse(is.na(c.factors[1]), 0, c.factors[1])
      # c.f1name <- names(c.factors[1]) TODO remove
      c.f2name <- names(c.factors[2])
      c.f2count <- ifelse(is.na(c.factors[2]), 0, c.factors[2])
      # c.f2name <- names(c.factors[2]) TODO remove
      c.totalcount <- c.f1count + c.f2count
      # c.f1percent <- c.f1count / c.totalcount TODO remove
      c.f2percent <- c.f2count / c.totalcount
      
      # creating the table columns
      # column name
      # TODO IMPROVEMENT add the option to choose which factor to consider for every dichotomous var as the baseline.
      if (output.var.names[i] == '') {
        varname <- t.f2name # we get the 2nd factor's name because we use the 2nd factor as a baseline.
      } else {
        varname <- output.var.names[i]
      }
      
      # calculate and display OR
      t.events <- length(which(unlist(treatment[i]) == t.f2name)) # we use f2name because we have the 2nd factor as the baseline.
      t.no_events <- length(which(unlist(treatment[i]) == t.f1name))
      c.events <- length(which(unlist(control[i]) == c.f2name))
      c.no_events <- length(which(unlist(control[i]) == c.f1name))
      
      if (t.events == 0 | c.events == 0 | t.no_events == 0 | c.no_events == 0) {
        t.events = t.events + 0.5
        c.events = c.events + 0.5
        t.no_events = t.no_events + 0.5
        c.no_events = c.no_events + 0.5
        continuity_correction = T
        continuity_correction_vars <- cbind(continuity_correction_vars, varname)
      }
      
      or.value <- (c.no_events * t.events) / (t.no_events * c.events)
      var.log.or <- 1/c.no_events + 1/t.no_events + 1/t.events + 1/c.events
      
      logor.ci.lower <- log(or.value) - 1.96*sqrt(var.log.or)
      logor.ci.upper <- log(or.value) + 1.96*sqrt(var.log.or)
      
      testval <- round(log(or.value)/sqrt(var.log.or), digits = 3)
      
      pval <- round(2*(1 - pnorm(abs(testval))), digits = 3)
      if (pval < 0.001) pval <- '<0.001'
      
      table.to.export[padding + i, 1] <- paste(varname, '(%)')
      # treatment group value - percentage
      table.to.export[padding + i, 2] <- paste0(format(round(t.f2percent*100, digits = 2), nsmall = 2), '%') # we use f2percent because we have the 2nd factor as the baseline.
      # control group value - percentage
      table.to.export[padding + i, 3] <- paste0(format(round(c.f2percent*100, digits = 2), nsmall = 2), '%')
      # p-value
      table.to.export[padding + i, 4] <- format(pval, nsmall = 3)
      
      # get a start value for the column index as we dynamically show statistics
      col.ind = 5
      
      if ("MD" %in% stats) {
        # mean difference with 95 percent confidence interval
        table.to.export[padding + i,col.ind] <- ''
        col.ind <- col.ind + 1
      }
      if ("OR" %in% stats) {
        # odds ratio - 95 percent OR confidence interval
        table.to.export[padding + i,col.ind] <- paste0(format(round(or.value, digits = 2), nsmall = 2), ' [', format(round(exp(logor.ci.lower), digits = 2), nsmall = 2), ', ', format(round(exp(logor.ci.upper), digits = 2), nsmall = 2), ']')
        col.ind <- col.ind + 1
      }
      if ("test-value" %in% stats) {
        table.to.export[padding + i,col.ind] <- format(testval, nsmall = 3)
      }
      
      tabledata[padding + i,1]  <- varname
      tabledata[padding + i,3]  <- 'dichotomous'
      tabledata[padding + i,4]  <- format(round(t.f2percent*100, digits = 2), nsmall = 2)
      tabledata[padding + i,5]  <- ''
      tabledata[padding + i,6]  <- ''
      tabledata[padding + i,7]  <- ''
      tabledata[padding + i,8]  <- ''
      tabledata[padding + i,9]  <- ''
      tabledata[padding + i,10] <- ''
      tabledata[padding + i,11] <- ''
      tabledata[padding + i,12] <- format(round(c.f2percent*100, digits = 2), nsmall = 2)
      tabledata[padding + i,13] <- ''
      tabledata[padding + i,14] <- ''
      tabledata[padding + i,15] <- ''
      tabledata[padding + i,16] <- ''
      tabledata[padding + i,17] <- ''
      tabledata[padding + i,18] <- ''
      tabledata[padding + i,19] <- ''
      tabledata[padding + i,20] <- pval
      tabledata[padding + i,21] <- format(round(or.value, digits = 2), nsmall = 2)
      tabledata[padding + i,22] <- ''
      tabledata[padding + i,23] <- ''
      tabledata[padding + i,24] <- ''
      tabledata[padding + i,25] <- format(round(exp(logor.ci.lower), digits = 2), nsmall = 2)
      tabledata[padding + i,26] <- format(round(exp(logor.ci.upper), digits = 2), nsmall = 2)
      tabledata[padding + i,27] <- format(testval, nsmall = 3)
      tabledata[padding + i,28] <- ''
      
    } else if (import.col.names[i] %in% ordinal) {
      
      test.values <- chisq.test(unlist(imported_data[import.col.names[i]]), unlist(group))
      
      var.levels <- rownames(test.values$observed)
      
      c.count <- test.values$observed[,1]
      t.count <- test.values$observed[,2]
      
      # column name
      table.to.export[padding + i, 1] <- output.var.names[i]
      # treatment group value stays empty for the ordinal data
      table.to.export[padding + i, 2] <- ''
      # control group value stays empty as well
      table.to.export[padding + i, 3] <- ''
      
      pval <- format(round(test.values$p.value, digits = 3), nsmall = 3)
      if (pval < 0.001) pval <- '<0.001'
      
      # p-value
      table.to.export[padding + i, 4] <- pval
      
      # get a start value for the column index as we dynamically show statistics
      col.ind = 5
      
      if ("MD" %in% stats) {
        # mean difference with 95 percent confidence interval
        table.to.export[padding + i, col.ind] <- ''
        col.ind <- col.ind + 1
      }
      if ("OR" %in% stats) {
        # leaving empty the odds ratio column
        table.to.export[padding + i, col.ind] <- ''
        col.ind <- col.ind + 1
      }
      if ("test-value" %in% stats) {
        table.to.export[padding + i, col.ind] <- format(round(test.values$statistic, digits = 3), nsmall = 3)
      }
      
      tabledata[padding + i,1]  <- output.var.names[i]
      tabledata[padding + i,3]  <- 'ordinal'
      tabledata[padding + i,4]  <- ''
      tabledata[padding + i,5]  <- ''
      tabledata[padding + i,6]  <- ''
      tabledata[padding + i,7]  <- ''
      tabledata[padding + i,8]  <- ''
      tabledata[padding + i,9]  <- ''
      tabledata[padding + i,10] <- ''
      tabledata[padding + i,11] <- ''
      tabledata[padding + i,12] <- ''
      tabledata[padding + i,13] <- ''
      tabledata[padding + i,14] <- ''
      tabledata[padding + i,15] <- ''
      tabledata[padding + i,16] <- ''
      tabledata[padding + i,17] <- ''
      tabledata[padding + i,18] <- ''
      tabledata[padding + i,19] <- ''
      tabledata[padding + i,20] <- pval
      tabledata[padding + i,21] <- ''
      tabledata[padding + i,22] <- ''
      tabledata[padding + i,23] <- ''
      tabledata[padding + i,24] <- ''
      tabledata[padding + i,25] <- ''
      tabledata[padding + i,26] <- ''
      tabledata[padding + i,27] <- format(testval, nsmall = 3)
      tabledata[padding + i,28] <- length(var.levels)
      
      # we add a row for each factor of the ordinal data
      for (k in 1:length(var.levels)) {
        padding <- padding + 1
        # name for the factor
        table.to.export[padding + i, 1] <- paste0('---- level: ', var.levels[k])
        # treatment group value stays empty for the ordinal data
        table.to.export[padding + i, 2] <- paste0(format(round((t.count[k] / n.t) * 100, digits = 2), nsmall = 2), '%')
        # control group value stays empty as well
        table.to.export[padding + i, 3] <- paste0(format(round((c.count[k] / n.c) * 100, digits = 2), nsmall = 2), '%')
        # other columns will be empty
        table.to.export[padding + i, 4] <- ''
        table.to.export[padding + i, 5] <- ''
        table.to.export[padding + i, 6] <- ''
        table.to.export[padding + i, 7] <- ''
        
        tabledata[padding + i,1]  <- paste0('---- level: ', var.levels[k])
        tabledata[padding + i,3]  <- 'ordinal-level'
        tabledata[padding + i,4]  <- format(round((t.count[k] / n.t) * 100, digits = 2), nsmall = 2)
        tabledata[padding + i,5]  <- ''
        tabledata[padding + i,6]  <- ''
        tabledata[padding + i,7]  <- ''
        tabledata[padding + i,8]  <- ''
        tabledata[padding + i,9]  <- ''
        tabledata[padding + i,10] <- ''
        tabledata[padding + i,11] <- ''
        tabledata[padding + i,12] <- format(round((c.count[k] / n.c) * 100, digits = 2), nsmall = 2)
        tabledata[padding + i,13] <- ''
        tabledata[padding + i,14] <- ''
        tabledata[padding + i,15] <- ''
        tabledata[padding + i,16] <- ''
        tabledata[padding + i,17] <- ''
        tabledata[padding + i,18] <- ''
        tabledata[padding + i,19] <- ''
        tabledata[padding + i,20] <- ''
        tabledata[padding + i,21] <- ''
        tabledata[padding + i,22] <- ''
        tabledata[padding + i,23] <- ''
        tabledata[padding + i,24] <- ''
        tabledata[padding + i,25] <- ''
        tabledata[padding + i,26] <- ''
        tabledata[padding + i,27] <- ''
        tabledata[padding + i,28] <- ''
      }
      
    } else { # continuous data case
      
      wilcox.res <- wilcox.test(as.numeric(unlist(imported_data[import.col.names[i]]))~unlist(group), imported_data, alternative = "two.sided")
      t.test.res <- t.test(as.numeric(unlist(imported_data[import.col.names[i]]))~unlist(group), imported_data)
      
      # calculating quantiles and median for control and treatment group even if the user has not selected the median iqr for this variable so that we can export the data to tabledata var
      c.q25 <- quantile(as.numeric(unlist(control[i])), na.rm = T)[2]
      c.q75 <- quantile(as.numeric(unlist(control[i])), na.rm = T)[4]
      
      t.q25 <- quantile(as.numeric(unlist(treatment[i])), na.rm = T)[2]
      t.q75 <- quantile(as.numeric(unlist(treatment[i])), na.rm = T)[4]
      
      c.median <- median(as.numeric(unlist(control[i])), na.rm = T)
      t.median <- median(as.numeric(unlist(treatment[i])), na.rm = T)
      
      if (import.col.names[i] %in% median.iqr) {
        
        test.values <- wilcox.res
        
        # column name
        table.to.export[padding + i, 1] <- paste0(output.var.names[i], ' (median, IQR)')
        # treatment group value with standard deviation
        table.to.export[padding + i, 2] <- paste0(t.median, ' (', t.q25, ',', t.q75, ')')
        # control group value with standard deviation
        table.to.export[padding + i, 3] <- paste0(c.median, ' (', c.q25, ',', c.q75, ')')
        
        tabledata[padding + i,4]  <- ''
        tabledata[padding + i,5]  <- ''
        
        tabledata[padding + i,12] <- ''
        tabledata[padding + i,13] <- ''
        
        tabledata[padding + i,20] <- ''
        tabledata[padding + i,21] <- ''
        tabledata[padding + i,22] <- ''
        
      } else {
        
        test.values <- t.test.res
        
        # mean for control and treatment group
        c.mean <- test.values$estimate[1]
        t.mean <- test.values$estimate[2]
        mean.difference <- c.mean - t.mean
        
        # standard deviation for control and treament group
        c.sd <- round(sd(as.numeric(unlist(control[i])), na.rm = T), digits = 2)
        t.sd <- round(sd(as.numeric(unlist(treatment[i])), na.rm = T), digits = 2)
        
        # column name
        table.to.export[padding + i, 1] <- paste0(output.var.names[i], ' (mean, sd)')
        # treatment group value with standard deviation
        table.to.export[padding + i, 2] <- paste0(format(round(t.mean, digits = 2), nsmall = 2), '\U00B1', format(t.sd, nsmall = 2))
        # control group value with standard deviation
        table.to.export[padding + i, 3] <- paste0(format(round(c.mean, digits = 2), nsmall = 2), '\U00B1', format(c.sd, nsmall = 2))
        
        tabledata[padding + i,5]  <- format(round(t.mean, digits = 2), nsmall = 2)
        tabledata[padding + i,6]  <- t.sd
        
        tabledata[padding + i,13] <- format(round(c.mean, digits = 2), nsmall = 2)
        tabledata[padding + i,14] <- c.sd
        
        tabledata[padding + i,21] <- format(round(mean.difference, digits = 2), nsmall = 2)
        tabledata[padding + i,22] <- format(round(test.values$conf.int[1], digits = 2), nsmall = 2)
        tabledata[padding + i,23] <- format(round(test.values$conf.int[2], digits = 2), nsmall = 2)
        
      }
      
      # p-value
      pval <- round(test.values$p.value, digits = 3)
      if (pval < 0.001) pval <- '<0.001'
      
      # p-value
      table.to.export[padding + i, 4] <- format(pval, nsmall = 3)
      
      # get a start value for the column index as we dynamically show statistics
      col.ind = 5
      
      if ("MD" %in% stats) {
        if (import.col.names[i] %in% median.iqr) {
          table.to.export[padding + i, col.ind] <- ''
        } else {
          # mean difference with 95 percent confidence interval
          table.to.export[padding + i, col.ind] <- paste0(format(round(mean.difference, digits = 2), nsmall = 2), ' [', format(round(test.values$conf.int[1], digits = 2), nsmall = 2), ', ', format(round(test.values$conf.int[2], digits = 2), nsmall = 2), ']')
        }
        col.ind <- col.ind + 1
      }
      if ("OR" %in% stats) {
        # leaving empty the odds ratio column
        table.to.export[padding + i, col.ind] <- ''
        col.ind <- col.ind + 1
      }
      if ("test-value" %in% stats) {
        table.to.export[padding + i, col.ind] <- format(round(test.values$statistic, digits = 3), nsmall = 3)
      }
      
      tabledata[padding + i,1]  <- output.var.names[i]
      tabledata[padding + i,3]  <- 'continuous'
      tabledata[padding + i,4]  <- ''
      
      tabledata[padding + i,7]  <- t.median
      tabledata[padding + i,8]  <- t.q25
      tabledata[padding + i,9]  <- t.q75
      tabledata[padding + i,10] <- as.numeric(min(unlist(treatment[i])))
      tabledata[padding + i,11] <- as.numeric(max(unlist(treatment[i])))
      tabledata[padding + i,12] <- ''
      
      tabledata[padding + i,15] <- c.median
      tabledata[padding + i,16] <- c.q25
      tabledata[padding + i,17] <- c.q75
      tabledata[padding + i,18] <- as.numeric(min(unlist(control[i])))
      tabledata[padding + i,19] <- as.numeric(max(unlist(control[i])))
      
      tabledata[padding + i,20] <- pval
      
      tabledata[padding + i,24] <- ''
      tabledata[padding + i,25] <- ''
      tabledata[padding + i,26] <- ''
      tabledata[padding + i,27] <- format(round(test.values$statistic, digits = 3), nsmall = 3)
      tabledata[padding + i,28] <- ''
    }
  }
  
  if (export.word) {
    
    # initializing a blank docx document
    doc <- read_docx()
    
    # converting the data.frame to regulartable so that we can apply styling using a theme from flextable package
    output <- regulartable(table.to.export)
    
    # auto-adjust width for table columns
    output <- autofit(output, add_w = 0.2, add_h = 0)
    
    # applying the styling to the table
    output <- theme_zebra(output, odd_header = "#CFCFCF", odd_body = "#F8F8F8",
                          even_header = "transparent", even_body = "transparent")
    
    # adding vertical borders between columns
    output <- vline( output, border = fp_border(color = "gray80", width = 1), part = "all" )
    # remove right table border
    output <- vline_right(output, border = fp_border(width = 0), part = "all")
    # align text to center
    output <- align(output, align = "center", part = "all")
    # align first column to right
    output <- align(output, j = 1, align = "right", part = "body")
    # make header bold
    output <- bold(output, bold = TRUE, part = "header")
    # make first column bold
    output <- bold(output, j = 1, bold = TRUE, part = "body")
    
    # adding the table to the document body
    body_add_blocks(doc, blocks = block_list(output))
    
    # adding note for continuity correction if applied
    if (continuity_correction) body_add_par(doc, value = paste('Note: Continuity correction was applied for the variables: ', paste(continuity_correction_vars, collapse = ', ')), style = "centered")
    
    # make the orientation landscape
    body_end_section_landscape(doc)
    
    # finaly exporting the docx file
    print(doc, target = paste0(export.path, '/', export.filename, '.docx'))
    
    print('The file is exported successfully! You can find it in the following directory:')
    print(paste0(export.path, '/', export.filename, '.docx'))
  }
  
  print('Here is the table one in a data.frame format:')
  if (continuity_correction) print(paste('Note: Continuity correction was applied for the variables: ', paste(continuity_correction_vars, collapse = ', ')))
  print(table.to.export)
  
  output <- list()
  output$table <- table.to.export
  output$continuity_correction <- continuity_correction
  output$continuity_correction_vars <- continuity_correction_vars
  output$tabledata <- tabledata
  
  return(output)
}
