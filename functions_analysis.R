#########
# Functions for data analysis and testing
# Fabian Springer, 2024
########

require(lmerTest)
require(progress)

# Define which categories will be considered as first/second labels.
# lev_2_categories are considered as baseline in the testing functions
lev_1_categories <- c("male", "1","N1","M1","L1","high", "multinodular", "Inflamed", "present", "Tumor", "viral_HCC", "ALD/ASH_HCC", "HBV_HCC","yes","responder","iCCA","CRLM","Fuso+","Prev+","Trep+","Campy+","Bacteroideae+")
lev_2_categories <- c("all","Adj. non-tumor_CCC","Adj. non-tumor_CRLM","Adj. non-tumor_HCC","Adj. non-tumor","EarlyFib","LateFib","0","Healthy","Normal","Bact-")


f_run_linear_models_parallel <- function(
  dset_name = "all", mat1, mat2, meta, random_effect_variable = "randomEffFac",paired_wilcox_by = NULL,
  threshold_for_prev = -3,prevalence_threshold = FALSE,
  n_cores_max = 10,compute_CI = FALSE,cont_or_cat_vec = NULL,custom_lmer_formula = NULL) {
  #* Accepts two matrices (mat1, mat2) and a meta data frame. Runs linear (mixed) models in parallel for each combination of rows in mat1 and mat2.
  #* Function is considered to be functional on cluster environments and is parallelized using the parallel package.  
  require(parallel) # For parallelization
  require(pbapply)
  # Initialization and checks
  stopifnot(all(colnames(mat1) == colnames(mat2)))
  stopifnot(random_effect_variable %in% colnames(meta))
  stopifnot(is.matrix(mat1) & is.matrix(mat2))
  if(!is.null(paired_wilcox_by)) {
    stopifnot(paired_wilcox_by %in% colnames(meta))
  }
  #if no cont_or_cat vector is given, assume binary features in mat1
  if(is.null(cont_or_cat_vec)){
    cont_or_cat_vec <- rep("categorical",nrow(mat1))
  }
  stopifnot("cont_or_cat vector has less entries than rownumbers in mat1" = length(cont_or_cat_vec)==nrow(mat1))
  
  # Create task list
  tasks <- expand.grid(i = seq_len(nrow(mat1)), j = seq_len(nrow(mat2)))

  num_cores <- detectCores()
  print(paste("Number of cores available: ", num_cores))
  if(n_cores_max < num_cores-2){
    n_cores_to_use <- n_cores_max
  }else{
    n_cores_to_use <- num_cores-2
  }
  print(paste("Creating cluster with: ", n_cores_to_use))
  cl <- makeCluster(n_cores_to_use)
  
  # Export variables and load libraries to the cluster
  # Export variables and load libraries to the cluster
  clusterExport(
    cl = cl, varlist = c(
      "mat1", "mat2", "random_effect_variable", "paired_wilcox_by", "threshold_for_prev", "prevalence_threshold",
      "f_single_run_lm", "tasks", "f_lm", "f_lmer", "f_lm_cont", "f_lmer_cont", "f_wilcox","f_kruskal_wallis","f_lmer_anova",
      "compute_CI", "meta", "cont_or_cat_vec", "lev_1_categories","lev_2_categories","custom_lmer_formula"
    ),
    envir = environment()
  )
  clusterEvalQ(cl=cl, library(lmerTest))
  #message(colnames(meta))
  # Run tasks in parallel and track progress
  res_list <- pblapply(cl = cl, X = seq_len(nrow(tasks)), FUN = function(idx) {
    f_single_run_lm(
      tasks[idx, "i"],
      tasks[idx, "j"],
      mat1, mat2, meta=meta, random_effect_variable, #model_method,
      threshold_for_prev = threshold_for_prev,
      prevalence_threshold = prevalence_threshold,
      paired_wilcox_by = paired_wilcox_by,
      compute_CI = compute_CI,
      custom_lmer_formula = custom_lmer_formula,
      cont_or_cat_vec = cont_or_cat_vec
    )
  })
  
  # Stop the cluster
  on.exit(stopCluster(cl))
  
  # Aggregate results
  lmem_res_df <- lapply(res_list, function(x) as.data.frame((x), stringsAsFactors = FALSE)) %>%
    bind_rows() %>% 
    as_tibble()
  
  # convert selected columns to numeric
  cols_to_convert <- c(
    "effect_size", "lower95CI", "upper95CI", "p_value", "t_value", 
    "p.val_wilcox","gFC","L2FC_mean",
    "p.val_aov","p.val_kruskal",
    "N_Group1", "N_Group2","N_Pairs","N_Samples", "Prev_Group1", "Prev_Group2"
  )
  lmem_res_df <-
    lmem_res_df %>%
      add_column(
        test_type = "linear (mixed) model",        
        dset_name = dset_name
      ) %>%
      mutate(across(
        .cols = all_of(cols_to_convert[cols_to_convert %in% colnames(lmem_res_df)]),
        .fns = ~ as.numeric(.)
      )) %>% 
    arrange(p_value) %>%
    relocate(feat1)    
  
  return(lmem_res_df)
}

# # i <- 1
# # j <- 2
# cont_or_cat_vec <- rep("categorical",nrow(mat1))
# for(i in seq(1,nrow(mat1))){
#   for(j in seq(1,nrow(mat2))){
#     message("i:",i," j:",j)
#     tmp <- f_single_run_lm(i,j,mat1,mat2,meta,random_effect_variable,paired_wilcox_by,cont_or_cat_vec,custom_lmer_formula = custom_lmer_formula)
#   }
# }

f_single_run_lm <- function(i, j, mat1, mat2, meta, random_effect_variable,paired_wilcox_by, cont_or_cat_vec, 
threshold_for_prev = -3, prevalence_threshold = FALSE, compute_CI = FALSE,custom_lmer_formula = NULL) {
  #* This function is called by f_run_linear_models_parallel with a specific combination of rows in matrix1 and matrix2.
  #* The function performs a prevalence filtering (if selected) and calls the correct linear (mixed) model function (for categorical or cintinuous features)
  
  feat1 <- rownames(mat1)[i]
  feat2 <- rownames(mat2)[j]
  feature_type <- cont_or_cat_vec[i]
  x <- mat1[i, ]
  y <- mat2[j, ]

  idx <- which(!(is.na(x)) & !(is.na(y)))
  if (length(idx) == 0) { # if no non-NA values are present, return NULL
    return(NULL)
  }
  x <- x[idx]
  y <- y[idx]

  # Check prevalence if selected
  if (prevalence_threshold != FALSE) {
    if (sum(y > threshold_for_prev) / length(y) < prevalence_threshold) {
      return(NULL)
    }
  }

  if (length(unique(x)) < 2) {
    return(NULL) # Returning NULL if condition is met
  }

  # Check whether lm or lmems should be run
  
  if (length(unique(meta[names(y), ][[random_effect_variable]])) > 1) {
    model_method <- "lmer"
    # message("Running linear-mixed effects models with:\n", random_effect_variable, "\nas random effect")
  } else {
    model_method <- "lm"
    # message("Running simple linear models")
  }

  #* Run continuous lmems or lms ----
  if (feature_type == "continuous") {
    if (model_method == "lmer") {
      if (!is.null(custom_lmer_formula)) {
        formula <- as.formula(custom_lmer_formula)
      } else {
        formula <- as.formula(paste0("y~x + (1|", random_effect_variable, ")"))
      }
      tmp_df <- f_lmer_cont(x = x, y = y, meta = meta, formula = formula, feat_name_x = feat1, feat_name_y = feat2)
    } else if (model_method == "lm") {
      if(!is.null(custom_lmer_formula)){
        formula <- as.formula(custom_lmer_formula)
      } else {
        formula <- as.formula("y~x")
      }
      tmp_df <- f_lm_cont(x = x, y = y, meta = meta, formula = formula, feat_name_x = feat1, feat_name_y = feat2)
    }
    tmp_df_list <- list(tmp_df) #to be in agreement with categorical features
  } else if (feature_type == "categorical") {
    
    # Define formula for linear mixed models
    if (model_method == "lmer") {
      if (!is.null(custom_lmer_formula)) {
        formula <- as.formula(custom_lmer_formula)
      } else {
        formula <- as.formula(paste0("y~x + (1|", random_effect_variable, ")"))
      }
    } else if(!is.null(custom_lmer_formula)){
      formula <- as.formula(custom_lmer_formula)
    } else {
      formula <- as.formula("y~x")
    }

    #* Run categorical lmems or lms with any one vs all combination ----    
    all_x_levels <- unique(x)
    
    # Perform kruskal wallis test if more than two x-levels are present
    if (length(all_x_levels) > 2) {
      kruskal_res_df <- f_kruskal_wallis(x = x, y = y, feat_name_x = feat1, feat_name_y = feat2)

      # perform ANOVA using custom formula
      if (model_method == "lmer") {
        anova_res_df <- f_lmer_anova(x = x, y = y, meta = meta, formula = formula, feat_name_x = feat1, feat_name_y = feat2)
      } else {
        aov_res <- summary(aov(y ~ x))
        anova_res_df <- c(
          feat1 = feat1,
          feat2 = feat2,
          p.val_aov = as.character(aov_res[[1]][["Pr(>F)"]][1])
        )
      }
    }
    # Temporary check: Stop if more than 10 unique features in categorical x variable
    stopifnot("More than 20 unique features in categorical x -> recheck"=length(all_x_levels) < 20)
    comparisons <- combn(unique(x), 2, simplify = FALSE)
    tmp_df_list <- list()
    #for (c in seq(1, length(all_x_levels))) {
    c <- 1
    for (c in seq(1, length(comparisons))) {
      x_binary_idx <- which(x %in% comparisons[[c]])
      x_binary <- x[x_binary_idx]
      y_binary <- as.numeric(y[x_binary_idx])

      if (model_method == "lmer") {
        tmp_df <- f_lmer(
          x = x_binary,
          y = y_binary,
          meta = meta,
          formula = formula,
          feat_name_x = feat1,
          feat_name_y = feat2,
          threshold_for_prev = threshold_for_prev,
          compute_CI = compute_CI
        )
        if(!is.null(paired_wilcox_by)){
          # compute paired wilcoxon test
          tmp_w_df <- f_wilcox(
            x = x_binary,
            y = y_binary,
            meta = meta,
            paired_wilcox_by = paired_wilcox_by,
            feat_name_x = feat1,
            feat_name_y = feat2,
            threshold_for_prev = threshold_for_prev,
            formula = NULL
          )
        #join wilcoxon test to LM
        tmp_df <- c(tmp_df,tmp_w_df[c("gFC","p.val_wilcox")])
        }

        if(length(all_x_levels) > 2){
          tmp_df <- c(tmp_df,anova_res_df[c("p.val_aov")])
        }

      } else if (model_method == "lm") {
        #formula <- as.formula("y~x")

        tmp_df <- f_lm(
          x = x_binary,
          y = y_binary,
          formula = formula,
          meta = meta,
          feat_name_x = feat1,
          feat_name_y = feat2,
          threshold_for_prev = threshold_for_prev,
          compute_CI = compute_CI
        )

        # compute wilcoxon test if only two groups are present
        tmp_w_df <- f_wilcox(
          x = x_binary,
          y = y_binary,
          meta = meta,
          feat_name_x = feat1,
          feat_name_y = feat2,
          paired_wilcox_by = paired_wilcox_by,
          threshold_for_prev = threshold_for_prev,
          formula = NULL
        )

        if (is.null(paired_wilcox_by)) {
          # join wilcoxon test to LM
          tmp_df <- c(tmp_df, tmp_w_df[c("gFC", "p.val_wilcox")])
        }else{
          tmp_df <- c(tmp_df, tmp_w_df[c("L2FC_mean", "p.val_wilcox","N_Pairs")])
        }
        # add anova and kruskal pvalues
        if(length(all_x_levels) > 2){
          tmp_df <- c(tmp_df,anova_res_df[c("p.val_aov")],kruskal_res_df[c("p.val_kruskal")])
        }

      }

      tmp_df_list[[c]] <- c(
        "feat1_group" = feat1, # add feat1_group (e.g. Child_Pugh_Score) to have grouping of categorical variables for p-value correction
        tmp_df
      )
    }
  }
  return(do.call(rbind, tmp_df_list))
}

f_lm <- function(x,y,formula,meta,feat_name_x,feat_name_y,threshold_for_prev = -3,compute_CI = FALSE){
  #* A wrapper for the lm function. Takes a vector x (categorical) and y (continuous) and runs a lm(y~x).
  # If x has more than 2 levels, the function will run a one-vs-all comparison for each level of x.

  dat_df <- as.data.frame(cbind(x,y))
  df_merged <- merge(meta,dat_df,by="row.names",all.x=F)
  df_merged$y <- as.numeric(df_merged$y)
  # Define which level of x to take as reference
  x_levels <- sort(as.character(na.omit((unique(dat_df$x)))))  
  if(any(x_levels %in% lev_1_categories)){
    lev1 <- x_levels[x_levels %in% lev_1_categories]
    lev2 <- x_levels[!(x_levels %in% lev_1_categories)]
  }else if(any(x_levels %in% lev_2_categories)){    
    lev2 <- x_levels[x_levels %in% lev_2_categories]
    lev1 <- x_levels[!(x_levels %in% lev_2_categories)]
  }else{
    lev1 <- x_levels[1]
    lev2 <- x_levels[2]
  }  
  if(any(c(length(lev1) == 0, length(lev2) == 0))){ # If all x-levels are in the same category, just keep the default order
    lev1 <- x_levels[1]
    lev2 <- x_levels[2]
  }

  df_merged$x <- factor(df_merged$x,levels = c(lev2,lev1))
  N_group1 <- nrow(subset(df_merged,x == lev2))
  N_group2 <- nrow(subset(df_merged,x == lev1))

  # Compute prevalence
  Prev_group1 <- sum(subset(df_merged,x == lev2)$y > threshold_for_prev) / nrow(subset(df_merged,x == lev2))
  Prev_group2 <- sum(subset(df_merged,x == lev1)$y > threshold_for_prev) / nrow(subset(df_merged,x == lev1))

  tryCatch(
    {
      res <- lm(formula,data = df_merged)
      coef <- coefficients(summary(res))
      p_value <- coef[nrow(coef),4]
      effect_size <- coef[nrow(coef),1]
      t_value <- coef[nrow(coef),3]
      if (isTRUE(compute_CI)) {
        suppressMessages(CI <- confint(res))
        lower95CI <- CI[nrow(CI), 1]
        upper95CI <- CI[nrow(CI), 2]
      } else {
        lower95CI <- NA
        upper95CI <- NA
      }      
      return(c(feat1 = paste0(feat_name_x,"_",lev1),
               feat2 = feat_name_y,
               Group1 = lev2,
               Group2 = lev1,
               effect_size = effect_size,
               lower95CI = lower95CI,
               upper95CI = upper95CI,
               p_value = p_value,
               t_value = t_value,
               N_Group1 = N_group1,
               N_Group2 = N_group2,
               Prev_Group1 = Prev_group1,
               Prev_Group2 = Prev_group2,
               formula = paste(deparse(formula, width.cutoff = 500), collapse="")))
    },
    error=function(e){
      return(c(feat1 = paste0(feat_name_x,"_",lev1),
               feat2 = feat_name_y,
               Group1 = lev2,
               Group2 = lev1,
               effect_size = NA,
               lower95CI = NA,
               upper95CI = NA,
               p_value = NA,
               t_value = NA,
               N_Group1 = N_group1,
               N_Group2 = N_group2,
               Prev_Group1 = Prev_group1,
               Prev_Group2 = Prev_group2,
               formula = paste(deparse(formula, width.cutoff = 500), collapse="")))
    }
  )
  
}

f_kruskal_wallis <- function(x,y,feat_name_x,feat_name_y){
  #* A wrapper for the kruskal.test function. Takes a vector x (categorical) and y (continuous) and runs a kruskal.test(y~x).
  
  tryCatch(
    {
      res <- kruskal.test(x = y,g=x)
      
      return(c(feat1 = feat_name_x,
               feat2 = feat_name_y,
               #effect_size = 0,
               p.val_kruskal = as.numeric(res$p.value)))
    },
    error=function(e){
      return(c(feat1 = feat_name_x,
               feat2 = feat_name_y,
               #effect_size = NA,
               p.val_kruskal = NA))
    }
  )
}

f_lmer_anova <- function(x,y,meta,formula,feat_name_x,feat_name_y){
  #* Compute anova using a custom formula ----

  dat_df <- as.data.frame(cbind(x,y))
  df_merged <- merge(meta,dat_df,by="row.names",all.x=F)
  df_merged$y <- as.numeric(df_merged$y)
  tryCatch(
    {
      res <- lmerTest::lmer(formula = formula,data = df_merged)
      aov <- anova(res)
      p_value <- aov[nrow(aov),6]
      effect_size <- 0
      return(c(feat1 = feat_name_x,
               feat2 = feat_name_y,
               #effect_size = effect_size,
               p.val_aov = p_value))
    },
    error=function(e){
      return(c(feat1 = feat_name_x,
               feat2 = feat_name_y,
               #effect_size = NA,
               p.val_aov = NA))
    }
  )
}

f_wilcox <- function(x,y,meta,feat_name_x,feat_name_y,paired_wilcox_by = NULL,threshold_for_prev = -3,formula=NULL){
  #* compute wilcoxon test analogous to simple linear models ----
  
  if(is.null(formula)){
    formula <- as.formula("y~x")
  }else {
     formula <- as.formula(formula)
  }
  if(!is.null(paired_wilcox_by)){
    paired = TRUE
    stopifnot(paired_wilcox_by %in% colnames(meta))
  }else{
    paired = FALSE
  }
  
  dat_df <- as.data.frame(cbind(x,y))
  df_merged <- merge(meta,dat_df,by="row.names",all.x=F)
  df_merged$y <- as.numeric(df_merged$y)
  # Define which level of x to take as reference
  x_levels <- sort(as.character(na.omit((unique(dat_df$x)))))  
  if(any(x_levels %in% lev_1_categories)){
    lev1 <- x_levels[x_levels %in% lev_1_categories]
    lev2 <- x_levels[!(x_levels %in% lev_1_categories)]
  }else if(any(x_levels %in% lev_2_categories)){    
    lev2 <- x_levels[x_levels %in% lev_2_categories]
    lev1 <- x_levels[!(x_levels %in% lev_2_categories)]
  }else{
    lev1 <- x_levels[1]
    lev2 <- x_levels[2]
  }  
  if(any(c(length(lev1) == 0, length(lev2) == 0))){ # If all x-levels are in the same category, just keep the default order
    lev1 <- x_levels[1]
    lev2 <- x_levels[2]
  }

  df_merged$x <- factor(df_merged$x,levels = c(lev2,lev1))

  # if paired testing is performed, remove incomplete cases
  if(!is.null(paired_wilcox_by)){
    #complete_pairs <- df_merged %>% group_by(!!as.symbol(paired_wilcox_by)) %>% summarise(n = n()) %>% filter(n == 2) %>% pull(!!as.symbol(paired_wilcox_by)) %>% sort()
    complete_pairs <- unique(df_merged[[paired_wilcox_by]][
      ave(df_merged[[paired_wilcox_by]], df_merged[[paired_wilcox_by]], FUN = length) == 2
    ])
    complete_pairs <- sort(complete_pairs)
    
    # filter and sort
    # df_merged <- df_merged %>%
    #   filter(!!as.symbol(paired_wilcox_by) %in% complete_pairs) %>%
    #   arrange(!!as.symbol(paired_wilcox_by), x)    
    df_merged <- df_merged[df_merged[[paired_wilcox_by]] %in% complete_pairs, ]
    df_merged <- df_merged[order(df_merged[[paired_wilcox_by]], df_merged$x), ]
    
    N_Pairs <- length(complete_pairs)
        
  }
  
  N_group1 <- nrow(subset(df_merged,x == lev2))
  N_group2 <- nrow(subset(df_merged,x == lev1))
  # Compute prevalence
  Prev_group1 <- sum(subset(df_merged,x == lev2)$y > threshold_for_prev) / nrow(subset(df_merged,x == lev2))
  Prev_group2 <- sum(subset(df_merged,x == lev1)$y > threshold_for_prev) / nrow(subset(df_merged,x == lev1))

  #Compute Log2FC: Assumes y contains log10-transformed values
  y1 <- subset(df_merged, x == lev1)$y
  y2 <- subset(df_merged,x == lev2)$y
    
  tryCatch(
    {
      res <- rstatix::wilcox_test(formula, paired = paired,data = df_merged)      
      p_value <- as.numeric(res[1,7])
      
      # L10FC <- median(y1) - median(y2)
      # L2FC <- L10FC * log2(10)      
      
      #implement gFC calculation as in the SIAMCAT paper
      # assumes log-transformed data (- attention: no check implemented)
      probs.fc <- seq(.1, .9, .05)
      q.p <- quantile(y1, probs = probs.fc)
      q.n <- quantile(y2, probs = probs.fc)
      gFC <- sum(q.p - q.n) / length(q.p)

      if (is.null(paired_wilcox_by)) {
        res_vec <- c(
          feat1 = paste0(feat_name_x, "_", lev1),
          feat2 = feat_name_y,
          Group1 = lev2,
          Group2 = lev1,
          p.val_wilcox = p_value,
          gFC = gFC,
          N_Group1 = N_group1,
          N_Group2 = N_group2,
          Prev_Group1 = Prev_group1,
          Prev_Group2 = Prev_group2
        )
      }else {
        # For paired wilcoxon test, return number of complete cases
        #compute mean L2FC
        L10FC  <- y1 - y2
        meanL2FC  <- mean(L10FC * log2(10)) #convert to log2

         res_vec <- c(
          feat1 = paste0(feat_name_x, "_", lev1),
          feat2 = feat_name_y,
          Group1 = lev2,
          Group2 = lev1,
          p.val_wilcox = p_value,
          L2FC_mean = meanL2FC,          
          N_Pairs = N_Pairs,
          Prev_Group1 = Prev_group1,
          Prev_Group2 = Prev_group2
        )
      }

      return(res_vec)
    },
    error=function(e){
      return(c(feat1 = paste0(feat_name_x,"_",lev1),
               feat2 = feat_name_y,
               Group1 = lev2,
               Group2 = lev1,               
               p.val_wilcox = NA,
               gFC = NA,              
               N_Group1 = N_group1,
               N_Group2 = N_group2,
               Prev_Group1 = Prev_group1,
               Prev_Group2 = Prev_group2))
    }
  )

}

f_lm_cont <- function(x, y, meta,formula,feat_name_x, feat_name_y) {
  #* A wrapper for the lm function - for continuous variables. Takes a vector x and y and runs lm(y~x)
  dat_df <- as.data.frame(cbind(x, y))
  df_merged <- merge(meta, dat_df, by = "row.names", all.x = F)
  df_merged$x <- as.numeric(df_merged$x)
  df_merged$y <- as.numeric(df_merged$y)
  N_Samples <- nrow(df_merged)
  tryCatch(
    {
      res <- lm(formula, data = df_merged)
      coef <- coefficients(summary(res))      
      p_value <- coef[nrow(coef), 4]
      effect_size <- coef[nrow(coef), 1]
      t_value <- coef[nrow(coef), 3]
      
      return(c(
        feat1 = feat_name_x,
        feat2 = feat_name_y,
        effect_size = effect_size,
        p_value = p_value,
        t_value = t_value,
        N_Samples = N_Samples,
        formula = paste(deparse(formula, width.cutoff = 500), collapse="")
      ))
    },
    error = function(e) {
      return(c(
        feat1 = feat_name_x,
        feat2 = feat_name_y,
        effect_size = NA,
        p_value = NA,
        t_value = NA,
        N_Samples = N_Samples,
        formula = paste(deparse(formula, width.cutoff = 500), collapse="")
      ))
    }
  )
}

f_lmer <- function(x,y,meta,formula,feat_name_x,feat_name_y,threshold_for_prev = -3, compute_CI = FALSE){
  #* A wrapper for the lmer function of the lmerTest package. Takes a vector x (categorical) and y (continuous) and a formula object.
  # Runs lmerTest::lmer with the given formula
  # If x has more than 2 levels, the function will run a one-vs-all comparison for each level of x.

  dat_df <- as.data.frame(cbind(x,y))
  df_merged <- merge(meta,dat_df,by="row.names",all.x=F)
  df_merged$y <- as.numeric(df_merged$y)
  N_Samples <- nrow(df_merged)
# Define which level of x to take as reference  x_levels <- sort(as.character(na.omit((unique(dat_df$x)))))
  x_levels <- sort(as.character(na.omit((unique(dat_df$x)))))  
  if(any(x_levels %in% lev_1_categories)){
    lev1 <- x_levels[x_levels %in% lev_1_categories]
    lev2 <- x_levels[!(x_levels %in% lev_1_categories)]
  }else if(any(x_levels %in% lev_2_categories)){    
    lev2 <- x_levels[x_levels %in% lev_2_categories]
    lev1 <- x_levels[!(x_levels %in% lev_2_categories)]
  } else{
    lev1 <- x_levels[1]
    lev2 <- x_levels[2]
  }
  if(any(c(length(lev1) == 0, length(lev2) == 0))){ # If all x-levels are in the same category, just keep the default order
    lev1 <- x_levels[1]
    lev2 <- x_levels[2]
  }
  df_merged$x <- factor(df_merged$x,levels = c(lev2,lev1))
  N_group1 <- nrow(subset(df_merged,x == lev2)) #mixup is on purpose
  N_group2 <- nrow(subset(df_merged,x == lev1))

  # Compute prevalence
  Prev_group1 <- sum(subset(df_merged,x == lev2)$y > threshold_for_prev) / nrow(subset(df_merged,x == lev2))
  Prev_group2 <- sum(subset(df_merged,x == lev1)$y > threshold_for_prev) / nrow(subset(df_merged,x == lev1))
  
  tryCatch(
    {
      res <- lmerTest::lmer(formula = formula,data = df_merged)
      coef <- coefficients(summary(res))
      p_value <- coef[nrow(coef),5]
      effect_size <- coef[nrow(coef),1]
      t_value <- coef[nrow(coef),4]

      if (isTRUE(compute_CI)) {
        suppressMessages(CI <- confint(res))
        lower95CI <- CI[nrow(CI), 1]
        upper95CI <- CI[nrow(CI), 2]
      } else {
        lower95CI <- NA
        upper95CI <- NA
      }      
      return(c(feat1 = paste0(feat_name_x,"_",lev1),
               feat2 = feat_name_y,
               Group1 = lev2, #the mixup is on purpuse 
               Group2 = lev1,
               effect_size = effect_size,
               upper95CI = upper95CI,
               lower95CI = lower95CI,
               p_value = p_value,
               t_value = t_value,
               N_Group1 = N_group1,
               N_Group2 = N_group2,
               Prev_Group1 = Prev_group1,
               Prev_Group2 = Prev_group2,
               formula = paste(deparse(formula, width.cutoff = 500), collapse="")
               ))
    },
    error=function(e){
      return(c(feat1 = paste0(feat_name_x,"_",lev1),
               feat2 = feat_name_y,
               Group1 = lev2,
               Group2 = lev1,
               effect_size = NA,
               upper95CI = NA,
               lower95CI = NA,
               p_value = NA,
               t_value = NA,
               N_Group1 = N_group1,
               N_Group2 = N_group2,
               Prev_Group1 = Prev_group1,
               Prev_Group2 = Prev_group2,
               formula = paste(deparse(formula, width.cutoff = 500), collapse="")
               ))
    }
  )
  
}

f_lmer_cont <- function(x, y, meta, formula, feat_name_x, feat_name_y) {
  #* A wrapper for the lmer function of the lmerTest package of two contunious variables.
  # Runs lmerTest::lmer with the given formula
  dat_df <- as.data.frame(cbind(x, y))
  df_merged <- merge(meta, dat_df, by = "row.names", all.x = F)
  df_merged$x <- as.numeric(df_merged$x)
  df_merged$y <- as.numeric(df_merged$y)
  N_Samples <- nrow(df_merged)
  tryCatch(
    {
      res <- lmerTest::lmer(formula = formula, data = df_merged)
      coef <- coefficients(summary(res))
      p_value <- coef[nrow(coef), 5]
      effect_size <- coef[nrow(coef), 1]
      t_value <- coef[nrow(coef), 4]
      return(c(
        feat1 = feat_name_x,
        feat2 = feat_name_y,
        effect_size = effect_size,
        p_value = p_value,
        t_value = t_value,
        N_Samples = N_Samples,
        formula = paste(deparse(formula, width.cutoff = 500), collapse="")
      ))
    },
    error = function(e) {
      return(c(
        feat1 = feat_name_x,
        feat2 = feat_name_y,
        effect_size = NA,
        p_value = NA,
        t_value = NA,
        N_Samples = N_Samples,
        formula = paste(deparse(formula, width.cutoff = 500), collapse="")
      ))
    }
  )
}

f_run_fisher_test_parallel <- function(
  #* Parallelizing function to perform Fisher's exact tests in parallel for each combination of rows in mat1 and mat2 
  dset_name = "all", mat1, mat2,
  threshold_for_prev = -3,prevalence_threshold = FALSE,
  n_cores_max = 10) {
  
  require(parallel) # For parallelization
  require(pbapply)
  # Initialization and checks
  stopifnot(all(colnames(mat1) == colnames(mat2)))    
  # Create task list
  tasks <- expand.grid(i = seq_len(nrow(mat1)), j = seq_len(nrow(mat2)))

  num_cores <- detectCores()
  print(paste("Number of cores available: ", num_cores))
  if(n_cores_max < num_cores-2){
    n_cores_to_use <- n_cores_max
  }else{
    n_cores_to_use <- num_cores-2
  }
  print(paste("Creating cluster with: ", n_cores_to_use))
  cl <- makeCluster(n_cores_to_use)
  
  # Export variables and load libraries to the cluster
  # Export variables and load libraries to the cluster
  clusterExport(cl=cl, varlist = c("mat1", "mat2","threshold_for_prev","prevalence_threshold","f_single_run_fisher_test","tasks"),envir=environment())
  clusterEvalQ(cl=cl, library(lmerTest))
  #message(colnames(meta))
  # Run tasks in parallel and track progress
  res_list <- pblapply(cl = cl, X = seq_len(nrow(tasks)), FUN = function(idx) {
    f_single_run_fisher_test(
      tasks[idx, "i"],
      tasks[idx, "j"],
      mat1, mat2,
      threshold_for_prev = threshold_for_prev,
      prevalence_threshold = prevalence_threshold
    )
  })
  
  # Stop the cluster
  stopCluster(cl)
  
  # Aggregation logic
  fisher_res_df <- do.call(rbind, res_list) %>% as.data.frame()
  fisher_res_df <-
    fisher_res_df %>%
    add_column(
      test_type = "Fisher_test",      
      dset_name = dset_name
    ) %>%
    mutate(      
      N_Group1 = as.numeric(N_Group1),
      N_Group2 = as.numeric(N_Group2),
      Prev_Group1 = as.numeric(Prev_Group1),
      Prev_Group2 = as.numeric(Prev_Group2)
    ) %>%
    arrange(p.val_fisher) %>%
    relocate(feat1) %>% 
    as_tibble()
  
  return(fisher_res_df)
}  

f_single_run_fisher_test <- function(i, j, mat1, mat2, threshold_for_prev,prevalence_threshold) {
  #* This function is called by f_run_fisher_test_parallel with a specific combination of rows in matrix1 and matrix2.
  feat1 <- rownames(mat1)[i]
  feat2 <- rownames(mat2)[j]
  x <- mat1[i, ]  # Binary variable
  y <- mat2[j, ]  # Continuous variable

  # remove NA samples for current clinical test
  idx <- which(!(is.na(x)) & !(is.na(y)))
  if(length(idx) == 0){ #if no non-NA values are present, return NULL
    return(NULL)
  }
  x <- x[idx]
  y <- y[idx]

  # Check prevalence if selected (based on all samples, not just the ones with an annotation for the current (clinical) feature)
  if(prevalence_threshold != FALSE){
    if(sum(y > threshold_for_prev) / length(y) < prevalence_threshold){
      message("Prevalence too low")
      return(NULL)      
    }
  }
  
  if (length(unique(x)) < 2) {
    return(NULL) # Returning NULL if condition is met
  }

  all_x_levels <- unique(x)
  tmp_df_list <- list()

  for (c in seq(1, length(all_x_levels))) {

    # if there are more than two x-levels, run one-vs-all comparisons for each level of x
    x_binary <- x
    x_binary[x != all_x_levels[c]] <- "all"

    if (length(all_x_levels) == 2) {
      # pretty stupid to manually reset x_binary within every iteration of the for loop
      # but for now most efficient
      x_binary <- x
      if (c > 1) {
        next
      } # break for loop after 1 iteration to not compute everything N times
    }
    group_levels <- rev(sort(unique(x_binary)))    
    if (group_levels[1] %in% lev_1_categories | group_levels[2] %in% lev_2_categories) { # Make sure to re-order groups for consistency with lmem result
      group_levels <- rev(group_levels)
    }

    # Fix order of testing for CRLM vs iCCA (to be consistent with LMEM) -> a bit hacky for now but works
    if("iCCA" %in% group_levels & "CRLM" %in% group_levels){
      group_levels <- c("iCCA","CRLM")
    }

    # Binarize y based on threshold
    y_binarized <- ifelse(y > threshold_for_prev, 1, 0)    
    # skip iteration if only one category of samples is present 
    if(length(unique(y_binarized)) < 2){
      next
    }
    # Compute Fisher's Exact Test
    contingency_table <- table(x_binary, y_binarized)
    contingency_table <- contingency_table[group_levels,]
    fisher_test_result <- fisher.test(contingency_table)
  
    # Calculate proportions for each group
    proportion_group1 <- sum(x_binary== group_levels[1] & y_binarized == 1) / sum(x_binary == group_levels[1])
    proportion_group2 <- sum(x_binary == group_levels[2] & y_binarized == 1) / sum(x_binary == group_levels[2])

    # Return a data frame with the results
    tmp_df <- data.frame(
      feat1_group = feat1, # add feat1_group (e.g. Child_Pugh_Score) to have grouping of categorical variables for p-value correction
      feat1 = paste0(feat1,"_",group_levels[2]),
      feat2 = feat2,
      Group1 = group_levels[1],
      Group2 = group_levels[2],
      p.val_fisher = fisher_test_result$p.value,
      odds_ratio = fisher_test_result$estimate,      
      N_Group1 = sum(x == group_levels[1]),
      N_Group2 = sum(x == group_levels[2]),
      Prev_Group1 = proportion_group1,
      Prev_Group2 = proportion_group2)    
    tmp_df_list[[c]] <- tmp_df      
  }  
  

  return(do.call(rbind, tmp_df_list))
}

f_run_spearman <- function(dset_name = "all",mat1,mat2,prevalence_threshold = FALSE,threshold_for_prev = -3){
  #* Accepts two matrices (mat1, mat2) and runs spearman correlations in parallel for each combination of rows in mat1 and mat2.
  stopifnot(all(colnames(mat1)==colnames(mat2)))
  pb <- progress_bar$new(total = nrow(mat1)*nrow(mat2))
  spearman_res_df <- tibble()
  for(i in seq(1,nrow(mat1))){
    for(j in seq(1,nrow(mat2))){
      feat1 <- rownames(mat1)[i]
      feat2 <- rownames(mat2)[j]
      x <- mat1[i,]
      y <- mat2[j, ]

      #keep only non-NAs
      idx <- as.numeric(which(!(is.na(x)) & !(is.na(y))))
      x <- as.numeric(x[idx])
      y <- as.numeric(y[idx])
      
      prevalence <- sum(y > threshold_for_prev) / length(y) 

      # Check prevalence if selected
      if (prevalence_threshold != FALSE) {
        if (prevalence < prevalence_threshold) {
          pb$tick()
          next
        }
      }
          
      N_samples <- length(x)
      #do the spearman correlation
      tmp_df <- f_spearman(x = x, y = y, feat_name_x = feat1, feat_name_y = feat2) #%>%             
      spearman_res_df <- bind_rows(spearman_res_df, c(tmp_df, "Prevalence" = as.numeric(prevalence)))      
      pb$tick()
    }
  }
  return(spearman_res_df %>% add_column(test_type = "spearman",
                                        dset_name = dset_name) %>% 
           mutate(effect_size = as.numeric(effect_size),
                  p_value = as.numeric(p_value)) %>% 
           arrange(p_value))
}

f_spearman <- function(x,y,feat_name_x,feat_name_y){
  #* Wrapper for base R spearman correlation of x and y
  tryCatch(
    {
      res <- cor.test(x = x,y=y,method = "spearman")
      return(c(feat1 = feat_name_x,
               feat2 = feat_name_y,
               effect_size = as.numeric(res$estimate),
               p_value = as.numeric(res$p.value),
               N_samples = length(x)))
    },
    error=function(e){
      return(c(feat1 = feat_name_x,
               feat2 = feat_name_y,
               effect_size = NA,
               p_value = NA,
               N_samples = length(x)))
    }
  )
}

#* Compute Alpha- and beta-diversities / Wilcoxon tests ----
f_pairwise_wilcoxon_tests <- function(df){
  # Takes dataframe with grouping variable and metric (e.g. richness/shannon diversity) and a value and runs pairwise wilcoxon tests
  stopifnot(c("group","value","metric") %in% colnames(df))
  res_df <- df %>%    
    group_by(metric) %>%
    nest() %>%    
    mutate(wilcox_results = map(
      data, ~ pairwise.wilcox.test(.x$value, .x$group, p.adjust.method = "fdr") %>%
        broom::tidy()
    )) %>%
    unnest(wilcox_results) %>%
    dplyr::select(-data) %>%
    dplyr::rename(p.val_adj_wilcox = p.value, Group1 = group1, Group2 = group2) %>% # since pairwise.wilcox.test adjusts p-values inside the function
    ungroup() 

    return(res_df)
}

f_compute_distance_metrics <- function(df,relAB_mat,threshold_for_prevalence = 0,prevalence_threshold = 0){
  require(ecole)
  stopifnot(c("Sample_ID","group") %in% colnames(df))

  # subset rel abundance matrix to keep only samples in current selection
  relAB_sel <- relAB_mat[,df$Sample_ID]
  # Keep only species with 5% prevalence in current selection
  dim(relAB_sel)
  prev <- rowSums(relAB_sel > threshold_for_prevalence) / ncol(relAB_sel)
  
  relAB_sel <- relAB_sel[prev >= prevalence_threshold,]

  summary(rowSums(relAB_sel)>0)
  summary(colSums(relAB_sel)>0)
  # In two cases, the species are not present in any sample - remove them
  relAB_sel <- relAB_sel[,colSums(relAB_sel)>0]
  dim(relAB_sel)
  #compute Bray-Curtis and Euclidean distances
  bray_curtis_dist <- vegan::vegdist(t(relAB_sel), method = "bray")
  euc_dist <- vegan::vegdist(t(log10(relAB_sel+1e-5)), method = "euclidean")

  # make sure to order distance matrix and metadata in the same way
  sample_ids <- rownames(as.matrix(bray_curtis_dist))  
  df <- df %>% filter(Sample_ID %in% sample_ids) %>% mutate(rowN = Sample_ID) %>% column_to_rownames("rowN")
  df <- df[sample_ids, ]
  # Run PERMANOVA
  set.seed(1)
  permanova_bray_res <- ecole::permanova_pairwise(
    bray_curtis_dist,
    as.character(df$group),
    permutations = 999,
    padj = "fdr"
  ) %>% 
  mutate(distance_metric = "Bray-Curtis")
  permanova_euc_res <- ecole::permanova_pairwise(
    euc_dist,
    as.character(df$group),
    permutations = 999,
    padj = "fdr"
  ) %>% 
  mutate(distance_metric = "Euclidean")

  permanova_combined_res_df <- bind_rows(permanova_bray_res, permanova_euc_res) %>%
    separate(pairs, into = c("Group1", "Group2"), sep = " vs ") %>%
    dplyr::select(Group1, Group2, pval, p.adj, distance_metric)
  
  return(permanova_combined_res_df %>% as_tibble())

}

f_save_to_excel <- function(res_list,out_name,column_groups=list(group1 = 5:9, group2 = 10:12, group3 = 13:16)){
  require(openxlsx)
  # color code columns:
  # 5-9: lm
  # 10-12: wilcox
  # 13-16: fisher
  
  # Define your colors
  colors <-  c(group1 = "#ADD8E6", group2 = "#98FB98", group3 = "#F08080", group4 = "#FFFFE0",group5 = "#FFB6C1")

  
  # Define your column groups
  #column_groups <- list(group1 = 5:9, group2 = 10:12, group3 = 13:16)
  
  # Create a new workbook
  wb <- createWorkbook()
  
  # Loop through each element in the list
  for(i in seq_along(res_list)) {
    sheetName <- substr(names(res_list)[i], start = 1, stop = 30)
    sheetName <- str_replace_all(sheetName,pattern = "\\/",replacement = "-")
    # Add a worksheet to the workbook for the current dataframe
    addWorksheet(wb, sheetName = sheetName)
    
      # Write the dataframe to the worksheet    
    writeData(wb, sheet = sheetName, x = res_list[[i]])
    
    # Add color to specific columns
    for(group_name in names(column_groups)) {
      # Create the style
      sty <- createStyle(fgFill = colors[group_name])
      
      # Get the column indices
      col_indices <- column_groups[[group_name]]
      
      # Add the style to each column in the group, excluding the header (row 1)
      for (col_index in col_indices) {
        addStyle(wb, sheet = sheetName, style = sty, rows = 1:(nrow(res_list[[i]]) + 1), cols = col_index)
      }
    }
    
    
    # Set column width to 'auto' for each column in the worksheet
    setColWidths(wb, sheet = sheetName, cols = 1:ncol(res_list[[i]]),widths = "auto")
    # Add filters to the column headers
    addFilter(wb, sheet = sheetName, cols = 1:ncol(res_list[[i]]),rows = 1)
    
    
    
  }
  
  # Save the workbook
  saveWorkbook(wb, out_name, overwrite = TRUE)
}

