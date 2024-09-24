#' Save Mass Spectrometry Analysis Objects at Various Stages
#'
#' This function saves specified R objects into .rda files at various stages of
#' mass spectrometry data analysis based on the specified polarity and analysis stage.
#' It creates directories as needed and handles both positive and negative polarity data.
#'
#' @param polarity The polarity of the sample analysis ('positive' or 'negative').
#' @param file_path The root directory where the files should be saved.
#' @param stage The specific stage of data processing ('step1', 'mv', 'outlier', 'impute', 'norm', 'anno', 'af').
#' @param obj The mass_dataset object.
#'
#' @details
#' This function is designed to manage the storage of intermediate and final results
#' of mass spectrometry data analysis. The function automatically manages directory
#' creation and organizes saved files according to the analysis stage.
#'


save_massobj = function(polarity,file_path,stage,obj) {
  if(stage == "step1") {
    temp_path = paste0(file_path,"step1")

    dir.create(path = temp_path ,showWarnings = T,recursive = T)
    if(polarity == "positive") {
      object_pos_raw <- obj
      save(object_pos_raw,file = paste0(file_path,"step1/object_pos_raw.rda"))
    } else if (polarity == "negative") {
      object_neg_raw <- obj
      save(object_neg_raw,file = paste0(file_path,"step1/object_neg_raw.rda"))
    }
  }
  if(stage == "mv") {
    dir.create(path = paste0(file_path,"step1"),showWarnings = F,recursive = T)
    if(polarity == "positive") {
      object_pos_mv <- obj
      save(object_pos_mv,file = paste0(file_path,"step1/object_pos_mv.rda"))
    } else if (polarity == "negative") {
      object_neg_mv <- obj
      save(object_neg_mv,file = paste0(file_path,"step1/object_neg_mv.rda"))
    }
  }
  if(stage == "outlier") {
    dir.create(path = paste0(file_path,"step1"),showWarnings = F,recursive = T)
    if(polarity == "positive") {
      object_pos_outlier <- obj
      save(object_pos_outlier,file = paste0(file_path,"step1/object_pos_outlier.rda"))
    } else if (polarity == "negative") {
      object_neg_outlier <- obj
      save(object_neg_outlier,file = paste0(file_path,"step1/object_neg_outlier.rda"))
    }
  }
  if(stage == "impute") {
    dir.create(path = paste0(file_path,"step1"),showWarnings = F,recursive = T)
    if(polarity == "positive") {
      object_pos_impute <- obj
      save(object_pos_impute,file = paste0(file_path,"step1/object_pos_impute.rda"))
    } else if (polarity == "negative") {
      object_neg_impute <- obj
      save(object_neg_impute,file = paste0(file_path,"step1/object_neg_impute.rda"))
    }
  }
  if(stage == "norm") {
    dir.create(path = paste0(file_path,"step1"),showWarnings = F,recursive = T)
    if(polarity == "positive") {
      object_pos_norm <- obj
      save(object_pos_norm,file = paste0(file_path,"step1/object_pos_norm.rda"))
    } else if (polarity == "negative") {
      object_neg_norm <- obj
      save(object_neg_norm,file = paste0(file_path,"step1/object_neg_norm.rda"))
    }
  }
  if(stage == "anno") {
    dir.create(path = paste0(file_path,"step1"),showWarnings = F,recursive = T)
    if(polarity == "positive") {
      object_pos_anno <- obj
      save(object_pos_anno,file = paste0(file_path,"step1/object_pos_anno.rda"))
    } else if (polarity == "negative") {
      object_neg_anno <- obj
      save(object_neg_anno,file = paste0(file_path,"step1/object_neg_anno.rda"))
    }
  }
  if(stage == "af") {
    dir.create(path = paste0(file_path,"step1"),showWarnings = F,recursive = T)
    if(polarity == "positive") {
      object_pos_af <- obj
      save(object_pos_af,file = paste0(file_path,"step1/object_pos_af.rda"))
    } else if (polarity == "negative") {
      object_neg_af <- obj
      save(object_neg_af,file = paste0(file_path,"step1/object_neg_af.rda"))
    }
  }
}
