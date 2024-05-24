#import packages 
source("packages.R")

##### Fonction pour vérifier si une colonne de type character peut être convertie en integer ou factor #####

is_convertible_to_integer <- function(x) {
  all(is.na(x) | grepl("^-?\\d+$", x))  # Vérifie si toutes les valeurs sont des nombres entiers ou NA
}

##### Fonction qui crée une variable_label pour chaque variable concernée #####

# Cette fonction fusionne les bases identiques dont l'une des versions a ses modalités en num et l'autre en character
# Pour chaque variable numérique dont un label correspond, la variable correspondante se crée avec le nom "variable_label" 


merge_with_labels <- function(num_df, let_df) {
  # Identifier les colonnes concernées (type character et contenant des chiffres de 1 à 99 ou NA)
  cols_to_merge <- sapply(num_df, function(col) {
    is.character(col) && all(grepl("^[0-9]{1,2}$", col[!is.na(col)]) | is.na(col))
  })
  
  # Obtenir les noms des colonnes à fusionner
  cols_to_merge <- names(cols_to_merge)[cols_to_merge]
  
  # Vérifier que les colonnes correspondantes de let_df existent et ont la même longueur
  for (col_name in cols_to_merge) {
    if (col_name %in% names(let_df) && nrow(let_df) == nrow(num_df)) {
      label_col_name <- paste0(col_name, "_label")
      num_df[[label_col_name]] <- let_df[[col_name]]
    } else {
      warning(paste("La colonne", col_name, "n'existe pas dans let_df ou les longueurs ne correspondent pas."))
    }
  }
  
  return(num_df)
}



###### Fonction qui permet de regrouper les modalités one hot encodées en une seule variable liste #####

#Le one hot encoding est le fait de diviser une variable à modalités multiples en plusieurs variables binaires

reverse_one_hot_encoding <- function(df, prefixes) {
  for (prefix in prefixes) {
    # Sélectionner les colonnes correspondant au préfixe de la variable
    cols <- grep(paste0("^", prefix, "_"), names(df), value = TRUE)
    
    # Extraire les numéros des colonnes
    nums <- sub(paste0(prefix, "_"), "", cols)
    
    # Créer une nouvelle colonne de liste avec les numéros où la valeur est 1, exclure les NA
    liste_col <- paste0(prefix, "_liste")
    df[[liste_col]] <- apply(df[cols], 1, function(row) {
      valid_nums <- nums[row == 1 & !is.na(row)]
      if (length(valid_nums) == 0) {
        return(NA)
      } else {
        return(paste(valid_nums, collapse = ","))
      }
    })
    
    # Réorganiser les colonnes pour placer la colonne _liste avant les colonnes one-hot encodées
    col_pos <- match(cols[1], names(df))
    new_order <- c(names(df)[1:(col_pos-1)], liste_col, names(df)[col_pos:(length(names(df)))])
    df <- df[, new_order]
    
    # Supprimer la colonne _liste de la fin si elle y est
    if (names(df)[length(names(df))] == liste_col) {
      df <- df[, -length(names(df))]
    }
  }
  
  return(df)
}


##### Fonction pour filtrer les lignes avec progress > 54 (réponses exploitables) #####
filter_exploitable <- function(df) {
  df %>% filter(Progress > 54)
}

###### Fonction pour filtrer les lignes avec progress == 100 (réponses complètes) #####
filter_complete <- function(df) {
  df %>% filter(Progress == 100)
}

##### Fonction pour conserver seulement le premier élément des listes
get_first_element <- function(x) {
  if (is.list(x)) {
    return(x[[1]])
  } else {
    return(x)
  }
}

