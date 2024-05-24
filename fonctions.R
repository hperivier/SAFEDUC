#import packages 
source("packages.R")

##### Fonction qui cr�e une variable_label pour chaque variable concern�e #####

# Cette fonction fusionne les bases identiques dont l'une des versions a ses modalit�s en num et l'autre en character
# Pour chaque variable num�rique dont un label correspond, la variable correspondante se cr�e avec le nom "variable_label" 


merge_with_labels <- function(num_df, let_df) {
  # Identifier les colonnes concern�es (type character et contenant des chiffres de 1 � 99 ou NA)
  cols_to_merge <- sapply(num_df, function(col) {
    is.character(col) && all(grepl("^[0-9]{1,2}$", col[!is.na(col)]) | is.na(col))
  })
  
  # Obtenir les noms des colonnes � fusionner
  cols_to_merge <- names(cols_to_merge)[cols_to_merge]
  
  # V�rifier que les colonnes correspondantes de let_df existent et ont la m�me longueur
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



###### Fonction qui permet de regrouper les modalit�s one hot encod�es en une seule variable liste #####

#Le one hot encoding est le fait de diviser une variable � modalit�s multiples en plusieurs variables binaires

reverse_one_hot_encoding <- function(df, prefixes) {
  for (prefix in prefixes) {
    # S�lectionner les colonnes correspondant au pr�fixe de la variable
    cols <- grep(paste0("^", prefix, "_"), names(df), value = TRUE)
    
    # Extraire les num�ros des colonnes
    nums <- sub(paste0(prefix, "_"), "", cols)
    
    # Cr�er une nouvelle colonne de liste avec les num�ros o� la valeur est 1, exclure les NA
    liste_col <- paste0(prefix, "_liste")
    df[[liste_col]] <- apply(df[cols], 1, function(row) {
      valid_nums <- nums[row == 1 & !is.na(row)]
      if (length(valid_nums) == 0) {
        return(NA)
      } else {
        return(paste(valid_nums, collapse = ","))
      }
    })
    
    # R�organiser les colonnes pour placer la colonne _liste avant les colonnes one-hot encod�es
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


# Fonction pour filtrer les lignes avec progress > 54 (r�ponses exploitables)
filter_exploitable <- function(df) {
  df %>% filter(Progress > 54)
}

# Fonction pour filtrer les lignes avec progress == 100 (r�ponses compl�tes)
filter_complete <- function(df) {
  df %>% filter(Progress == 100)
}
