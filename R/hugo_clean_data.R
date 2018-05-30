# hugo_clean_data <- function(data, ...) {
#   
# }

# Funkcja przyjmuje jako argument data.frame z danymi
# 1. imputuje braki danych
# 2. w zmiennych skośnych obcina wartości odstające do pewnego kwantyla
# 3. spłaszcza zmienne factorowe

# Dodatkowe argumenty:
# - quantile - do jakiego kwantyla obciąć skośne zmienne numeryczne
# - prop - frakcja występowania, poniżej której wartość zmiennej katogorycznej jest uznana za rzadką,
#          i łączona z innymi rzadkimi w jeden factor 

# Funkcja zapisuje do katalogu data zbiór z wyczyszczonymi danymi i informuje o tym użytkownika.