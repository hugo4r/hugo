#' This function cleans data.
#' Funkcja przyjmuje jako argument data.frame z danymi
#' 1. imputuje braki danych
#' 2. w zmiennych skośnych obcina wartości odstające do pewnego kwantyla
#' 3. spłaszcza zmienne factorowe
#' Funkcja zapisuje do katalogu data zbiór z wyczyszczonymi danymi i informuje o tym użytkownika.
#'
#' @param data Data that will be cleaned.
#' @param quantile Kwantyl do jakiego obcina skośne zmienne numeryczne
#' @param prop Frakcja występowania, poniżej której wartość zmiennej katogorycznej jest uznana za rzadką, i łączona z innymi rzadkimi w jeden factor 
#'
#' @author Eliza Kaczorek
#' @export

