#Zadanie 5/B

przyznaj_nagrode = function() {
  
  rzut = sample(1:6, 1)
  
  print(paste("Wylosowano:", rzut))
  
  if (rzut == 6) {
    return("Super bonus!")
    
  } else if (rzut == 4 || rzut == 5) {
    return("Nagroda standardowa")
    
  } else {
    return("Brak nagrody...")
  }
}

#Test funkcji
print(przyznaj_nagrode())
print(przyznaj_nagrode())
print(przyznaj_nagrode())
print(przyznaj_nagrode())
