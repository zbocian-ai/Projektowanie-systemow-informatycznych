#Zadanie 1/A

wartosc_przyszla = function(kapital, stopa, lata) {
  if(kapital > 0 && lata >= 1){
  FV = kapital * (1 + stopa)^lata }
  else {
  FV = kapital }
  return(FV)
}

#Test funkcji
print(wartosc_przyszla(5000, 0.05, 1))
# print(wartosc_przyszla(5000, -0.05, 1))
# print(wartosc_przyszla(5000, 0.05, 0))
