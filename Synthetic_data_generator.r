#IMIONA I NAZWISKA

#kobiety
#generujemy mozliwe imiona kobiet, defniujemy ile tych imion jest
wektor_imion_kobiet <- c("Alicja", "Ewa", "Paulina", "Marzena", "Oliwia", "Dorota", "Barbara", "Anna",
                         "Dominika", "Julia", "Joanna", "Ewelina" ,"Katarzyna", "Weronika", "Natalia", 
                         "Małgorzata", "Kamila", "Magdalena", "Karolina", "Kinga", "Aneta", "Beata", 
                         "Edyta", "Iga", "Iza", "Klaudia", "Wiktoria", "Sylwia")
liczba_imion_k <- length(wektor_imion_kobiet)

#nazwiska analogicznie
wektor_nazwisk_kobiet <- c("Nowak", "Sikora", "Adamczyk", "Wieczorek", "Pawlak", "Walczak",
                           "Olszewska", "Kowalska", "Wiśniewska","Szewczyk", "Jaworska", "Zając", 
                           "Kowalczyk", "Lewandowska", "Kaczmarek", "Malinowska", "Adamczyk",
                           "Krawczyk", "Wojciechowska", "Jankowska", "Mazur", "Piotrowska", "Nowicka",
                           "Zielińska", "Szymańska", "Kasprzak", "Wójcik", "Kamińska", "Nowicka", 
                           "Michalak", "Zalweska", "Włodarczyk", "Sokołowska", "Mazurek", "Sobczak",
                           "Laskowska", "Kaczmarczyk", "Janik", "Cieślak")
liczba_nazwisk_k <- length(wektor_nazwisk_kobiet)


#Mezczyzni
wektor_imion_mezczyzn <- c("Karol", "Kamil", "Adrian",  "Antoni", "Mariusz", "Sebastian", "Michał", 
                           "Jakub", "Kacper", "Radosław", "Łukasz", "Daniel", "Przemysław", "Andrzej",
                           "Adam", "Jan", "Grzegorz", "Krzysztof", "Dawid", "Piotr", "Szymon", "Mateusz",
                           "Marek", "Bartłomiej", "Paweł", "Krystian", "Artur", "Błażej", "Dariusz",
                           "Damian", "Fabian", "Filip", "Maciej", "Igor", "Patryk", "Rafał", "Wiktor",
                           "Wojciech")
liczba_imion_m <- length(wektor_imion_mezczyzn)

wektor_nazwisk_mezczyzn <- c("Nowak", "Sikora", "Adamczyk", "Wieczorek", "Pawlak", "Walczak", 
                             "Olszewski", "Kowalski", "Wiśniewski","Szewczyk", "Jaworski", "Zając", 
                             "Kowalczyk", "Lewandowski", "Kaczmarek", "Malinowski", "Adamczyk",
                             "Krawczyk", "Wojciechowski", "Jankowski", "Mazur", "Piotrowski", "Wójcik",
                             "Zieliński", "Szymański", "Kasprzak", "Kamiński", "Michalak", "Zalewska", 
                             "Włodarczyk", "Sokołowski", "Mazurek", "Sobczak", "Laskowski", 
                             "Kaczmarczyk", "Janik", "Cieślak")
liczba_nazwisk_m <- length(wektor_nazwisk_mezczyzn)



#DATY ROZPOCZeCIA REMONTOW, PLANOWANEGO UKONCZENIA, FAKTYCZNEGO UKONCZENIA, PRZYJECIA ZLECENIA 
#I ID EKIPY ODPOWIEDZIALNEJ ZA REMONT

data_startu <- as.Date("2018-01-02")  #data pierwszego remontu
data_startu_num <- as.numeric(data_startu)  #w formacie numeric

data_dzisiejsza <- as.numeric(Sys.Date())   #data z dzisiaj (zmienia sie)
n <- floor( (data_dzisiejsza - data_startu_num)/40 )+ 25  #tworzymy dlugosc wektorow, tak zeby na 
#pewno starczylo elementzw, ale tez, zeby nie bylo ich za duzo, pozniej wezmiemy tylko te,
#ktorych ktorych data konca remontu jest przed dzisiejsza data
wektor_czasy_remontu <- 12:70  #mozliwe dlugosci remontow w dniach

#wektor przerw (ilosc dni) miedzy kolejnymi remontami
wektor_przerw <- c(1, 2, 3, 4, 5)
wektor_prawdo_przerw <- c(0.05, 0.5, 0.4, 0.045, 0.005) #prawdopodobienstwa z jakimi dane przerwy wystepuja

# przyjecia zlecen
wektor_czasy_przyjecia_zlecen <- -34:-10   #mozliwe roznice miedzy przyjeciem zlecenia, a faktycznym zaczeciem remontu
wektor_prawdo_daty_zlecen <- seq(0.015, 0.065, length.out = 25)  #wymyslona funkcja prawdo przypominajaca 
#funkcje liniowa ( dla 10 dni prawdo najwieksze, dla 34 najmniejsze, zmniejsza sie liniowo)


wlasciwe_starty_remontow <- c()  #wektor, poczatek pierwszego remontu
#tworzymy wektory do ktorych bedziemy doczepiac elementy na koncu kazdej iteracji wielkiej petli
planowane_dlugosci_remontow <- c()     
faktyczne_dlugosci_remontow <- c()
konce_remontow_planowane <- c()
konce_remontow <- c()
id_ekipy_zlecenia <- c()
przyjecia_zlecen_poszczegolne<-c()
przyjecia_zlecen <- c()
liczba_ekip <- 5

for (i in 1:liczba_ekip){    #tworzymy piec harmonogramow remontow ( dla kazdej ekipy )
  wlasciwe_starty_remontow_poszczegolne <- data_startu_num
  przyjecia_zlecen_poszczegolne <- c()
  konce_remontow_planowane_poszczegolne <- c()
  konce_remontow_poszczegolne <- c()
  
  if (i<4){    #dla pierwszych trzech ekip
    planowane_dlugosci_remontow <- sample(wektor_czasy_remontu, n, TRUE)
    P <- rpois(n,2)-2  #wektor roznic miedzy planowanym czasem remontu, a faktycznym 
    faktyczne_dlugosci_remontow <- planowane_dlugosci_remontow + P  
    
    wektor_wlasciwych_przerw <- sample(wektor_przerw, n,  TRUE, prob = wektor_prawdo_przerw)
    
    
    data_startu_num_2 <- data_startu_num  #data, ktora bedziemy zmieniali w kazdej iteracji 
    #naszej pod-petli(while),  data startu kadego kolejnego remontu
    
    #dopoki data_startu kolejnego remontu jest mniejsza niz obecna data+35 dni, 
    #dodajemy kolejne wartosci do odpowiednich wektorow, na koncu zmieniamy date startu
    #na date startu kolejnego remontu
    j<-1
    while (data_startu_num_2 < data_dzisiejsza + 35){
      wlasciwe_starty_remontow_poszczegolne <- append(wlasciwe_starty_remontow_poszczegolne, data_startu_num_2+faktyczne_dlugosci_remontow[j]+wektor_wlasciwych_przerw[j])
      
      konce_remontow_planowane_poszczegolne <-append(konce_remontow_planowane_poszczegolne, data_startu_num_2+planowane_dlugosci_remontow[j]  )
      konce_remontow_poszczegolne <- append(konce_remontow_poszczegolne, data_startu_num_2+faktyczne_dlugosci_remontow[j]  )
      
      data_startu_num_2 <- data_startu_num_2+faktyczne_dlugosci_remontow[j]+wektor_wlasciwych_przerw[j] 
      j <- j+1
    }
    #sztucznie dodajemy jeszcze jedna wartosc do naszych wektorow
    konce_remontow_planowane_poszczegolne <-append(konce_remontow_planowane_poszczegolne, data_startu_num_2+planowane_dlugosci_remontow[j]  )
    konce_remontow_poszczegolne <- append(konce_remontow_poszczegolne, data_startu_num_2+faktyczne_dlugosci_remontow[j]  )
    
    
    #probka roznic miedzy zleceniem remontu,a jego startem 
    roznice_zlec_remont <- sample(wektor_czasy_przyjecia_zlecen, length(wlasciwe_starty_remontow_poszczegolne), TRUE, wektor_prawdo_daty_zlecen ) 
    #tworzymy wektor przyjecia zlecen odejmujac od startu wylosowana ilosc dni
    przyjecia_zlecen_poszczegolne <- wlasciwe_starty_remontow_poszczegolne + roznice_zlec_remont
    przyjecia_zlecen_poszczegolne <- przyjecia_zlecen_poszczegolne[ which(przyjecia_zlecen_poszczegolne<data_dzisiejsza)]
    #dodajemy do id ekipy numer ekipy tyle razy ile przyjela zlecen przez okres
    id_ekipy_zlecenia <- append(id_ekipy_zlecenia, rep(i,length(przyjecia_zlecen_poszczegolne) ))  
    
    #kasowanie tych dat poczatkow/koncow remontow, ktorych konce sa po dzisiejszej dacie
    konce_remontow_planowane_poszczegolne <- konce_remontow_planowane_poszczegolne [which(konce_remontow_poszczegolne<data_dzisiejsza)]
    konce_remontow_poszczegolne <- konce_remontow_poszczegolne [which(konce_remontow_poszczegolne<data_dzisiejsza)]
    wlasciwe_starty_remontow_poszczegolne <- wlasciwe_starty_remontow_poszczegolne [which(konce_remontow_poszczegolne<data_dzisiejsza)]
    
    #dodajemy tyle wartosci NA do naszych wektorow, zeby wektory mia;y w sumie ta sama dlugosc co wektor przyjecia zlecen
    wlasciwe_starty_remontow_poszczegolne <- append(wlasciwe_starty_remontow_poszczegolne, rep (NA, length(przyjecia_zlecen_poszczegolne)-length(wlasciwe_starty_remontow_poszczegolne)))
    konce_remontow_poszczegolne <- append(konce_remontow_poszczegolne, rep (NA, length(przyjecia_zlecen_poszczegolne)-length(konce_remontow_poszczegolne)))
    konce_remontow_planowane_poszczegolne <- append(konce_remontow_planowane_poszczegolne, rep (NA, length(przyjecia_zlecen_poszczegolne)-length(konce_remontow_planowane_poszczegolne)))
    
    #na koncu petli dodajemy nasze "poszczegolne" wektory ( KAZDEJ EKIPY ) do wektorow lacznych
    #w ktorych sa wszystkie zlecenia naszej firmy
    konce_remontow_planowane <- append(konce_remontow_planowane, konce_remontow_planowane_poszczegolne)
    konce_remontow <- append(konce_remontow, konce_remontow_poszczegolne)
    
    przyjecia_zlecen <- append(przyjecia_zlecen, przyjecia_zlecen_poszczegolne)
    wlasciwe_starty_remontow <- append(wlasciwe_starty_remontow, wlasciwe_starty_remontow_poszczegolne)
    
  }
  else if(i==4){ #dla jednej ekipy zmieniamy P- roznice miedyz planowanymi a faktycznymi dlugosciami remontow, tak zeby byla super 
    planowane_dlugosci_remontow <- sample(wektor_czasy_remontu, n, TRUE)
    P <- -rpois(n, 1.5)  #wektor roznic miedzy planowanym czasem remontu, a faktycznym 
    faktyczne_dlugosci_remontow <- planowane_dlugosci_remontow + P  
    #CALA RESZTA TEJ CZESCI JEST ANALOGICZNIE JAK W PRZYPADKU POWYZEJ
    wektor_wlasciwych_przerw <- sample(wektor_przerw, n,  TRUE, prob = wektor_prawdo_przerw)
    
    data_startu_num_2 <- data_startu_num  
    j<-1
    while (data_startu_num_2 < data_dzisiejsza + 35){
      wlasciwe_starty_remontow_poszczegolne <- append(wlasciwe_starty_remontow_poszczegolne, data_startu_num_2+faktyczne_dlugosci_remontow[j]+wektor_wlasciwych_przerw[j])
      konce_remontow_planowane_poszczegolne <-append(konce_remontow_planowane_poszczegolne, data_startu_num_2+planowane_dlugosci_remontow[j]  )
      konce_remontow_poszczegolne <- append(konce_remontow_poszczegolne, data_startu_num_2+faktyczne_dlugosci_remontow[j]  )
      
      data_startu_num_2 <- data_startu_num_2+faktyczne_dlugosci_remontow[j]+wektor_wlasciwych_przerw[j] 
      j <- j+1
    }
    konce_remontow_planowane_poszczegolne <-append(konce_remontow_planowane_poszczegolne, data_startu_num_2+planowane_dlugosci_remontow[j]  )
    konce_remontow_poszczegolne <- append(konce_remontow_poszczegolne, data_startu_num_2+faktyczne_dlugosci_remontow[j]  )
    
    roznice_zlec_remont <- sample(wektor_czasy_przyjecia_zlecen, length(wlasciwe_starty_remontow_poszczegolne), TRUE, wektor_prawdo_daty_zlecen ) 
    
    przyjecia_zlecen_poszczegolne <- wlasciwe_starty_remontow_poszczegolne + roznice_zlec_remont
    przyjecia_zlecen_poszczegolne <- przyjecia_zlecen_poszczegolne[ which(przyjecia_zlecen_poszczegolne<data_dzisiejsza)]
    id_ekipy_zlecenia <- append(id_ekipy_zlecenia, rep(i,length(przyjecia_zlecen_poszczegolne) ))  
    
    konce_remontow_planowane_poszczegolne <- konce_remontow_planowane_poszczegolne [which(konce_remontow_poszczegolne<data_dzisiejsza)]
    konce_remontow_poszczegolne <- konce_remontow_poszczegolne [which(konce_remontow_poszczegolne<data_dzisiejsza)]
    wlasciwe_starty_remontow_poszczegolne <- wlasciwe_starty_remontow_poszczegolne [which(konce_remontow_poszczegolne<data_dzisiejsza)]
    
    wlasciwe_starty_remontow_poszczegolne <- append(wlasciwe_starty_remontow_poszczegolne, rep (NA, length(przyjecia_zlecen_poszczegolne)-length(wlasciwe_starty_remontow_poszczegolne)))
    konce_remontow_poszczegolne <- append(konce_remontow_poszczegolne, rep (NA, length(przyjecia_zlecen_poszczegolne)-length(konce_remontow_poszczegolne)))
    konce_remontow_planowane_poszczegolne <- append(konce_remontow_planowane_poszczegolne, rep (NA, length(przyjecia_zlecen_poszczegolne)-length(konce_remontow_planowane_poszczegolne)))
    
    konce_remontow_planowane <- append(konce_remontow_planowane, konce_remontow_planowane_poszczegolne)
    konce_remontow <- append(konce_remontow, konce_remontow_poszczegolne)
    
    przyjecia_zlecen <- append(przyjecia_zlecen, przyjecia_zlecen_poszczegolne)
    wlasciwe_starty_remontow <- append(wlasciwe_starty_remontow, wlasciwe_starty_remontow_poszczegolne)
  }
  
  else{  #dla jednej(ostatniej ekipy) zmieniami roznice miedzy planowanymi koncami remontow, a faktycznymi tak, zeby byla slaba
    planowane_dlugosci_remontow <- sample(wektor_czasy_remontu, n, TRUE )
    P <- rpois(n, 2)
    #CALA RESZTA JEST ANALOGICZNIE JAK W PRZYPADKU PIERWSZYM
    faktyczne_dlugosci_remontow <- planowane_dlugosci_remontow + P  
    
    wektor_wlasciwych_przerw <- sample(wektor_przerw, n,  TRUE, prob = wektor_prawdo_przerw)
    
    data_startu_num_2 <- data_startu_num  
    j<-1
    while (data_startu_num_2 < data_dzisiejsza + 35){
      wlasciwe_starty_remontow_poszczegolne <- append(wlasciwe_starty_remontow_poszczegolne, data_startu_num_2+faktyczne_dlugosci_remontow[j]+wektor_wlasciwych_przerw[j])
      konce_remontow_planowane_poszczegolne <-append(konce_remontow_planowane_poszczegolne, data_startu_num_2+planowane_dlugosci_remontow[j]  )
      konce_remontow_poszczegolne <- append(konce_remontow_poszczegolne, data_startu_num_2+faktyczne_dlugosci_remontow[j]  )
      
      data_startu_num_2 <- data_startu_num_2+faktyczne_dlugosci_remontow[j]+wektor_wlasciwych_przerw[j] 
      j <- j+1
      
    }
    konce_remontow_planowane_poszczegolne <-append(konce_remontow_planowane_poszczegolne, data_startu_num_2+planowane_dlugosci_remontow[j]  )
    konce_remontow_poszczegolne <- append(konce_remontow_poszczegolne, data_startu_num_2+faktyczne_dlugosci_remontow[j]  )
    
    roznice_zlec_remont <- sample(wektor_czasy_przyjecia_zlecen, length(wlasciwe_starty_remontow_poszczegolne), TRUE, wektor_prawdo_daty_zlecen )  
    
    przyjecia_zlecen_poszczegolne <- wlasciwe_starty_remontow_poszczegolne + roznice_zlec_remont
    przyjecia_zlecen_poszczegolne <- przyjecia_zlecen_poszczegolne[ which(przyjecia_zlecen_poszczegolne<data_dzisiejsza)]
    id_ekipy_zlecenia <- append(id_ekipy_zlecenia, rep(i,length(przyjecia_zlecen_poszczegolne) ))  
    
    konce_remontow_planowane_poszczegolne <- konce_remontow_planowane_poszczegolne [which(konce_remontow_poszczegolne<data_dzisiejsza)]
    konce_remontow_poszczegolne <- konce_remontow_poszczegolne [which(konce_remontow_poszczegolne<data_dzisiejsza)]
    
    wlasciwe_starty_remontow_poszczegolne <- wlasciwe_starty_remontow_poszczegolne [which(konce_remontow_poszczegolne<data_dzisiejsza)]
    
    wlasciwe_starty_remontow_poszczegolne <- append(wlasciwe_starty_remontow_poszczegolne, rep (NA, length(przyjecia_zlecen_poszczegolne)-length(wlasciwe_starty_remontow_poszczegolne)))
    konce_remontow_poszczegolne <- append(konce_remontow_poszczegolne, rep (NA, length(przyjecia_zlecen_poszczegolne)-length(konce_remontow_poszczegolne)))
    konce_remontow_planowane_poszczegolne <- append(konce_remontow_planowane_poszczegolne, rep (NA, length(przyjecia_zlecen_poszczegolne)-length(konce_remontow_planowane_poszczegolne)))
    
    konce_remontow_planowane <- append(konce_remontow_planowane, konce_remontow_planowane_poszczegolne)
    konce_remontow <- append(konce_remontow, konce_remontow_poszczegolne)
    
    przyjecia_zlecen <- append(przyjecia_zlecen, przyjecia_zlecen_poszczegolne)
    wlasciwe_starty_remontow <- append(wlasciwe_starty_remontow, wlasciwe_starty_remontow_poszczegolne)
  }
  #po rozpatrzeniu wszystkich ekip, na koncu petli tworzymy wektory w typie 'Date'
  wlasciwe_starty_remontow_data <- as.Date(wlasciwe_starty_remontow, origin=as.Date("1970-01-01"))
  przyjecia_zlecen_data <- as.Date(przyjecia_zlecen, origin=as.Date("1970-01-01"))
  konce_remontow_data <- as.Date(konce_remontow, origin=as.Date("1970-01-01"))
  konce_remontow_planowane_data <- as.Date(konce_remontow_planowane, origin=as.Date("1970-01-01"))
}  
#ZAKRESY

zakresy_zlecenia <- c()  
m <- length(przyjecia_zlecen)  # ilosc wszystkich remontow do ktorych mamy przypisac zakres
probka_zakresy <- c(0,1,2)
prawdo_zakresy <- c(0.2,0.6,0.2)
klucz <- sample(probka_zakresy, m, TRUE, prawdo_zakresy) #wektor prawdo 60% na 1, 20% na 0, 20% na 2


#w odpoweidni sposob tworzymy wektor faktycznych dlugosci remontow:
faktyczne_dlugosci_remontow<-c()

for (i in 1:length(konce_remontow)){
  faktyczne_dlugosci_remontow<- append(faktyczne_dlugosci_remontow, konce_remontow[i]-wlasciwe_starty_remontow[i])
}

#petla tworzaca zakresy, lostow, ale zalezna tez od dugosci remontow (np. dla remontow 
#miedzy 23, a 32 dniami jest 60% na zakres 2, 20% na zakres , 20% na zakres )
for (i in 1:m){
  if(is.na(faktyczne_dlugosci_remontow[i])){
    zakresy_zlecenia <- append(zakresy_zlecenia, NA)
  }
  else{  
    if (faktyczne_dlugosci_remontow[i]<23 && klucz[i] == 1){
      zakresy_zlecenia <- append(zakresy_zlecenia, 1)
    }   
    else if (faktyczne_dlugosci_remontow[i]<23 && klucz[i] != 1){
      zakresy_zlecenia <- append(zakresy_zlecenia, 2)
    }
    else if (23<=faktyczne_dlugosci_remontow[i] && faktyczne_dlugosci_remontow[i]<33 && klucz[i] == 1){
      zakresy_zlecenia <- append(zakresy_zlecenia, 2)
    }
    else if (23<=faktyczne_dlugosci_remontow[i] && faktyczne_dlugosci_remontow[i]<33 && klucz[i] == 0){
      zakresy_zlecenia <- append(zakresy_zlecenia, 1)
    }
    else if (23<=faktyczne_dlugosci_remontow[i] && faktyczne_dlugosci_remontow[i]<33 && klucz[i] == 2){
      zakresy_zlecenia <- append(zakresy_zlecenia, 3)
    }
    else if (33<=faktyczne_dlugosci_remontow[i] && faktyczne_dlugosci_remontow[i]<48 && klucz[i] == 1){
      zakresy_zlecenia <- append(zakresy_zlecenia, 3)
    }
    else if (33<=faktyczne_dlugosci_remontow[i] && faktyczne_dlugosci_remontow[i]<48 && klucz[i] == 1){
      zakresy_zlecenia <- append(zakresy_zlecenia, 2)
    }
    else if (33<=faktyczne_dlugosci_remontow[i] && faktyczne_dlugosci_remontow[i]<48 && klucz[i] == 1){
      zakresy_zlecenia <- append(zakresy_zlecenia, 4)
    }
    else if (48<=faktyczne_dlugosci_remontow[i] && faktyczne_dlugosci_remontow[i]<58 && klucz[i] == 1){
      zakresy_zlecenia <- append(zakresy_zlecenia, 4)
    }
    else if (48<=faktyczne_dlugosci_remontow[i] && faktyczne_dlugosci_remontow[i]<58 && klucz[i] == 1){
      zakresy_zlecenia <- append(zakresy_zlecenia, 3)
    }
    else if (48<=faktyczne_dlugosci_remontow[i] && faktyczne_dlugosci_remontow[i]<58 && klucz[i] == 1){
      zakresy_zlecenia <- append(zakresy_zlecenia, 5)
    }
    else if (58<=faktyczne_dlugosci_remontow[i] && klucz[i] == 1){
      zakresy_zlecenia <- append(zakresy_zlecenia, 5)
    }
    else {
      zakresy_zlecenia <- append(zakresy_zlecenia, 4)
    }
  }
}  
liczba_zlecen <- length(zakresy_zlecenia)

#adresy_zlecenia
#wczytujemy nasz plik danych adresow
dane_adresy <- read.csv("C:/STUDIA/Semestr 5/Bazy danych/EMUIA_Adresy_StanNa20210201.csv", sep=";", encoding="UTF-8")
#bierzemy tylko potrzebne kolumny: skrocony adres i kod pocztowy 
wektor_kody_pocztowe <- dane_adresy[,5]
wektor_adresy <- dane_adresy[,6]

#length(unique(wektor_kody_pocztowe))  -sprawdzenie ile jest roznych kodow pocztowych

indeksy_adresow <- sample(1:length(wektor_kody_pocztowe), liczba_zlecen, TRUE)
#tworzymy puste wektory o odpowiedniej dlugosci, zeby podmieniac wartosci 
adresy_zlecenia <- rep(0, liczba_zlecen)
kody_pocztowe_zlecenia <- rep(0,liczba_zlecen)
adresy_zlecenia <- wektor_adresy[indeksy_adresow]
kody_pocztowe_zlecenia <- wektor_kody_pocztowe[indeksy_adresow]


#ZALICZKI, zaplaty itd
zaplaty <-c()
liczba_zlecen <- length(zakresy_zlecenia)
for (i in 1:liczba_zlecen){
  if(is.na(zakresy_zlecenia[i])){  #jesli wartosc w zakresach jest rownana, to remont jeszcze niezakonczony
    zaplaty <- append(zaplaty, NA)   #i dodajemy wartosc na rowniez w wektor zakresow
   }
  else{  
    if (zakresy_zlecenia[i] == 5){ #jesli wartosc wektora zakresow rowna 5 to remont najdrozszy, cena schodzi w dol wraz z zakresem
      zaplaty <- append(zaplaty, round(rnorm(1, 73000, 1500), digits=2))  #rozklad N(50000, 4000), zaokraglenie do dwoch miejsc(do groszy)
    }
    else if(zakresy_zlecenia[i] == 4){
      zaplaty <- append(zaplaty, round(rnorm(1, 45000, 1200), digits=2))
    }
    else if(zakresy_zlecenia[i] == 3){
      zaplaty <- append(zaplaty, round(rnorm(1, 30500, 1000), digits=2))
    }
    else if(zakresy_zlecenia[i] == 2){
      zaplaty <- append(zaplaty, round(rnorm(1, 25000, 800), digits=2))
    }
    else{
      zaplaty <- append(zaplaty, round(rnorm(1, 21000, 600), digits=2))
    }
  }
}


zaliczki <- c()  
zaliczki <- round(zaplaty/5, digits = 2)    #zaliczki = 20% zaplat za remonty

kwoty_reszty <- c()
kwoty_reszty <- zaplaty - zaliczki   #oczywiste

daty_zaliczek <- c()
wektor_daty_zaliczek <- -10:-3   #terminy wplaty zaliczek - od 10 do 3 dni przed remontem

R <- sample(wektor_daty_zaliczek, liczba_zlecen, TRUE)  #losowa probka

daty_zaliczek <- wlasciwe_starty_remontow + R
#przeobrazamy  na typ Date
daty_zaliczek_data <- as.Date(daty_zaliczek, origin=as.Date("1970-01-01"))

#daty wplacenia reszty
wektor_daty_wplaty_reszty <- 1:7   #od 1 dnia do 7 dni PO KONCU REMONTU
W <- sample(wektor_daty_wplaty_reszty, liczba_zlecen, TRUE, wektor_daty_wplaty_reszty)
daty_wplaty_reszty <- konce_remontow + W
daty_wplaty_reszty_data <- as.Date(daty_wplaty_reszty, origin=as.Date("1970-01-01")) #typ Date

#kwota kar
kwoty_kar <- c()
for (i in 1:liczba_zlecen){
  if (is.na(konce_remontow[i])){  #jesli wartosc remontow rowna jest NA, dodajemy NA rowniez tutaj
    kwoty_kar <- append(kwoty_kar, NA)
  }  
  else{  
    if (konce_remontow_planowane[i] < konce_remontow[i]){  #jesli skoncza remont po terminie to jest kara
    kwoty_kar <- append(kwoty_kar, (konce_remontow[i] - konce_remontow_planowane[i])*(0.03*zaliczki[i] ) ) 
                        #kary w wysokosci: ilosc dni po terminie *3% zaliczki
    }
    else{
      kwoty_kar <- append(kwoty_kar, 0)    #jesli zdazymy w terminie, nie ma kary
    }
  }
}
kwoty_kar <- round(kwoty_kar, digits=2)  #do pełnych groszy

#koszty remontu
wektor_kosztow_remontu <- (16:20)/100    #od 18% do 23%
K <- sample(wektor_kosztow_remontu, liczba_zlecen, TRUE)   
koszty_remontu <- round(zaplaty*K, digits=2)   #koszty ktore ponosimy podczas wykonania remontu wynosza:
#cena zlecenia*(25%-35%)

#INWESTORZY 

liczba_inwestorow <- 110
id_inwestora <- 1:liczba_inwestorow
imiona_inwestorow <- c()
nazwiska_inwestorow <- c()
for (i in 1:liczba_inwestorow) {
  x <- rbinom(1,1, 0.1)  #10% szans, ze inwestorem jest kobieta
  if (x==1){   #przypadek kobiety
    imie <- sample(wektor_imion_kobiet, 1, TRUE)
    nazwisko <- sample(wektor_nazwisk_kobiet, 1, TRUE)
  }
  else {
    imie <- sample(wektor_imion_mezczyzn, 1, TRUE, )
    nazwisko <- sample(wektor_nazwisk_mezczyzn, 1, TRUE)
  }
  imiona_inwestorow <- append(imiona_inwestorow, imie)
  nazwiska_inwestorow <- append(nazwiska_inwestorow, nazwisko)
}
#NIPy inwestorow
liczba_inwestorow_detalicznych <- liczba_inwestorow-5

inwest_cyfry_nip <- sample(0:9, 9, TRUE, rep(0.1,10))
wagi_nip <- c(6, 5, 7, 2, 3, 4, 5, 6, 7)
#wzor na obliczanie cyfry kontrolnej dolaczamy ja od razu do wektora cyfr
inwest_cyfry_nip <- append(inwest_cyfry_nip, (sum( inwest_cyfry_nip*wagi_nip )) %% 11 )  

inwestorzy_nipy <- rep(NA, liczba_inwestorow_detalicznych)

for ( j in (liczba_inwestorow_detalicznych+1):(liczba_inwestorow)){
  inwest_nip_poszczegolny <- c()
  for (i in 1:10){ #petla tworzaca  NIP
    if (i == 4 | i==6 | i==8){
      inwest_nip_poszczegolny <- paste(inwest_nip_poszczegolny, inwest_cyfry_nip[i], sep="-")
    }
    else{
      inwest_nip_poszczegolny <- paste(inwest_nip_poszczegolny, inwest_cyfry_nip[i], sep="") 
    }
  }
  inwestorzy_nipy <- append(inwestorzy_nipy, inwest_nip_poszczegolny)
}
#adresy inwestorow
inwestorzy_adresy <- sample(wektor_adresy, liczba_inwestorow, FALSE)

#telefony inwestorow

numery_tel_inwestorzy <- c()

for (j in 1:liczba_inwestorow){  #tworzymy tyle numerow, ile inwestorow
  nr_tel_poszczegolny <- c()  #numer tworzony w kazdej iteracji petli
  for (i in 1:9){         #tworzymy numer telefonu
    
    if (i==1){             #pierwsza cyfra od 1 do 9
      cyfra <- sample(1:9, 1)
      nr_tel_poszczegolny <- append(nr_tel_poszczegolny, cyfra)
    }
    else{                #reszta cyfr od 0 do 9
      
      cyfry <- sample(0:9, 1)
      nr_tel_poszczegolny <- as.numeric (paste(nr_tel_poszczegolny, cyfry ,sep="") )
    }#laczymy calosc
  }
  numery_tel_inwestorzy <- append(numery_tel_inwestorzy, nr_tel_poszczegolny)
}
numery_tel_inwestorzy_char <- paste(numery_tel_inwestorzy) 

#maile inwestorow
maile_inwestorzy <- c()

for (i in 1:liczba_inwestorow){
  maile_inwestorzy <- append(maile_inwestorzy,paste(imiona_inwestorow[i], nazwiska_inwestorow[i]) )
}
#usuwanie polskich znakow
maile_inwestorzy <- gsub("ń", "n", maile_inwestorzy)
maile_inwestorzy <- gsub("ś", "s", maile_inwestorzy)
maile_inwestorzy <- gsub("ą", "a", maile_inwestorzy)
maile_inwestorzy <- gsub("ć", "c", maile_inwestorzy)
maile_inwestorzy <- gsub("ę", "e", maile_inwestorzy)
maile_inwestorzy <- gsub("ł", "l", maile_inwestorzy)
maile_inwestorzy <- gsub("ż", "z", maile_inwestorzy)
maile_inwestorzy <- gsub("ź", "z", maile_inwestorzy)
maile_inwestorzy <- gsub("ó", "o", maile_inwestorzy)

#nie ma Ń, Ę, Ą, Óna poczatku polskich imion/nazwisk, wiec z wielkich liter zmieniamy tylko te:
maile_inwestorzy <- gsub("Ś", "S", maile_inwestorzy)
maile_inwestorzy <- gsub("Ć", "C", maile_inwestorzy)
maile_inwestorzy <- gsub("Ł", "L", maile_inwestorzy)
maile_inwestorzy <- gsub("Ż", "Z", maile_inwestorzy)
maile_inwestorzy <- gsub("Ź", "Z", maile_inwestorzy)

#usuwanie spacji lub zamienianie jej na kropke/podloge
Z <- c(0,1,2)


#losujemy n numerow miedzy 0, 1, a 2 z rownym prawdo. , 
znaki <- sample(Z, liczba_inwestorow, TRUE)

#zamieniamy spacje na jedna z trzech opcji
for (i in 1:liczba_inwestorow){
  if (znaki[i] == 0){                      # 0-usuniecie spacji
    maile_inwestorzy[i] <- gsub (" ", "", maile_inwestorzy[i])
  }
  else if (znaki[i] == 1){                 # 1- kropka zamiast spacji, 
    maile_inwestorzy[i] <- gsub (" ", ".", maile_inwestorzy[i])
  } 
  else {maile_inwestorzy[i] <- gsub (" ", "_", maile_inwestorzy[i])}  # 2-podloga zamiast spacji
} 
#losowania maila: 83% szans na gmail, 10% na wp, 5% na interie, 2% na o2
z2 <- c(0, 1, 2, 3)  #legenda: 0-gmail, 1-wp, 2-interia, 3-o2
prawdo_z2 = c(0.83, 0.1, 0.05, 0.02)
znaki2 <- sample(z2, liczba_inwestorow, TRUE, prawdo_z2)

for (i in 1:liczba_inwestorow){
  if (znaki2[i] == 0){#dodajemy odpowiednie koncowki do maili
    maile_inwestorzy[i] <- paste(maile_inwestorzy[i], "@gmail.com", sep="")
  }
  else if(znaki2[i] == 1){
    maile_inwestorzy[i] <- paste(maile_inwestorzy[i], "@wp.pl", sep="")
  }
  else if(znaki2[i] == 2){
    maile_inwestorzy[i] <- paste(maile_inwestorzy[i], "@interia.pl", sep="")
  }
  else{maile_inwestorzy[i] <- paste(maile_inwestorzy[i], "@o2.pl", sep="")}
}

#ilosc inwestycji, rodzaj inwestora
id_inwestora_zlecenia <- rep(0, liczba_zlecen)      #towrzymy wektor pod ktory bedziemy podstawiac wartosci                               


indeksy_zlecen_inw_detal <- sample(1:liczba_zlecen, liczba_inwestorow_detalicznych)
indeksy_zlecen_inw_firmy <- (1:liczba_zlecen)[- indeksy_zlecen_inw_detal]

liczba_zlecen_firmy <- length(indeksy_zlecen_inw_firmy)

S <- sample( (liczba_inwestorow_detalicznych+1):liczba_inwestorow, liczba_zlecen_firmy, TRUE)
#probka, ktora losuje nam firmy-zleceniodawcow
for( i in 1:length(indeksy_zlecen_inw_detal )){ #podstawiamy dla klientow detalicznych
  id_inwestora_zlecenia[ indeksy_zlecen_inw_detal[i] ] <-  i
}
id_inwestora_zlecenia [which (id_inwestora_zlecenia==0)] <- S      #podstawiamy probke dla firm w puste(=0) miejsca

#rodzaj inwestora
rodzaj_inwestora <- append( rep("Klient detaliczny", liczba_inwestorow_detalicznych), rep("Firma", liczba_inwestorow-liczba_inwestorow_detalicznych))
#liczba_zlecen
liczba_zlecen_inwestorzy <- rep(1, liczba_inwestorow_detalicznych)
for (i in (liczba_inwestorow_detalicznych+1):liczba_inwestorow){#tylko dla firm!, przechodzimy po kazdej firmie
  suma_poszczegolna <- 0
  for (j in 1:length(S)){  #przechodzimy po kazdej wartosci probki losoweja
    if (S[j]==i){  # jesli ta wartosc jest ronwa odpowiedniej firmie, to dodajemy firmie jedno zlecenie
      suma_poszczegolna <- suma_poszczegolna+1
    }
  }
  #po kazdej iteracji petli, dodajemy do wektora zlecen nasza sume () 
  liczba_zlecen_inwestorzy <- append(liczba_zlecen_inwestorzy, suma_poszczegolna)
}

#PRACOWNICY
imie_pracownicy <- c()
nazwisko_pracownicy <- c()
n <- 28 #ilosc robotnikow mezczyzn
#tworzymy legende indeksow
indeks_prezesa <- 1   
indeks_menadzera <- 2
indeks_ksiegowej <- 3
indeks_brygadzistow <- 4:8
indeksy_robotnikow_k <- 9:10
indeksy_robotnikow_m <- seq(11, by=1, length.out=n)
id_stanowiska_pracownicy <- c(1,2,3, rep(4,5), rep(5, n+2))

imie_pracownicy <- append(imie_pracownicy, sample(wektor_imion_mezczyzn, 1, TRUE ))
nazwisko_pracownicy <- append(nazwisko_pracownicy, sample(wektor_nazwisk_mezczyzn, 1, TRUE))
#menadzer
imie_pracownicy <- append(imie_pracownicy, sample(wektor_imion_mezczyzn, 1, TRUE ))
nazwisko_pracownicy <- append(nazwisko_pracownicy, sample(wektor_nazwisk_mezczyzn, 1, TRUE))
#ksiegowa
imie_pracownicy <- append(imie_pracownicy, sample(wektor_imion_kobiet, 1, TRUE ))
nazwisko_pracownicy <- append(nazwisko_pracownicy, sample(wektor_nazwisk_kobiet, 1, TRUE))
#brygadzisci
imie_pracownicy <- append(imie_pracownicy, sample(wektor_imion_mezczyzn, 5, TRUE ))
nazwisko_pracownicy <- append(nazwisko_pracownicy, sample(wektor_nazwisk_mezczyzn, 5, TRUE))
#robotnicy kobiety
imie_pracownicy <- append(imie_pracownicy, sample(wektor_imion_kobiet, 2, TRUE))
nazwisko_pracownicy <- append(nazwisko_pracownicy, sample(wektor_nazwisk_kobiet, 2, TRUE))
#robotnicy mezczyzni
imie_pracownicy <- append(imie_pracownicy, sample(wektor_imion_mezczyzn, n, TRUE ))
nazwisko_pracownicy <- append(nazwisko_pracownicy, sample(wektor_nazwisk_mezczyzn, n, TRUE))


liczba_pracownikow <- length(imie_pracownicy)
id_pracownika <- 1:length(imie_pracownicy)
#legenda w tym wektorze: indeks[1]-prezes,  [2], menadzer, [3]-ksiegowa, 4:8- brygadzisci, 9:10-robotnicy_k, 11:40-robotnicy_m

#DATY ZWOLNIEN I ZATRUDNIEN

liczba_robotnikow <- liczba_pracownikow-10  #robotnicy - mezczyzni
indeksy_robotnikow <- 11:liczba_pracownikow
indeksy_zwolnionych <- sample(indeksy_robotnikow, 10, FALSE )   
#losujemy indeksy zwolnionych, nie moga sie powtarzac

indeksy_niezwolnionych <- indeksy_robotnikow[-(indeksy_zwolnionych-10)]    
#usuwamy tych zwolnionych kiedys przez nas (aktualnie nie pracujacych)z wektory indeksu
indeksy_zatrudnionych_nie_od_poczatku <- c()
indeksy_zatrudnionych_nie_od_poczatku <- sample(indeksy_niezwolnionych, 10, FALSE) 
#losujemy 10 indeksow, ktore beda zatrudnione nie od poczatku  pracuja do dzis 


wektor_dat_zwolnienia <- rep(0, liczba_pracownikow) #tworzymy wektor
wektor_dat_zwolnienia[-indeksy_zwolnionych] <- NA
wektor_dat_zwolnienia <- as.Date(wektor_dat_zwolnienia, origin = as.Date("1970-01-01"))

wektor_dat_zatrudnienia <- rep(0, liczba_pracownikow)
wektor_dat_zatrudnienia[-indeksy_zatrudnionych_nie_od_poczatku] <- as.Date(data_startu_num - 30, origin = as.Date("1970-01-01"))
wektor_dat_zatrudnienia <- as.Date(wektor_dat_zatrudnienia, origin = as.Date("1970-01-01"))


data_startu_zatrudnien <- as.Date(data_startu_num - 30, origin = as.Date("1970-01-01"))
dzis_data <- as.Date(data_dzisiejsza, origin = "1970-01-01")

wektor_zwolnien <-as.Date( sample(seq(data_startu_num, data_dzisiejsza, by=1), length(indeksy_zwolnionych)), origin=as.Date("1970-01-01") )
wektor_dat_zwolnienia[indeksy_zwolnionych] <- wektor_zwolnien

wektor_zatrudnien <- as.Date( as.numeric(wektor_zwolnien) + 15, origin = as.Date("1970-01-01"))
#zatrudnienia nowych 15 dni po zwolnieniu kazdego starego 
wektor_dat_zatrudnienia[indeksy_zatrudnionych_nie_od_poczatku] <- wektor_zatrudnien


# EKIPY ROBOTNIKOW (wartosci dla pracownikow biurowych)
id_ekipy_pracownicy <- c(rep(6, 3), 1:5, rep(0, liczba_pracownikow-8 ))  #pierwsze 3 osoby to prezes menadzer i ksiegowa- nie maja ekipy, 
#brygadzisci kazdy po jednej, pozniej tyle zer, zeby zgadzalo 

indeksy_nie_od_pocz_do_konca <- append(indeksy_zwolnionych, indeksy_zatrudnionych_nie_od_poczatku)
indeksy_od_poczatku <- 9:liczba_pracownikow

#wektor osob ktore pracuja u nas od poczatku istnienia firmy az po dzis dzien
indeksy_od_poczatku <- setdiff(indeksy_od_poczatku, indeksy_zatrudnionych_nie_od_poczatku)

#losowanie ekip w 20 indeksach
ekipa_remontowa_pracownicy <- sample(c(rep(1,4), rep(2,4), rep(3,4), rep(4,4), rep(5,4)))

#dopasowujemy numery ekip do ludzi, ktorzy pracuja od poczatku
id_ekipy_pracownicy[indeksy_od_poczatku] <- ekipa_remontowa_pracownicy 

#petla, ktora przypisuje taka sama ekipe nowozatrudnionemu, co zwolnionemu 2 tygodnie wczesniej
for (i in 1:length(indeksy_zwolnionych)){
  id_ekipy_pracownicy[ indeksy_zatrudnionych_nie_od_poczatku[i]] <- id_ekipy_pracownicy [ indeksy_zwolnionych [i]]
}


#PESELE, WIEK ZATRUDNIONYCH
data_urodzin_prez <- sample(seq(as.Date("1961-01-01"), as.Date("1981-01-01"), by="day"), 1)
data_urodzin_men_i_ks <- sample(seq(as.Date("1961-01-01"), as.Date("1986-01-01"), by="day"), 2)

data_urodzin_robo_sredni <- sample(seq(as.Date("1975-01-01"), as.Date("1992-01-01"), by="day"), length(which(id_ekipy_pracownicy<4)) )
data_urodzin_robo_super_1 <- sample(seq(as.Date("1956-01-01"), as.Date("1965-01-01"), by="day"), floor(length(which(id_ekipy_pracownicy==4))/2) )
data_urodzin_robo_super_2 <- sample(seq(as.Date("1992-01-01"), as.Date("2001-01-01"), by="day"),length(which(id_ekipy_pracownicy==4))- floor(length(which(id_ekipy_pracownicy==4))/2) ) 
data_urodzin_robo_slabi <- sample(seq(as.Date("1984-01-01"), as.Date("1995-01-01"), by="day"), length(which(id_ekipy_pracownicy==5)) )

daty_urodzen <- rep(0, liczba_pracownikow)
daty_urodzen[which(id_ekipy_pracownicy==6)] <- c(data_urodzin_prez, data_urodzin_men_i_ks)
daty_urodzen[which(id_ekipy_pracownicy<4)]  <- data_urodzin_robo_sredni
daty_urodzen[which(id_ekipy_pracownicy==4)] <- c(data_urodzin_robo_super_1, data_urodzin_robo_super_2)
daty_urodzen[which(id_ekipy_pracownicy==5)] <- data_urodzin_robo_slabi

daty_urodzen <- as.Date(daty_urodzen, origin = as.Date("1970-01-01"))


#daty urodzen losowo: prezes miedzy 40, a 60 lat, menadzer i ksiegowa 35-60 lat, brygadzisci 30-50, robotnicy różnie

pesele <- paste(substring(daty_urodzen, 3, 4), substring(daty_urodzen, 9, 10), substring(daty_urodzen, 6, 7))
#wziecie odpowienich cyfr do peselu ( za pomoca wspolrzednych stringa )
pesele <- gsub(" ", "", pesele)

wektor_cyfr <- 0:9
wektor_prawdo_cyfr <- rep(1/length(wektor_cyfr), length(wektor_cyfr))
trzy_losowe_cyfry<-sample(wektor_cyfr, 3, TRUE, wektor_prawdo_cyfr)
trzy_losowe_cyfry<- paste(trzy_losowe_cyfry[1], trzy_losowe_cyfry[2], trzy_losowe_cyfry[3], sep="" )

for (i in 1:liczba_pracownikow){
  trzy_losowe_cyfry<-sample(wektor_cyfr, 3, TRUE, wektor_prawdo_cyfr)
  trzy_losowe_cyfry<- paste(trzy_losowe_cyfry[1], trzy_losowe_cyfry[2], trzy_losowe_cyfry[3], sep="" )
  pesele[i] <- paste(pesele[i], trzy_losowe_cyfry, sep="") 
}

#przedostatnia cyfra peselu, kobiety
indeksy_kobiet <- c(3,9,10)
indeksy_mezczyzn <- (1:liczba_pracownikow)[- indeksy_kobiet]
for (i in indeksy_kobiet){
  pesele[i] <- paste(pesele[i], sample(c(0,2,4,6,8),1, TRUE, c(0.2,0.2,0.2,0.2,0.2)), sep="")
}
#analogicznie, mezczyzni
for(i in indeksy_mezczyzn ) {#omijamy indeksy kobiet
  pesele[i] <- paste(pesele[i], sample(c(1,3,5,7,9),1, TRUE, c(0.2,0.2,0.2,0.2,0.2)), sep="")
}

#ostatnia cyfra,  kontrola, wzor
wagi_cyfr <- c(1,3,7,9,1,3,7,9,1,3) # wagi do naszego wzoru

ostatnie_cyfry <- c()
for (i in 1:liczba_pracownikow){ #przechodzimy po wszystkich peselach
  pesel <- pesele[i]
  I <- 0
  for (j in 1:10){  #przechodzimy po kolei po kazdej cyfrze naszego peselu
    kazda_cyfra <- as.integer( substring(pesel, j, j) )
    #wzor na rozdzielenie stringa, wziecie cyfry i nadaniej jej rodzaju inta
    I <- I+kazda_cyfra*wagi_cyfr[j]
  }
  cyfra <- 10 - (I %% 10)   # nasza cyfra jest 10 - (odjac) reszta z dzielenia I (obliczonego wzorem) przez 10 
  if (cyfra == 10){  # jesli cyfra jest rowna 10, to zamieniamy ja na 0 
    cyfra <- 0
  }
  
  ostatnie_cyfry <- append(ostatnie_cyfry, cyfra)
}
for (i in 1:liczba_pracownikow){  #laczenie naszych poprzednich 10 cyfrowych peseli z ostatnimi cyframi
  pesele[i] <- paste(pesele[i], ostatnie_cyfry[i], sep="")
}
pesele_num <- as.numeric(pesele)  #konwertowanie gotowych peseli na typ liczbowy (nwm czy potrzebne)


#Telefony pracownikow
numery_tel_pracownicy <- c()
for (j in 1:liczba_pracownikow){  #tworzymy tyle numerow, ile inwestorow
  nr_tel_poszczegolny <- c()  #numer tworzony w kazdej iteracji petli
  for (i in 1:9){         #tworzymy numer telefonu
    
    if (i==1){             #pierwsza cyfra od 1 do 9
      cyfra <- sample(1:9, 1)
      nr_tel_poszczegolny <- append(nr_tel_poszczegolny, cyfra)
    }
    else{                #reszta cyfr od 0 do 9
      
      cyfry <- sample(0:9, 1)
      nr_tel_poszczegolny <- as.numeric (paste(nr_tel_poszczegolny, cyfry ,sep="") )
    }#laczymy calosc
  }
  numery_tel_pracownicy <- append(numery_tel_pracownicy, nr_tel_poszczegolny)
}
numery_tel_pracownicy_char <- paste(numery_tel_pracownicy) 


#WIEK
library('eeptools')
wiek_pracownikow <- floor(age_calc(daty_urodzen, units="years"))  #podloga z funkcji obliczajacej lata

#ID STANOWISKA

#wiemy z gory jakie sa id prezesa, menadzera, ksiegowej i brygadzistow
id_stanowiska <- c(1, 2, 3, rep(4, 5), rep(5, liczba_pracownikow-8 ))

#studenci/niestudenci

studenci <- rep(0, liczba_pracownikow) 
studenci[which(wiek_pracownikow<26)] <- 1  #ci co maja ponizej 26, to u nas studenci
liczba_studentow <- length(which(studenci == 1))

#rodzaj umowy, wszyscy zlecenie
rodzaj_umowy <- c(NA, rep("Umowa o pracę", 2), rep("Umowa zlecenie", (liczba_pracownikow)-3))

#praktyki zawodowe
praktyki_zawodowe <- rep(0, liczba_pracownikow)
praktyki_zawodowe[sample(9:38, 2)] <- 1    #losujemy dwoch losowych robotnikow, dajemy im '1' (studenci) 


#WYP:ATY
stawki_godzinowe <- rep(0, liczba_pracownikow)
stawka_godzinowa_prezesa <- round(rnorm(1, 50, 2) , digits=1)
stawka_godzinowa_menadzera <- round(rnorm(1, 40, 1.5) , digits=1)
stawka_godzinowa_ksiegowej <- round(rnorm(1, 38, 1.5) , digits=1)
stawki_godzinowe_brygadzistow <- round(rnorm(5, 30, 1.2) , digits=1)
stawki_godzinowe_robotnikow <- round(rnorm(n+2-liczba_studentow, 22, 1) , digits=1)

#n to liczba robotn. mezczyzn + 2 robotnicy kobiety - ilosc studenci , ktorzy u nas pracuja bezplatnie

stawki_godzinowe[- which(studenci == 1)] <- c(stawka_godzinowa_prezesa, stawka_godzinowa_menadzera, stawka_godzinowa_ksiegowej, stawki_godzinowe_brygadzistow, stawki_godzinowe_robotnikow)

dni_w_miesiacu <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
#czteroletni cykl, rok przestepny itd.


data_startu_zatrudnien_num <- as.numeric(data_startu_zatrudnien, origin = as.Date("1970-01-01"))
liczba_zaczetych_lat_funkcjonowania_firmy <- ceiling((data_dzisiejsza - data_startu_zatrudnien_num)/(365*4+1))
#powielamy wektor czteroletnich cykli tyle razy ile potrzebujemy 
dni_w_miesiacu <- rep(dni_w_miesiacu, liczba_zaczetych_lat_funkcjonowania_firmy)

#wektor, data startu zatrudnien to 2 styczen, bedziemy liczyc od pierwszego
daty_startu_kolejnych_miesiecy <- data_startu_num-1
#liczba, zmieniajaca sie w kazdej iteracji petli, sluzaca do porownywania 
data_startu_kazdego_miesiaca <- data_startu_num-1 

#ponizsza petla tylko dla robotnikow wiec od daty startow remontow !
#wszyscy nierobotnicy u nas pracuja od pocz. do konca
j <- 1
while(data_startu_kazdego_miesiaca < data_dzisiejsza){
  daty_startu_kolejnych_miesiecy <- append(daty_startu_kolejnych_miesiecy, data_startu_kazdego_miesiaca+dni_w_miesiacu[j])
  data_startu_kazdego_miesiaca <- data_startu_kazdego_miesiaca+dni_w_miesiacu[j]
  
  j <- j+1
}

#usuniecie ostatniego elementu, poniewaz jest to start miesiaca juz po dzisiejszej dacie
daty_startu_kolejnych_miesiecy  <- daty_startu_kolejnych_miesiecy[1:length(daty_startu_kolejnych_miesiecy)-1]  

liczba_rozpocz_miesiecy <- length(daty_startu_kolejnych_miesiecy)

indeksy_zwolnionych <- sort(indeksy_zwolnionych)
#sortowanie indeksow pracownikow niepracujacych z nami od poczatku istnienia firmy

wektor_zwolnien_num <- as.numeric(wektor_zwolnien)

#robimy funkcje, ktorej argumentem jest indeks wektor osob zwolnionych, a zwraca nam miesiace w 
#jakich ta osoba pracowala
miesiace_pracy_zwolnionych <- function(x){
  miesiace_pracownika <- c()
  data_startu_kazdego_miesiaca <- data_startu_num-1
  j <- 1
  
  while (data_startu_kazdego_miesiaca < wektor_zwolnien_num[x]) {
    miesiace_pracownika <- append(miesiace_pracownika, j)
    
    data_startu_kazdego_miesiaca <- data_startu_kazdego_miesiaca+dni_w_miesiacu[j]
    j <- j+1
  }
  return(miesiace_pracownika)
}

#SPRAWDZENIE CZY DOBRZE DZIALA

#zatrudnieni nie od poczatku
indeksy_zatrudnionych_nie_od_poczatku <- sort(indeksy_zatrudnionych_nie_od_poczatku)

wektor_zatrudnien_num <- as.numeric(wektor_dat_zatrudnienia[indeksy_zatrudnionych_nie_od_poczatku], origin= as.Date("1970-01-01"))

#robimy funkcje, ktorej argumentem jest indeks wektor osob zatrudnionych nie od poczatku, 
#a zwraca nam miesiace w jakich ta osoba pracowala


miesiace_pracy_nowo_zatrudnionych <- function(x){
  #szukamy indeksu pierwszej wartosci nowego miesiaca, ktory zaczyna sie po zatrudnieniu i zapisujemy 
  #przepracowane miesiace: poczawszy od  poprzedniego indeksu 
  #(kawalek poprzedniego miesiaca przepracowal), az do ostatniego startu miesiaca
  miesiace_pracownika <- c()
  data_startu_kazdego_miesiaca <- data_startu_num-1
  j <- 1
  
  while (data_startu_kazdego_miesiaca < wektor_zatrudnien_num[x]) {
    
    data_startu_kazdego_miesiaca <- data_startu_kazdego_miesiaca+dni_w_miesiacu[j]
    j <- j+1
  }
  miesiace_pracownika <- (j-1):liczba_rozpocz_miesiecy
  return(miesiace_pracownika)
}

#TWORZENIE ID PRACOWNIKA, ID MIESIACA DO TABELI WYPLATY
 
id_pracownika_wyplaty <- c()
id_miesiaca_wyplaty <- c()

for (i in 1:liczba_rozpocz_miesiecy){   #przechodzimy po kazdym mozliwym miesiacu
  indeksy_pracownikow_w_konkretnych_miesiacach <- setdiff(1:liczba_pracownikow,  indeksy_nie_od_pocz_do_konca)
  #niezaleznie od miesiaca, do kazdego dokladamy indeksy pracownikow, ktorzy pracuja u nas caly czas od poczatku
  
  for (j in 1:length(wektor_zwolnien_num)){   #przechodzimy po wszystkich ludziach ktorzy nie pracowali u nas przez cay czas
    if (i <= max(miesiace_pracy_zwolnionych(j)) ) {  
      indeksy_pracownikow_w_konkretnych_miesiacach <- append(indeksy_pracownikow_w_konkretnych_miesiacach, indeksy_zwolnionych[j])
    }  
  }
  for (j in 1:length(wektor_zatrudnien_num)){   #przechodzimy po wszystkich ludziach ktrzy nie pracowali u nas przez caly czas
    if (i >= min(miesiace_pracy_nowo_zatrudnionych(j)) ) {  #
      indeksy_pracownikow_w_konkretnych_miesiacach <- append(indeksy_pracownikow_w_konkretnych_miesiacach, indeksy_zatrudnionych_nie_od_poczatku[j])
    }  
  }
  indeksy_pracownikow_w_konkretnych_miesiacach <- sort(indeksy_pracownikow_w_konkretnych_miesiacach)
  id_pracownika_wyplaty <- append(id_pracownika_wyplaty, indeksy_pracownikow_w_konkretnych_miesiacach)
  
  id_miesiaca_wyplaty <- append(id_miesiaca_wyplaty, rep(i, length(indeksy_pracownikow_w_konkretnych_miesiacach)))
}



liczba_id_miesiaca <- length(id_miesiaca_wyplaty)

#ILOSC GODIZN PRZEPRACOWANYCH

dni_w_ost_mies <- c()
for (i in 1:length(wektor_zwolnien_num)){
  ost_miesiac <- miesiace_pracy_zwolnionych(i)[length(miesiace_pracy_zwolnionych(i))] #ostatni miesiac kazdego zwolnionego
  dni_w_ost_mies <- append(dni_w_ost_mies, wektor_zwolnien_num[i]-daty_startu_kolejnych_miesiecy[ost_miesiac]) 
  #tworzymy wektor ilosci dni przepracowanych w ost mies. zwolnionych                         
}

godziny_zwol_ost_mies <- floor((5.5/7)*dni_w_ost_mies*8)    #wzor na "odjecie" weekendow


dni_w_pierw_mies <- c()

for (i in 1:length(wektor_zatrudnien_num)){
  ost_miesiac <- miesiace_pracy_nowo_zatrudnionych(i)[1] #pierwszy miesiac kazdego nowo zatrudnionego
  dni_w_pierw_mies <- append(dni_w_pierw_mies, wektor_zatrudnien_num[i]-daty_startu_kolejnych_miesiecy[ost_miesiac]) 
  #tworzymy wektor ilosci dni przepracowanych w ost mies. zwolnionych                         
}

godziny_zatrud_pierw_mies <- floor((4.8/7)*dni_w_pierw_mies*8)    #losowy wzor na "odjecie" weekendow

godziny_pracy <- rep(0, times= length(id_miesiaca_wyplaty))   #tworzymy "niepusty" wektor o dlugosci takiej jak 
#id_miesiaca, zeby podstawiac dane: petla ktore podstawi godziny pracy dla pracujacych "niepelne" miesiace

for (j in 1:length(indeksy_zwolnionych)) {  #przechodzimy po wsystkich zwolnionych
  ost_ind <- max(which(id_pracownika_wyplaty== indeksy_zwolnionych[j]))   #ostatni indeks miesiaca kazdego zwolnionego 
  godziny_pracy[ost_ind] <- godziny_zwol_ost_mies[j]  
}

for (j in 1:length(indeksy_zatrudnionych_nie_od_poczatku)) {  #przechodzimy po wsystkich zwolnionych
  pierwszy_ind <- min(which(id_pracownika_wyplaty== indeksy_zatrudnionych_nie_od_poczatku[j]))   #ostatni indeks miesiaca kazdego zwolnionego 
  godziny_pracy[pierwszy_ind] <- godziny_zwol_ost_mies[j]  
}

dziwne_godz_ilosc <- length(which(godziny_pracy!=0))
dziwne_godz <- which(godziny_pracy!=0)

wektor_godzin <- 152:168
liczba_mozliwych_godz <- length(wektor_godzin)
wektor_prawdo_godzin <- rep(1/liczba_mozliwych_godz, liczba_mozliwych_godz)

#podstawiamy pod reszte indeksy, wartosci dla ludzi pracujacych pelne miesiace (155h-168h)
godziny_pracy[-dziwne_godz] <- sample(wektor_godzin, length(id_miesiaca_wyplaty)-dziwne_godz_ilosc , TRUE, wektor_prawdo_godzin)

#POLACZENIE STAWEK I WYPLAT
wyplaty_brutto <- rep(0, length(id_miesiaca_wyplaty))   #tworzymy "niepusty" wektor wyplat, zeby podstawiac odpowiednie dane

for (i in 1:liczba_pracownikow){  #przechodzimy  po wszystkich pracownikach
  wektor_ind_danego_pracownika <- which(id_pracownika_wyplaty==i)
  wyplaty_brutto[wektor_ind_danego_pracownika] <- stawki_godzinowe[i]*godziny_pracy[wektor_ind_danego_pracownika]
}
wyplaty_netto <- wyplaty_brutto*0.83     #podatek 17%


#MIESIECZNE KOSZTY INNE
liczba_miesiecy <- length(daty_startu_kolejnych_miesiecy)

ZUS <- rep(1457.49, liczba_miesiecy)
Paliwo <- round( rnorm(liczba_miesiecy ,800,100 )  , digits=2)

wektor_kwot_reklama <- c(500,2000)
wektor_prawdo_kwot_reklam <- c(0.95, 0.05)

Reklama <- sample(wektor_kwot_reklama, liczba_miesiecy, TRUE, wektor_prawdo_kwot_reklam)

Oplaty_bankowe <- round(rnorm(liczba_miesiecy, 250, 20), digits=2)

Wynajem_biura <- rep(2000, liczba_miesiecy) 





#GWARANCJE
#szukamy tych indeksow remontow, ktorych koniec byl przynajmniej 20 dni temu

liczba_remontow_gwar <- 10
wektor_mozliwych_ind_rem_gwar <- which(konce_remontow < data_dzisiejsza - 25)

wektor_prawdo_mozliwych_ind_rem_gwar <- rep(1/length(wektor_mozliwych_ind_rem_gwar), length(wektor_mozliwych_ind_rem_gwar))

#losujemy indeksy zlecen, ktore beda szly na gwarancje
ind_zlec_na_rem_gwar <- sample(wektor_mozliwych_ind_rem_gwar, liczba_remontow_gwar, FALSE, wektor_prawdo_mozliwych_ind_rem_gwar)
ind_zlec_na_rem_gwar <- sort(ind_zlec_na_rem_gwar)  #sortujemy 


#tworzymy wektor roznic miedyz koncem orygianlnego remontu a startem teog na gwarancji, losowo 1-15 dni z takim samym prawdo
L <- sample(1:15, liczba_remontow_gwar, TRUE, prob = rep(1/length(1:15), length(1:15)))
starty_rem_gwar <-  konce_remontow[ind_zlec_na_rem_gwar] + L
starty_rem_gwar_data <- as.Date(starty_rem_gwar, origin = "1970-01-01")

#tworzymy wektor dlugosci remontow gwarancyjnych, losowo 1-10 dni, takie samo prawdo
U <- sample(1:10, liczba_remontow_gwar, TRUE, prob = rep(1/length(1:10), length(1:10)))
konce_rem_gwar <- starty_rem_gwar +U
konce_rem_gwar_data <- as.Date(konce_rem_gwar, origin = "1970-01-01")

#losujemy cene przez ktora mnozymy te dlugosci
C <- sample(15:100, liczba_remontow_gwar, TRUE, prob =rep(1/length(15:100), length(15:100)) )
koszty_rem_gwar <- round(U*C, digits=2)

#wektor zakresow, 75% na 1 (najlatwiejszy), 25% na 2
zakresy_zrem_gwar <- sample(c(1,2), liczba_remontow_gwar, TRUE, c(0.75,0.25))

id_ekipy_rem_gwar <- id_ekipy_zlecenia[ind_zlec_na_rem_gwar]



#PODWYKONAWCY
podwyk_nazwy_firmy <- c("Budowex", "SolidBud", "Remontux", "GoodBud", "AmikoBud","Okieneo", "Architalk", "EkoBud", "LuksGruz", "San Remont")
liczba_firm <- length(podwyk_nazwy_firmy)
podwyk_cyfry_nip <- sample(0:9, 9, TRUE, rep(0.1,10))
wagi_nip <- c(6, 5, 7, 2, 3, 4, 5, 6, 7)
#wzor na obliczanie cyfry kontrolnej dolaczamy ja od razu do wektora cyfr
podwyk_cyfry_nip <- append(podwyk_cyfry_nip, (sum( podwyk_cyfry_nip*wagi_nip )) %% 11 )  

podwyk_nipy <- c()

for (j in 1:liczba_firm){
  podwyk_nip_poszczegolny <- c()
  for (i in 1:10){ #petla tworzaca  NIP
    if (i == 4 | i==6 | i==8){
    podwyk_nip_poszczegolny <- paste(podwyk_nip_poszczegolny, podwyk_cyfry_nip[i], sep="-")
    }
  else{
    podwyk_nip_poszczegolny <- paste(podwyk_nip_poszczegolny, podwyk_cyfry_nip[i], sep="") 
    }
  }
  podwyk_nipy <- append(podwyk_nipy, podwyk_nip_poszczegolny)
}

#adresy, korzystamy z tego co wczesniej
podwyk_adresy <- sample(wektor_adresy, liczba_firm, TRUE)

podwyk_numery_tel <- c()
#telefony, korzystamy z tego co wczesniej
for (j in 1:liczba_firm){  #przechodzimy po wszystkich inwestorach
  podwyk_nr_tel_poszczegolny <- c()
  for (i in 1:9){         #tworzymy numer telefonu
    
    if (i==1){             #pierwsza cyfra od 1 do 9
      cyfra <- sample(1:9, 1)
    }
    else{                #reszta cyfr od 0 do 9
      
      cyfra <- sample(0:9, 1)
    }
    podwyk_nr_tel_poszczegolny <- paste(podwyk_nr_tel_poszczegolny, cyfra ,sep="")
  }
  podwyk_numery_tel <- append(podwyk_numery_tel, as.numeric(podwyk_nr_tel_poszczegolny))
}

#specjalizacja podwykonawcy
podwyk_specjalizacje <- c("Okna", "Meble", "Dachy", "Drzwi", "Łazienki", "Meble", "Panele", "Drzwi", "Wkłady kominowe", "Podłogi")
koszty_materialy <- c()

#Koszty extra , materialy 
for (i in 1:length(daty_startu_kolejnych_miesiecy)-1 ){ #bedziemy przechodzic po kazdym przedziale miesiecy [data startu[i], data startu[i+1]]
  ind_rem_w_poszczegolnych_miesiacach <- c()
  for (j in 1:length(koszty_remontu)){   #przechodzimy
    if (is.na(wlasciwe_starty_remontow[j]) ){} #OMIJAMY WARTOSCI NA
    else{  
      if ( daty_startu_kolejnych_miesiecy[i] <= wlasciwe_starty_remontow[j] && wlasciwe_starty_remontow[j] <= daty_startu_kolejnych_miesiecy[i+1] ) {
        ind_rem_w_poszczegolnych_miesiacach <- append(ind_rem_w_poszczegolnych_miesiacach, j)
      } 
    }  
  }
  koszty_materialy <- append(koszty_materialy, sum(koszty_remontu[ind_rem_w_poszczegolnych_miesiacach]) )
}

koszty_materialy <- koszty_materialy[- 1] #usuwamy sztucznie pierwsza wartosc, ktora wynosi 0
koszty_materialy <- append(koszty_materialy, sum(koszty_materialy) - sum(koszty_remontu[-which(is.na(koszty_remontu))]) )
#dodajemy na koncu wartosc kosztow materialo remontow rozpoczetych po dacie startu obecnego miesiaca
# jest to wartosc sumy kosztow wszystkich remontow - wartosc kosztow w (n-1) miesiacach

#podwykonawcy przy zleceniach

#szukamy ilosci zlecen nie bedacych na
liczba_zlecen_not_na <- length(wlasciwe_starty_remontow[- which(is.na(wlasciwe_starty_remontow))])
podwyk_liczba_zlecen <- floor(liczba_zlecen/2)

id_zlecen <- 1:liczba_zlecen
mozliwe_ind_rem_podwyk <- id_zlecen[- which(is.na(wlasciwe_starty_remontow))]
wektor_prawdo_ind_rem_podwyk <- rep(1/length(mozliwe_ind_rem_podwyk), length(mozliwe_ind_rem_podwyk))

#losujemy, ktore remonty maja podwykonawce
wektor_remontow_podwyk <- sample(mozliwe_ind_rem_podwyk, podwyk_liczba_zlecen, FALSE)

podwyk_id_firmy <- 1:liczba_firm
podwyk_id_firmy_zlecenia<-sample(podwyk_id_firmy, podwyk_liczba_zlecen, TRUE)  #losujemy id firm, przypisujemy do kazdej budowy podwykonawcy


podwyk_id_zlecenia <- c(wektor_remontow_podwyk[which(podwyk_id_firmy_zlecenia==1)], wektor_remontow_podwyk[which(podwyk_id_firmy_zlecenia==2)], wektor_remontow_podwyk[which(podwyk_id_firmy_zlecenia==3)],
                        wektor_remontow_podwyk[which(podwyk_id_firmy_zlecenia==4)], wektor_remontow_podwyk[which(podwyk_id_firmy_zlecenia==5)], wektor_remontow_podwyk[which(podwyk_id_firmy_zlecenia==6)],
                        wektor_remontow_podwyk[which(podwyk_id_firmy_zlecenia==7)], wektor_remontow_podwyk[which(podwyk_id_firmy_zlecenia==8)], wektor_remontow_podwyk[which(podwyk_id_firmy_zlecenia==9)], 
                        wektor_remontow_podwyk[which(podwyk_id_firmy_zlecenia==10)])

podwyk_id_firmy_zlecenia <- sort(podwyk_id_firmy_zlecenia)

#losujemy losowo od 2% do 5%
B <- sample(c(0.02,0.03,0.04,0.05), podwyk_liczba_zlecen,TRUE)

#wzór na koszty za podwykonawce, 2%-5% * cena zlecenia
podwyk_koszty <- round(zaplaty[podwyk_id_zlecenia]*B, digits=2)

#TABELA EKIPY RMEONTOWE
liczba_ekip <- 6
id_ekipy <- 1:liczba_ekip
liczba_czlonkow_ekipy <- c()
for (i in id_ekipy){ #patrzymy ile jest pracownikow obecnie pracujacych z poszczegolnymi id ekip, przypisujemy
  liczba_czlonkow_ekipy <- append(liczba_czlonkow_ekipy, length( which( id_ekipy_pracownicy[-indeksy_zwolnionych] == paste(i) )) )
}

liczba_budow_ekipy <-c()
for (i in id_ekipy){ #patrzymy ile jest budow z poszczegolnymi id ekip, przypisujemy
  liczba_budow_ekipy <- append(liczba_budow_ekipy, length( which( id_ekipy_zlecenia == i )) )
}


#TABELA STANOWISKA

stanowisko <- c("Prezes", "Menadżer", "Księgowa", "Brygadzista", "Pracownik Budowlany" )
Opisy_stanowisk <- c("Najważniejsza osoba w firmie. Sprawuje władzę nad wszystkimi pracownikami i 
kontroluje całoształt działalności. Wdraża w życie plany i strategie dotyczące rozwoju biznesu. 
Oprócz tego jest reprezentantem spółki na zewnątrz, dba o jej interesy.",
"Mózg firmy. Organizuje i przydziela pracę poszczególnym pracownikom. Jest odpowiedzialny za 
organizację szkoleń dla pracowników oraz jego obowiązkiem jest motywowanie zespołów do działania.",
"Zajmuje się prowadzniem wszystkiego, co jest związane z rachunkowością. Do jej zadań należy 
prowadzenie księgi przychodów i rozchodów, pilnowanie terminów płatności, obliczanie podatków oraz 
przygotowywanie deklaracji.", "Inaczej szef zespołu. Wyznacza pracownikom grupy zadania, które mają 
wykonać oraz kontroluje sposób, w jaki są one realizowane. Dba o przestrzeganie zasad bhp podczas wykonywania remontów.",
"Członek zespołu. Wykonuje wszystkie prace remontowe, m.in. malowanie, tapetowanie, szpachlowanie ścian, 
nakładanie kafelek, kostek, tynkowanie itp. Zajmuje się wykonywaniem prac wykończeniowych. Współpracuje z innymi robotnikami.")
 
  
#TABELA zakresy
ID_zakres <- 1:5
zakresy_remontow <- c("Remont pokojów lub salonu.", "Remont łazienki.", "Remont kuchni.", "Remont kawalerki.",
                      "Generalny remont mieszkania.")
zakresy_opisy <- c("Obejmuje on szpachlowanie ścian, wymianę instalacji elektrycznej czy podłóg. Do zakresu 
należy również malowanie bądź tapetowanie ścian, montaż gniazdek elektrycznych oraz dobór mebli do 
pomieszczenia.", "Fachowcy zajmują się naprawą ubytków, pęknięć w ścianach i sufitach oraz gruntowaniem ścian. 
W zakres remontu wchodzi układanie płytek, malowanie oraz montaż biały (umywalka, toaleta, wanna, itd).", 
"Obejmuje on skucie starych płytek ze ścian i podłogi, układanie nowych płytek, a także nakładanie gładzi 
na ściany i sufity. Do działań zespołu remontowego należą również założenie instalacji elektrycznej, 
wodno-kanalizacyjnej oraz centralnego ogrzewania.", "Obejmuje on wszystkie pomieszczenia mieszkania: 
pokój, kuchnię, łazienkę oraz przedpokój.", "Opcja dla większych mieszkań. Oprócz standardowego remontu 
poszczególnych pomieszczeń, w zakres wchodzi również wyburzanie ścian, przenoszenie instalacji 
elektrycznych, wymiany podłóg itp.")
  
#TABELA SPRZET

liczba_sprzetu_budow <- 40
wektor_firm_sprzetu_budow <- c("Bosch", "Hilti", "Festool")

liczba_sprzetu_elektr <- 10
wektor_firm_sprzetu_elektr <- c("Samsung", "Lenovo")

liczba_sprzetu <- liczba_sprzetu_budow+liczba_sprzetu_elektr
sprzet_id <- 1:liczba_sprzetu

sprzet_budow <- sample(wektor_firm_sprzetu_budow, liczba_sprzetu_budow, TRUE)
sprzet_elektr <- sample(wektor_firm_sprzetu_elektr, liczba_sprzetu_elektr, TRUE)
sprzet <- append(sprzet_budow, sprzet_elektr) 

sprzet_liczby <- paste (sample(10:99, liczba_sprzetu, FALSE) )
sprzet_litery <- sample(letters, liczba_sprzetu, TRUE)
sprzet_litery_2 <- sample(letters, liczba_sprzetu, TRUE)                     

for (i in 1:liczba_sprzetu){
  sprzet[i] <- paste(sprzet[i], sprzet_liczby[i], sprzet_litery[i], sep=" ")
  sprzet[i] <- paste(sprzet[i], sprzet_litery_2[i], sep="")
}
indeksy_sprzet_budow <- 1:liczba_sprzetu_budow
indeksy_sprzet_elektr <- (liczba_sprzetu_budow+1):liczba_sprzetu

sprzet_data_zakupu <- rep(0, liczba_sprzetu)
#losujemy 40% sprzetow budowlanych, ktore mielismy od poczatku 
losowe_indeksy <- append( sample( indeksy_sprzet_budow, floor((2*liczba_sprzetu_budow)/5), FALSE),
                          sample(indeksy_sprzet_elektr, floor((9*liczba_sprzetu_elektr)/10), FALSE) )
sprzet_data_zakupu[losowe_indeksy] <- data_startu_num
#pozostalym budowlanym przypisujemy dowolny dzien od startu do dzis
sprzet_data_zakupu[-losowe_indeksy] <- sample(data_startu_num:data_dzisiejsza-365, liczba_sprzetu-length(losowe_indeksy), TRUE)
sprzet_data_zakupu_data <- as.Date(sprzet_data_zakupu, origin= as.Date("1970-01-01"))

sprzet_data_sprzedazy <- c()
for (i in 1:liczba_sprzetu_budow){
  if (i %% 4 == 0) {  #dla co czwartego sprzetu 
    sprzet_data_sprzedazy <- append(sprzet_data_sprzedazy, NA )
  }
  else if(sprzet[i]=="Bosch"){
    sprzet_data_sprzedazy <- append(sprzet_data_sprzedazy, sample( (sprzet_data_zakupu[i]+290):(sprzet_data_zakupu[i]+350), 1 ) )
  }
  else {  #dla co drugi
    sprzet_data_sprzedazy <- append(sprzet_data_sprzedazy, sample((sprzet_data_zakupu[i]+360):(data_dzisiejsza), 1) ) 
  }
}
sprzet_data_sprzedazy <- append(sprzet_data_sprzedazy, rep(NA, liczba_sprzetu_elektr) )

sprzet_data_sprzedazy_data <- as.Date(sprzet_data_sprzedazy, origin= as.Date("1970-01-01"))
#cena zakupu /sprzedazy sprzetu 
wektor_cen_zakupu_sprzetu_budow <- 100*(5:20)
wektor_cen_zakupu_sprzetu_elektr <- 100*(11:35)
ceny_zakupu_sprzetu <- append(sample(wektor_cen_zakupu_sprzetu_budow, liczba_sprzetu_budow, TRUE), sample(wektor_cen_zakupu_sprzetu_elektr, liczba_sprzetu_elektr, TRUE) )

ceny_sprzedazy_sprzetu <- rep(NA, liczba_sprzetu)

liczba_sprzedanego_sprzetu <- liczba_sprzetu- length (which(is.na(sprzet_data_sprzedazy ) ) )                          
wektor_straty_na_sprzecie <- sample(0.1*c(3:8), liczba_sprzedanego_sprzetu, TRUE)
#podstawiamy pod te ceny sprzedazy, ktore sa inne niz NA
ceny_sprzedazy_sprzetu[- which(is.na(sprzet_data_sprzedazy))] <- 
  wektor_straty_na_sprzecie*ceny_zakupu_sprzetu[- which(is.na(sprzet_data_sprzedazy))]


#grupa remontowa sprzetu, losujemy numery ekip 
sprzet_grupa_remontowa <- c(rep(1, floor(liczba_sprzetu_budow/5)), rep(2, floor(liczba_sprzetu_budow/5)),
                            rep(3, floor(liczba_sprzetu_budow/5)), rep(4, floor(liczba_sprzetu_budow/5)) )
sprzet_grupa_remontowa <- append(sprzet_grupa_remontowa, rep(5, liczba_sprzetu_budow-length(sprzet_grupa_remontowa)))
sprzet_grupa_remontowa <- append(sprzet_grupa_remontowa, rep(NA, liczba_sprzetu_elektr))


#TABELA MIESIACE
miesiace_miesiac <- unique(id_miesiaca_wyplaty)
miesiace_miesiac <- miesiace_miesiac - floor((miesiace_miesiac-1)/12)*12     #konwertowanie numerow miesiecy, z np. 13 na 1,  26 na 2 itd.

miesiace_rok <-c()
k<-0
#ilość pełnych lat
#floor(length(miesiace_miesiac)/12)
#pozostała ilość miesięcy
#length(miesiace_miesiac) %% 12

for (i in 1:floor(length(miesiace_miesiac)/12) ){
  
  miesiace_rok <- append(miesiace_rok, rep(2018+k, 12))
  k <- k+1
}
miesiace_rok <- append(miesiace_rok, rep(2018+k, length(miesiace_miesiac) %% 12) )





