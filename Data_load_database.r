#Biblioteka do bazy danych
library(RMariaDB)

#Wczytanie bazy danych

db_con <- dbConnect(
  drv = RMariaDB::MariaDB(), 
  dbname = "team1",
  username = "team1",
#  password = xxxxxxxx,    dane wrażliwe, dostepne w oryginale
#  host = xxxxxxxxx, 
  port = 3306
)


#Tworzenie tabel

### Gwarancje 

q_del_gwarancje = "DROP TABLE IF EXISTS Gwarancje;"

results = dbSendQuery(db_con,q_del_gwarancje)
dbClearResult(results)

q_gwarancje = "CREATE TABLE `Gwarancje`
(`ID_gwarancji`      INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
 `ID_zlecenia`       INT NULL, 
 `ID_ekipy`          INT NULL,
 `ID_zakresu`        INT NULL,
 `Początek_remontu`  DATE NULL, 
 `Koniec_remontu`    DATE NULL, 
 `Koszt_remontu`     FLOAT NULL)"

results = dbSendQuery(db_con,q_gwarancje)
dbClearResult(results)

### Podwykonawcy_zlecenia

q_del_podwykonawcy_zlecenia = "DROP TABLE IF EXISTS Podwykonawcy_zlecenia;"

results = dbSendQuery(db_con,q_del_podwykonawcy_zlecenia)
dbClearResult(results)

q_podwykonawcy_zlecenia = "CREATE TABLE `Podwykonawcy_zlecenia`
(`ID_podwykonawcy_zlecenia` INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
 `ID_Podwykonawcy` INT NULL, 
 `ID_zlecenia`     INT NULL, 
 `Koszt`           FLOAT NULL)"

results = dbSendQuery(db_con,q_podwykonawcy_zlecenia)
dbClearResult(results)

### Podwykonawcy 

q_del_podwykonawcy = "DROP TABLE IF EXISTS Podwykonawcy;"

results = dbSendQuery(db_con,q_del_podwykonawcy)
dbClearResult(results)

q_podwykonawcy = "CREATE TABLE `Podwykonawcy`
(`ID_Podwykonawcy`  INT NOT NULL AUTO_INCREMENT PRIMARY KEY, 
  `NIP`             CHAR(20) NULL, 
  `Adres`           CHAR(100) NULL, 
  `Telefon`         CHAR(22) NULL , 
  `Specjalizacja`   CHAR(50) NULL, 
  `Nazwa_firmy`     CHAR(25) NULL)"

results = dbSendQuery(db_con,q_podwykonawcy)
dbClearResult(results)


### Wypłaty 

q_del_Wypłaty = "DROP TABLE IF EXISTS Wypłaty;"

results = dbSendQuery(db_con,q_del_wypłaty)
dbClearResult(results)

q_wypłaty = "CREATE TABLE `Wypłaty`
(`ID_Wypłaty`        INT NOT NULL AUTO_INCREMENT PRIMARY KEY, 
  `ID_pracownika`    INT NOT NULL,
  `ID_miesiąca`      INT NOT NULL,    
  `Stawka_godzinowa` FLOAT NULL   , 
  `Godziny`          SMALLINT NULL, 
  `Wypłata`          FLOAT NULL )"

results = dbSendQuery(db_con,q_Wypłaty)
dbClearResult(results)

### Pracownicy

q_del_pracownicy = "DROP TABLE IF EXISTS Pracownicy;"

results = dbSendQuery(db_con,q_del_pracownicy)
dbClearResult(results)

q_pracownicy = "CREATE TABLE `Pracownicy`
(`ID_pracownika` INT NOT NULL AUTO_INCREMENT PRIMARY KEY, 
 `ID_ekipy`   INT NULL,
 `ID_stanowiska` INT NULL,
 `Imię`       CHAR(30) NOT NULL, 
 `Nazwisko`   CHAR(30) NOT NULL, 
 `Pesel`      CHAR(11) NULL, 
 `Telefon`    CHAR(22) NULL,
 `Data_zatrudnienia` DATE NULL, 
 `Data_zwolnienia`   DATE NULL, 
 `Wiek`              TINYINT NULL, 
 `Stawka_godzinowa`  FLOAT NULL,
 `Rodzaj_umowy`      CHAR(30) NULL,
 `Status_studenta`   BIT NULL,
 `Praktyki_zawodowe` BIT NULL)"

results = dbSendQuery(db_con,q_pracownicy)
dbClearResult(results)


### wyposażenie

q_del_wyposażenie = "DROP TABLE IF EXISTS Wyposażenie;"

results = dbSendQuery(db_con,q_del_wyposażenie)
dbClearResult(results)

q_wyposażenie = "CREATE TABLE `Wyposażenie`
  (`ID_wyposażenia`   INT AUTO_INCREMENT PRIMARY KEY, 
  `ID_ekipy`   INT NULL   , 
  `Sprzęt`            CHAR(50) NULL  , 
  `Cena_zakupu`       FLOAT NULL   , 
  `Cena_sprzedaży`    FLOAT NULL   , 
  `Data_zakupu`       DATE NULL  , 
  `Data_sprzedaży`    DATE NULL );"

results = dbSendQuery(db_con,q_wyposażenie)
dbClearResult(results)

### Zlecenia

q_del_zlecenia = "DROP TABLE IF EXISTS Zlecenia;"

results = dbSendQuery(db_con,q_del_zlecenia)
dbClearResult(results)

q_zlecenia = "CREATE TABLE `Zlecenia`
(`ID_zlecenia`     INT NOT NULL AUTO_INCREMENT PRIMARY KEY , 
 `ID_zakres`       INT NULL,
 `ID_inwestora`    INT NOT NULL, 
 `ID_ekipy`        INT NULL , 
 `Adres`           CHAR(100) NULL, 
 `Kod_pocztowy`    CHAR(10) NULL, 
 `Data_przyjęcia`  DATE NULL, 
 `Pl_data_ukon`    DATE NULL, 
 `Data_rozp`       DATE NULL, 
 `Data_ukon`       DATE NULL, 
 `Wartość`         FLOAT NULL, 
 `Zaliczka`        FLOAT NULL, 
 `Data_zaliczki`   DATE NULL, 
 `Data_zapłaty`    DATE NULL, 
 `Kwota_kar`       FLOAT NULL, 
 `Koszty_materiały`  FLOAT NULL);"

results = dbSendQuery(db_con,q_zlecenia)
dbClearResult(results)


##Inwestorzy

q_del_inwestorzy = "DROP TABLE IF EXISTS Inwestorzy;"

results = dbSendQuery(db_con,q_del_inwestorzy)
dbClearResult(results)

q_inwestorzy = "CREATE TABLE `Inwestorzy`
(`ID_inwestora`    INT NOT NULL AUTO_INCREMENT PRIMARY KEY, 
 `Imię`            CHAR(30) NULL, 
 `Nazwisko`        CHAR(30) NULL, 
 `NIP`             CHAR(20) NULL, 
 `Telefon`         CHAR(22) NULL, 
 `Email`           CHAR(75) NULL, 
 `Adres`           CHAR(100) NULL, 
 `Ilość_inwestycji` SMALLINT NUll, 
 `Rodzaj_inwestora` CHAR(25) NULL);"

results = dbSendQuery(db_con,q_inwestorzy)
dbClearResult(results)


### Zakres_Remontu

q_del_zakres_remontu = "DROP TABLE IF EXISTS Zakres_remontu;"

results = dbSendQuery(db_con,q_del_zakres_remontu)
dbClearResult(results)


q_zakres_remontu = "CREATE TABLE `Zakres_remontu`
(`ID_zakres` INT NOT NULL AUTO_INCREMENT PRIMARY KEY, 
 `Zakres_remontu` CHAR(40) NULL,
 `Opis`      TEXT NOT NULL)"

results = dbSendQuery(db_con,q_zakres_remontu)
dbClearResult(results)

### Ekipy_remontowe

q_del_ekipy_remontowe = "DROP TABLE IF EXISTS Ekipy_remontowe;"

results = dbSendQuery(db_con,q_del_ekipy_remontowe)
dbClearResult(results)


q_ekipy_remontowe = "CREATE TABLE `Ekipy_remontowe`
(`ID_ekipy` INT NOT NULL PRIMARY KEY)"

results = dbSendQuery(db_con,q_ekipy_remontowe)
dbClearResult(results)


### Stanowiska 

q_del_stanowiska = "DROP TABLE IF EXISTS Stanowiska;"

results = dbSendQuery(db_con,q_del_stanowiska)
dbClearResult(results)

q_stanowiska = "CREATE TABLE `Stanowiska`
(`ID_stanowiska` INT NOT NULL AUTO_INCREMENT PRIMARY KEY, 
 `Stanowisko`    CHAR(30) NOT NULL, 
 `Opis`          TEXT NOT NULL)"

results = dbSendQuery(db_con,q_stanowiska)
dbClearResult(results)


### Inne_koszty

q_del_koszty_inne = "DROP TABLE IF EXISTS Koszty_inne;"

results = dbSendQuery(db_con,q_del_koszty_inne)
dbClearResult(results)

q_koszty_inne = "CREATE TABLE `Koszty_inne`
(`ID_koszty`   INT NOT NULL AUTO_INCREMENT PRIMARY KEY, 
 `ID_miesiąca`   INT NOT NULL, 
 `Paliwo`        FLOAT NULL, 
 `ZUS`           FLOAT NULL, 
 `Reklama`       FLOAT NULL, 
 `Biuro`         FLOAT NULL, 
 `Konto_bankowe` FLOAT NULL)"

results = dbSendQuery(db_con,q_koszty_inne)
dbClearResult(results)

##Miesiące

q_del_Miesiące = "DROP TABLE IF EXISTS Miesiące;"

results = dbSendQuery(db_con,q_del_Miesiące)
dbClearResult(results)

q_Miesiące = "CREATE TABLE `Miesiące`
(`ID_miesiąca`       INT NOT NULL AUTO_INCREMENT PRIMARY KEY, 
  `Miesiąc`          TINYINT NOT NULL, 
  `Rok`              INT NOT NULL)"

results = dbSendQuery(db_con,q_Miesiące)
dbClearResult(results)

#-------------------------------------------------------------------------------------
# Klucze obce

## Gwarancje

q_ob_ID_zlecenia_gwarancje = "ALTER TABLE Gwarancje
ADD FOREIGN KEY (ID_zlecenia) REFERENCES Zlecenia(ID_zlecenia);"

results = dbSendQuery(db_con,q_ob_ID_zlecenia_gwarancje)
dbClearResult(results)

q_ob_ID_ekipy_gwarancje = "ALTER TABLE Gwarancje
ADD FOREIGN KEY (ID_ekipy) REFERENCES Ekipy_remontowe(ID_ekipy);"

results = dbSendQuery(db_con,q_ob_ID_ekipy_gwarancje)
dbClearResult(results)

q_ob_ID_zakresu_gwarancje = "ALTER TABLE Gwarancje
ADD FOREIGN KEY (ID_zakresu) REFERENCES Zakres_remontu(ID_zakres);"

results = dbSendQuery(db_con,q_ob_ID_zakresu_gwarancje)
dbClearResult(results)


## Podwykonawcy_Zlecenia

q_ob_ID_zlecenia_podwyponawcy_zlecenia = "ALTER TABLE Podwykonawcy_zlecenia
ADD FOREIGN KEY (ID_zlecenia) REFERENCES Zlecenia(ID_zlecenia);"

results = dbSendQuery(db_con,q_ob_ID_zlecenia_podwyponawcy_zlecenia)
dbClearResult(results)

q_ob_ID_podwykonawcy_podwyponawcy_zlecenia = "ALTER TABLE Podwykonawcy_zlecenia
ADD FOREIGN KEY (ID_podwykonawcy) REFERENCES Podwykonawcy(ID_Podwykonawcy);"

results = dbSendQuery(db_con,q_ob_ID_podwykonawcy_podwyponawcy_zlecenia)
dbClearResult(results)


## Pracownicy

q_ob_ID_ekipy_pracownicy = "ALTER TABLE Pracownicy
ADD FOREIGN KEY (ID_ekipy) REFERENCES Ekipy_remontowe(ID_ekipy);"

results = dbSendQuery(db_con,q_ob_ID_ekipy_pracownicy)
dbClearResult(results)

q_ob_ID_stanowiska_pracownicy = "ALTER TABLE Pracownicy
ADD FOREIGN KEY (ID_stanowiska) REFERENCES Stanowiska(ID_Stanowiska);"

results = dbSendQuery(db_con,q_ob_ID_stanowiska_pracownicy)
dbClearResult(results)

## Wypłaty

q_ob_ID_pracownika_Wypłaty = "ALTER TABLE Wypłaty
ADD FOREIGN KEY (ID_pracownika) REFERENCES Pracownicy(ID_pracownika);"

results = dbSendQuery(db_con,q_ob_ID_pracownika_Wypłaty )
dbClearResult(results)

q_ob_ID_miesiąca_Wypłaty = "ALTER TABLE Wypłaty
ADD FOREIGN KEY (ID_miesiąca) REFERENCES Miesiące(ID_miesiąca);"

results = dbSendQuery(db_con,q_ob_ID_miesiąca_Wypłaty)
dbClearResult(results)


## wyposażenie

q_ob_ID_ekipy_wyposażenie = "ALTER TABLE wyposażenie
ADD FOREIGN KEY (ID_ekipy) REFERENCES Ekipy_remontowe(ID_ekipy);"

results = dbSendQuery(db_con,q_ob_ID_ekipy_wyposażenie )
dbClearResult(results)

## Zlecenia

q_ob_ID_zakres_zlecenia = "ALTER TABLE Zlecenia
ADD FOREIGN KEY (ID_zakres) REFERENCES Zakres_remontu(ID_zakres);"

results = dbSendQuery(db_con,q_ob_ID_zakres_zlecenia)
dbClearResult(results)

q_ob_ID_inwestora_zlecenia = "ALTER TABLE Zlecenia
ADD FOREIGN KEY (ID_inwestora) REFERENCES Inwestorzy(ID_inwestora);"

results = dbSendQuery(db_con,q_ob_ID_inwestora_zlecenia)
dbClearResult(results)

q_ob_ID_ekipy_zlecenia = "ALTER TABLE Zlecenia
ADD FOREIGN KEY (ID_ekipy) REFERENCES Ekipy_remontowe(ID_ekipy);"

results = dbSendQuery(db_con,q_ob_ID_ekipy_zlecenia)
dbClearResult(results)

## Koszty_inne


q_ob_ID_miesiąca_koszty_inne = "ALTER TABLE Koszty_inne
ADD FOREIGN KEY (ID_miesiąca) REFERENCES Miesiące(ID_miesiąca);"

results = dbSendQuery(db_con,q_ob_ID_miesiąca_koszty_inne)
dbClearResult(results)


# ------------------------------------------------
# DODAWANIE DANYCH


#---------------------------
#TABELA "ZAKRESY REMONTU"

Zakres_remontu <- zakresy_remontow
Opis <- zakresy_opisy
Zakres_remontu_dane <- data.frame(Zakres_remontu, Opis)
dbWriteTable(db_con, "Zakres_remontu", Zakres_remontu_dane, row.names=FALSE, append = TRUE)


#TABELA "STANOWISKA"

Stanowisko <- stanowisko 
Opis <- Opisy_stanowisk

Stanowiska_dane <- data.frame(Stanowisko, Opis)
dbWriteTable(db_con, "Stanowiska", Stanowiska_dane, row.names=FALSE, append = TRUE)


#TABELA "INWESTORZY"

Imię <- imiona_inwestorow
Nazwisko <- nazwiska_inwestorow
NIP <- inwestorzy_nipy
Telefon <- numery_tel_inwestorzy_char
Email <- maile_inwestorzy
Adres <- inwestorzy_adresy
Ilość_inwestycji <- ilosc_zlecen_inwestorzy
Rodzaj_inwestora <- rodzaj_inwestora
Inwestorzy_dane <- data.frame(Imię, Nazwisko, NIP, Telefon, Email, Adres, Ilość_inwestycji,
                              Rodzaj_inwestora)
dbWriteTable(db_con, "Inwestorzy", Inwestorzy_dane, row.names=FALSE, append = TRUE)


#TABELA "PODWYKONAWCY'

NIP <- podwyk_nipy 
Adres <- podwyk_adresy
Telefon <- podwyk_numery_tel
Specjalizacja <- podwyk_specjalizacje
Nazwa_firmy <- podwyk_nazwy_firmy
Podwykonawcy_dane <- data.frame(NIP, Adres, Telefon, Specjalizacja, Nazwa_firmy)

dbWriteTable(db_con, "Podwykonawcy", Podwykonawcy_dane, row.names=FALSE, append = TRUE)

#TABELA "EKIPY REMONTOWE
ID_ekipy <- id_ekipy
Ekipy_remontowe_dane <- data.frame(ID_ekipy)
dbWriteTable(db_con, "Ekipy_remontowe", Ekipy_remontowe_dane, row.names=FALSE, append = TRUE)


#TABELA "ZLECENIA"

ID_zakres <- zakresy_zlecenia
ID_inwestora <- id_inwestora_zlecenia
ID_ekipy <- id_ekipy_zlecenia
Adres <- adresy_zlecenia
Kod_pocztowy <- kody_pocztowe_zlecenia
Data_przyjęcia <- przyjecia_zlecen_data
Pl_data_ukon <- konce_remontow_planowane_data
Data_rozp <- wlasciwe_starty_remontow_data
Data_ukon <- konce_remontow_data
Wartość <- zaplaty
Zaliczka <- zaliczki
Data_zaliczki <- daty_zaliczek_data
Data_zapłaty <- daty_wplaty_reszty_data
Kwota_kar <- kwoty_kar
Koszty_materiały <- koszty_remontu
Zlecenia_dane <- data.frame(ID_zakres, ID_inwestora, ID_ekipy, Adres, Kod_pocztowy, Data_przyjęcia,
                            Pl_data_ukon, Data_rozp, Data_ukon, Wartość, Zaliczka, Data_zaliczki, 
                            Data_zapłaty, Kwota_kar, Koszty_materiały)
dbWriteTable(db_con, "Zlecenia", Zlecenia_dane, row.names=FALSE, append = TRUE)
