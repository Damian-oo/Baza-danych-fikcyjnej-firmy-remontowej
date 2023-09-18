# Baza-danych-fikcyjnej-firmy-remontowej

Opis projektu: 

Ten grupowy projekt relizowany był w styczniu i lutym 2021 roku. Miał on na celu stworzenie relacyjnej bazy danych dla pewnej instytucji, w naszym wypadku była to fikcyjna firma remontowa. Jako bazę danych wybraliśmy **MariaDB**, składała się ona z 13 tabel, które są przedstawione wraz z odpowiednimi relacjami na diagramie ER (*Entity Relationship*). Moim zadaniem było stworzenie skyptu generującego syntetyczne i zrandomizowane dane, a następnie załadowanie ich do odpowiednich tabel naszej bazy danych. Do stworzenia całego skryptu wykorzystałem jezyk **R**. Żeby stworzyć połączenie między **R** i **MariaDB** oraz załadować fikcyjne dane użyłem funkcji *dbConnect*, *dbSendQuery*, *dbClearResult* i *dbWriteTable* z pakietu **RMariaDB**.

Dane dotyczące adresów z miasta zostały zaczerpnięte z Ewidencji Miejscowości, Ulic i Adresów Wrocławia (EWUiA), stan na 02.01.2021.
Źródło: https://geoportal.wroclaw.pl/emuia/
