promenna <- Math$SATM

# Extrémy
minimum <- min(promenna)
maximum <- max(promenna)

# Kvartily ... dolní (Q1), horní (Q3); 25%,75% dat je menší než tento počet
dolni_kvartil <- quantile(promenna, 0.25)
horni_kvartil <- quantile(promenna, 0.75)

# Medián = Q2; prostřední hodnota seřazených dat ; robustní vůči odlehlým hodnotám
median <- median(promenna)

# Průměr -  součet hodnot / počet hodnot ; citlivý na odlehlé hodnoty
prumer <- mean(promenna)

# summary() - poskytuje minimum, 1. kvantil, 2.kvantil = medián, průměr, 3. kvantil, maximum
# summary(promenna)

# ROZSAH (Range) = rozdíl mezi maximální a minimální hodnotou v souboru
rozsah <- max(promenna) - min(promenna)

# ROZPTYL = průměrná kvadratická odchylka od průměru, jak moc jsou hodnoty rozptýlené kolem průměru
rozptyl <- var(promenna)

# SMĚRODATNÁ ODCHYLKA = odmocnina z rozptylu; jak moc se průměrně liší hodnoty od průměru
smerodatna_odchylka <- sd(promenna)

# IQR (interkvartilní rozsah) = robustní
IQR <- horni_kvartil - dolni_kvartil

# MAD = robustní míra variability, odchylka od mediánu
mad <- mad(promenna)

# VARIAČNÍ KOEFICIENT = CoefVar(), relativní rozptyl dat vůči průměru, v %, možnost srovnávat různé datasety, jednotky
variacni_koeficient <- sd(promenna) / mean(promenna) * 100

# Dolní a horní mez pro mezikvartilní rozsah
dolni_mez <- dolni_kvartil - 1.5 * IQR
horni_mez <- horni_kvartil + 1.5 * IQR

popisne_statistiky <- c("n", "Průměr", "Medián", "Minimum", "Maximum", "Dolní Kvantil", "Horní Kvantil", "Rozsah", "Rozptyl", "Standardní odchylka", "IQR","MAD", "Dolní mez", "Horní mez", "Var. koef.")

hodnota <- c(length(promenna), prumer, median, minimum, maximum, dolni_kvartil, horni_kvartil, rozsah, rozptyl, smerodatna_odchylka, IQR,mad, dolni_mez, horni_mez,  variacni_koeficient)

popisna_statistika <- data.frame(popisne_statistiky, hodnota)

popisna_statistika

hist(promenna, main = "histogram pro proměnnou ACTM")

boxplot(promenna, main = "boxový graf pro proměnnou ACTM")

#------------------------------------------------------------------------------------------------------------------------------------------------

#bezne absolutní četnosti - počet výskytů konkrétních hodnot v sadě dat
table(cars$Type)
#kumulativní absolutní četnost - součet výskytů všech hodnot až do i-té hodnoty proměnné
cumsum(table(cars$Type))
#běžné relativní četnosti - procentuální podíl počtu výskytů konkrétní hodnoty ze všech hodnot v sadě
round(prop.table(table(cars$Type)),4)
#kumulativní relativní četnost - součet procentuálního podílu všech hodnot až do i-té hodnoty proměnné
cumsum(round(prop.table(table(cars$Type)),4))

#celá tabulka 
cbind("běžné relativní četnosti" =table(cars$Type), "kumulativní absolutní četnost"=cumsum(table(cars$Type)),
     "běžné relativní četnosti" = round(prop.table(table(cars$Type)),4), "kumulativní relativní četnost"=cumsum(round(prop.table(table(cars$Type)),4)))


barplot(table(cars$Type),col="purple",main="Sloupcovy graf pro promennou
Type",ylab="Pocty")

#absolutní četnost
popis<-paste(sort(unique(cars$Origin)),table(cars$Origin),"výskytů")
pie(table(cars$Origin),lab=popis,col="white",main="Kolacovy graf pro promennou Type")
#relativní četnost
popis<-paste(sort(unique(cars$Origin)),"(",round(prop.table(table(cars$Origin))*100,2),"%)")
pie(table(cars$Origin),lab=popis,col="white",main="Kolacovy graf pro promennou Type")

#Frekvenční křivka - vizuální zobrazení četnosti
type_counts <- table(factor(cars$Type, levels = c("Compact", "Small", "Sporty", "Midsize", "Large", "Van")))
# Seřadit tak aby hodnoty měli toto pořadí, ať to dává smysl v grafu
# Compact Small Sporty Midsize Large Van
# Vytvoření frekvenční křivky (spojnicového grafu)
plot(type_counts,
     type = "o", # Typ grafu: spojnicový s body
     main = "Frekvenční křivka pro cars$Type",
     xlab = "Typ auta",
     ylab = "Frekvence",
     col = "blue",
     lwd = 2, # Šířka čáry
     pch = 16) # Typ bodu


    
    
    
    
    
    
    
    
    
    
    
    
    
    
