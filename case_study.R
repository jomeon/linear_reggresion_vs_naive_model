install.packages('dplyr')
install.packages('zoo')
install.packages('ggplot2')
install.packages("forecast")
install.packages("rmarkdown")
library(zoo)
library(dplyr)
library(ggplot2)
library(forecast)
#ustawianie seedu


# odrzucenie i uzasadnienie:

#Analiza modelu losowego może nie być adekwatna do charakteru zgromadzonych danych. W kontekście informacji o 
#przepływie wody  oraz opadach atmosferycznych w wieloletnim okresie czasowym w Conon at Moy Bridge.
#Planuję zastosować modele naiwny oraz regresji liniowej, aby dokładniej zbadać i porównać ze sobą zebrane dane. #Celem jest zidentyfikowanie ewentualnych korelacji między przepływem wody a opadami atmosferycznymi. Dzięki tym #algorytmom będę w stanie dokładniej zrozumieć zależności między tymi zmiennymi oraz lepiej ocenić ich wzajemne #oddziaływanie.
#Ostateczny wybór tych metod opiera się na ich zdolności do analizy szeregów czasowych oraz umożliwieniu #szczegółowego zbadania ewentualnych związków między tymi zmiennymi. W przypadku anulowania poprzedniego modelu, te #techniki pozwalają na bardziej szczegółową analizę danych i wykrycie ewentualnych powiązań, co przyczyni się do #lepszego zrozumienia dynamiki między przepływem wody a opadami atmosferycznymi w badanej lokalizacji.
#Model ANOVA (ANalysis Of VAriance) jest narzędziem statystycznym używanym głównie do porównywania średnich wartości między trzema lub więcej grupami. Jest on szczególnie użyteczny, gdy chcemy zrozumieć, czy istnieją istotne różnice między średnimi wartościami zmiennych w różnych grupach. Jednakże, jego zastosowanie może być ograniczone w przypadku analizy szeregów czasowych, takich jak dane dotyczące przepływu wody i opadów atmosferycznych w wieloletniej perspektywie czasowej.
#Istnieje kilka powodów, dla których model ANOVA może być niewystarczający do analizy tych danych:
#  Zmienna czasowa: Modele ANOVA nie uwzględniają zmiennych czasowych ani struktury sekwencyjnej danych, co jest kluczowe w przypadku analizy szeregów czasowych. Szeregi czasowe charakteryzują się zależnościami czasowymi i trendami, których nie można w pełni uwzględnić za pomocą ANOVA.
#Brak uwzględnienia sekwencji: Model ANOVA zakłada niezależność obserwacji, co może nie być prawdziwe w przypadku danych szeregów czasowych, gdzie pomiary w kolejnych okresach czasowych są ze sobą powiązane.
#Uwzględnienie dynamiki czasowej: Modele ANOVA nie są w stanie uchwycić zmian w czasie ani prognozować przyszłych wartości, co jest istotne w analizie szeregów czasowych, takich jak te dotyczące przepływu wody czy opadów atmosferycznych.
#Dlatego też wybór modelu naiwnego oraz regresji liniowej wydaje się bardziej adekwatny do analizy danych szeregów czasowych. Te modele są specjalnie zaprojektowane do obsługi danych sekwencyjnych, pozwalając na uwzględnienie trendów, sezonowości i zależności czasowych, co pozwoli lepiej zrozumieć dynamikę między przepływem wody a opadami atmosferycznymi w badanej lokalizacji.


set.seed(123)


#wczytywanie danych i porzadkowanie
daily_rain <- read.csv("C:/Users/szymo/Downloads/daily_rain_4001_cdr.csv", skip=19)
river_flow <- read.csv("C:/Users/szymo/Downloads/river_flow1947_2018_4001_gdf.csv", skip = 19)
river_flow 
daily_rain

daily_rain <- daily_rain[, -ncol(daily_rain)]
river_flow <- river_flow[, -ncol(river_flow)]
summary(daily_rain)

#odfiltrowanie dat od 1947-10-01 bo to minimum w river flow
daily_rain$data <- as.Date(daily_rain$data , format = "%Y-%m-%d")

#Odfiltruj dane, wybierając daty od 1947-10-01 do najnowszych
filtered_data_rain <- daily_rain %>%
  filter(data >= as.Date("1947-10-01"))z

#Identycznie z river flow, do 2017-12-31 bo to max daily rain
river_flow$data <- as.Date(river_flow$data , format = "%Y-%m-%d")

#Odfiltrowanie danych, wybierając daty < 2017-12-31
filtered_data_river <- river_flow %>%
  filter(data <= as.Date("2017-12-31"))

#badanie braków danych w river
any(is.na(filtered_data_river$last) | is.nan(filtered_data_river$last))
#wystapily braki danych (true)
total_missing <- sum(is.na(filtered_data_river$last) | is.nan(filtered_data_river$last), na.rm = TRUE)
total_missing #2041 brakow danych

#Badanie braków danych w rain
any(is.na(filtered_data_rain$last) | is.nan(filtered_data_rain$last))
#Brak braku danych (false)

#Usuwanie braku danych dla długich przedziałów czasowych, gdzie nie było pomiarów
#river
filtered_data_river <- filtered_data_river %>%
  filter(!(data >= as.Date("1970-10-01") & data <= as.Date("1975-12-31")) &
           !(data >= as.Date("1958-05-01") & data <= as.Date("1958-05-31")) &
           !(data >= as.Date("1955-04-01") & data <= as.Date("1955-06-30")))

#rain
filtered_data_rain <- filtered_data_rain %>%
  filter(!(data >= as.Date("1970-10-01") & data <= as.Date("1975-12-31")) &
           !(data >= as.Date("1958-05-01") & data <= as.Date("1958-05-31")) &
           !(data >= as.Date("1955-04-01") & data <= as.Date("1955-06-30")))

#Interpolowanie wartosci dla konkretnej daty
interpolated_value <- na.approx(filtered_data_river$last, x = filtered_data_river$data, xout = as.Date("1949-12-20"))

#Aktualizacja wartości dla konkretnej daty "1949-12-20" w kolumnie "last"
filtered_data_river[filtered_data_river$date == as.Date("1949-12-20"), "last"] <- interpolated_value

#Zmiana nazw kolumn przed mergem
filtered_data_river <- filtered_data_river %>% rename(river_flow = last)
filtered_data_rain <- filtered_data_rain %>% rename(rain = last)

#Mergowanie w jedne dane
merged_df <- merge(filtered_data_river, filtered_data_rain, by = "data", all = TRUE)

#Korelacje
coefficient <- cor.test(merged_df$river_flow, merged_df$rain)
coefficient$estimate
#0.4233146 cor

#Dzielenie zbioru danych na testowy i treningowy

# Tworzenie indeksu losowego dla podziału danych  80% danych do treningu
indexes <- sample(nrow(merged_df), size = 0.8 * nrow(merged_df))

# Tworzenie zbioru treningowego i testowego na podstawie indeksów
train_data <- merged_df[indexes, ]
test_data <- merged_df[-indexes, ]

#regresja liniowa
linear_r <- lm(river_flow ~ rain, data = train_data)
summary(linear_r)
summary(linear_r)$r.squared
#0.175 is explained by the model (values)

#Wnioski z analizy tego modelu mogą obejmować to, że opady deszczu wydają się mieć istotny wpływ na przepływ rzeki, ale wciąż jest to tylko jeden z wielu czynników wpływających na tę zmienną. Model może mieć ograniczenia, a istnienie innych zmiennych mogących wpływać na przepływ rzeki może być powodem niskiego współczynnika determinacji. Może być konieczne uwzględnienie dodatkowych zmiennych w modelu, aby lepiej wyjaśnić zmienność w przepływie rzeki.
plot(train_data$rain, train_data$river_flow, 
     xlab = "Opady deszczu", ylab = "Przepływ rzeki",
     main = "Wykres rozrzutu: Opady deszczu vs Przepływ rzeki")

#Dodanie linii regresji
abline(lm(river_flow ~ rain, data = train_data), col = "red")


#Predykcja na zbiorze testowym
#4 losowe wartosci dla zbioru testowego
random_indices <- sample(1:nrow(test_data), 4)
random_test_data <- test_data[random_indices, ]
predictions <- predict(linear_r, newdata = random_test_data)

#Obliczanie bledow dla calego zbioru testowego
linear_rmse <- sqrt(mean((test_data$river_flow - predictions)^2))
print(linear_rmse)
#39.58 dla ogolnego zbioru

#Plot z porownywaniem wartosci losowych wybranych ze zbioru testowego (4) z predykcja(4)
plot(random_test_data$river_flow, predictions, 
     xlab = "Rzeczywiste wartości", ylab = "Przewidywane wartości",
     main = "Porównanie rzeczywistych i przewidywanych wartości")
points(random_test_data$river_flow, predictions, col = "red", pch = 16)

# Dodanie linii prostoliniowej o nachyleniu 1 dla porównania idealnej predykcji
abline(a = 0, b = 1, col = "red")

# Obliczanie reszt
residuals <- random_test_data$river_flow - predictions

# Wykres reszt
plot(random_test_data$data, residuals, 
     xlab = "Data", ylab = "Reszty",
     main = "Wykres reszt dla modelu regresji liniowej")
abline(h = 0, col = "red")

#reszty odbiegaja od 0, do ktorego powinny zmierzac
#Model regresji liniowej sprawdza się najlepiej, gdy zmienna zależna (w tym przypadku przepływ rzeki) jest liniowo związana ze zmienną niezależną (opady deszczu).

######### MODEL NAIWNY
naive_model <- naive(train_data$river_flow)

# Predykcja na zbiorze testowym
naive_predictions <- forecast(naive_model, h = length(random_test_data$river_flow))$mean

# Obliczenie RMSE dla modelu naiwnego
naive_rmse <- sqrt(mean((random_test_data$river_flow - naive_predictions)^2))
naive_rmse


#wykrescaly


ggplot() +
  geom_line(aes(x = random_test_data$data, y = random_test_data$river_flow), color = "blue") +
  geom_line(aes(x = random_test_data$data, y = predict(linear_r, newdata = random_test_data)), color = "red") +
  geom_line(aes(x = random_test_data$data, y = forecast(naive_model, h = length(random_test_data$river_flow))$mean), color = "green") +
  labs(title = "Porównanie modeli", x = "Data", y = "Przepływ rzeki")
scale_color_manual(values = c("blue" = "Actual", "red" = "Linear Regression", "green" = "Naive Model"))
#####


linear_predictions <- predict(linear_r, newdata = random_test_data)
linear_rmse <- sqrt(mean((random_test_data$river_flow - linear_predictions)^2))
linear_mae <- mean(abs(random_test_data$river_flow - linear_predictions))
linear_mape <- mean(abs((random_test_data$river_flow - linear_predictions) / random_test_data$river_flow)) * 100

# Obliczanie metryk błędów dla modelu naiwnego
naive_predictions <- forecast(naive_model, h = length(random_test_data$river_flow))$mean
naive_rmse <- sqrt(mean((random_test_data$river_flow - naive_predictions)^2))
naive_mae <- mean(abs(random_test_data$river_flow - naive_predictions))
naive_mape <- mean(abs((random_test_data$river_flow - naive_predictions) / random_test_data$river_flow)) * 100

####

cat("Model regresji liniowej:\n")
cat("RMSE: ", linear_rmse, "\n")
cat("MAE: ", linear_mae, "\n")
cat("MAPE: ", linear_mape, "%\n")

cat("\nModel naiwny:\n")
cat("RMSE: ", naive_rmse, "\n")
cat("MAE: ", naive_mae, "\n")
cat("MAPE: ", naive_mape, "%\n")

### ANALIZA OBU

#Błąd średniokwadratowy (RMSE): RMSE jest miarą różnicy między wartościami przewidywanymi przez model a rzeczywistymi wartościami. Dla modelu regresji liniowej RMSE wynosi 22.46, a dla modelu naiwnego wynosi 32.42. Im mniejsza wartość RMSE, tym lepiej model przewiduje wyniki. Zatem, model regresji liniowej ma lepszą wydajność w porównaniu do modelu naiwnego pod względem RMSE.

#Błąd bezwzględny średniej (MAE): MAE jest średnią wartością bezwzględnych różnic między przewidywaniami a rzeczywistymi wartościami. Dla modelu regresji liniowej MAE wynosi 19.34, a dla modelu naiwnego wynosi 21.42. Podobnie jak w przypadku RMSE, mniejsza wartość MAE wskazuje na lepszą wydajność modelu. Zatem, model regresji liniowej również ma lepszą wydajność pod względem MAE.

#Błąd procentowy średniej absolutnej (MAPE): MAPE jest średnią wartością bezwzględnych różnic między przewidywaniami a rzeczywistymi wartościami, wyrażoną jako procent rzeczywistych wartości. Dla modelu regresji liniowej MAPE wynosi 36.89%, a dla modelu naiwnego wynosi 28.59%. W przeciwnym do RMSE i MAE przypadku, większa wartość MAPE wskazuje na gorszą wydajność modelu. Zatem, model naiwny ma lepszą wydajność pod względem MAPE.
###

###
# Wykres porównawczy dla modelu naiwnego
plot(random_test_data$river_flow, naive_predictions, 
     xlab = "Rzeczywiste wartości", ylab = "Przewidywane wartości",
     main = "Porównanie rzeczywistych i przewidywanych wartości dla modelu naiwnego")
points(random_test_data$river_flow, naive_predictions, col = "red", pch = 16)
abline(a = 0, b = 1, col = "red")



#