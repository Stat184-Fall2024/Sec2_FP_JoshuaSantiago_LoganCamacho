raw_data <- read.table('Car_Models.csv', sep = ",", header = 1)
data_clean <- raw_data %>% #Separating unit of measurement from number
  separate_wider_delim(
    cols = Horsepower,
    delim = " ",
    names = c("Horsepower", "Unit"),
    too_few = "align_start",
    too_many = "merge"
  ) %>%
  separate_wider_delim(
    cols = Torque,
    delim = " ", 
    names = c("Torque", "Units"),
    too_few = "align_start",
    too_many = "merge"
  ) 
#Taking the average value of rows with ranges given
averages_clean <- data_clean[c(17,19,20,22,24:28,30:32,35,37:41,43,47,49,69:73,91:97,114:115,117:118,132:145,227),] %>%
  separate_wider_delim(
    cols = Horsepower,
    delim = "-",
    names = c("lower", "upper"),
    too_few = "align_start"
  ) %>%
  separate_wider_delim(
    cols = Torque,
    delim = "-",
    names = c("lower1", "upper1"),
    too_few = "align_start"
  ) %>%
  #rowwise() %>%
  mutate( #setting na values to 0 and finding the averages 
    lower = coalesce(as.numeric(lower), 0),
    upper = coalesce(as.numeric(upper), 0),
    Horsepower = (as.numeric(lower) + as.numeric(upper)) / 2,
    lower1 = coalesce(as.numeric(lower1), 0),
    upper1 = coalesce(as.numeric(upper1), 0),
    Torque = (as.numeric(lower1) + as.numeric(upper1)) / 2
  )
#Cleaning rows whose ranges were seperated from the initial sep
ranges_clean <- data_clean[c(100:104, 108:111),] %>%
  unite(
    col = "Temp",
    Horsepower,
    Unit,
    sep = " ",
    na.rm = TRUE
  ) %>%
  unite(
    col = "Temp1",
    Torque, 
    Units,
    sep = " ",
    na.rm = TRUE
  ) %>%
  separate_wider_delim(
    cols = Temp,
    delim = " - ",
    names = c("temp_lower", "temp_upper"),
    too_few = "align_start"
  ) %>%
  separate_wider_delim(
    cols = Temp1,
    delim = " - ",
    names = c("temp_lower1", "temp_upper1"),
    too_few = "align_start"
  ) %>%
  separate_wider_delim(
    cols = temp_lower,
    delim = " ",
    names = c("lower", "lower_unit"),
    too_few = "align_start"
  ) %>%
  separate_wider_delim(
    cols = temp_upper,
    delim = " ",
    names = c("upper", "Unit"),
    too_few = "align_start"
  ) %>%
  separate_wider_delim(
    cols = temp_lower1,
    delim = " ",
    names = c("lower1", "lower1_unit"),
    too_few = "align_start"
  ) %>%
  separate_wider_delim(
    cols = temp_upper1,
    delim = " ",
    names = c("upper1", "Units"),
    too_few = "align_start"
  ) %>%
  mutate( 
    Horsepower = (as.numeric(lower) + as.numeric(upper)) / 2,
    Torque = (as.numeric(lower1) + as.numeric(upper1)) / 2
  )
#appending the ranges_clean and averages_clean back to the data_clean and dropping the uncleaned values
averages_clean_append <- averages_clean[-c(3:4, 6:7)]
ranges_clean_append <- ranges_clean[-c(3:5, 7:9)]
data_clean <- rbind(data_clean, averages_clean_append)
data_clean <- rbind(data_clean, ranges_clean_append)
data_clean <- data_clean[-c(17,19,20,22,24:28,30:32,35,37:41,43,47,49,69:73,91:97,114:115,117:118,132:145,227,100:104,108:111),]

#Converting Unit na values to hp and Units na values to lb-ft
data_clean <- data_clean %>%
  mutate_at(
    c("Unit"), ~replace_na(.,"hp")
  ) %>%
  mutate_at(
    c("Units"), ~replace_na(.,"lb-ft")
  )
#Converting all Horsepower values to hp measurements and Torque values to lb-ft measurements
hp_convert <- data_clean %>%
  filter(Unit != "hp") %>%
  mutate(
    Horsepower = as.numeric(Horsepower) * 0.98632007, #conversion value found online
    Unit = "hp"
  )
data_clean <- rbind(data_clean, hp_convert)

torque_convert <- data_clean %>%
  filter(Units != "lb-ft")%>%
  mutate(
    Torque = as.numeric(Torque) * 0.7375621493, #conversion value found online
    Units = "lb-ft"
  )
data_clean <- rbind(data_clean, torque_convert)
data_clean <- data_clean[-c(11:16, 80, 276:284, 336:343, 405, 407:411, 413:417, 420, 424:429, 433:436, 440),]
data_clean <- data_clean[-c(382),] #missed one value when removing duplicates

#Renaming Horsepower and Torque columns to include unit of measurement and dropping Unit and Units columns
names(data_clean)[3] <- "Horsepower_(hp)"
names(data_clean)[5] <- "Torque_(lb-ft)"
data_clean <- data_clean[-c(4,6)]
