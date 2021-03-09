# Load, filter, and reformat GHCN-D data ####

# install.packages("LaF")

library(LaF)
library(tidyverse)
library(lubridate)
library(ggplot2)

source(here::here("utils", "read_data.R"))



stations <- read_ghcnd_stations() %>%
  filter_station_subset()
summary(stations)

data <- read_data_subset(stations)
station_IDs <- unique(data$ID)

data2 <- data
station_IDs <- unique(data2$ID)

head(d1 <- rearrange_data_1(data2, stations, "TMIN"))
head(d2 <- rearrange_data_2(data2, stations, "TMIN"))

# Find common data subsets

d2_tmp <- cbind(d2, "UKE00000000" = rowMeans(!is.na(d2[,station_IDs])))
for (id in station_IDs) {
  d2_tmp[,id] <- !is.na(d2[,id])
}
d2_tmp <-
  d2_tmp %>%
  pivot_longer(cols = c("UKE00000000", station_IDs),
               names_to = "Station",
               values_to = "Present")
ggplot(d2_tmp) +
  geom_point(aes(DecYear, Present, group = Station)) +
  facet_wrap(vars(Station))
ggplot(d2_tmp %>% group_by(Station, Month) %>% summarise(Present = mean(Present))) +
  geom_point(aes(Month, Present, group = Station)) +
  facet_wrap(vars(Station))
ggplot(d2_tmp %>% group_by(Station, Year) %>% summarise(Present = mean(Present))) +
  geom_point(aes(Year, Present, group = Station)) +
  facet_wrap(vars(Station))

# These old comments are a bit unmatched now
#plot(d2$DecYear, !is.na(d2[,station_IDs[1]])) # Recent
#plot(d2$DecYear, !is.na(d2[,station_IDs[2]])) # Recent, short
#plot(d2$DecYear, !is.na(d2[,station_IDs[3]])) # Not recent, long
#plot(d2$DecYear, !is.na(d2[,station_IDs[4]])) # Full
#plot(d2$DecYear, !is.na(d2[,station_IDs[5]])) # Full
#plot(d2$DecYear, !is.na(d2[,station_IDs[6]])) # Full except gap
#plot(d2$DecYear, !is.na(d2[,station_IDs[7]])) # Full
#plot(d2$DecYear, !is.na(d2[,station_IDs[8]])) # Full
#plot(d2$DecYear, !is.na(d2[,station_IDs[9]])) # Full
#plot(d2$DecYear, !is.na(d2[,station_IDs[10]])) # Not recent, long
#plot(d2$DecYear, !is.na(d2[,station_IDs[11]])) # Full
#plot(d2$DecYear, !is.na(d2[,station_IDs[12]])) # Nearly full, recent missing
#plot(d2$DecYear, !is.na(d2[,station_IDs[13]])) # Not recent, long
#plot(d2$DecYear, !is.na(d2[,station_IDs[14]])) # Recent, continuation of 3

# Convert BRAEMAR-1 UKE00115807 to continuation of BRAEMAR UKE00105874

d1_ <- d1 %>% mutate(
  Name = if_else(Name == "BRAEMAR-1", true = "BRAEMAR", false = Name),
  ID = if_else(ID == "UKE00115807", true = "UKE00105874", false = ID)
)

#ok_IDs <- station_IDs[c(4,5,7,8,9,11)]

ok_IDs <-
  d1_ %>% group_by(ID) %>%
  summarise(Name = Name[1], Start = min(DecYear), End = max(DecYear)) %>%
  filter(Start <= 1960, End >= 2017.99) %>%
  pull(ID)

# Extract the data

head(d0 <- rearrange_data_0(data2 %>% filter(ID %in% c(ok_IDs, "UKE00115807"), DecYear >= 1960),
                            stations,
                            c("TMIN", "TMAX", "PRCP")))
d0_ <- d0 %>% mutate(
  Name = if_else(Name == "BRAEMAR-1", true = "BRAEMAR", false = Name),
  ID = if_else(ID == "UKE00115807", true = "UKE00105874", false = ID)
)
unique(d0_$Name)


ghcnd_stations <-
  stations %>%
  select(ID, Name, Latitude, Longitude, Elevation) %>%
  filter(ID %in% ok_IDs)
ghcnd_stations

ghcnd_values <-
  d0_ %>%
  select(ID, Year, Month, Day, DecYear, TMAX, TMIN, PRCP) %>%
  pivot_longer(cols = c(TMAX, TMIN, PRCP),
               names_to = "Element",
               values_to = "Value") %>%
  filter(!is.na(Value))

joined <- left_join(ghcnd_values, ghcnd_stations, by = "ID")
joined %>%
  head()


joined %>%
  group_by(ID,Name,Year,Element) %>%
  summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
  filter(Element %in% c("TMIN", "TMAX")) %>%
  ggplot(aes(Year, Value, col = Element)) +
  geom_point() +
  facet_wrap(vars(Name))

joined %>%
  group_by(ID,Name,Year,Element) %>%
  summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(Year, Value, col = Name)) +
  geom_point() +
  facet_wrap(vars(Element))


if (FALSE) {
  use_data(ghcnd_stations, ghcnd_values, compress = "xz")
}






# Old code ####

if(FALSE) {

plot(d2$DecYear, rowSums(!is.na(d2[,ok_IDs])))
hist(rowSums(!is.na(d2[,ok_IDs])))
sum(6 == rowSums(!is.na(d2[,ok_IDs]))) # 19465 ok obs
ok_DecYear <- d2[rowSums(!is.na(d2[,ok_IDs])) == 6, "DecYear", drop=TRUE]

data3 <- data2 %>%
  filter(ID %in% ok_IDs, DecYear %in% ok_DecYear)

# Extract the data
head(d1 <- rearrange_data_1(data3, stations, "TMIN"))
head(d2 <- rearrange_data_2(data3, stations, "TMIN"))
head(d0 <- rearrange_data_0(data3, stations, c("TMIN", "TMAX", "PRCP")))

d0 %>%
  pivot_wider(names_from = Element, values_from = Value)


ggplot(d0 %>% filter(!is.na(TMAX) & !is.na(PRCP))) +
  geom_point(aes(TMIN, PRCP, col = Name)) +
  facet_wrap(vars(factor(1 + (floor(DecYear*12) %% 12))))

ggplot(d0 %>% filter(!is.na(TMAX) & !is.na(PRCP))) +
  geom_density_2d(aes(TMIN, PRCP, col = Name)) +
  facet_wrap(vars(Name, factor(1 + (floor(DecYear*12) %% 12))))




# Split into training/test
set.seed(12345L)
obs_times <- sample(d2$DecYear, size = 20000*4/5, replace = FALSE)

if (FALSE) {
  obs1 <- d1$DecYear %in% obs_times
  obs2 <- d2$DecYear %in% obs_times
  write.csv(d1[obs1,], file = "TMINallobs.csv", row.names = FALSE)
  write.csv(d1[!obs1,], file = "TMINalltest.csv", row.names = FALSE)
  write.csv(d2[obs2,], file = "TMINoneobs.csv", row.names = FALSE)
  # write.csv(d2[!obs2,], file = "TMINonetest.csv", row.names = FALSE)
}

TMINallobs <- read.csv("TMINallobs.csv", header = TRUE, stringsAsFactors = FALSE)
TMINalltest <- read.csv("TMINalltest.csv", header = TRUE, stringsAsFactors = FALSE)
TMINoneobs <- read.csv("TMINoneobs.csv", header = TRUE, stringsAsFactors = FALSE)
TMINonetest <- read.csv("TMINonetest.csv", header = TRUE, stringsAsFactors = FALSE)



ggplot(data = TMINallobs %>% filter(ID == "UKE00105875")) +
  geom_point(aes(DecYear, Value))
# All stations:
ggplot(data = TMINallobs) +
  geom_point(aes(DecYear, Value, col = ID))





plot(d1$DecYear-floor(d1$DecYear), d1$Value, pch=20)


ggplot(mapping=aes(DecYear, Value)) +
  geom_line(data = data %>%
              filter(Element == "TMAX"), col=2) +
  geom_line(data = data %>%
              filter(Element == "TMIN"), col=4)
ggplot(mapping=aes(DecYear, Value)) +
  geom_line(data = data %>%
              filter(Element == "TMIN"), col=4)

fit1 <- lm(Value ~ 1 + Elevation + Latitude + Longitude +
             cos(DecYear*2*pi) + sin(DecYear*2*pi) +
             cos(2*DecYear*2*pi) + sin(2*DecYear*2*pi) +
             cos(3*DecYear*2*pi) + sin(3*DecYear*2*pi) +
             I(DecYear-2000), data = d1)
summary(fit1)
fit2 <- lm(Value ~ -1 + factor(ID) + factor(ID):cos(DecYear*2*pi) +
             factor(ID):sin(DecYear*2*pi) + factor(ID):I(DecYear-2000), data = d1)
summary(fit2)

pr_1 <- predict(fit1, newdata = d1, interval = "confidence", se.fit = TRUE)
pred.sd <- sqrt(pr_1$se.fit^2 + sigma(fit1)^2)
plot(pr_1$se.fit, pred.sd)

ggplot(d2, mapping=aes(DecYear, !is.na(d2$UKE00105885))) +
  geom_point()

ggplot(data = d1) +
  geom_point(aes(DecYear, Value, col=ID))
ggplot(data = d2) +
  geom_point(aes(DecYear, UKE00105642))

summary(stations)
plot.ecdf(stations$Elevation)
plot(stations$Longitude, stations$Latitude, pch=20)
plot(stations$Longitude, stations$Latitude, pch=20,
     xlim=-4.1+c(-1, 1)*2,
     ylim=56.5+c(-1, 1)*2,
     asp = 1)
points(-3.270965,55.953509, col=2)
points(-5.670106,57.656889, col=2)
points(-4.1,56.5, col=2)

ggplot(mapping = aes(DecYear, Value)) +
  geom_line(data = data %>% filter(Element == "TMIN")) +
  geom_line(data = data %>% filter(Element == "TMAX"))
}
