library(ggplot2)
library(marinecs100b)
remotes::install_github("MarineCS-100B/marinecs100b")

# Questionable organization choices ---------------------------------------

# P1 Call the function dir() at the console. This lists the files in your
# project's directory. Do you see woa.csv in the list? (If you don't, move it to
# the right place before proceeding.)


# P2 Critique the organization of woa.csv according to the characteristics of
# tidy data.
#multiple lines of headers
#blank spaces instead of NA confuses computer
#names aren't consistent


# Importing data ----------------------------------------------------------

# P3 P3 Call read.csv() on woa.csv. What error message do you get? What do you
# think that means?
read.cvs("woa.csv")
#error, could not find function
#likely due to too many lines of headers
# P4 Re-write the call to read.csv() to avoid the error in P3.
woa <- read.csv("woa.csv", skip = 1)


# Fix the column names ----------------------------------------------------

# P5 Fill in the blanks below to create a vector of the depth values.

depths <- c(
  seq(0, 100, by = 5),
  seq(125, 500, by = 25),
  seq(550, 2000, by = 50),
  seq(2100, 5500, by = 100)
)

depths
colnames(woa)
# P6 Create a vector called woa_colnames with clean names for all 104 columns.
# Make them the column names of your WOA data frame.

woa_colnames <- c("latitude", "longitude", depths)
colnames(woa) <- woa_colnames
woa
# Analyzing wide-format data ----------------------------------------------

# P7 What is the mean water temperature globally in the twilight zone (200-1000m
# depth)?

twilight_woa <- woa[, which(woa_colnames == 200) : which(woa_colnames == 1000)]
twilight_woa

wide_average <- function(dataset){
  sum <- 0
  n <- 0
  for (i in 1:nrow(dataset)){
    for (j in 1:ncol(dataset)){
      if (!is.na(dataset[i, j])){
        sum <- sum + dataset[i, j]
        n <- n+1
      }
    }
  }
  return (sum/n)
}
wide_average(twilight_woa)


# Analyzing long-format data ----------------------------------------------

# P8 Using woa_long, find the mean water temperature globally in the twilight zone.
woa_long[1, 3]
s <- 0
x <- 0
for (i in 1:nrow(woa_long)){
  if ((woa_long[i, 3] >= 200 & woa_long[i, 3] <= 1000) & !is.na(woa_long[i, 4])){
    s <- s + woa_long[i, 4]
    x <- x + 1
  }
}
s/x
woa_long
# P9 Compare and contrast your solutions to P8 and P9.
#Same avearge
#P8 is requires less lines of code

# P10 Create a variable called mariana_temps. Filter woa_long to the rows in the
# location nearest to the coordinates listed in the in-class instructions.
d_from_m <- function(lat, long){
  x= 11.21 - lat
  y = 142.12 - long
  z = sqrt(x^2 + y^2)
  return (z)
}
clostest_index <- 1
for (i in 2:nrow(woa_long)){
  if (d_from_m(woa_long[i, 1], woa_long[i, 2]) < d_from_m(woa_long[i-1, 1], woa_long[i-1, 2])){
    closest_index <- i
  }
}
closest_lat <- woa_long[closest_index, 1]
closest_long <- woa_long[closest_index, 2]

mariana_temps <- woa_long[woa_long$latitude == closest_lat & woa_long$longitude == closest_long, ]
mariana_temps
# P11 Interpret your temperature-depth profile. What's the temperature at the surface? How about in the deepest parts? Over what depth range does temperature change the most?
#surface temperature is -1.672C
#temp at deepest point is 0.089C
#temperature changes the most between 0m and 200m

# ggplot is a tool for making figures, you'll learn its details in COMM101
ggplot(mariana_temps, aes(temp_c, depth_m)) +
  geom_path() +
  scale_y_reverse()
