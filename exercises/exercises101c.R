# Fill in the blanks below to pull out the price of the Chasing M2 Pro
rovs <- data.frame(
  model = c("Chasing M2 Pro", "Qysea Fifish Pro V6", "BlueROV2"),
  price_usd = c(3459, 2999, 6500)
)
rovs[???, ???]


# Pull out the model column from rovs using both square bracket and dollar sign
# indexing


# The file flowcytodata.csv contains artificial flow cytometry data, a technique
# for counting phytoplankton and other single-cell organisms. Use the data from
# that file to complete the following exercises.
flowcyto <- read.csv("flowcytodata.csv")


# Move the comments below so they go with the correct line of code.

# Pull out the abundances from the first cast at the first station

# Pull out all rows where abundance was more than 400

# Set all the values in abundance to NA for the first cast at the first station

flowcyto[flowcyto$abundance > 400, ]
flowcyto[flowcyto$station_id == 1 & flowcyto$cast == 1, 5]
flowcyto[flowcyto$station_id == 1 & flowcyto$cast == 1, 5] <- NA
