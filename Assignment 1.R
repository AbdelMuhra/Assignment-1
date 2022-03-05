library("tidyverse")

# a) read dataset into a variable
videos <- read_csv("videos.csv")
# b)
avg_file_size <- mean(videos$`File Size`)
print(avg_file_size)

avg_song_length <- mean(videos$`Song Length`)
print(avg_song_length)

sd_file_size <- sd(videos$`File Size`)
print(sd_file_size)

sd_song_length <- sd(videos$`Song Length`)
print(sd_song_length)

# c) check for normality using shapiro wilk test
file_size_shapirotest<- shapiro.test(videos$`File Size`)
if (file_size_shapirotest$p.value < 0.05) {
  print("not normal")
} else {
  print('normal')
}
  
  
song_length_shapirotest<- shapiro.test(videos$`Song Length`)
if (song_length_shapirotest$p.value < 0.05) {
  print('not normal')
} else {
  print('normal')
}

# d) 
# avg_file_size_high_quality <- avg_file_size[videos$`Video Quality` == 'High']
# avg_file_size_low_quality <- avg_file_size[videos$`Video Quality` == 'Low']
# t.test(x= avg_file_size[videos$`Video Quality` == 'High'],
#        y= avg_file_size[videos$`Video Quality` == 'Low'])







# EXTRA below i made a plot comparing the size and length of songs
#with 'high' & 'low' video quality;

ggplot(data = videos,
       filter(videos$`Video Quality` %in% c('High','Low')),
       mapping = aes(x = videos$`File Size`, y = videos$`Song Length`,
                     color = videos$`Video Quality`)) +
  geom_point(alpha = 0.5, size = 2) +
  labs(y = "Song Length (Seconds)", x = "File Size (MB)",
       color = "Video Quality")

















