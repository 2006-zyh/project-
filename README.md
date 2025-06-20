# dsci-100-project_template
file data
players

[players.csv](https://github.com/user-attachments/files/20823383/players.csv)

sessions
[sessions.csv](https://github.com/user-attachments/files/20823385/sessions.csv)


code
# Read datasets
players <- read_csv("players.csv")
sessions <- read_csv("sessions.csv")
# Convert subscription status to binary (1 = subscribed, 0 = not subscribed)
players <- players |>
  mutate(subscribe = as.integer(subscribe))
players
sessions_clean <- sessions |>
  mutate(
    start_time = ymd_hms(start_time),
    end_time = ymd_hms(end_time),
    session_duration = as.numeric(difftime(end_time, start_time, units = "mins"))
  ) |>
  group_by(hashedEmail) |>
  summarise(
    avg_session_duration = mean(session_duration, na.rm = TRUE),
    total_sessions = n()
  )
sessions_clean
full_data <- players |>
  left_join(sessions_clean, by = "hashedEmail")

full_data
clean_data <- full_data[
  !(full_data$gender %in% c("Two-Spirited", "Prefer not to say", "Non-binary")) &
  !is.na(full_data$subscribe) &
  !is.na(full_data$experience) &
  !is.na(full_data$played_hours) &
  !is.na(full_data$gender) &
  !is.na(full_data$Age) &
  !is.na(full_data$avg_session_duration) &
  !is.na(full_data$total_sessions),
]


model <- glm(subscribe ~ experience + played_hours + gender + Age +
               avg_session_duration + total_sessions,
             data = clean_data,
             family = "binomial",
             control = glm.control(epsilon = 1e-8, maxit = 100, trace = FALSE))

summary(model)

predicted <- ifelse(predict(model, type = "response") > 0.5, 1, 0)
mean(predicted == clean_data[["subscribe"]])
barplot(table(clean_data$gender, clean_data$subscribe),
        beside = TRUE, col = c("lightblue", "salmon"),
        legend = TRUE, xlab = "Gender", ylab = "Count of Players",
        main = "Subscription Count by Gender")
