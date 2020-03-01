temp <- df_split %>%
    head(1000) %>%
    group_by(cookie) %>%
    mutate(first_conversion = ifelse(any(interaction == 'conversion'),
                                     min(time[interaction == 'conversion'], na.rm = TRUE),
                                     NA)) %>%
    ungroup() %>%
    filter(is.na(first_conversion) | time <= first_conversion) %>%
    data.frame()

temp    
warnings()
