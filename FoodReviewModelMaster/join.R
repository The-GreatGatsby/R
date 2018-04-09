# "Mutating" joins add variables to the LHS
View(band_members)
View(band_instruments)

# band_membersデータと、band_instrumentsデータ
# band_membersのデータとband_instrumentsのデータが被っていれば、その行のデータを併せて出力。
# 何一つかぶってない行は消す。だからmickとkeithは出力されない。
band_members %>% inner_join(band_instruments)

# 被ってない部分はNAで残す。
# しかし、基準はband_member（left側）に合わせるので、band_membersデータと被ってないkeithは出力されない。
band_members %>% left_join(band_instruments)

# 被ってない部分はNAで残す。
# 今度は基準がband_instruments（right側）なので、band_instrumentsと被ってないmickは出力されない
band_members %>% right_join(band_instruments)

#全部出力。被ってるところは一つにして、被ってない所はそのまま出力して、データがないところはNA。
band_members %>% full_join(band_instruments)

# "Filtering" joins keep cases from the LHS
band_members %>% semi_join(band_instruments)
#被ってない部分だけ出力
band_members %>% anti_join(band_instruments)




# To suppress the message, supply by
band_members %>% inner_join(band_instruments, by = "name")
# This is good practice in production code

# Use a named `by` if the join variables have different names
band_members %>% full_join(band_instruments2, by = c("name" = "artist"))
# Note that only the key from the LHS is kept