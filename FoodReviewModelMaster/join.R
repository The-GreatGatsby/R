# "Mutating" joins add variables to the LHS
View(band_members)
View(band_instruments)

# band_members�f�[�^�ƁAband_instruments�f�[�^
# band_members�̃f�[�^��band_instruments�̃f�[�^������Ă���΁A���̍s�̃f�[�^�𕹂��ďo�́B
# ������Ԃ��ĂȂ��s�͏����B������mick��keith�͏o�͂���Ȃ��B
band_members %>% inner_join(band_instruments)

# ����ĂȂ�������NA�Ŏc���B
# �������A���band_member�ileft���j�ɍ��킹��̂ŁAband_members�f�[�^�Ɣ���ĂȂ�keith�͏o�͂���Ȃ��B
band_members %>% left_join(band_instruments)

# ����ĂȂ�������NA�Ŏc���B
# ���x�͊��band_instruments�iright���j�Ȃ̂ŁAband_instruments�Ɣ���ĂȂ�mick�͏o�͂���Ȃ�
band_members %>% right_join(band_instruments)

#�S���o�́B����Ă�Ƃ���͈�ɂ��āA����ĂȂ����͂��̂܂܏o�͂��āA�f�[�^���Ȃ��Ƃ����NA�B
band_members %>% full_join(band_instruments)

# "Filtering" joins keep cases from the LHS
band_members %>% semi_join(band_instruments)
#����ĂȂ����������o��
band_members %>% anti_join(band_instruments)




# To suppress the message, supply by
band_members %>% inner_join(band_instruments, by = "name")
# This is good practice in production code

# Use a named `by` if the join variables have different names
band_members %>% full_join(band_instruments2, by = c("name" = "artist"))
# Note that only the key from the LHS is kept