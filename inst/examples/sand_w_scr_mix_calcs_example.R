mix_sheet <- sand_w_scr_mix_calcs(
  x = NULL,
  mix_date = Sys.Date(),
  final_OD_kg = 30,
  sample_name = NULL,
  sandy_name = 'my-angular-sand-',
  silty_name = 'crushed-quartz',
  clayey_name = c('tile-clay', 'gumbo-clay'),
  final_sand_pct = 0.6,
  final_scr = 1,
  sand_sandy = 0.98,
  silt_sandy = 0.01,
  clay_sandy = 0.01,
  sand_silty = 0.04,
  silt_silty = 0.88,
  clay_silty = 0.08,
  sand_clayey = c(0.01, 0.57),
  silt_clayey = c(0.29, 0.17),
  clay_clayey = c(0.7, 0.26),
  w_sandy = 0.001,
  w_silty = 0.0001,
  w_clayey = c(0.02, 0.01)
)

