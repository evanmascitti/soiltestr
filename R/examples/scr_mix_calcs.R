
# this generates component masses which yield 100 kg of
# combined silt-size and clay-size particles.

new_fines_mix_data <- scr_mix_calcs(
  final_OD_kg = 100,
  scr = 2,
  silt_silty = 0.9,
  clay_silty = 0.06,
  silt_clayey = 0.43,
  clay_clayey = 0.54,
  w_silty = 0.001,
  w_clayey = 0.04
)
new_fines_mix_data
