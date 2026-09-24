# Run from the repository root with STATEDU_FINE_GRAY_BUILD_DIR set to a build.
for(test in c('verify','extra','edge'))source(file.path('native/fine_gray/tests',paste0(test,'.R')))
cat('PASS: 21 basic, 40 additional and 100 edge conditions against installed cmprsk\n')
