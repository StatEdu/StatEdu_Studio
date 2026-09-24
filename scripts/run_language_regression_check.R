Sys.setlocale('LC_CTYPE', 'English_United States.utf8')
root <- normalizePath(Sys.getenv('STATEDU_LANGUAGE_TEST_PROFILE', 'tmp/language-regression-profile'), mustWork=FALSE)
dir.create(root, recursive=TRUE, showWarnings=FALSE)
Sys.setenv(STATEDU_MODULE_CACHE='false', STATEDU_NO_PACKAGE_INSTALL='true',
  STATEDU_USER_DATA_DIR=root, STATEDU_RESULT_STORE=file.path(root,'results.json'),
  APPDATA=file.path(root,'appdata'), LOCALAPPDATA=file.path(root,'localappdata'))
shiny::runApp('.', host='127.0.0.1',
  port=as.integer(Sys.getenv('STATEDU_LANGUAGE_TEST_PORT','43873')), launch.browser=FALSE)
