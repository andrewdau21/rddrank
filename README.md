options(gargle_oauth_cache = ".secrets")
list.files(".secrets/")
gs4_auth(cache = ".secrets", email = "")
ss <- gs4_get("1r7oyQTwVckAoBhqCglPiWop_jsmyVCvINkjQDe33-qU")
books <- read_sheets(ss)
