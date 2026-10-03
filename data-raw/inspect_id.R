readRenviron(".Renviron")
for (f in c("config.R", "log.R", "ids.R", "db.R")) source(file.path("R", f))
doc <- db_insert("organizations", list(
  slug = paste0("id-test-", new_id()),
  name = "inspect"
))
got <- db_one("organizations", q_field("slug", doc$slug))
cat("class=", paste(class(got[["_id"]]), collapse = ","), "\n", sep = "")
cat("typeof=", typeof(got[["_id"]]), "\n", sep = "")
str(got[["_id"]])
cat("names=", paste(names(got), collapse = ","), "\n", sep = "")
