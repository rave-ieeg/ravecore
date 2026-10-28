test_that("nev_cache_key() hashes a file once until its size or time changes", {
  path <- tempfile(fileext = ".nev")
  on.exit(unlink(path), add = TRUE)
  writeBin(as.raw(1:64), path)
  mtime <- as.POSIXct("2026-01-02 03:04:05", tz = "UTC")
  Sys.setFileTime(path, mtime)

  # the key names the cache folder `neuroevent/<md5>`: it must stay the MD5
  # of the file so caches written before still match
  key <- nev_cache_key(path)
  expect_identical(key, unname(tools::md5sum(path)))

  # same size and modification time: the stored key is reused and the file
  # is not read again (so this content change goes unnoticed)
  writeBin(as.raw(64:1), path)
  Sys.setFileTime(path, mtime)
  expect_identical(nev_cache_key(path), key)

  # a new modification time means the file changed: hash it again
  Sys.setFileTime(path, mtime + 60)
  expect_identical(nev_cache_key(path), unname(tools::md5sum(path)))
  expect_false(identical(nev_cache_key(path), key))
})
