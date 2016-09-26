library(subtools)
context("Regexes")

test_that(".extr_filename file name extraction", {
  expect_equal(.extr_filename("abcd.txt"), "abcd.txt")
  expect_equal(.extr_filename("ab.cd.txt"), "ab.cd.txt")
  expect_equal(.extr_filename("dir/abcd.txt"), "abcd.txt")
  expect_equal(.extr_filename("dir//dir/ab.cd.txt"), "ab.cd.txt")
  expect_equal(.extr_filename("dir//dir/ab.cd.txt/"), "ab.cd.txt")
})


test_that(".extr_extension file extension extraction", {
  expect_equal(.extr_extension("abcd.txt"), "txt")
  expect_equal(.extr_extension("abcd..txt"), "txt")
  expect_equal(.extr_extension("ab.cd.txt"), "txt")
  expect_equal(.extr_extension("dir.dir/ab.cd.txt"), "txt")
  expect_equal(.extr_extension("dir/dir//.cd.txt"), "txt")
})


test_that(".extr_snum season number extraction", {
  expect_equal(is.na(.extr_snum("abcd5.txt")), TRUE)
  expect_equal(.extr_snum("MySerie.S05E09.txt"), 5)
  expect_equal(.extr_snum("MySerie.S05.txt"), 5)
  expect_equal(.extr_snum("MySerie_S5_.txt"), 5)
  expect_equal(.extr_snum("MySerie.Season_5.txt"), 5)
})

test_that(".extr_enum season number extraction", {
  expect_equal(is.na(.extr_enum("abcd9.txt")), TRUE)
  expect_equal(.extr_enum("MySerie.S05E09.txt"), 9)
  expect_equal(.extr_enum("MySerie.E09.txt"), 9)
  expect_equal(.extr_enum("MySerie_E9_.txt"), 9)
  expect_equal(.extr_enum("MySerie.Episode_9.txt"), 9)
})

