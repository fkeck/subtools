library(subtools)
context("Utilities for timecodes operations")

test_that(".format_subtime adds milli-seconds", {
  expect_equal(.format_subtime("00:00:00"), "00:00:00.000")
  expect_equal(.format_subtime("00:00:00.0"), "00:00:00.000")
  expect_equal(.format_subtime("0"), "00:00:00.000")
})


test_that(".format_subtime deals with comma sep", {
  expect_equal(.format_subtime("00:00:00,123"), "00:00:00.123")
  expect_equal(.format_subtime("01:02:03,000"), "01:02:03.000")
})


test_that(".format_subtime guaranties a fixed length output", {
  expect_equal(nchar(.format_subtime("00:00:00,123")), 12)
  expect_equal(nchar(.format_subtime("0:0:0")), 12)
  expect_equal(nchar(.format_subtime("1:2:3.123")), 12)
  expect_equal(nchar(.format_subtime("1")), 12)
})


test_that(".add_timecodes : various additions", {
  expect_equal(.add_timecodes("00:00:00.000", "00:00:00.000"), "00:00:00.000")
  expect_equal(.add_timecodes("01:02:03.004", "01:02:03.004"), "02:04:06.008")
  expect_equal(.add_timecodes("00:00:00.999", "00:00:00.001"), "00:00:01.000")
  expect_equal(.add_timecodes("00:59:59.999", "00:00:00.001"), "01:00:00.000")
  expect_equal(.add_timecodes("99:00:00.000", "01:00:00.000"), "100:00:00.000")
})

