test_that("as coercion works", {

  as_subtitle(
    c("WEBVTT",
      "X-TIMESTAMP-MAP=MPEGTS:181083,LOCAL:00:00:00.000",
      "",
      "3",
      "00:00:21.199 --> 00:00:22.333", ">> FEMALE SPEAKER:",
      "Don't stay up too late.",
      "",
      ""
    ), format = "webvtt"
  ) -> st

  expect_equal(
    st[["Text_content"]],
    ">> FEMALE SPEAKER: Don't stay up too late."
  )

})
