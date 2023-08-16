test_that("with_ext", {
  expect_equal(with_ext("a", "md", dir = NULL), "a.md")
  expect_equal(with_ext("a", "html", dir = "foo"), "foo/a.html")
})

test_that("weekday", {
  expect_equal(weekdays_ja(as.Date("2023/04/03")), "月")
  expect_equal(weekdays_ja(as.Date("2023/04/04")), "火")
  expect_equal(weekdays_ja(as.Date("2023/04/05")), "水")
  expect_equal(weekdays_ja(as.Date("2023/04/06")), "木")
  expect_equal(weekdays_ja(as.Date("2023/04/07")), "金")
  expect_equal(weekdays_ja(as.Date("2023/04/08")), "土")
  expect_equal(weekdays_ja(as.Date("2023/04/09")), "日")
})

