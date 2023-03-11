test_that("palettes work", {
  expect_equal(asp_palettes$Dark[["bg-grey"]], "#4c4d4c")
  expect_equal(asp_palettes$Light[["bg-white"]], "#ffffff")
  expect_equal(asp_palettes$Dark[["yellow-full"]], "#ffd635")
  expect_equal(asp_palettes$Dark[["green-psycho"]], "#00b050")
  expect_equal(asp_palettes$Light[["smoke-capital"]], "#bdbdbd")
})
