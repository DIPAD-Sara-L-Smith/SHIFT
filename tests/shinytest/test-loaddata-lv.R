app <- ShinyDriver$new("../../", loadTimeout = 1e05)
app$snapshotInit("test-loaddata-lv")

app$uploadFile(`load_data_ui_1-file` = "../../example-data/licenced_vehicles.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$snapshot()
