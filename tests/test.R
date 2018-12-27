app <- ShinyDriver$new("..")
app$snapshotInit("test")

app$snapshot()
app$setInputs(n = 10)
app$snapshot()
app$setInputs(n = 1000)
app$snapshot()
