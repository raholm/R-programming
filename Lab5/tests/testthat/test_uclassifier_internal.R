library(Lab4)
context("UClassifier Internals")

test_that("Invalid text input is invalid", {
    expect_error(.check.text_input())
    expect_error(.check.text_input(1))
    expect_error(.check.text_input(data.frame("INVALID INPUT")))
    expect_error(.check.text_input(list("INVALID INPUT")))
})

test_that("Valid text input is correctly formatted", {
    expect_equal(.format.text_input("TEST"),
                 list(texts="TEST"))
    expect_equal(.format.text_input(list(text="TEST")),
                 list(texts="TEST"))
    expect_equal(.format.text_input(data.frame(text="TEST")),
                 list(texts="TEST"))

    expect_equal(.format.text_input(c("TEST1", "TEST2")),
                 list(texts=c("TEST1", "TEST2")))
    expect_equal(.format.text_input(list(text=c("TEST1", "TEST2"))),
                 list(texts=c("TEST1", "TEST2")))
    expect_equal(.format.text_input(data.frame(text=c("TEST1", "TEST2"))),
                 list(texts=c("TEST1", "TEST2")))
})

test_that("Valid text input is correctly parsed to JSON", {
    expect_equal(.to_json.text_input("TEST"),
                 toJSON(list(texts="TEST")))
    expect_equal(.to_json.text_input(list(text="TEST")),
                 toJSON(list(texts="TEST")))
    expect_equal(.to_json.text_input(data.frame(text="TEST")),
                 toJSON(list(texts="TEST")))

    expect_equal(.to_json.text_input(c("TEST1", "TEST2")),
                 toJSON(list(texts=c("TEST1", "TEST2"))))
    expect_equal(.to_json.text_input(list(text=c("TEST1", "TEST2"))),
                 toJSON(list(texts=c("TEST1", "TEST2"))))
    expect_equal(.to_json.text_input(data.frame(text=c("TEST1", "TEST2"))),
                 toJSON(list(texts=c("TEST1", "TEST2"))))
})

test_that("Invalid class input is invalid", {
    expect_error(.check.class_input(1))
    expect_error(.check.class_input(list("INVALID CLASS")))
    expect_error(.check.class_input(data.frame("INVALID CLASS")))
})

test_that("Valid class input is correctly formatted", {
    expect_equal(.format.class_input("CLASS"), "CLASS")
    expect_equal(.format.class_input(list(class="CLASS")), "CLASS")
    expect_equal(.format.class_input(data.frame(class="CLASS")), "CLASS")

    expect_equal(.format.class_input(c("CLASS1", "CLASS2")), c("CLASS1", "CLASS2"))
    expect_equal(.format.class_input(list(class=c("CLASS1", "CLASS2"))), c("CLASS1", "CLASS2"))
    expect_equal(.format.class_input(data.frame(class=c("CLASS1", "CLASS2"))), c("CLASS1", "CLASS2"))
})

test_that("Adding classes works properly", {
    classifier <- UClassifier("Test", "Test", "Test", "Test")

    .cache.add_class(classifier, "class1")
    expect_equal(classifier$cache$class, "class1")

    .cache.add_class(classifier, "class2")
    expect_equal(classifier$cache$class, c("class1", "class2"))

    .cache.add_class(classifier, "class2")
    expect_equal(classifier$cache$class, c("class1", "class2"))

    .cache.add_class(classifier, c("class3", "class4"))
    expect_equal(classifier$cache$class, c("class1", "class2", "class3", "class4"))
})

test_that("Removing classes works properly", {
    classifier <- UClassifier("Test", "Test", "Test", "Test")

    .cache.add_class(classifier, "class1")
    expect_equal(classifier$cache$class, "class1")
    .cache.remove_class(classifier, "class1")
    expect_equal(classifier$cache$class, NULL)

    .cache.add_class(classifier, c("class1", "class2"))
    expect_equal(classifier$cache$class, c("class1", "class2"))
    .cache.remove_class(classifier, "class1")
    expect_equal(classifier$cache$class, "class2")
    .cache.remove_class(classifier, c("class1", "class2"))
    expect_equal(classifier$cache$class, NULL)
})
