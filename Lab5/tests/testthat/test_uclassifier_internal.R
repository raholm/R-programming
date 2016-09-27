library(Lab4)
context("UClassifier Internals")

test_that("Invalid text input is invalid", {
    expect_error(.check.text_input())
    expect_error(.check.text_input(1))
    expect_error(.check.text_input(data.frame("THIS IS INVALID INPUT")))
    expect_error(.check.text_input(list("THIS IS INVALID INPUT")))
})

test_that("Valid text input is correctly formatted", {
    expect_equal(.format.text_input("THIS IS A TEST"),
                 list(texts="THIS IS A TEST"))
    expect_equal(.format.text_input(list(texts="THIS IS A TEST")),
                 list(texts="THIS IS A TEST"))
    expect_equal(.format.text_input(data.frame(texts="THIS IS A TEST")),
                 list(texts="THIS IS A TEST"))

    expect_equal(.format.text_input(c("THIS IS A TEST1", "THIS IS A TEST2")),
                 list(texts=c("THIS IS A TEST1", "THIS IS A TEST2")))
    expect_equal(.format.text_input(list(texts=c("THIS IS A TEST1", "THIS IS A TEST2"))),
                 list(texts=c("THIS IS A TEST1", "THIS IS A TEST2")))
    expect_equal(.format.text_input(data.frame(texts=c("THIS IS A TEST1", "THIS IS A TEST2"))),
                 list(texts=c("THIS IS A TEST1", "THIS IS A TEST2")))
})

test_that("Valid text input is correctly parsed to JSON", {
    expect_equal(.to_json.text_input("THIS IS A TEST"),
                 toJSON(list(texts="THIS IS A TEST")))
    expect_equal(.to_json.text_input(list(texts="THIS IS A TEST")),
                 toJSON(list(texts="THIS IS A TEST")))
    expect_equal(.to_json.text_input(data.frame(texts="THIS IS A TEST")),
                 toJSON(list(texts="THIS IS A TEST")))

    expect_equal(.to_json.text_input(c("THIS IS A TEST1", "THIS IS A TEST2")),
                 toJSON(list(texts=c("THIS IS A TEST1", "THIS IS A TEST2"))))
    expect_equal(.to_json.text_input(list(texts=c("THIS IS A TEST1", "THIS IS A TEST2"))),
                 toJSON(list(texts=c("THIS IS A TEST1", "THIS IS A TEST2"))))
    expect_equal(.to_json.text_input(data.frame(texts=c("THIS IS A TEST1", "THIS IS A TEST2"))),
                 toJSON(list(texts=c("THIS IS A TEST1", "THIS IS A TEST2"))))
})
