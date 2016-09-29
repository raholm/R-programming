library(Lab4)
context("UClassifier")

username <- Sys.getenv("UCLASSIFY_USERNAME")
read_token <- Sys.getenv("UCLASSIFY_READ_TOKEN")
write_token <- Sys.getenv("UCLASSIFY_WRITE_TOKEN")

test_that("Creating/Removing a classifier works properly", {
    classifier <- UClassifier("UnitTestClassifier",
                              username,
                              read_token,
                              write_token)

    expect_equal(.API.classifier_exists(classifier), TRUE)

    remove_classifier(classifier)
    expect_equal(.API.classifier_exists(classifier), FALSE)
})

test_that("Adding/Removing classes works properly", {
    classifier <- UClassifier("UnitTestClassifier",
                              username,
                              read_token,
                              write_token)

    classifier$add_class("class1")
    classifier$add_class("class2")

    ## Make sure it fetches the data using the API
    classifier$cache$dirty = TRUE
    classifier$cache$information = NULL
    
    expect_equal(classifier$get_information()$className, c("class1", "class2"))

    classifier$remove_class("class1")

    ## Make sure it fetches the data using the API
    classifier$cache$dirty = TRUE
    classifier$cache$information = NULL
    
    expect_equal(classifier$get_information()$className, "class2")

    classifier$remove_class("class2")

    ## Make sure it fetches the data using the API
    classifier$cache$dirty = TRUE
    classifier$cache$information = NULL
    
    expect_equal(classifier$get_information()$className, NULL)
    
    remove_classifier(classifier)
})

test_that("Train/Untrain works properly", {
    classifier <- UClassifier("UnitTestClassifier",
                              username,
                              read_token,
                              write_token)

    data <- data.frame(text=c("This is class1 text", "This is class2 text test test test"),
                       class=c("class1", "class2"), stringsAsFactors=FALSE)
    
    classifier$add_class("class1")
    classifier$add_class("class2")

    classifier$train(data$text, data$class)

    information <- classifier$get_information()

    expect_equal(information$className, c("class1", "class2"))
    expect_equal(information$uniqueFeatures, c(4, 5))
    expect_equal(information$totalCount, c(4, 7))

    classifier$untrain(data$text, data$class)
    
    information <- classifier$get_information()

    expect_equal(information$className, c("class1", "class2"))
    expect_equal(information$uniqueFeatures, c(0, 0))
    expect_equal(information$totalCount, c(0, 0))
    
    remove_classifier(classifier)
})

test_that("Classify works properly", {
    classifier <- UClassifier("UnitTestClassifier",
                              username,
                              read_token,
                              write_token)

    data <- data.frame(text=c("This is class1 text", "This is class2 text test test test"),
                       class=c("class1", "class2"), stringsAsFactors=FALSE)
    
    classifier$add_class("class1")
    classifier$add_class("class2")

    classifier$train(data$text, data$class)

    classify <- classifier$classify(c("class1", "This is a test"))

    expect_equal(classify$classification.className, c("class1", "class1"))
    expect_equal(classify$classification.className.1, c("class2", "class2"))
    expect_equal(classify$textCoverage, c(1.00, 0.75))
    expect_true(classify$classification.p[1] > classify$classification.p.1[1])
    expect_true(classify$classification.p[2] < classify$classification.p.1[2])
    
    remove_classifier(classifier)
})

test_that("Get Keywords works properly", {
    classifier <- UClassifier("UnitTestClassifier",
                              username,
                              read_token,
                              write_token)

    data <- data.frame(text=c("This is class1 text", "This is class2 text test test test"),
                       class=c("class1", "class2"), stringsAsFactors=FALSE)
    
    classifier$add_class("class1")
    classifier$add_class("class2")

    classifier$train(data$text, data$class)

    keywords <- classifier$get_keywords("class1 class2 text test")

    expect_true(keywords$keyword == "text" && keywords$className == "class1" && keywords$p > 0.5)
    expect_true(keywords$keyword.1 == "class2" && keywords$className.1 == "class2" && keywords$p.1 > 0.5)
    expect_true(keywords$keyword.2 == "class1" && keywords$className.2 == "class1" && keywords$p.2 > 0.5)
    expect_true(keywords$keyword.3 == "test" && keywords$className.3 == "class2" && keywords$p.3 > 0.5)
    
    remove_classifier(classifier)
})
