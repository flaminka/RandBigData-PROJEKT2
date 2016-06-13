library(testthat)

test_that("poprawaOrtografii", {
   
   expect_error( poprawaOrtografii(1) )
   expect_error( poprawaOrtografii(data.frame(var1 = 'fss')) )
   #serwis już niedostępny
   #expect_type(object = poprawaOrtografii("bank"), type = "character")

})


test_that("zamianaCzasownikow", {
   
   expect_error( zamianaCzasownikow(data.frame(var1="can")) )
   expect_error( zamianaCzasownikow(2429) )
   expect_type(object = zamianaCzasownikow("bank"), type = "character")
   expect_equal(zamianaCzasownikow("pływać"),expected = "pływanie")
   
})


test_that("usuwanieStopwordsow", {
   
   expect_error( usuwanieStopwordsow(data.frame(var1="can")) )
   expect_error( usuwanieStopwordsow(2429) )
   expect_type(object = usuwanieStopwordsow("bank"), type = "character")
   expect_equal(usuwanieStopwordsow("pływać|a|oraz|mi"),expected = "pływać")
   
})


test_that("wydobywanieRzeczownikow", {
   
   expect_error( wydobywanieRzeczownikow(data.frame(var1="can")) )
   expect_error( usuwanieStopwordsow(2429) )
   #expect_type(object = wydobywanieRzeczownikow("bank"), type = "character")
   #expect_equal(wydobywanieRzeczownikow("żartuję przecież kochanie"),expected = "kochanie")
   
})


test_that("ramkaTylkoRzeczowniki", {
   
   sciezka <- file.path(getwd(), "doTestowRaw.csv")
   expect_error( ramkaTylkoRzeczowniki(sciezka_dane =sciezka, sciezka_zapis = getwd(), nazwa_wynikowa = 242 ) )
   expect_error( ramkaTylkoRzeczowniki(data.frame(body="cos", source="fs") ) )

})


test_that("filterByPlotDf", {
  
  
  expect_true( is.data.frame(filterByPlotDF(dateStart = "2013-02-11", dateStop = "2013-03-01",dane =  dane$ALIOR) ) )
  expect_error( filterByPlotDf(data.frame(body="cos", source="fs") ) )
  
})


test_that("filterByPlotList", {
  
  
  expect_true( is.list( filterByPlotList(dateStart = "2013-02-11", dateStop = "2013-03-01",dane =  dane$ALIOR) ))
  expect_error( filterByPlotList(data.frame(body="cos", source="fs") ) )
  
})


test_that("finalDFRzecz", {
  
  
  expect_error( finalDFRzecz("ala|ma|kota"))
  expect_error( finalDFRzecz(data.frame(body="cos", source="fs") ) )
  
})


test_that("separateNouns", {
  
  
  expect_true( is.data.frame(separateNouns(Alior) ))
  expect_error( separateNouns(data.frame(body="cos", source="fs") ) )
  
})

