test_that("Input parsing works", {

test_input <- day10InputParser(".#..#
.....
#####
....#
...##")

asteroid_list <- tibble(
        coords = list(
                c(0L, 2L)
                , c(1L, 0L)
                , c(4L, 0L)
                , c(1L, 2L)
                , c(2L, 2L)
                , c(3L, 2L)
                , c(4L, 2L)
                , c(4L, 3L)
                , c(4L, 4L)
                , c(3L, 4L)
        )
)

expect_true(all(test_input$coords %in% asteroid_list$coords))
})

test_that("View checking works", {
        asteroid_list <- tibble(
                coords = list(
                        c(0L, 2L)
                        , c(1L, 0L)
                        , c(4L, 0L)
                        , c(1L, 2L)
                        , c(2L, 2L)
                        , c(3L, 2L)
                        , c(4L, 2L)
                        , c(4L, 3L)
                        , c(4L, 4L)
                        , c(3L, 4L)
                )
        )
max_visible <- asteroid_list %>%
        mutate(View.Counts = map(coords, day10AsteroidViewCount, asteroid_list)) %>%
        unnest(cols = c(View.Counts))  %>%
        pull(View.Counts) %>%
        max()

expect_equal(max_visible, 8L)
})
