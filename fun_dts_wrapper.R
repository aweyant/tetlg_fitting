dts_wrapper <- function(df, col1_ind, col2_ind, nboots = 2000) {
  dts_test(a = (df[,col1_ind] %>% as.vector()),
           b = (df[,col2_ind] %>% as.vector()), nboots = nboots)
}