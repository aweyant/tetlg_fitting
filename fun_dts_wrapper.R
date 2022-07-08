dts_wrapper <- function(df, col1_ind, col2_ind, nboots = 2000) {
  dts_test(a = df[,col1_ind], b = df[,col2_ind], nboots = nboots)
}