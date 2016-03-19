source('results.maxwell.R')

# res.est
# res.est.sd
# res.ni
# res.ni.sd
# res.h1
# res.ub

datatable <- matrix(ncol = 5, nrow = 28)
datatable[1, 2] <- "Scenario"
datatable[1, 3] <- "N = 25"
datatable[1, 4] <- "N = mixed"
datatable[1, 5] <- "N = 1750"

datatable[2, 1] <- "µ = 0"
datatable[11, 1] <- "µ = 0.2"
datatable[20, 1] <- "µ = 0.5"

datatable[c(2, 11, 20), 2] <- "Current"
datatable[c(5, 14, 23), 2] <- "Maxwell"
datatable[c(8, 17, 26), 2] <- "Ideal"

datatable[2, 3] <- sprintf("%%H1: %g", res.h1[1] * 100)
datatable[3, 3] <- sprintf("%g (%g)", res.est[1], res.est.sd[1])
datatable[4, 3] <- sprintf("[%g (%g)]", res.ni[1], res.ni.sd[1])

datatable[5, 3] <- sprintf("%%H1: %g", res.ub[2] * 100)
datatable[6, 3] <- sprintf("%g (%g)", res.est[2], res.est.sd[2])
datatable[7, 3] <- sprintf("[%g (%g)]", res.ni[2], res.ni.sd[2])

datatable[8, 3] <- sprintf("%%H1: %g", res.h1[3] * 100)
datatable[9, 3] <- sprintf("%g (%g)", res.est[3], res.est.sd[3])
datatable[10, 3] <- sprintf("[%g (%g)]", res.ni[3], res.ni.sd[3])

datatable[11, 3] <- sprintf("%%H1: %g", res.h1[4] * 100)
datatable[12, 3] <- sprintf("%g (%g)", res.est[4], res.est.sd[4])
datatable[13, 3] <- sprintf("[%g (%g)]", res.ni[4], res.ni.sd[4])

datatable[14, 3] <- sprintf("%%H1: %g", res.ub[5] * 100)
datatable[15, 3] <- sprintf("%g (%g)", res.est[5], res.est.sd[5])
datatable[16, 3] <- sprintf("[%g (%g)]", res.ni[5], res.ni.sd[5])

datatable[17, 3] <- sprintf("%%H1: %g", res.h1[6] * 100)
datatable[18, 3] <- sprintf("%g (%g)", res.est[6], res.est.sd[6])
datatable[19, 3] <- sprintf("[%g (%g)]", res.ni[6], res.ni.sd[6])

datatable[20, 3] <- sprintf("%%H1: %g", res.h1[7] * 100)
datatable[21, 3] <- sprintf("%g (%g)", res.est[7], res.est.sd[7])
datatable[22, 3] <- sprintf("[%g (%g)]", res.ni[7], res.ni.sd[7])

datatable[23, 3] <- sprintf("%%H1: %g", res.ub[8] * 100)
datatable[24, 3] <- sprintf("%g (%g)", res.est[8], res.est.sd[8])
datatable[25, 3] <- sprintf("[%g (%g)]", res.ni[8], res.ni.sd[8])

datatable[26, 3] <- sprintf("%%H1: %g", res.h1[9] * 100)
datatable[27, 3] <- sprintf("%g (%g)", res.est[9], res.est.sd[9])
datatable[28, 3] <- sprintf("[%g (%g)]", res.ni[9], res.ni.sd[9])

datatable[2, 4] <- sprintf("%%H1: %g", res.h1[10] * 100)
datatable[3, 4] <- sprintf("%g (%g)", res.est[10], res.est.sd[10])
datatable[4, 4] <- sprintf("[%g (%g)]", res.ni[10], res.ni.sd[10])

datatable[5, 4] <- sprintf("%%H1: %g", res.ub[11] * 100)
datatable[6, 4] <- sprintf("%g (%g)", res.est[11], res.est.sd[11])
datatable[7, 4] <- sprintf("[%g (%g)]", res.ni[11], res.ni.sd[11])

datatable[8, 4] <- sprintf("%%H1: %g", res.h1[12] * 100)
datatable[9, 4] <- sprintf("%g (%g)", res.est[12], res.est.sd[12])
datatable[10, 4] <- sprintf("[%g (%g)]", res.ni[12], res.ni.sd[12])

datatable[11, 4] <- sprintf("%%H1: %g", res.h1[13] * 100)
datatable[12, 4] <- sprintf("%g (%g)", res.est[13], res.est.sd[13])
datatable[13, 4] <- sprintf("[%g (%g)]", res.ni[13], res.ni.sd[13])

datatable[14, 4] <- sprintf("%%H1: %g", res.ub[14] * 100)
datatable[15, 4] <- sprintf("%g (%g)", res.est[14], res.est.sd[14])
datatable[16, 4] <- sprintf("[%g (%g)]", res.ni[14], res.ni.sd[14])

datatable[17, 4] <- sprintf("%%H1: %g", res.h1[15] * 100)
datatable[18, 4] <- sprintf("%g (%g)", res.est[15], res.est.sd[15])
datatable[19, 4] <- sprintf("[%g (%g)]", res.ni[15], res.ni.sd[15])

datatable[20, 4] <- sprintf("%%H1: %g", res.h1[16] * 100)
datatable[21, 4] <- sprintf("%g (%g)", res.est[16], res.est.sd[16])
datatable[22, 4] <- sprintf("[%g (%g)]", res.ni[16], res.ni.sd[16])

datatable[23, 4] <- sprintf("%%H1: %g", res.ub[17] * 100)
datatable[24, 4] <- sprintf("%g (%g)", res.est[17], res.est.sd[17])
datatable[25, 4] <- sprintf("[%g (%g)]", res.ni[17], res.ni.sd[17])

datatable[26, 4] <- sprintf("%%H1: %g", res.h1[18] * 100)
datatable[27, 4] <- sprintf("%g (%g)", res.est[18], res.est.sd[18])
datatable[28, 4] <- sprintf("[%g (%g)]", res.ni[18], res.ni.sd[18])

datatable[2, 5] <- sprintf("%%H1: %g", res.h1[19] * 100)
datatable[3, 5] <- sprintf("%g (%g)", res.est[19], res.est.sd[19])
datatable[4, 5] <- sprintf("[%g (%g)]", res.ni[19], res.ni.sd[19])

datatable[5, 5] <- sprintf("%%H1: %g", res.ub[20] * 100)
datatable[6, 5] <- sprintf("%g (%g)", res.est[20], res.est.sd[20])
datatable[7, 5] <- sprintf("[%g (%g)]", res.ni[20], res.ni.sd[20])

datatable[8, 5] <- sprintf("%%H1: %g", res.h1[21] * 100)
datatable[9, 5] <- sprintf("%g (%g)", res.est[21], res.est.sd[21])
datatable[10, 5] <- sprintf("[%g (%g)]", res.ni[21], res.ni.sd[21])

datatable[11, 5] <- sprintf("%%H1: %g", res.h1[22] * 100)
datatable[12, 5] <- sprintf("%g (%g)", res.est[22], res.est.sd[22])
datatable[13, 5] <- sprintf("[%g (%g)]", res.ni[22], res.ni.sd[22])

datatable[14, 5] <- sprintf("%%H1: %g", res.ub[23] * 100)
datatable[15, 5] <- sprintf("%g (%g)", res.est[23], res.est.sd[23])
datatable[16, 5] <- sprintf("[%g (%g)]", res.ni[23], res.ni.sd[23])

datatable[17, 5] <- sprintf("%%H1: %g", res.h1[24] * 100)
datatable[18, 5] <- sprintf("%g (%g)", res.est[24], res.est.sd[24])
datatable[19, 5] <- sprintf("[%g (%g)]", res.ni[24], res.ni.sd[24])

datatable[20, 5] <- sprintf("%%H1: %g", res.h1[25] * 100)
datatable[21, 5] <- sprintf("%g (%g)", res.est[25], res.est.sd[25])
datatable[22, 5] <- sprintf("[%g (%g)]", res.ni[25], res.ni.sd[25])

datatable[23, 5] <- sprintf("%%H1: %g", res.ub[26] * 100)
datatable[24, 5] <- sprintf("%g (%g)", res.est[26], res.est.sd[26])
datatable[25, 5] <- sprintf("[%g (%g)]", res.ni[26], res.ni.sd[26])

datatable[26, 5] <- sprintf("%%H1: %g", res.h1[27] * 100)
datatable[27, 5] <- sprintf("%g (%g)", res.est[27], res.est.sd[27])
datatable[28, 5] <- sprintf("[%g (%g)]", res.ni[27], res.ni.sd[27])


write.csv(datatable, 'table_ALT.csv', row.names = FALSE)
