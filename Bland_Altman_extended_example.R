
# Example
method_1 <- rnorm(30, mean = 50, sd = 10)
method_2 <- rnorm(30, mean = 40, sd = 20)

plot_BA <- Bland_Altman_extended(method1 = method_1,
                                 method2 = method_2,
                                 x_lab = "method_1",
                                 y_lab = "method_2",
                                 plot_name = "Exmaple")

