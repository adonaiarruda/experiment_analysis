power.t.test(delta = 4, sd = 2,
             sig.level = 0.05, n = 5,
             type = "two.sample", alternative = "one.sided")

power.t.test(delta = 30, sd = 30,
             sig.level = 0.05, power = 0.8,
             type = "two.sample", alternative = "one.sided")

power.t.test(delta = 2, sd = 1,
             sig.level = 0.05, power = 0.8,
             type = "two.sample", alternative = "one.sided")

power.t.test(delta = 60, sd = 30,
             sig.level = 0.05, power = 0.8,
             type = "two.sample", alternative = "one.sided")


power.t.test(delta = 60, sd = 30,
             sig.level = 0.05, n = c(10,12),
             type = "two.sample", alternative = "two.sided")
