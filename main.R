# Main
source("skey.R")

print(sprintf("da <- prepData(), %s, sec", ptime(
        da <- prepData(file.path(getwd(), "data"))
)))

print(sprintf("ng <- trainModels(da), %s, sec", ptime(
        ng <- trainModels(da)
)))

print(sprintf("ng <- mergeModels(ng), %s, sec", ptime(
        ng <- mergeModels(ng)
)))

print(sprintf("re <- testModels(ng, da), %s, sec", ptime(
        re <- testModels(ng, da)
)))

print(sprintf("First prediction match = %s%%", format(sum(re$match)/sum(re$count)*100, digits=5, nsmall=4)))