query.dynamic.weights<-function(student.grades, percent, base.weights) {
    rank<-order(student.grades)
    rank<-rank - mean(rank)
    dynamic.weights<-base.weights + (base.weights * rank * percent / length(student.grades))
    dynamic.weights
}

query.dynamically.weighted.grades<-function(grades, percent = 20, base.weights = FALSE, raw.weights = FALSE) {
    number.of.assignments<-length(grades)
    percent<-percent * 0.01
    if(class(base.weights) == "logical" && base.weights == FALSE) {
        base.weights<-array(1, number.of.assignments)
    } else {
        if(length(base.weights) != number.of.assignments) {
            stop("number of grades does not match number of base weights")
        }
    }
    if(!raw.weights) {
        apply(grades, 1, function(x) weighted.mean(x, query.dynamic.weights(x, percent, base.weights)))
    } else {
        apply(grades, 1, function(x) query.dynamic.weights(x, percent, base.weights))
    }
}
