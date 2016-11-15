package io.github.maximerihouey

/**
  * Created by maxime on 06/11/16.
  */
class LogistiqueRegression {

  var fitted: Boolean = false
  var intercept: Double = 0.0
  var coefficients: Array[Double] = null
  var featuresMultiple: Array[Array[Double]] = null
  var labels: Array[Integer] = null
  var labelsDouble: Array[Double] = null
  var alpha = 0.000005

  def fit(featuresMultiple: Array[Array[Double]], labels: Array[Integer]){
    this.featuresMultiple = featuresMultiple
    this.labels = labels
    this.labelsDouble = Array.ofDim[Double](labels.length)
    for(i <- 0 to this.labels.length-1){
      labelsDouble(i) = this.labels(i).toDouble
    }
    this.coefficients = Array.ofDim[Double](featuresMultiple(0).length)

    var k: Integer = 0
    val kMax: Integer = 500
    val epsilonMin: Double = 0.001
    var logLikelihoodVal: Double = logLikelihood()
    var logLikelihoodPreviousVal: Double = logLikelihoodVal - 1.0
    //println("---------------------- %d | %f".format(0, logLikelihoodVal))

    while((k < kMax) && (Math.abs(logLikelihoodVal - logLikelihoodPreviousVal) > epsilonMin) && (logLikelihoodVal > logLikelihoodPreviousVal)){
      updateCoefficients()
      k += 1

      logLikelihoodPreviousVal = logLikelihoodVal
      logLikelihoodVal = logLikelihood()
      //println("---------------------- %d | %f".format(k, logLikelihoodVal))
    }

    fitted = true
  }

  def predict(featuresMultiple: Array[Array[Double]]): Array[Integer] = {
    val predictions = Array.ofDim[Integer](featuresMultiple.length)
    for(i <- 0 to (featuresMultiple.length-1)){
      predictions(i) = this.predict(featuresMultiple(i))
    }
    return predictions
  }

  def predict(features: Array[Double]): Integer = {
    val probability = posterior(features)

    if(probability <= 0.5){
      return 1
    }else{
      return 0
    }
  }

  def logLikelihood(): Double = {
    var logLikelihoodVal: Double = 0.0
    for(i <- 0 to this.featuresMultiple.length-1){
      if (this.labels(i) == 1){
        logLikelihoodVal += Math.log(1 - posterior(this.featuresMultiple(i)))
      }else{
        logLikelihoodVal += Math.log(posterior(this.featuresMultiple(i)))
      }
    }
    return logLikelihoodVal
  }

  def posterior(features: Array[Double]): Double = {
    var probability = intercept
    for(i <- 0 to features.length-1){
      probability += coefficients(i) * features(i)
    }
    // Sigmoid of the regression
    return 1.0 / (1.0 + Math.exp(-probability))
  }

  def gradient(): Array[Double] = {
    val gradient = Array.ofDim[Double](coefficients.length+1)
    for(i <- 0 to featuresMultiple.length-1){
      val posteriorVal = posterior(this.featuresMultiple(i))
      gradient(0) += (this.labelsDouble(i) - posteriorVal)
      for(j <- 0 to coefficients.length-1){
        gradient(j+1) += (this.labelsDouble(i) - posteriorVal) * this.featuresMultiple(i)(j)
      }
    }
    return gradient
  }

  def updateCoefficients() = {
    val full_gradient = gradient()
    this.intercept -= full_gradient(0) * this.alpha
    for(i <- 0 to this.coefficients.length-1){
      this.coefficients(i) -= full_gradient(i+1) * this.alpha
    }
  }
}
