package io.github.maximerihouey

/**
  * Created by maxime on 04/11/16.
  */
object Test {

  def buildGaussianDataset(size: Integer, nDims: Integer, mean: Double): Array[Array[Double]] ={
    val array = Array.ofDim[Double](size, nDims);
    for(j <- 0 to (nDims-1)){
      for(i <- 0 to (size-1)){
        array(i)(j) = mean + scala.util.Random.nextGaussian()
      }
    }
    return array;
  }

  def main(args: Array[String]) {

    val size = 1000

    val class_0_features = buildGaussianDataset(size, 2, -1)
    val class_0_labels = Array.fill[Double](size)(0.0)
    val class_0_labels_int = Array.fill[Integer](size)(0)

    val class_1_features = buildGaussianDataset(size, 2, 1)
    val class_1_labels = Array.fill[Double](size)(1.0)
    val class_1_labels_int = Array.fill[Integer](size)(1)

    val X_data = class_0_features ++ class_1_features
    val y_data = class_0_labels ++ class_1_labels

    // Logistique regression
    val y_data_int = class_0_labels_int ++ class_1_labels_int

    val logit = new LogistiqueRegression()
    logit.fit(X_data, y_data_int)
    val predictions2 = logit.predict(X_data)
    val accuracy2 = (y_data_int zip predictions2).map(row => if(row._1 == row._2) 1.0 else 0.0).sum / predictions2.length
    println("\nAccuracy Logistique: %f | Count: %d\n".format(accuracy2, predictions2.length))

    println(">>>> Coeficients >>>>")
    println("> %f".format(logit.intercept))
    for(i <- 0 to logit.coefficients.length-1){
      println("> %f".format(logit.coefficients(i)))
    }
  }
}
