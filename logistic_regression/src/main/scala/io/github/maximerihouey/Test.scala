package io.github.maximerihouey

import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.sql.{DataFrame, Row, SparkSession}
import org.apache.spark.sql.types._
import org.apache.spark.sql.functions._
import org.apache.spark.sql._
import org.apache.spark.ml.classification.LogisticRegression
import org.apache.spark.ml.linalg.{Vector, Vectors}

import org.apache.log4j.Logger
import org.apache.log4j.Level

/**
  * Created by maxime on 04/11/16.
  */
object Test {

  //////////////////////////////////////////////////////
  Logger.getLogger("org").setLevel(Level.OFF)
  Logger.getLogger("akka").setLevel(Level.OFF)
  println("\n\n\n\n\n")
  //////////////////////////////////////////////////////

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
    ///////////////////////////////////////////////////////
    val conf = new SparkConf().setAppName("Simple Application")
    val sc = new SparkContext(conf)
    val sqlContext = new SQLContext(sc)

    import sqlContext.implicits._
//    val sparkSession = SparkSession.builder
//      .master("local")
//      .appName("logistic_regression_scala")
//      .getOrCreate()
    ///////////////////////////////////////////////////////

    val size = 1000

    val class_0_features = buildGaussianDataset(size, 2, -1)
    val class_0_labels = Array.fill[Double](size)(0.0)
    val class_0_labels_int = Array.fill[Integer](size)(0)

    val class_1_features = buildGaussianDataset(size, 2, 1)
    val class_1_labels = Array.fill[Double](size)(1.0)
    val class_1_labels_int = Array.fill[Integer](size)(1)

    val X_data = class_0_features ++ class_1_features
    val y_data = class_0_labels ++ class_1_labels

    // Spark Logistic regression
    val dataFrame = sc.parallelize(y_data zip X_data.map(row => Vectors.dense(row))).toDF("label","features")
    val splits = dataFrame.randomSplit(Array(0.8, 0.2), seed = 11L)
    val DfTrain = splits(0).cache()
    val DfTest = splits(1).cache()

    val fitted_model = new LogisticRegression().fit(DfTrain)
    val predictions = fitted_model.transform(DfTest)

    predictions.show()
    val nbAccurate = predictions.select("label", "prediction").map(
      row => if (row.getDouble(0) == row.getDouble(1)) 1.0 else 0.0
    ).select(sum("value")).first().get(0)

    val nbAccurateDouble : Double = nbAccurate.asInstanceOf[Double]
    println("\nAccuracy Spark     : %f | Count: %d\n".format(nbAccurateDouble / predictions.count(), predictions.count()))

    val whole_predictions = new LogisticRegression().fit(dataFrame).transform(dataFrame)
    val whole_nbAccurate = whole_predictions.select("label", "prediction").map(
      row => if (row.getDouble(0) == row.getDouble(1)) 1.0 else 0.0
    ).select(sum("value")).first().get(0)
    val whole_nbAccurateDouble : Double = whole_nbAccurate.asInstanceOf[Double]
    println("\nAccuracy Spark (train) : %f | Count: %d\n".format(whole_nbAccurateDouble / whole_predictions.count(), whole_predictions.count()))


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
