package observatory.utils

import org.apache.log4j.Logger
import org.apache.log4j.Level

trait SparkJob {
  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

}