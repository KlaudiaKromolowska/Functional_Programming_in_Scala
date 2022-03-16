package timeusage

import org.apache.spark.sql.functions.col
import org.apache.spark.sql.{Column, ColumnName, DataFrame, Row}

import scala.util.Random
import scala.util.Properties.isWin

class TimeUsageSuite extends munit.FunSuite:
  lazy val (headerColumns, df) = TimeUsage.read("src/main/resources/atussum.csv")
  val (primary, working, other) = TimeUsage.classifiedColumns(headerColumns)
  val dataFrameSummary: DataFrame = TimeUsage.timeUsageSummary(primary, working, other, df.sample(withReplacement = false, 0.1))

  test ("Classifying columns") {
    val columns = List("t0101","t0505","t1303","t12")
    val (primaryList, workList, otherList) = TimeUsage.classifiedColumns(columns)
    val (primaryExpected, workExpected, otherExpected) =((List("t0101")).map(c => col(c)), (List("t0505")).map(c => col(c)), (List("t1303","t12")).map(c => col(c)))

    assert(primaryList==primaryExpected && workList==workExpected && otherList==otherExpected)
  }

  test("timeUsageSummary") {
    assert(dataFrameSummary.columns sameElements Array("working", "sex", "age", "primaryNeeds", "work", "other"))
  }

  test("timeUsageGrouped") {
    val dataFrameGrouped = TimeUsage.timeUsageGrouped(dataFrameSummary)
    dataFrameGrouped.show(10)
  }

  test ("timeUsageSQL") {
    val dataFrameGrouped = TimeUsage.timeUsageGrouped(dataFrameSummary)
    dataFrameGrouped.show(10)
  }

  test ("timeUsageTyped") {
    val dataFrameGrouped = TimeUsage.timeUsageSummaryTyped(dataFrameSummary)
    dataFrameGrouped.show(10)
  }

  test("timeUsageGroupedTyped") {
    val dataFrameTyped = TimeUsage.timeUsageSummaryTyped(dataFrameSummary)
    val typedGrouped = TimeUsage.timeUsageGroupedTyped(dataFrameTyped)
    typedGrouped.show()
  }


