import scala.annotation.tailrec

object Homework1 {
  def main(args: Array[String]) = {
    val weekDays: List[String] = List("Poniedziałek", "Wtorek", "Środa", "Czwartek", "Piątek", "Sobota", "Niedziela")

    // Zadanie 1
    // a)
    def printDays(weekDays: List[String]): String = {
      var result = ""
      for (i <- 0 until weekDays.length) {
        result = result + weekDays(i)
        if(i < weekDays.length - 1) {
          result = result + ", "
        }
      }
      result
    }

    println("### 1. Task a)")
    println(printDays(weekDays))

    // b)
    def printDaysStartsWithP(weekDays: List[String]):String = {
      var result = ""
      for (day <- weekDays) {
        if (day.take(1) == "P") {
          result = result + day + ", "
        }
      }
      if(result.takeRight(2) == ", "){
        return result.substring(0, result.length - 2)
      }
      result
    }

    println("### 1. Task b)")
    println(printDaysStartsWithP(weekDays))

    // c)
    def printDaysWhile(weekDays: List[String]):String = {
      var result = ""
      var i = 0;
      while (i < weekDays.length) {
        result = result + weekDays(i) + ", "
        i += 1
      }
      if(result.takeRight(2) == ", "){
        return result.substring(0, result.length - 2)
      }
      result
    }

    println("### 1. Task c)")
    println(printDaysWhile(weekDays))

    // Zadanie 2
    // a)
    def printDaysRecursion(weekDays: List[String]): String = {
      if (weekDays.isEmpty)
        ""
      else if (weekDays.length == 1){
        weekDays.head
      }
      else weekDays.head + ", " + printDaysRecursion(weekDays.tail)

    }

    println("### 2. Task a)")
    println(printDaysRecursion(weekDays))

    // b)
    def printDaysRecursionTheEnd(weekDays: List[String]): String = {
      if (weekDays.isEmpty)
        ""
      else if (weekDays.length == 1){
        weekDays.head
      }
      else printDaysRecursionTheEnd(weekDays.tail) + ", " + weekDays.head
    }

    println("### 2. Task b)")
    println(printDaysRecursionTheEnd(weekDays))

    // Zadanie 3
    def printDaysTailRecursion(weekDays: List[String]): String = {
      @tailrec
      def concatDays(weekDays: List[String], concatWeek: String): String = {
        weekDays match {
          case Nil => concatWeek
          case x :: tail => concatDays(tail, concatWeek + x + ", ")
        }
      }
      val result = concatDays(weekDays, ",")
      result.substring(1, result.length - 2)
    }

    println("### 3.")
    println(printDaysTailRecursion(weekDays))

    // Zadanie 4
    // a)
    def printDaysFoldLeft(weekDays: List[String]): String = {
      val result = weekDays.foldLeft("")(_ + _ + ", ")
      if(result.takeRight(2) == ", "){
        return result.substring(0, result.length - 2)
      }
      result
    }

    println("### 4. Task a)")
    println(printDaysFoldLeft(weekDays))

    // b)
    def printDaysFoldRight(weekDays: List[String]): String = {
      val result = weekDays.foldRight("")(", " + _ + _ )
      if(result.take(2) == ", "){
        return result.substring(2, result.length)
      }
      result
    }

    println("### 4. Task b)")
    println(printDaysFoldRight(weekDays))

    // c)
    def printDaysFoldLeftStartsWithP(weekdays: List[String]): String = {
      val result = weekDays.foldLeft("")((x, y) => {
        if(y.startsWith("P")){
          x + y + ", "
        } else {
          x + ""
        }
      })
      if(result.takeRight(2) == ", "){
        return result.substring(0, result.length - 2)
      }
      result
    }

    println("### 4. Task c)")
    println(printDaysFoldLeftStartsWithP(weekDays))

    // Zadanie 5
    val productItems = Map("chleb" -> 2.20, "masło" -> 3.40, "ser" -> 5.60)
    val productItemsNewPrice = productItems map {case (key, value) => (key, value * 0.90)}

    println("### 5.")
    println(productItemsNewPrice)

    // Zadanie 6
    val tupleExample = Tuple3(7.77, "maslo", true)

    def printTuplesExample(tuple: (Double, String, Boolean)) = {
      tuple.productIterator.foreach { item => println(item)}
    }

    println("### 6.")
    printTuplesExample(tupleExample)

    // Zadanie 7
    val people = Map("Jan" -> "Nowak", "Krzysztof" -> "Jerzyna", "John" -> "Snow")

    def showPerson(people: Option[String]) = people match {
      case Some(x) => x
      case None => "Nie znam takiej osoby"
    }

    println("### 7.")
    println(showPerson(people.get("Jan")))
    println(showPerson(people.get("Krzysztof")))
    println(showPerson(people.get("John")))
    println(showPerson(people.get("Pawel")))

    // Zadanie 8
    val numbers = List (-3, -2, -1, 0, 1, 2, 3, 0, 4, 5, 6, 0, 0, 7, 8, 9, 0, 10, 11, 12)

    def eliminateZeroRecursion(numbers: List[Int], num: Int = 0): List[Int] = numbers match {
      case x :: y =>
        if(x == num)
          eliminateZeroRecursion(y, num)
        else
          x :: eliminateZeroRecursion(y, num)
      case Nil => Nil
    }

    println("### 8.")
    println(eliminateZeroRecursion(numbers))

    // Zadanie 9

    def addOneToItem(numbers: List[Int]) = {
      numbers map (item => item + 1)
    }

    println("### 9.")
    println(addOneToItem(numbers))

    // Zadanie 10
    val numbers2 = List(-10, -8, -6, -4, -1, 0, 2, 6, 10, 12, 15, 17)

    def printAbsoluteValues(numbers: List[Int]) = {
      numbers.filter(_ >= -5).filter(_ <= 12).map(_.abs)
    }

    println("### 10.")
    println(printAbsoluteValues(numbers2))

  }
}
