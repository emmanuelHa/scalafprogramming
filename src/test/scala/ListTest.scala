import org.scalatest.FlatSpec


object Runner extends App {
  org.scalatest.run(new ListTest())
}

class ListTest extends FlatSpec {

  "A List" should "sum up values" in {
    val sum = datastructures.List.sum(datastructures.List(1,2,3,4))
    assert(sum == 10)
  }

  "An empty List" should "sum up to 0" in {
    val sum = datastructures.List.sum(datastructures.List())
    assert(sum == 0)
  }

  "A List" should "product" in {
    val product = datastructures.List.product(datastructures.List(1,2,3,4))
    assert(product == 24)
  }

  "An empty List" should "product to 0" in {
    val product = datastructures.List.product(datastructures.List())
    assert(product == 1)
  }

  "A List" should "get tail" in {
    val tail = datastructures.List.tail(datastructures.List(1,2,3,4))
    assert(tail == datastructures.List(2,3,4))
  }

  "A List" should "change head" in {
    val headChanged = datastructures.List.setHead(datastructures.List(1,2,3,4), 5)
    assert(headChanged == datastructures.List(5,2,3,4))
  }

  "A List" should "drop n first elements" in {
    val dropped = datastructures.List.drop(datastructures.List(1,2,3,4), 2)
    assert(dropped == datastructures.List(3,4))
  }

  "A List" should "drop while predicate true" in {
    val xs: datastructures.List[Int] = datastructures.List(1,2,3,4)
    val dropped = datastructures.List.dropWhile(xs)(x =>x<3)
    assert(dropped == datastructures.List(3,4))
  }

  "A List" should "append to another list" in {
    val xs: datastructures.List[Int] = datastructures.List(1,2,3)
    val xs2: datastructures.List[Int] = datastructures.List(4,5,6)

    val appended = datastructures.List.append(xs, xs2)
    assert(appended == datastructures.List(1,2,3,4,5,6))
  }

  "A List" should "sum up with foldRight values" in {
    val sum = datastructures.List.sum2(datastructures.List(1,2,3,4))
    assert(sum == 10)
  }

  "A List" should "product with foldRight to values" in {
    val sum = datastructures.List.product2(datastructures.List(1,2,3,4))
    assert(sum == 24)
  }

  "A List" should "return length with foldRight" in {
    val sum = datastructures.List.length(datastructures.List(1,2,3,4))
    assert(sum == 4)
  }

  "A List" should "add 1 with foldRight" in {
    val sum = datastructures.List.add1(datastructures.List(1,2,3,4))
    assert(sum == datastructures.List(2,3,4,5))
  }

  "A List" should "convert double to string with foldRight" in {
    val sum = datastructures.List.doubleToString(datastructures.List(1.0,2.0,3.0))
    assert(sum == datastructures.List("1.0","2.0","3.0"))
  }

  "A List" should "sum up with foldLeft values" in {
    val sum = datastructures.List.sum3(datastructures.List(1,2,3,4))
    assert(sum == 10)
  }

  "A List" should "product with foldLeft to values" in {
    val sum = datastructures.List.product3(datastructures.List(1,2,3,4))
    assert(sum == 24)
  }

  "A List" should "return length with foldLeft" in {
    val sum = datastructures.List.length3(datastructures.List(1,2,3,4))
    assert(sum == 4)
  }

  "A List" should "reverse with foldLeft" in {
    val reversedList = datastructures.List.reverse(datastructures.List(1,2,3,4))
    assert(reversedList == datastructures.List(4,3,2,1))
  }

  "A List" should "map tail recursive with foldLeft" in {
    val map = datastructures.List.map_tail_recursive(datastructures.List(1,-2,3,-4))(x => Math.abs(x))
    assert(map == datastructures.List(1,2,3,4))
  }

  "A List" should "filter with foldLeft" in {
    val map = datastructures.List.filter_fold_left(datastructures.List(1,2,3,4))(x => x > 2)
    assert(map == datastructures.List(3,4))
  }

}
