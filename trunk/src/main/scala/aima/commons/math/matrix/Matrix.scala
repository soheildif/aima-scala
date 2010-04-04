package aima.commons.math.matrix

//Rep for m X n immutable matrix
class Matrix(in: Array[Array[Double]]) {

  val elems = in.readOnly

  //Returns jth elem in ith row
  // 0 <= i < m And 0 <= j < n
  def apply(i: Int, j: Int): Double = elems(i)(j)

  //Returns # of rows
  def m: Int = elems.length

  //Returns # of cols
  def n: Int = elems(0).length

  def *(that: Matrix): Matrix = {
    if(this.n != that.m)
      throw new IllegalArgumentException("Can't be multiplied")
    else {
      val newArr = new Array[Array[Double]](this.m, that.n)
      val thatElems = that.elems
      val indices = elems(0).indices.readOnly

      for(i <- 0 to this.m-1) {
        val row = newArr(i)
        val rowThis = elems(i)
        for(j <- 0 to that.n-1) {
          row(j) = indices.foldLeft(0.0)((sum,k) => sum + rowThis(k) * thatElems(k)(j))
        }
      }
      new Matrix(newArr)
    }
  }

  //check if corresponding elements in given arrays are
  //equal upto given delta
  def equals(delta: Double): Boolean
}
