package matr

import matr.TupleSupport.MatrixTupleReader
import matr.TupleSupport.RowTupleReader
import matr.util.RowMajorIndex

/** Central entry point for creating Matrices. Modules implementing the `Matrix` trait should also provide an instance
  * of this type class.
  */
trait MatrixFactory[R <: Int, C <: Int, T](using ValueOf[R], ValueOf[C], Numeric[T]):

    private val rowDim: R = valueOf[R]
    private val colDim: C = valueOf[C]
    private val num: Numeric[T] = summon[Numeric[T]]

    /** Returns a fresh `Matrix.Builder` instance.
      */
    def builder: Matrix.Builder[R, C, T]

    /** Creates a Matrix containing the specified elements, assuming row-major order. Dimensions will be checked at
      * runtime.
      */
    def rowMajor(elements: T*): Matrix[R, C, T] =
        require(
          elements.length == rowDim * colDim,
          s"Size of given element collection must be ${rowDim * colDim}, but was ${elements.size}"
        )
        val buildr: Matrix.Builder[R, C, T] = builder
        var idx: Int = 0
        while (idx < elements.length) do
            val (rowIdx, colIdx) = RowMajorIndex.fromIdx(idx, colDim)
            buildr(rowIdx, colIdx) = elements(idx)
            idx = idx + 1
        buildr.result

    /** Creates a Matrix containing the elements returned by the specified function.
      *
      * @param fillElem
      *   function returning the element at the specified position (row index, column index)
      */
    def tabulate(fillElem: (Int, Int) => T): Matrix[R, C, T] =
        val buildr: Matrix.Builder[R, C, T] = builder
        buildr.iterate: (rowIdx, colIdx) =>
            buildr(rowIdx, colIdx) = fillElem(rowIdx, colIdx)
        buildr.result

    /** Creates a Matrix of zeros.
      */
    def zeros: Matrix[R, C, T] = tabulate((_, _) => num.zero)

    /** Creates a Matrix of ones.
      */
    def ones: Matrix[R, C, T] = tabulate((_, _) => num.one)

    /** Creates the identity Matrix.
      */
    def identity(using Matrix.Requirements.IsSquare[R, C]): Matrix[R, C, T] =
        tabulate: (rowIdx, colIdx) =>
            if rowIdx == colIdx then
                num.one
            else
                num.zero

    /** Creates a Matrix with the specified elements that are structured row by row from tuples.
      *
      * Dimensions will be checked at compile-time.
      *
      * When invoking this method the following import has to be in place:
      * ```
      * import matr.TupleSupport.given
      * ```
      *
      * The creation of i.e. a 2x3 Matrix looks like this:
      * ```
      * val m = MatrixFactory[2, 3, Int].fromTuple(
      *   (11, 12, 13),
      *   (21, 22, 23)
      * )
      * ```
      */
    def fromTuple[MatrixTuple <: Tuple, RowTuple <: Tuple](matrixTuple: MatrixTuple)(
        using
        MatrixTupleReader[MatrixTuple, RowTuple],
        RowTupleReader[RowTuple, T],
        Tuple.Size[MatrixTuple] =:= R,
        Tuple.Size[RowTuple] =:= C
    )
        : Matrix[R, C, T] =
        val buildr: Matrix.Builder[R, C, T] = builder
        val setElem: (Int, Int, T) => Unit = (rowIdx, colIdx, elem) => buildr.update(rowIdx, colIdx, elem)
        val setRow: (Int, RowTuple) => Unit =
            (rowIdx, rowTuple) => summon[RowTupleReader[RowTuple, T]].readRow(rowIdx, rowTuple, colDim, setElem)
        summon[MatrixTupleReader[MatrixTuple, RowTuple]].readMatrix(matrixTuple, rowDim, setRow)
        buildr.result

object MatrixFactory:

    /** Returns the implicitly available `MatrixFactory`.
      */
    def apply[R <: Int, C <: Int, T](using mf: MatrixFactory[R, C, T]): MatrixFactory[R, C, T] = mf
