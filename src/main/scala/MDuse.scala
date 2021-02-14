

import scalation.linalgebra.MatrixD
import scalation.linalgebra.Eigenvalue
import scalation.linalgebra.Eigenvector
import scalation.linalgebra._
import scalation.linalgebra.Fac_LU
import scala.util.Random

import MatrixD.eye



class MDuse(a: MatrixD){
  //Initialize parameters
  var r_lambda = 40 //normalization parameter
  var nf = 200  //dimension of latent vector of each user and item
  val alpha = 40  //confidence level
  val ni = a.dim2 //number of items 
  val nu = a.dim1 //number of users 

  //created a matrix with random values
  def createRM(row_number : Int, column_number : Int) : MatrixD = {
 	  var matrixA = new MatrixD (row_number, column_number)
    var i = 0
    var j = 0
		for (i <- 0 to row_number-1 ) {
			for (j <- 0 to column_number-1 ) {
				matrixA(i,j) = Random.nextInt(10000) * 0.000001
        
			}
		}
	  matrixA
	} // create a random matrix 

  //Replace all elements in the matrix with squared value
  def square(a:MatrixD): (MatrixD) = {
    val S = a.copy()
    for (i <- 0 to a.dim1-1 ) {
			for (j <- 0 to a.dim2-1 ) { 
          S(i,j) = a(i,j) * a(i,j)
			}
		}
    S
  }

  // Initialize Binary Rating Matrix P
  def ConP(nu : Int, ni : Int) : MatrixD = {
    val P = a.copy()
		for (i <- 0 to a.dim1-1 ) {
			for (j <- 0 to a.dim2-1 ) {
        if(P(i,j) > 0)  
          P(i,j) = 1 
			}
		}
    //println(P.toString())
    P
  }

  // Initialize Confidence Matrix C
  def ConfC(a: MatrixD) : MatrixD = {
    val C = a * alpha + 1
    //println(C.toString())
    C
  }

  //Set up loss function
  def Lossf(c: MatrixD, p: MatrixD, xTy: MatrixD, X: MatrixD, Y: MatrixD, r_lambda: Int): (Double, Double, Double, Double) = {
    val predict_error = square(p - xTy)
    //println("1:"+predict_error)
    val confidence_error = (c**predict_error).sum
    //println("2:"+confidence_error)
    val regularization =  r_lambda * (square(X).sum + square(Y).sum)
    //println("3:"+regularization)
    val total_loss = confidence_error + regularization
    //println("4:"+total_loss)
    (predict_error.sum, confidence_error, regularization, total_loss)
  }

  //construct diagonal matrix with row
  def diagMR(mat : MatrixD, tarRow: Int) : MatrixD = {
    val tempMat = mat.selectRows(Array(tarRow))
    val tarMat = new MatrixD(tempMat.dim2, tempMat.dim2)
    for(i <- 0 to tempMat.dim2 -1){
        tarMat(i,i) = tempMat(0,i)
    }
    tarMat
  }
 
  //construct diagonal matrix with column
  def diagMC(mat : MatrixD, tarCol: Int) : MatrixD = {
    val tempMat = mat.selectCols(Array(tarCol))
    val tarMat = new MatrixD(tempMat.dim1, tempMat.dim1)
    for(i <- 0 to tempMat.dim1 -1){
        tarMat(i,i) = tempMat(i,0)
    }
    tarMat
  }

  //set row to the matrix row
  def setRow(mat : MatrixD, tarRow: VectoD, index: Int) : MatrixD = {
    
    for(j <- 0 to mat.dim2 -1){
        mat(index,j) = tarRow(j)
    }
    mat
  }

  //convert vector to matrix
  def makeVec(mat : MatrixD) : VectoD = {
    val vec = new VectorD(mat.dim1)
    
    for(j <- 0 to mat.dim1 -1){
        vec(j) = mat(j,0)
    }
    vec
  }
  


  def optimize_user(X: MatrixD, Y: MatrixD, C: MatrixD, P: MatrixD, nu:Int, nf:Int, r_lambda:Int)={
    val yT = Y.t
    
    for (i <- 0 to nu-1 ) {
      val Cu = diagMR(C, i)
      
      val yT_Cu_y = yT * Cu * Y
      val lI = eye(nf) * r_lambda
      val yT_Cu_pu = makeVec((yT * Cu) * ((P.selectRows(Array(i))).t))
    
        //X(i,j) = MatrixD.bsolve(yT_Cu_y + lI, yT_Cu_pu)
      //setRow(X, solveM(yT_Cu_y + lI, yT_Cu_pu), i)
      //setRow(X, (yT_Cu_y + lI).solve(yT_Cu_pu), i)
      //println(solveM(yT_Cu_y + lI, yT_Cu_pu))
      //setRow(X, , i)
      
    }

  //   for u in range(nu):
  //       Cu = np.diag(C[u])
  //       yT_Cu_y = np.matmul(np.matmul(yT, Cu), Y)
  //       lI = np.dot(r_lambda, np.identity(nf))
  //       yT_Cu_pu = np.matmul(np.matmul(yT, Cu), P[u])
  //       X[u] = np.linalg.solve(yT_Cu_y + lI, yT_Cu_pu)
  }

  def optimize_item(X: MatrixD, Y: MatrixD, C: MatrixD, P: MatrixD, nu:Int, nf:Int, r_lambda:Int)={
    val xT = X.t

    for (i <- 0 to Y.dim2-1 ) {
      val Ci = diagMC(C, i)
      val xT_Ci_x = xT * Ci * X
      val lI = eye(nf) * r_lambda
      val xT_Ci_pi = makeVec((xT * Ci) * (P.selectCols(Array(i))))
      Y.setCol(i, solveM(xT_Ci_x + lI, xT_Ci_pi))
    }
  }
    
  def solveM(a: MatrixD , b: VectoD): VectoD = {
      a.solve(lud_npp_new(a), b)
    }

     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor 'this' matrix into the product of upper and lower triangular
     *  matrices '(l, u)' using the 'LU' Factorization algorithm.
     *  Caveat:  This version requires square matrices and performs no partial pivoting.
     *  @see `Fac_LU` for a more complete implementation
     */
    def lud_npp_new(tar : MatrixD): (MatrixD, MatrixD) =
    {
        
        val l = new MatrixD (tar.dim1)          // lower triangular matrix
        val u = new MatrixD (tar)          // upper triangular matrix (a copy of this)  

        // for(j <- 0 to tar.dim2) {             
        //     for(i <- j+1 to tar.dim1) {  
        //         var mult = tar(i, j) / tar(j, j);  
        //         println("mult ="+ mult) 
        //         for(k <- j to tar.dim2-1) {  
        //              u(i, k) = tar(i ,k) - tar(j ,k ) * mult
        //              println("u : i = "+i + ", j = "+j) 
        //              //得出上三角矩阵U,通过减去矩阵的第一行,第二行,第一行(第二行)得到上三角矩阵
        //          }  
        //         l(i, j) = mult  //得到下三角矩阵是得出上三角矩阵的乘积因子
        //         println("l : i = "+i + ", j = "+j)
        //     }  
        // } 
        for(i <- 0 to tar.dim1 - 1) {
            var pivot = u(i, i)
            
            println("pivot = " + u(i, i))
            l(i,i) = 1.0   
            for(j <- i + 1 until tar.dim2){
                l(i, j) = 0.0
            }      
            println("before lu : ")
            println(l.toString())
            println(u)
            for(k <- i + 1 until tar.dim1) { 
                var mult = u(k, i) / pivot
                
                l(k, i) = mult
                println("mult ="+ mult, "asdfsd:"+l(k,i)) 
                for(j <- 0 to tar.dim2 - 1) {  
                     u(k, j) = u(k ,j) -  mult * u(i ,j)
                    
                     println("u : i = "+i + ", j = "+j+",u(k, j):"+ u(k, j) ) 
                     //得出上三角矩阵U,通过减去矩阵的第一行,第二行,第一行(第二行)得到上三角矩阵
                 }  
                  //得到下三角矩阵是得出上三角矩阵的乘积因子
                println("l : i = "+i + ", k = "+k)
            }
            //println("uuuuuuuuu", u)  
        }  
        (l, u)
    }

}


object MDTest extends App{
  // val r = new MatrixD ((10, 11), 1, 0, 2, 4, 4, 0, 0, 1, 0, 1, 0,
  //                                0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 1, 
  //                                0, 0, 6, 0, 0, 0, 0, 1, 0, 4, 0,
  //                                0, 3, 4, 7, 3, 0, 0, 2, 2, 0, 0,
  //                                0, 5, 5, 0, 3, 0, 0, 0, 0, 0, 0,
  //                                0, 0, 0, 0, 0, 1, 5, 0, 0, 5, 0,
  //                                0, 0, 4, 0, 0, 0, 3, 0, 0, 0, 5,
  //                                0, 0, 0, 3, 0, 4, 0, 3, 0, 0, 4,
  //                                0, 3, 0, 0, 0, 0, 5, 0, 5, 5, 0,
  //                                0, 0, 0, 3, 0, 0, 2, 3, 4, 5, 7
  //                                )
  val r = new MatrixD((3,3), 1, 1, 1,
                             4, 3, -1,
                             3, 5, 3
   )
  val target = new MDuse(r)
  
  val X = target.createRM(target.nu, target.nf)
  val Y = target.createRM(target.ni, target.nf)
  // println("X = "+ X.toString)
  // println("Y = "+ Y.toString)
  

  val p = target.ConP(target.nu,target.ni)
  val c = target.ConfC(r) 
  //target.Lossf(c, p, predict, X, Y, target.r_lambda)
  // val (predict_error, confidence_error, regularization, total_loss) = 

  // val predict_error = target.square(p - predict)
  // //println(predict_error)
  // println(c.dim1 +", "+c.dim2)
  // println(predict_error.dim1 +", "+predict_error.dim2)
  // val confidence_error = (c**predict_error).sum

  //  println("2- "+confidence_error)
  // val regularization =  target.r_lambda * ((target.square(X)).sum + (target.square(Y)).sum)
  // println("3- "+regularization)

  // val total_loss = confidence_error + regularization
  // println("4-"+total_loss)
  var predict_errors = ""
  var confidence_errors = ""
  var regularization_list = ""
  var total_losses = ""
  
  for (i <- 0 to 1) {
    if (i != 0){
        //target.optimize_user(X, Y, c, p, target.nu, target.nf, target.r_lambda)
        //target.optimize_item(X, Y, c, p, target.ni, target.nf, target.r_lambda)
    }
    var predict = X * (Y.t)
    var (predict_error, confidence_error, regularization, total_loss) = target.Lossf(c, p, predict, X, Y, target.r_lambda)

    // predict_errors += predict_error
    // confidence_errors += confidence_error
    // regularization_list += regularization
    // total_losses += total_loss

    println("----------------step "+i+"----------------")
    println("predict error: " + predict_error)
    println("confidence error: " + confidence_error)
    println("regularization: " + regularization)
    println("total loss: " + total_loss)
  }
  
  val tempMat = r.selectCols(Array(0))  
  var predict = X * (Y.t)
  println(tempMat)
  println("final predict")
  //println(predict)

}

