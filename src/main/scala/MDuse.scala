import scalation.linalgebra.MatrixD
import scalation.linalgebra.Eigenvalue
import scalation.linalgebra.Eigenvector
import scala.util.Random

class MDuse(a: MatrixD){

  //Initialize parameters
  var r_lambda = 40 //normalization parameter
  var nf = 200  //dimension of latent vector of each user and item
  val alpha = 40  //confidence level
  val ni = a.dim2 //number of items 
  val nu = a.dim1 //number of users 

  //Test function
  def testf = {
    // val e = (new Eigenvalue (a)).getE ()
    // val v = (new Eigenvector (a, e)).getV
    // println(s"e = "+ e + "v = "+v)
    // println(s"d1 = "+a.dim1+"d2 = "+a.dim2)

    // val nu = a.dim1
    // val ni = a.dim2
    // val X = createRM(nu, nf)
    // val Y = createRM(ni, nf)
    // println("X:" + X.toString())
    // println( "dim1 = "+ X.dim1 + "," + "dim2 = "+ X.dim2  )
    //  println("Y:" + Y.toString())
    // println( "dim1 = "+ Y.dim1 + "," + "dim2 = "+ Y.dim2  )

    
  }


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

  def square(a:MatrixD): (MatrixD) = {
    val S = a.copy()
    for (i <- 0 to a.dim1-1 ) {
			for (j <- 0 to a.dim2-1 ) { 
          S(i,j) = a(i,j) * a(i,j)
			}
		}
    S
  }

  //Initialize user and item matrix based on input matrix
  def data_preprocess : (MatrixD, MatrixD) = { 
    val X = createRM(nu, nf)
    val Y = createRM(ni, nf)
    // println("X:" + X.toString())
    // println( "dim1 = "+ X.dim1 + "," + "dim2 = "+ X.dim2  )
    (X,Y)
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



  // def optimize_user(X: MatrixD, Y: MatrixD, C: MatrixD, P: MatrixD, nu:Int, nf:Int, r_lambda:Int)={
  //   yT = Y.t

  //   for (i <- 0 to nu-1 ) {
  //     Cu = C[i].diag
  //     yT_Cu_y = outer(yT, Cu) * Y
  //     lI = eye(nf) * r_lambda
  //     yT_Cu_pu = outer(yT, Cu) dot P[i]
  //     X[i] = n
  //   }

  //   for u in range(nu):
  //       Cu = np.diag(C[u])
  //       yT_Cu_y = np.matmul(np.matmul(yT, Cu), Y)
  //       lI = np.dot(r_lambda, np.identity(nf))
  //       yT_Cu_pu = np.matmul(np.matmul(yT, Cu), P[u])
  //       X[u] = np.linalg.solve(yT_Cu_y + lI, yT_Cu_pu)
  // }
    

}


object MDTest extends App{
  val r = new MatrixD ((10, 11), 0, 0, 0, 4, 4, 0, 0, 0, 0, 0, 0,
                                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 
                                 0, 0, 0, 0, 0, 0, 0, 1, 0, 4, 0,
                                 0, 3, 4, 0, 3, 0, 0, 2, 2, 0, 0,
                                 0, 5, 5, 0, 0, 0, 0, 0, 0, 0, 0,
                                 0, 0, 0, 0, 0, 0, 5, 0, 0, 5, 0,
                                 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 5,
                                 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 4,
                                 0, 0, 0, 0, 0, 0, 5, 0, 0, 5, 0,
                                 0, 0, 0, 3, 0, 0, 0, 0, 4, 5, 0
                                 )

  val target = new MDuse(r)
  
  val X = target.createRM(target.nu, target.nf)
  val Y = target.createRM(target.ni, target.nf)
  // println("X = "+ X.toString)
  // println("Y = "+ Y.toString)
  val predict = X * (Y.t)

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
  val (predict_error, confidence_error, regularization, total_loss) = target.Lossf(c, p, predict, X, Y, target.r_lambda)


}