// Part 1 about finding Knight's tours
//=====================================

// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below. Also have a look whether the functions
// at the end are of any help.



type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

//(1) Complete the function that tests whether the position x
//    is inside the board and not yet element in the path.

def is_legal(dim: Int, path: Path, x: Pos) : Boolean = 
	(x._1>=0) && (x._2>=0) && (x._1<dim) && (x._2<dim) && !(path.contains(x))




//(2) Complete the function that calculates for a position x
//    all legal onward moves that are not already in the path. 
//    The moves should be ordered in a "clockwise" manner.
 

def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
	val listPairsPattern = List((1,2),(2,1),(2,-1),(1,-2),(-1,-2),(-2,-1),(-2,1),(-1,2))
	for (movePair <- listPairsPattern;
			if (is_legal(dim,path,(x._1+movePair._1,x._2+movePair._2)))) yield (x._1+movePair._1,x._2+movePair._2)
}

//some test cases
//
//assert(legal_moves(8, Nil, (2,2)) == 
//  List((3,4), (4,3), (4,1), (3,0), (1,0), (0,1), (0,3), (1,4)))
//assert(legal_moves(8, Nil, (7,7)) == List((6,5), (5,6)))
//assert(legal_moves(8, List((4,1), (1,0)), (2,2)) == 
//  List((3,4), (4,3), (3,0), (0,1), (0,3), (1,4)))
//assert(legal_moves(8, List((6,6)), (7,7)) == List((6,5), (5,6)))


//(3) Complete the two recursive functions below. 
//    They exhaustively search for knight's tours starting from the 
//    given path. The first function counts all possible tours, 
//    and the second collects all tours in a list of paths.

       







def count_tours(dim: Int, path: Path) : Int =  
	if (dim==1) 1 else{
		if (dim<=4) 0 else{
			if (legal_moves(dim,path,path.head).size>0) {
				(for (next <- legal_moves(dim,path,path.head)) yield count_tours(dim,next::path)).sum
			}
			else{
				if (path.size==dim*dim) 1 else 0
			}
		}
	}



def enum_tours(dim: Int, path: Path) : List[Path] = {
	if (dim==1) List(List((0,0))) else{
		if (dim<=4) List() else{
			if (legal_moves(dim,path,path.head).size>0){
				(for (next <- legal_moves(dim,path,path.head)) yield (enum_tours(dim,next::path))).flatten
			}
			else{
				if (path.size==dim*dim) List(path) else List()		
			}
		}
	}
}

         




//(5) Implement a first-function that finds the first 
//    element, say x, in the list xs where f is not None. 
//    In that case Return f(x), otherwise None. If possible,
//    calculate f(x) only once.


def first(xs: List[Pos], f: Pos => Option[Path]) : Option[Path] = xs match{
	case List() => None
	case (x::xs1) => {
	val checking = f(x)
	if (checking.isDefined) checking
	else first(xs1,f)
	}	
}


// test cases
//def foo(x: (Int, Int)) = if (x._1 > 3) Some(List(x)) else None
//first(List((1, 0),(2, 0),(3, 0),(4, 0)), foo)   // Some(List((4,0)))
//first(List((1, 0),(2, 0),(3, 0)), foo)          // None




//(6) Implement a function that uses the first-function from (5) for
//    trying out onward moves, and searches recursively for a
//    knight tour on a dim * dim-board.


def first_tour(dim: Int, path: Path) : Option[Path] = {
	if (dim==1) Some(List((0,0))) else{
		if (dim<=4) None else{
			if (legal_moves(dim,path,path.head).size>0){
				first(legal_moves(dim,path,path.head),(loca: Pos)=>{
					first_tour(dim,loca::path)
				}
				)
			}
			else{
				if (path.size==dim*dim) Some(path) else None
			}
		}
	}
}

 





/* Helper functions


// for measuring time
def time_needed[T](code: => T) : T = {
  val start = System.nanoTime()
  val result = code
  val end = System.nanoTime()
  println(f"Time needed: ${(end - start) / 1.0e9}%3.3f secs.")
  result
}

// can be called for example with
//     time_needed(count_tours(dim, List((0, 0))))
// in order to print out the time that is needed for 
// running count_tours

// for printing a board
def print_board(dim: Int, path: Path): Unit = {
  println
  for (i <- 0 until dim) {
    for (j <- 0 until dim) {
      print(f"${path.reverse.indexOf((j, dim - i - 1))}%3.0f ")
    }
    println
  } 
}


*/
