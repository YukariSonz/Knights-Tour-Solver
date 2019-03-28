// Part 2 about finding a single tour for a board using the Warnsdorf Rule
//=========================================================================

// !!! Copy any function you need from file knight1.scala !!!
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

def is_legal(dim: Int, path: Path, x: Pos) : Boolean = 
	(x._1>=0) && (x._2>=0) && (x._1<dim) && (x._2<dim) && !(path.contains(x))
def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
	val listPairsPattern = List((1,2),(2,1),(2,-1),(1,-2),(-1,-2),(-2,-1),(-2,1),(-1,2))
	for (movePair <- listPairsPattern;
			if (is_legal(dim,path,(x._1+movePair._1,x._2+movePair._2)))) yield (x._1+movePair._1,x._2+movePair._2)
}


def first(xs: List[Pos], f: Pos => Option[Path]) : Option[Path] = xs match{
	case List() => None
	case (x::xs1) => {
	val checking = f(x)
	if (checking.isDefined) checking
	else first(xs1,f)
	}	
}


//(6) Complete the function that calculates a list of onward
//    moves like in (2) but orders them according to Warnsdorf’s 
//    rule. That means moves with the fewest legal onward moves 
//    should come first.


def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = legal_moves(dim,path,x) sortBy(legal_moves(dim,path,_).size)


//(7) Complete the function that searches for a single *closed* 
//    tour using the ordered_moves function from (6). This
//    function will be tested on a 6 x 6 board. 


def can_back(x1: Pos, x2: Pos): Boolean = {
	val listPairsPattern = List((1,2),(2,1),(2,-1),(1,-2),(-1,-2),(-2,-1),(-2,1),(-1,2))
	(for (movePair <- listPairsPattern) yield (x1._1 + movePair._1 == x2._1 && x1._2 + movePair._2 == x2._2)).contains(true)
	
}

def first_closed_tour_heuristic(dim: Int, path: Path) : Option[Path] = {
	if (dim==1) Some(List((0,0))) else{
		if (dim<=4) None else{
			if (ordered_moves(dim,path,path.head).size>0){
				first(ordered_moves(dim,path,path.head),(loca: Pos)=>{
					first_closed_tour_heuristic(dim,loca::path)
				}
				)
			}
			else{
				if (path.size==dim*dim && can_back(path.head,path.last) ) Some(path) else None
				
			}
		}
	}
	
}

 


//(8) Same as (7) but searches for *non-closed* tours. This 
//    version of the function will be called with dimensions of 
//    up to 30 * 30.

def first_tour_heuristic(dim: Int, path: Path) : Option[Path] = {
	if (dim==1) Some(List((0,0))) else{
		if (dim<=4) None else{
			if (ordered_moves(dim,path,path.head).size>0){
				first(ordered_moves(dim,path,path.head),(loca: Pos)=>{
					first_tour_heuristic(dim,loca::path)
				}
				)
			}
			else{
				if (path.size==dim*dim) Some(path) else None
			}
		}
	}
}


