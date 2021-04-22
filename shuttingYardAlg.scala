// Shunting Yard Algorithm 
// including Associativity for Operators 
// =====================================

object SYA {


// type of tokens
type Toks = List[String]

def split(s: String) : Toks = s.split(" ").toList

abstract class Assoc
case object LA extends Assoc
case object RA extends Assoc
val parenthasis = List("(", ")")


// power is right-associative,
// everything else is left-associative
def assoc(s: String) : Assoc = s match {
  case "^" => RA
  case _ => LA
}



// the precedences of the operators
val precs = Map("+" -> 1,
  		"-" -> 1,
		"*" -> 2,
		"/" -> 2,
                "^" -> 4)

// the operations in the basic version of the algorithm
val ops = List("+", "-", "*", "/", "^")

def isNumeric(x: String) = x forall Character.isDigit
def isParentasis(x: String) = parenthasis.contains(x)


def syard(toks: Toks, st: Toks = Nil, out: Toks = Nil) : Toks = {
  if(toks.size == 0){
		out:::st
	}
	else{
		//there are more toksd
		if(isNumeric(toks.head)){
			//numeric
			syard(toks.drop(1), st, out:::List(toks.head))

		}
		else{
			//not numeric
			if(parenthasis.contains(toks.head)){
				if(toks.head=="("){
					//is opening
					syard(toks.drop(1), toks.head::st, out)
				}
				else {
					//is closing
					syard(toks.drop(1), st.drop((st.indexOf("("))+1), out:::st.slice(0,st.indexOf("(")))
				}
			}
			else{
				//handle presedence
				if (st.size == 0){
					syard(toks.drop(1), toks.head::st, out)
				}
				else{
					if (parenthasis.contains(st.head)){
						syard(toks.drop(1), toks.head::st, out)
					}
          else if (toks.head == "^"){
            syard(toks.drop(1), toks.head::st, out)
          }
					else if(precs(toks.head)<=precs(st.head)){
						val newStack = st.drop(1)
						syard(toks.drop(1), toks.head::newStack, out:+st(0))
					}
					else{
						syard(toks.drop(1), toks.head::st, out)
					}
				}
			 }
		}
	}
}

def compute(toks: Toks, st: List[Long] = Nil) : Long = {
  if(toks.size == 0){
		 st(0)
	 }
	 else if(isNumeric(toks.head)){
		 compute(toks.drop(1), toks.head.toInt::st)
	 }
	 else{
		 val opperation = toks.head
		 val first = st(1)
		 val second = st(0)
		 opperation match {
			 case "+" => compute(toks.drop(1), (first.toInt + second.toInt)::st.drop(2))
			 case "-" => compute(toks.drop(1), (first.toInt - second.toInt)::st.drop(2))
			 case "*" => compute(toks.drop(1), (first.toInt * second.toInt)::st.drop(2))
			 case "/" => compute(toks.drop(1), (first.toInt / second.toInt)::st.drop(2))
       case "^" => compute(toks.drop(1), scala.math.pow(first.toInt,second.toInt).toLong::st.drop(2))
		 }
	 }
}


// test cases
// compute(syard(split("3 + 4 * ( 2 - 1 )")))   // 7
// compute(syard(split("10 + 12 * 33")))       // 406
// compute(syard(split("( 5 + 7 ) * 2")))      // 24
// compute(syard(split("5 + 7 / 2")))          // 8
// compute(syard(split("5 * 7 / 2")))          // 17
// compute(syard(split("9 + 24 / ( 7 - 3 )"))) // 15
// compute(syard(split("4 ^ 3 ^ 2")))      // 262144
// compute(syard(split("4 ^ ( 3 ^ 2 )")))  // 262144
// compute(syard(split("( 4 ^ 3 ) ^ 2")))  // 4096
// compute(syard(split("( 3 + 1 ) ^ 2 ^ 3")))   // 65536

}
