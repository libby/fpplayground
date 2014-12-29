trait Cup {
    val price: Int
}
case class Coffee(price: Int = 4) extends Cup

case class CreditCard(id: Int)

case class Charge(cc: CreditCard, amount: Int) {

    def combine(other: Charge): Charge = {
        if (other.cc == this.cc) Charge(cc, other.amount + amount)
        else throw new Exception("Invalid credit card combo")
    }
    
}
// Lesson Push these side effects to the outer layers of the program
class Cafe {

    def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
        val cup = new Coffee
        (cup, Charge(cc, cup.price))
    }
    
    def buyCoffees(cc: CreditCard, n: Int): (List[Coffee], Charge) = {
        val purchases: List[(Coffee, Charge)] = List.fill(n)(buyCoffee(cc))
        val (coffees, charges) = purchases.unzip
        (coffees, charges.reduce( (c1, c2) => c1.combine(c2) ))
    } 
    
}

object Test {
    
    def run = {
        val myCard = new CreditCard(1)
        val cafe = new Cafe()
        val tx = List.fill(10)(cafe.buyCoffee(myCard))
        val total = tx.foldRight( Charge(myCard, 0) )( (tup, acc) => acc.combine(tup._2) )
        println(total)
    }
    
    def testBuyCoffees = {
        val myCard = new CreditCard(1)
        val cafe = new Cafe()
        val coffeesCharge = cafe.buyCoffees(myCard, 10)
        println(coffeesCharge)
    }
    
}