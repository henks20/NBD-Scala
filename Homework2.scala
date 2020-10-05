object Homework2 {
  def main(args: Array[String]) = {

    // Zadanie 1
    def checkDayType(day: String) = day match {
      case "Poniedzialek" | "Wtorek" | "Sroda" | "Czwartek" | "Piatek" => "Praca"
      case "Sobota" | "Niedziela" => "Weekend"
      case default => "Nie ma takiego dnia"
    }

    println("### 1.")
    println(checkDayType("Wtorek"))
    println(checkDayType("Niedziela"))
    println(checkDayType("Pomidor"))

    // Zadanie 2
    class KontoBankowe() {
      private var _stanKonta = 0;

      def this(stanKonta: Int) {
        this();
        this._stanKonta = stanKonta;
      }

      def wplata(num: Int) = {
        _stanKonta += num
      }

      def wyplata(num: Int) = {
        _stanKonta -= num
      }

      def printStanKonta(): Unit = {
        println(_stanKonta)
      }

    }

    var konto = new KontoBankowe()
    var konto2 = new KontoBankowe(200)
    println("### 2.")
    println("Konto 1: 0 + 50 = 50")
    konto.printStanKonta()
    konto.wplata(50)
    konto.printStanKonta()
    println("Konto 2: 200 - 11 = 189")
    konto2.printStanKonta()
    konto2.wyplata(11)
    konto2.printStanKonta()

    // Zadanie 3
    case class Osoba(imie: String, nazwisko: String) {}

    def sayHello(czlowiek: Osoba): String = {
      czlowiek match {
        case Osoba("John", _) => "Czesc John Snow"
        case Osoba("Jan", _) => "Chyba masz na nazwisko Nowak"
        case Osoba("Pawel", _) => "Czyzbys mial na imie Pawel?"
        case Osoba(_, _) => "Czesc nieznajomy"
      }
    }

    println("### 3.")
    println(sayHello(new Osoba("John", "Snow")))
    println(sayHello(new Osoba("Jan", "Nowak")))
    println(sayHello(new Osoba("Pawel", "Gawel")))
    println(sayHello(new Osoba("XYZ", "XYZ")))

    // Zadanie 4
    def processNumbers(num: Int, cb:(Int) => Unit ) = {
      for(i <- 1 to 3){
        cb(num)
      }
    }

    def provideNumber = (num: Int) => {
      println(num)
    }

    println("### 4.")
    processNumbers(7, provideNumber)


    //     Zadanie 5
    class OsobaZadanie5 {
      private val _imie: String = ""
      private val _nazwisko: String = ""
      private var _podatek: Double = 1.0

      def imie = _imie
      def nazwisko = _nazwisko
      def podatek = _podatek
    }

    trait Student extends OsobaZadanie5 {
      override def podatek: Double = 0.0
    }

    trait Pracownik extends OsobaZadanie5 {
      private var _pensja : Double = 0
      def pensja = _pensja

      override def podatek: Double = 0.2 * pensja
      def pensja_= (kwota : Double) = _pensja = (kwota - podatek)
    }

    trait Nauczyciel extends Pracownik {
      override def podatek: Double = 0.1 * pensja
    }

    var student = new Student{}
    var nauczyciel = new Nauczyciel{}
    var pracownik = new Pracownik{}

    pracownik.pensja = 100
    nauczyciel.pensja = 100

    println("### 5.")
    println("Podatek studenta: " + student.podatek)
    println("Pensja pracownika: " + pracownik.pensja)
    println("Pensja nauczyciela: " + nauczyciel.pensja)
    println("Podatek pracownika: " + pracownik.podatek)
    println("Podatek nauczyciela: " + nauczyciel.podatek)

  }

}
