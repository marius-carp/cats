package pattern

object Pattern extends App {



  def discursul_virusat(text: String): String = {

    case class Good(text: String, length: Int, chars: List[Char]) {
      def isGood(toCompare: String): Boolean = {
        length == toCompare.length && toCompare.toCharArray.toList.sorted == chars.sorted
      }
    }

    val split = text.split("! ")
    val first = split.head.split(" ")

    val punctuation = Array(".", "!", ":", ",", ";")


    val goodies = first.flatMap { txt =>
      punctuation.map { pct =>
        if(txt.endsWith(pct))
          txt.init
        else
          txt + pct
      } :+ txt
    }.map { txt =>
      Good(txt, txt.length, txt.toCharArray.toList)
    }

    val second = split.tail.mkString("! ")

    def findIn(word: String): String = {
      goodies.find(_.isGood(word)).map(_.text).getOrElse(word)
    }

    val result = for {
      word <- second.split(" ")

    } yield findIn(word)


    first.mkString(" ") + "! " + result.mkString(" ")
  }

  print(discursul_virusat("Repetă-ţi mereu: Am încredere în mine însumi; şi această formulă îţi va readuce în minte, imediat, reuşitele din trecut şi îţi va restabili încrederea şi siguranţa! Încrederea nî sine este prima cerinţă a marilor realizări. Coroana vieţii noastre este această încerdree nî noi. Dacă mi-am pierdut încrederea în mnei, voi avea universul împotriva mea. Căutam eeumr putere iş îcndreeera nî afara mea, dar acestea vin dinăuntru. Erau acolo dintotdeauna. Totuşi, în loc să aibă crdrneeeî în ceea ce le spun propriile minţi, oamenii au ca regulă o slăbiciune pentru acrdîneere în alţii, care pretind că au surse supranaturale de cunoaştere. Cu cât mai repede vei avea dreerecnî în tine, cu atât mai repede vei ști cum să trăiești. Dacă ai înecdrere nî tine, inspiri eeedrrcnî altora. Sunt cel mai bun! Am spus acest lucru chiar înainte să ştiu că sunt. Ai îcrnedere și acționează ca și cum ar fi imposibil să eșuezi. Încrederea este ceea ce ai înainte de a înţelege problema. Talentul este încaereder în tine însuţi, în forţa ta. Dacă nu ai încerdere, ai pierdut deja bătălia. Ai încredere nî tine, apoi vei ști cum să iubești. Trebuie să crezi în tine atunci când nimeni altcineva nu crede. Acest lucru te transformă într-un câştigător chiar în acel moment. mA mare drcîneere nî fraieri; prietenii mei numesc asta creîndere în sine."))


}
