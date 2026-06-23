import Comete._
import common._
import scala.collection.parallel.CollectionConverters._


package object Opinion {

  // Si n es el numero de agentes, estos se identifican
  // con los enteros entre 0 y n−1
  //O sea el conjunto de Agentes A es
  // implicitamente el conjunto {0,1,2, ??? , n−1}
  // Si b:BeliefConf , para cada i en Int , b[i] es un numero
  // entre 0 y 1 que indica cuanto cree el agente i en
  // la veracidad de la proposicion p
  // Si existe i: b(i)<0 o b(i)>1 b esta mal definida

  type SpecificBelief =Vector[Double]
  // Si b:SpecificBelief , para cada i en Int , b[i] es un
  // numero entre 0 y 1 que indica cuanto cree el
  // agente i en la veracidad de la proposicion p
  // El numero de agentes es b.length
  // Si existe i: b(i)<0 o b(i)>1 b esta mal definida.
  // Para i en Int\A, b(i) no tiene sentido

  type GenericBeliefConf = Int=>SpecificBelief
  // si gb:GenericBelief , entonces gb(n) =b tal que
  // b:SpecificBelief

  type AgentsPolMeasure=
    (SpecificBelief ,DistributionValues)=>Double
  // Si rho:AgentsPolMeasure y sb:SpecificBelief
  // y d:DistributionValues ,
  // rho(sb,d) es la polarizacion de los agentes
  // de acuerdo a esa medida

  def rho(alpha:Double, beta:Double):AgentsPolMeasure={
    // rho es la medida de polarizacion de agentes basada
    // en comete
  ???
  }

  // Tipos para Modelar la evolucion de la opinion en una red
  type WeightedGraph= (Int,Int)=> Double

  type SpecificWeightedGraph= (WeightedGraph, Int)

  type GenericWeightedGraph=
    Int=>SpecificWeightedGraph

  type FunctionUpdate=
    (SpecificBelief ,SpecificWeightedGraph)=>SpecificBelief

  def confBiasUpdate(sb:SpecificBelief ,
                     swg:SpecificWeightedGraph):SpecificBelief ={
  val (iGraph, n) = swg

    Vector.tabulate(n) { i =>
      val bi = sb(i)

      val neighbors = (0 until n).filter(j => iGraph(j, i) > 0)

      val totalPush = neighbors.map { j =>
        val bj = sb(j)
        val beta_ij = 1.0 - math.abs(bj - bi)
        val influence = iGraph(j, i)

        beta_ij * influence * (bj - bi)
      }.sum

      bi + (totalPush / neighbors.length)
    }
  }

  def showWeightedGraph(swg: SpecificWeightedGraph):
                        IndexedSeq[IndexedSeq[Double]] = {
      val (wg, n) = swg
      Vector.tabulate(n)(i => Vector.tabulate(n)(j => wg(i, j)))
}

  def simulate(fu:FunctionUpdate,
               swg:SpecificWeightedGraph,
               b0:SpecificBelief ,
               t:Int) : IndexedSeq[SpecificBelief] ={
  ???
  }
  // Versiones paralelas

  def rhoPar(alpha:Double,
             beta:Double):AgentsPolMeasure={
    // rho es la medida de polarizacion de agentes basada
    // en comete
  ???
  }

  def confBiasUpdatePar(sb:SpecificBelief ,
                        swg:SpecificWeightedGraph):SpecificBelief ={
  ???
  }

}
