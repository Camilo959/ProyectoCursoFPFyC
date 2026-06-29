import Comete._
import common._

import scala.annotation.tailrec
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

  def rho(alpha: Double, beta: Double): AgentsPolMeasure = {
    (b: SpecificBelief, d: DistributionValues) => {
      val k = d.length
      val n = b.length.toDouble

      // Instanciamos las funciones del paquete Comete
      val funcionComete = rhoCMT_Gen(alpha, beta)
      val funcionNormalizada = normalizar(funcionComete)

      // mapear creencias a sus intervalos
      val conteos = b.map { belief =>
          // Si la creencia es exactamente 1.0 (o mayor), va al último intervalo
          if (belief >= 1.0) k - 1
          else {
            // Encontramos el primer índice donde la creencia es menor al límite superior del intervalo
            val idx = (0 until k - 1).indexWhere(i => belief < (d(i) + d(i + 1)) / 2.0)

            // Si no se encontró (por algún error de precisión), lo forzamos al último intervalo
            if (idx == -1) k - 1 else idx
          }
        }
        .groupBy(identity) // Agrupamos por índice del intervalo
        .view.mapValues(lista => lista.size.toDouble / n) // Calculamos la frecuencia
        .toMap

      // 3. Construimos el vector final de frecuencias asegurando tamaño 'k'
      // Si un intervalo quedó sin agentes, le asignamos 0.0
      val frecuencias: Vector[Double] = Vector.tabulate(k)(i => conteos.getOrElse(i, 0.0))

      // 4. Evaluamos pasándole la tupla (Frequency, DistributionValues) requerida por el tipo Distribution
      funcionNormalizada((frecuencias, d))
    }
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
    // 1. Definimos el motor recursivo interno
    @tailrec
    def advanceTime(currentStep: Int, history: Vector[SpecificBelief]): IndexedSeq[SpecificBelief] = {

      // 2. Caso Base (Condicion de parada) (cuando ya termine)
      if (currentStep == t) {
        history
      }
      // 3. Paso Recursivo (Avanzar el reloj)
      else {
        val currentBelief = history.last            // Se extrae el vector de opiniones mas reciendo calculado hasta ese momento
        val nextBelief = fu(currentBelief, swg)      // Se aplica la regla de actualizacion para calcular las opiniones del siguiente turno

        // La funcion se llama a si misma avanzando el pasoActual y añadiendo un nuevo vector de opiniones a la lista
        advanceTime(currentStep + 1, history :+ nextBelief)
      }
    }

    // 4. Se arranca la simulacion en el turno 0, pasandole el vector de opiniones inicial (b0)
    advanceTime(0, Vector(b0))
  }
  // Versiones paralelas

  // Recuerda asegurarte de tener importado el manejador de colecciones paralelas
  // si tu versión de Scala lo requiere.

  def rhoPar(alpha: Double, beta: Double): AgentsPolMeasure = {
    (b: SpecificBelief, d: DistributionValues) => {
      val k = d.length
      val n = b.length.toDouble

      // 1. Funciones del paquete Comete (Se invocan de forma secuencial según la regla)
      val funcionComete = rhoCMT_Gen(alpha, beta)
      val funcionNormalizada = normalizar(funcionComete)

      // Al usar b.par, Scala divide el vector en varios fragmentos y los procesa al mismo tiempo
      val conteos = b.par.map { belief =>
          if (belief >= 1.0) k - 1
          else {
            val idx = (0 until k - 1).indexWhere(i => belief < (d(i) + d(i + 1)) / 2.0)
            if (idx == -1) k - 1 else idx
          }
        }
        .groupBy(identity)
        .seq // Volvemos a colección secuencial para poder usar .view.mapValues de forma segura
        .view.mapValues(lista => lista.size.toDouble / n)
        .toMap

      // 3. Ensamblaje del vector de frecuencias
      val frecuencias: Vector[Double] = Vector.tabulate(k)(i => conteos.getOrElse(i, 0.0))

      // 4. Retorno final evaluando la tupla (Distribution)
      funcionNormalizada((frecuencias, d))
    }
  }

  def confBiasUpdatePar(sb:SpecificBelief ,
                        swg:SpecificWeightedGraph):SpecificBelief ={
  ???
  }

}
