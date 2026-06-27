package object Comete {

  type DistributionValues =Vector[Double]
  // Tipo para los valores , reales ordenados entre 0 y 1,
  // incluidos 0 y 1, de una distribucion

  type Frequency=Vector[Double]
  // Pi k es una frecuencia de longitud k
  // si Pi k.lenght=k, 0<=Pi k(i)<=1, 0<=i<=k−1
  // y Pi k.sum==1

  type Distribution= (Frequency,DistributionValues)
  // (Pi, dv) es una distribucion si pi es una Frecuencia
  // y dv son los valores de distribucion y pi y dv son
  // de la misma longitud

  type PolMeasure=Distribution=>Double

  def minp(f:Double=>Double, min:Double, max:Double,
           prec:Double):Double={
    // Devuelve el punto p en [min,max] tal que f(p) es minimo
    // Suponiendo que f es concava
    // si max−min<prec, devuelve el punto medio de [min,max]
  ???
  }

  def rhoCMTGen(alpha:Double, beta:Double):PolMeasure={
    // Dados alpha y beta devuelve la funcion que calcula la medida
    // comete parametrizada en alpha y beta
    (distribution: Distribution) => {
      val (pi, y) = distribution
      
      val rhoAux = (p: Double) => {
        pi.zip(y).map { case (pi_i, y_i) =>
          math.pow(pi_i, alpha) * math.pow(math.abs(y_i - p), beta)
        }.sum
      }
      val pMin = min_p(rhoAux, 0.0, 1.0, 0.001)

      rhoAux(pMin)
      
    }
  }

  def normalizar(m:PolMeasure):PolMeasure= {
    // Recibe una medida de polarizacion, y devuelve la
    // correspondiente medida que la calcula normalizada
    // 1. Se define la distribucion del peor caso (dMax es del tipo Distribution)
    val dMax = (Vector(0.5, 0.5), Vector(0.0, 1.0))

    // 2. Se calcula la polarizacion maxima con el valor anterior, para la funcion m, que es del tipo Distribution => Double
    val polMax = m(dMax)

    // 3. Retornamos la función normalizada --> sintaxis de funcion anonima
    (d: Distribution) => {
      val polActual = m(d)
      if (polMax == 0.0) 0.0 else polActual / polMax
    }
  }

}
