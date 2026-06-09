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

  def @annotation.tailrec minp(f:Double=>Double, min:Double, max:Double,
           prec:Double):Double={
    // Devuelve el punto p en [min,max] tal que f(p) es minimo
    // Suponiendo que f es concava
    // si max−min<prec, devuelve el punto medio de [min,max]
    if (max - min < prec) (min + max)/2.0
    else {
      val m1 = min + (max - min) / 3.0
      val m2 = max - (max - min) / 3.0
      if (f(m1) < f(m2)) minp(f, min, m2, prec)
      else minp (f, m1, max, prec)
    }
  }

  def rhoCMTGen(alpha:Double, beta:Double):PolMeasure={
    // Dados alpha y beta devuelve la funcion que calcula la medida
    // comete parametrizada en alpha y beta
    ???
  }

  def normalizar(m:PolMeasure):PolMeasure= {
    // Recibe una medida de polarizacion, y devuelve la
    // correspondiente medida que la calcula normalizada
    ???
  }

}
