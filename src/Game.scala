import scala.util.Random
import scala.io.Source
import java.io.File
import java.io._
import java.util.Calendar
import java.time.LocalDate

//object Game extends App{
object Game{
  def main(args: Array[String]): Unit = {
    showResult(executeGame(fillMatrix(9,generateMatrix(9,9)),1,1))
  }
  //Ejecuta el menú del juego
  def executeMenu(counter: Int){
    println("Seleccione una de las siguientes opciones:")
    println("1 - Jugar.")
    println("2 - Guardar puntuación.")
    println("0 - Salir.")
    val value = scala.io.StdIn.readInt()
    value match { //Se compara la opción seleccionada
      case 0 => println("Gracias por jugar.")
      case 1 => showResult(executeGame(fillMatrix(9,generateMatrix(9,9)),1,1))
      case 2 => savePoints(counter)
      case _ => errorOptionSelected(counter: Int)
    }
  }
  //Se realiza el guardado de los puntos de un jugador en un archivo  
  def savePoints(counter: Int){
    println("Introduzca el nombre del jugador:")
    val name = scala.io.StdIn.readLine()
    val result = "Nombre: " + name + "; Puntos: " + counter
    val today = java.time.LocalDate.now() //Obtiene la fecha actual para el archivo
    val time = Calendar.getInstance() //Obtiene la hora actual para el archivo
    val file = new File("points"+ today + "_" + time.get(Calendar.HOUR_OF_DAY)+"-"+time.get(Calendar.MINUTE)+"-"+time.get(Calendar.SECOND)+".txt")
    val writer = new BufferedWriter(new FileWriter(file,true))
    writer.write(result)
    writer.close()
    println("Guardado correctamente.")
    executeMenu(counter)
  }
  //Muestra un error en relación a la opción seleccionada del menú
  def errorOptionSelected(counter: Int){
    println("Error. No existe la opción seleccionada. Por favor, seleccione otra opción.")
    executeMenu(counter)
  }
  //Muestra el resultado de la partida
  def showResult(counter: Int){
    println("¡Enhorabuena! ¡Ha obtenido " + counter + "!")
    executeMenu(counter)
  }
  //Devuelve los puntos finales de la partida y realiza la ejecución del juego
  def executeGame(matrix:List[List[String]], counter: Int,step: Int): Int ={
    if(getListFreePositions(matrix,0).!=(0))
    {
      println("Tablero paso " + step + ":")
      showMatrix(matrix,0)
      showOptimization(matrix)
      val listPos = getListPos(matrix)
      val posInitial = listPos.head
      val posFinal = (listPos.tail).head
      val matrixChangePos = paintMatrix(posFinal.head, (posFinal.tail).head, paintMatrix(posInitial.head, (posInitial.tail).head, matrix, "-"), getValueListOfLists(posInitial.head,(posInitial.tail).head,matrix))
      val matrixRemoveLines = paintListPosMatrix(getChanges(posFinal, matrixChangePos),"-",matrixChangePos)
      executeGame(getMatrixNextStep(matrix, matrixRemoveLines),addPoints(matrix, matrixRemoveLines, counter),step+1)
    }
    else counter
  }
  //Devuelve la lista de colores que se van a usar en el juego
  def getListColors():List[String]={
    "A" :: "N" :: "R" :: "V" :: "M" :: "G" :: Nil
  }
  //Obtiene la posición inicial y la posición final para una bola
  def getListPos(matrix:List[List[String]]): List[List[Int]]=
  {
    val posInicial = pedirPosInicial(matrix)
    val posFinal = pedirPosFinal(matrix)
    if(isPath(posInicial,posFinal,matrix,0)) List(posInicial,posFinal)
    else
    {
      println("Movimiento Inválido. No hay camino entre las posiciones indicadas. Introduzca otras posiciones.")
      getListPos(matrix)
    }
  }
  //Genera una matriz de posiciones vacías
  def generateMatrix(numRows: Int, numCols: Int): List[List[String]] ={
    if(numRows.!=(0)) generateMatrix(numRows-1,numCols):::List(generateRow(numCols))
    else Nil
  }
  //Genera una fila por posiciones vacías
  def generateRow(numElem: Int): List[String] ={
    if(numElem.!=(0)) generateRow(numElem-1):::List("-")
    else Nil
  }
  
  //Muestra una matriz por pantalla
  def showMatrix(matrix:List[List[String]], pos: Int):List[List[String]]={
    val coordX = pos / 9
    val coordY = pos % 9
    if(pos<81)
    {
      if(coordY.!=(0))
      {
        print(" " + getValueListOfLists(coordX,coordY,matrix))
        showMatrix(matrix,pos+1)
      }
      else
      {
        println()
        print(" " + getValueListOfLists(coordX,coordY,matrix))
        showMatrix(matrix,pos+1)
      }
    }
    else
    {
      println("")
      matrix
    }
  }
  //Devuelve el valor de una posición de una lista de listas de strings
  def getValueListOfLists(posX: Int, posY: Int, matrix: List[List[String]]): String = {
    if(posX.!=(0)) getValueListOfLists(posX-1, posY, matrix.tail)
    else getValueList(posY, matrix.head)
  }
  //Devuelve el valor de una posición de una lista de strings
  def getValueList(pos: Int, lista: List[String]): String = {
    if(pos.!=(0)) getValueList(pos-1, lista.tail)
    else lista.head
  }
  //Devuelve la fila con la que se está trabajando
  def getRow(posX: Int, matrix: List[List[String]]): List[String] = {
    matrix(posX)
  }
  //Obtención de un número random en un intervalo
  def random(min:Int, max:Int):Int = { 
    val random = scala.util.Random
    val randomAux = random.nextInt(max - min)
    val resultado = randomAux + min
    resultado
  }
  //Verifica si una casilla se encuentra o no vacía
  def isEmptyPosListOfLists(x: Int, y: Int, matrix: List[List[String]]):Boolean = {
    if(getValueListOfLists(x,y,matrix).!=("-")) false
    else true
  }
  //Pide las coordenadas de la posición inicial
  def pedirPosInicial(matrix: List[List[String]]):List[Int] = {
    println("Posición Inicial:")
    println("Introduzca posiciones cuyos valores en cada coordenada esté entorno al 0 y el 8.")
    println("Introduce su coordenada X:")
    val posX = scala.io.StdIn.readInt()
    println("Introduce su coordenada Y:")
    val posY = scala.io.StdIn.readInt()
    if(posX < 0 || posX > 8 || posY < 0 || posY > 8 || isEmptyPosListOfLists(posX, posY, matrix))
    {
      println("Movimiento Inválido. Introduzca una posición válida.")
      pedirPosInicial(matrix: List[List[String]])
    }
    else
    {
      List(posX, posY)
    }
  }
  //Pide las coordenadas de la posición final
  def pedirPosFinal(matrix: List[List[String]]):List[Int] = {
    println("Posición Final:")
    println("Introduzca posiciones cuyos valores en cada coordenada esté entorno al 0 y el 8.")
    println("Introduce su coordenada X:")
    val posX = scala.io.StdIn.readInt()
    println("Introduce su coordenada Y:")
    val posY = scala.io.StdIn.readInt()
    if(posX < 0 || posX > 8 || posY < 0 || posY > 8 || !isEmptyPosListOfLists(posX, posY, matrix))
    {
      println("Movimiento Inválido. Introduzca una posición válida.")
      pedirPosFinal(matrix: List[List[String]])
    }
    else
    {
      List(posX, posY)
    }
  }
  //Verifica la existencia de un camino entre dos posiciones
  def isPath(posInitial: List[Int],posFinal: List[Int], matrix: List[List[String]],counter: Int): Boolean ={
    if(counter <150 &&((!(posInitial.head.!=(posFinal.head) && !(!(posInitial.tail).head.!=((posFinal.tail).head))))||(isFreePos(posInitial.head, (posInitial.tail).head, matrix) && isFreePos(posFinal.head, (posFinal.tail).head, matrix))))
    {
      if(!(posInitial.head.!=(posFinal.head) && !(!(posInitial.tail).head.!=((posFinal.tail).head))))
      {
        true
      }
      else
      {
        if(isPath(List(posInitial.head,(posInitial.tail).head-1), posFinal,matrix,counter+1)) true
        else
        {
          if(isPath(List(posInitial.head,(posInitial.tail).head+1), posFinal,matrix,counter+1)) true
          else
          {
            if(isPath(List(posInitial.head-1,(posInitial.tail).head), posFinal,matrix,counter+1)) true
            else
            {
              isPath(List(posInitial.head+1,(posInitial.tail).head), posFinal,matrix,counter+1)
            }
          }
        }
      }
    }
    else false
  }
  //Comprueba si una posición se encuentra libre por algún camino (derecha, izquierda, arriba o abajo)
  def isFreePos(coordX: Int, coordY: Int, matrix: List[List[String]]): Boolean={
    if((!isOutOfRangePos(coordX,coordY-1) && isEmptyPosUpDown(matrix, coordX,coordY-1))||(!isOutOfRangePos(coordX,coordY+1) && isEmptyPosUpDown(matrix, coordX,coordY+1))||(!isOutOfRangePos(coordX-1,coordY) && isEmptyPosRightLeft(matrix, coordX-1,coordY))||(!isOutOfRangePos(coordX+1,coordY) && isEmptyPosRightLeft(matrix, coordX+1,coordY))) true
    else false
  }
  //Comprueba si una posición se encuentra fuera de rango
  def isOutOfRangePos(coordX: Int,coordY: Int):Boolean={
    if(coordX < 0 || coordX >= 9 || coordY < 0 || coordY >= 9) true
    else false
  }
  //Comprueba si la posición se encuentra vacía o fuera de rango por encima o por debajo de otra
  def isEmptyPosUpDown(matrix:List[List[String]],coordX:Int,coordY:Int):Boolean={
    if(coordY<9 && coordY>=0){
      if(isEmptyPosListOfLists(coordX,coordY,matrix)) true
      else false
    } 
    else true
  }
  //Comprueba si la posición se encuentra vacía o fuera de rango sabiendo que está a la derecha de otra
  def isEmptyPosRightLeft(matrix:List[List[String]],coordX:Int,coordY:Int):Boolean={
    if(coordX<9 && coordX>=0){
      if(isEmptyPosListOfLists(coordX,coordY,matrix)) true
      else false
    }
    else true    
  }
  //Rellena de forma aleatoria el tablero con un número de bolas pasado por parámetro
  def fillMatrix(numBolas: Int, matrix: List[List[String]]): List[List[String]] = {
    if(numBolas != 0) fillMatrix(numBolas - 1, fillRandomPos(matrix, getValueList(random(0, getListColors().length), getListColors())))
    else matrix
  }
  //Rellena una posición del tablero de forma aleatoria
  def fillRandomPos(matrix: List[List[String]], color : String): List[List[String]] = {
    val listPosEmpty = getListFreePositions(matrix, 0)
    val position = listPosEmpty(random(0, listPosEmpty.size))
    val posX = position.head
    val posY = (position.tail).head
    if(isEmptyPosListOfLists(posX, posY, matrix)) paintMatrix(posX, posY, matrix, color)
    else fillRandomPos(matrix, color)
  }
  //Pinta una lista de posiciones de un color
  def paintListPosMatrix(listPos: List[List[Int]], color: String, matrix: List[List[String]]): List[List[String]]={
    if(listPos.!=(List(Nil))&& listPos.!=(Nil))
    {
      paintListPosMatrix(listPos.tail, color, paintMatrix((listPos.head).head, ((listPos.head).tail).head, matrix, color))
    }
    else matrix
  }
  //Pinta una posición de acuerdo a todo el tablero
  def paintMatrix(posX: Int, posY: Int, matrix: List[List[String]], color : String): List[List[String]] = {
    if(posX.!=(0)) matrix.head :: paintMatrix(posX - 1, posY, matrix.tail, color)
    else paintRow(posY, matrix.head, color) :: matrix.tail
  }
  //Pinta una posición de acuerdo a una fila del tablero
  def paintRow(posY: Int, lista: List[String], color : String): List[String] = {
    if(posY.!=(0)) lista.head :: paintRow(posY - 1, lista.tail, color)
    else color :: lista.tail
  }
  //Rellena una posición del tablero
  def fillNormalPosition(posX: Int, posY: Int, matrix: List[List[String]], color : String): List[List[String]] = 
  {
  	paintMatrix(posX,posY,matrix,color)
  }
  //Permite obtener el número de cambios en el tablero
  def getMatrixChanges(matrix1: List[List[String]], matrix2: List[List[String]], position: Int, counter: Int): Int = {
    getListFreePositions(matrix2,0).length - getListFreePositions(matrix1,0).length
  }
  //Permite ajustar el valor del contador si es necesario
  def setValueCounter(value1: String, value2: String, counter: Int): Int = {
    if(!isEqual(value1, value2)) counter + 1
    else counter
  }
  //Compara dos elementos son iguales
  def isEqual(value1: String, value2: String): Boolean = {
    if(value1.!=(value2)) false
    else true
  }
  //Devuelve el matrix que se va a utilizar para el siguiente paso del juego
  def getMatrixNextStep(initialMatrix: List[List[String]], finalMatrix: List[List[String]]): List[List[String]] = {
    if(getMatrixChanges(initialMatrix, finalMatrix, 0, 0) >= 5) finalMatrix
    else fillMatrix(3, finalMatrix)
  }
  //Realiza la suma de puntos del nuevo paso
  def addPoints(initialMatrix:List[List[String]], finalMatrix:List[List[String]], puntos: Int):Int = {
    val bolasEliminadas=getMatrixChanges(initialMatrix,finalMatrix,0,0)
    if(bolasEliminadas >= 5) puntos + bolasEliminadas*75
  	else puntos
  }
  //Muestra predicción de posibles posiciones a poder seleccionar
  def showOptimization(matrix: List[List[String]]){
    val listPos = getListBestPosColorWithPath(matrix, getListBestPosColor(matrix, 0))
    val maxValue = getMaxNumBalls(listPos, 0)
    if(listPos.length.!=(0)) showListBestPosListOfLists(listPos, listPos.length, (maxValue*0.5).toInt, maxValue)
    else println("Selecciona cualquier casilla de las que quedan libres.")
    
  }
  //Muestra listas de las mejores posiciones en relación al contador de posiciones a mostrar y los márgenes entre los que se trabajan
  def showListBestPosListOfLists(listPos: List[List[Int]], counter: Int, minValue: Int, maxValue: Int){
    if(listPos.length.!=(0) && counter.!=(0) && minValue <= maxValue) showListBestPosListOfLists(listPos, showListBestPos(getPositionInRange(listPos, maxValue, maxValue),counter), minValue, maxValue-1)
    else println()
  }
  //Muestra una lista de las mejores posiciones
  def showListBestPos(listPos: List[List[Int]], counter: Int): Int={
    //Muestra si aún quedan elementos en la lista y el contador es mayor que 0
    if(listPos.length.!=(0) && counter.!=(0))
    {
      println("Posición (" + (listPos.head).head + "," + ((listPos.head).tail).head + ") --> Color recomendado: " + getValueList(((((listPos.head).tail).tail).tail).head,getListColors()))
      showListBestPos(listPos.tail, counter-1)
    }
    else counter
  }
  //Define si una posición con un color se encuentra en una lista de posiciones
  def containPositionColor(listPos: List[List[Int]], position: List[Int], posColor: Int): Boolean = {
    if(listPos.length.!=(0))
    {
      //Posición estudiada de la lista es distinta a la posición estudiada
      if((listPos.head).head.!=(position.head) || ((listPos.head).tail).head.!=((position.tail).head) || ((((listPos.head).tail).tail).tail).head.!=(posColor)) containPositionColor(listPos.tail, position, posColor)
      //Posición estudiada de la lista es igual a la posición estudiada
      else true
    }
    else false
  }
  //Define si una posición se encuentra en una lista de posiciones
  def containPosition(listaposiciones: List[List[Int]], position: List[Int]): Boolean = {
    if(listaposiciones.!=(List(List())) && listaposiciones.!=(List()))
    {
      //Posición estudiada de la lista es distinta a la posición estudiada
      if((listaposiciones.head).head.!=(position.head) || ((listaposiciones.head).tail).head.!=((position.tail).head)) containPosition(listaposiciones.tail, position)
      //Posición estudiada de la lista es igual a la posición estudiada
      else true
    }
    else false
  }
  //Devuelve la lista con las posiciones que tienen un número de bolas en el rango de bolas marcado
  def getPositionInRange(list: List[List[Int]], minValue: Int, maxValue: Int): List[List[Int]]={
    if(list.length.!=(0))
    {
      val numBalls = (((list.head).tail).tail).head
      if(numBalls >= minValue && numBalls <= maxValue) List(list.head) ::: getPositionInRange(list.tail, minValue, maxValue)
      else getPositionInRange(list.tail, minValue, maxValue)
    }
    else Nil
  }
  //Obtiene el mayor número de bolas entre las posiciones del tablero en la lista de listas
  def getMaxNumBalls(list: List[List[Int]], maxValue: Int): Int={
    if(list.length.!=(0))
    {
      if((((list.head).tail).tail).head > maxValue) getMaxNumBalls(list.tail, (((list.head).tail).tail).head)
      else getMaxNumBalls(list.tail, maxValue)
    }
    else maxValue
  }
  //Devuelve una lista con aquellas listas que tienen al menos un camino entre posiciones
  def getListBestPosColorWithPath(matrix: List[List[String]], list: List[List[Int]]): List[List[Int]]={
    if(list.length.!=(0))
    {
      if(isPathOptimized(matrix,list.head,0)) List(list.head) ::: getListBestPosColorWithPath(matrix,list.tail)
      else getListBestPosColorWithPath(matrix,list.tail)
    }
    else Nil
  }
  //Verifica si existe algún camino entre dos posiciones 
  def isPathOptimized(matrix: List[List[String]], pos: List[Int], position: Int): Boolean={
    val coordX = position / 9
    val coordY = position % 9
    val posColor = (((pos.tail).tail).tail).head
    if(position < 81){
      if(!getValueListOfLists(pos.head, (pos.tail).head, matrix).!=("-") && getValueListOfLists(coordX, coordY, matrix).!=("-") && isEqual(getValueListOfLists(coordX, coordY, matrix),getValueList(posColor,getListColors())) && isPath(List(coordX,coordY), List(pos.head, (pos.tail).head), matrix, 0)) true
      //if(!getValueListOfLists(coordX, coordY, matrix).!=("-") || getValueListOfLists(pos.head, (pos.tail).head, matrix).!=("-") || !isEqual(getValueListOfLists(coordX, coordY, matrix),getValueList(posColor,getListColors())) || !isPath(List(coordX,coordY), List(pos.head, (pos.tail).head), matrix, 0)) isPathOptimized(matrix,pos,position+1)
      //else true
      else isPathOptimized(matrix,pos,position+1)
    }
    else false
  }
  //Obtiene todas las posibles posiciones para colocar una bola
  def getListBestPosColor(matrix: List[List[String]], position: Int): List[List[Int]]={
    val coordX = position / 9
    val coordY = position % 9
    if(position < 81){
      val listPosRow = getListRowPosColor(matrix, coordX, coordY, 0)
      val listPosCol = getListColumnPosColor(matrix, coordX, coordY, 0)
      val listPosDiag = getListDiagonalPosColor(matrix, coordX, coordY, 0)
      joinLists(listPosRow,joinLists(listPosCol, listPosDiag)) ::: getListBestPosColor(matrix, position+1)
    }
    else Nil
  }
  //Une dos listas de posiciones teniendo en cuenta si ya se encuentran dichas posiciones o no
  def joinLists(l1: List[List[Int]], l2: List[List[Int]]): List[List[Int]]={
    if(l1.length.!=(0))
    {
      if(containPositionColor(l2,l1.head,((((l1.head).tail).tail).tail).head)) joinLists(l1.tail, addValuePos(getPosEqualColor(l2, l1.head, 0),l2,(((l1.head).tail).tail).head))
      else joinLists(l1.tail,l2 ::: List(l1.head))
    }
    else l2
  }
  //Elimina una posición del tablero a partir de su posición en la lista
  def removePosListOfLists(pos: Int, list: List[List[Int]]): List[List[Int]]={
    if(pos.!=(0)) List(list.head) ::: removePosListOfLists(pos, list.tail)
    else list.tail
  }
  //Devuelve la posición de una posición en una lista de listas la posición que coincida con una posición del tablero y el color colocado en dicha posición
  def getPosEqualColor(l1: List[List[Int]], pos: List[Int], position: Int): Int={
    if(!((l1.head).head.!=(pos.head)) && !(((l1.head).tail).head.!=((pos.tail).head)) && !(((((l1.head).tail).tail).tail).head.!=((((pos.tail).tail).tail).head))) position
    else getPosEqualColor(l1.tail,pos,position+1)
  }
  //Ajusta el valor del número de bolas en una posición de la lista de listas
  def addValuePos(pos: Int, list: List[List[Int]], value: Int): List[List[Int]]={
    if(pos.!=(0)) List(list.head) ::: addValuePos(pos-1, list.tail,value)
    else List(List((list.head).head, ((list.head).tail).head, (((list.head).tail).tail).head + value, ((((list.head).tail).tail).tail).head)) ::: list.tail
  }
  //Devuelve la lista de posiciones junto con el número de bolas y el color que tienen alguna bola a sus lados
  def getListRowPosColor(matrix:List[List[String]], coordX: Int, coordY: Int, posColor: Int): List[List[Int]]={
    if(posColor < getListColors().length)
    {
      //Obtiene la lista de posiciones junto con el número de bolas que tendría y el color que tendría dicha posición 
      val numBallsRow = getBallsRow(List(coordX,coordY), getRow(coordX,paintMatrix(coordX, coordY, matrix, getValueList(posColor,getListColors()))))-1
      //Separación de los casos que no tienen ninguna bola a sus lados del mismo color de los casos que sí
      if(numBallsRow>0) List(List(coordX, coordY, numBallsRow, posColor)) ::: getListRowPosColor(matrix, coordX,coordY, posColor+1)
      else getListRowPosColor(matrix, coordX,coordY, posColor+1)
    }
    else Nil
  }
  //Devuelve la lista de posiciones junto con el número de bolas y el color que tienen alguna bola por encima y por debajo
  def getListColumnPosColor(matrix:List[List[String]], coordX: Int, coordY: Int, posColor: Int): List[List[Int]]={
    if(posColor < getListColors().length)
    {
      //Obtiene la lista de posiciones junto con el número de bolas que tendría y el color que tendría dicha posición 
      val numBallsCol = getBallsColumn(List(coordX,coordY), paintMatrix(coordX, coordY, matrix, getValueList(posColor,getListColors())))-1
      //Separación de los casos que no tienen ninguna bola por encima o por debajo del mismo color de los casos que sí
      if(numBallsCol>0) List(List(coordX, coordY, numBallsCol, posColor))  ::: getListColumnPosColor(matrix, coordX,coordY, posColor+1)
      else getListColumnPosColor(matrix, coordX,coordY, posColor+1)
    }
    else Nil
  }
  //Devuelve la lista de posiciones junto con el número de bolas y el color que tiene al menos una bola a su alrededor
  def getListDiagonalPosColor(matrix:List[List[String]], coordX: Int, coordY: Int, posColor: Int): List[List[Int]]={
    if(posColor < getListColors().length)
    {
      //Obtiene la lista de posiciones junto con el número de bolas que tendría y el color que tendría dicha posición 
      val numBallsAround = getNumberBallsAroundEqualColor(paintMatrix(coordX, coordY, matrix, getValueList(posColor,getListColors())),coordX,coordY,posColor)
      //Separación de los casos que no tienen ninguna bola del mismo color a su alrededor de los casos que sí
      if(numBallsAround>0) List(List(coordX, coordY, numBallsAround,posColor))  ::: getListDiagonalPosColor(matrix, coordX,coordY, posColor+1)
      else getListDiagonalPosColor(matrix, coordX,coordY, posColor+1)
    }
    else Nil
  }
  //Obtiene el número de bolas que tienen el mismo color alrededor de una posición
  def getNumberBallsAroundEqualColor(matrix:List[List[String]], coordX: Int, coordY: Int, posColor: Int): Int ={
    val pos1 = isValid(coordX-1,coordY-1,getValueList(posColor, getListColors()),matrix)
    val pos2 = isValid(coordX-1,coordY,getValueList(posColor, getListColors()),matrix)
    val pos3 = isValid(coordX-1,coordY+1,getValueList(posColor, getListColors()),matrix) 
    val pos4 = isValid(coordX,coordY-1,getValueList(posColor, getListColors()),matrix)
    val pos5 = isValid(coordX,coordY+1,getValueList(posColor, getListColors()),matrix)
    val pos6 = isValid(coordX+1,coordY-1,getValueList(posColor, getListColors()),matrix)
    val pos7 = isValid(coordX+1,coordY,getValueList(posColor, getListColors()),matrix)
    val pos8 = isValid(coordX+1,coordY+1,getValueList(posColor, getListColors()),matrix)
    pos1 + pos2 + pos3 + pos4 + pos5 + pos6 + pos7 + pos8
  }
  //Devuelve un valor en relación a si dos valores son iguales y no las coordenadas de la posición están en los rango de la matriz
  def isValid(coordX: Int, coordY: Int, color: String, matrix: List[List[String]]): Int={
    if(!isOutOfRangePos(coordX, coordY) && isEqual(getValueListOfLists(coordX, coordY, matrix), color)) 1
    else 0
  }
  //Devuelve la lista de posiciones que se deben modificar
  def getChanges(position: List[Int], matrix: List[List[String]]): List[List[Int]] = {
    val listModRowCol = introducePosition(getChangesRow(position, matrix(position.head)) ::: getChangesColumn(position, matrix), position)
    getListPosDiagonals(matrix, 0,listModRowCol)
  }
  //Introduce la posición actual si hay más posiciones en la lista de posiciones 
  def introducePosition(listPos: List[List[Int]], position: List[Int]): List[List[Int]]={
    if(listPos.length.!=(0)) List(position) ::: listPos
    else listPos
  }
  
  //Devuelve la lista de posiciones que han de modificarse de una fila en relación del número de bolas seguidas de un color
  def getChangesRow(position: List[Int], row: List[String]): List[List[Int]] = {
    if(getBallsRow(position, row) > 4)getBallsListRow(position, row)
    else Nil
  }
  //Devuelve el número de bolas seguidas de un mismo color a partir de una posición en una fila
  def getBallsRow(position: List[Int], row: List[String]): Int = {
    (position.tail).head match { //Se compara el número de columna
      case 0 => 1 + getBallsRowLeftRight((position.tail).head + 1, row, getValueList((position.tail).head, row))
      case 8 => 1 + getBallsRowRightLeft((position.tail).head - 1, row, getValueList((position.tail).head, row))
      case _ => getBallsRowLeftRight((position.tail).head + 1, row, getValueList((position.tail).head, row)) + 1 + getBallsRowRightLeft((position.tail).head - 1, row, getValueList((position.tail).head, row))
    }
  }
  //Devuelve el número de bolas seguidas de un mismo color de izquierda a derecha en una fila
  def getBallsRowLeftRight(position: Int, row: List[String], color: String): Int = {
    if(position < 9) //No supera el margen de la derecha
    {
      //Comprobar si tienen el mismo color
      if(getValueList(position, row).!=(color)) 0
      else 1 + getBallsRowLeftRight(position + 1, row, color)
    }
    else 0 //Fuera de rango
  }
  //Devuelve el número de bolas seguidas de un mismo color de derecha a izquierda en una fila
  def getBallsRowRightLeft(position: Int, row: List[String], color: String): Int = {
    if(position >= 0) //No supera el margen de la izquierda
    {
      //Comprobar si tienen el mismo color
      if(getValueList(position, row).!=(color)) 0
      else 1 + getBallsRowRightLeft(position - 1, row, color)
    }
    else 0 //Fuera de rango
  }
  //Devuelve la lista de las posiciones de las bolas seguidas en una fila de un mismo color
  def getBallsListRow(position: List[Int], row: List[String]): List[List[Int]]={
    (position.tail).head match { //Se compara con respecto a la columna de la posición
      case 0 => getBallsListRowLeftRight(List(position.head,(position.tail).head + 1), row, getValueList((position.tail).head, row))
      case 8 => getBallsListRowRightLeft(List(position.head,(position.tail).head - 1), row, getValueList((position.tail).head, row))
      case _ => getBallsListRowLeftRight(List(position.head,(position.tail).head + 1), row, getValueList((position.tail).head, row)) ::: getBallsListRowRightLeft(List(position.head,(position.tail).head - 1), row, getValueList((position.tail).head, row))
    }
  }
  //Devuelve la lista de las posiciones de las bolas seguidas en una fila de un mismo color de izquierda a derecha
  def getBallsListRowLeftRight(position: List[Int], row: List[String], color: String): List[List[Int]]={
    if((position.tail).head < 9)
    {
      if(getValueList((position.tail).head,row).!=(color)) Nil
      else List(position) ::: getBallsListRowLeftRight(List(position.head,(position.tail).head + 1), row, color)
    }
    else Nil
  }
  //Devuelve la lista de las posiciones de las bolas seguidas en una fila de un mismo color de derecha a izquierda
  def getBallsListRowRightLeft(position: List[Int], row: List[String], color: String): List[List[Int]]={
    if((position.tail).head >= 0)
    {
      if(getValueList((position.tail).head,row).!=(color)) Nil
      else List(position) ::: getBallsListRowRightLeft(List(position.head,(position.tail).head - 1), row, color)
    }
    else Nil
  }
  //Devuelve la lista de posiciones que han de modificarse de una columna en relación del número de bolas seguidas de un color
  def getChangesColumn(position: List[Int], matrix: List[List[String]]): List[List[Int]] = {
    if(getBallsColumn(position, matrix) > 4)getBolasListaColumna(position, matrix)
    else Nil
  }
  //Devuelve el número de bolas seguidas de un mismo color a partir de una posición en una columna
  def getBallsColumn(position: List[Int], matrix: List[List[String]]): Int = {
    position.head match { //Se compara el número de Row
      case 0 => 1 + getBallsColUpDown(List(position.head + 1,(position.tail).head), matrix, getValueListOfLists(position.head, (position.tail).head, matrix))
      case 8 => 1 + getBallsColDownUp(List(position.head - 1,(position.tail).head), matrix, getValueListOfLists(position.head, (position.tail).head, matrix))
      case _ => getBallsColUpDown(List(position.head + 1,(position.tail).head), matrix, getValueListOfLists(position.head, (position.tail).head, matrix)) + 1 + getBallsColDownUp(List(position.head - 1,(position.tail).head), matrix, getValueListOfLists(position.head, (position.tail).head, matrix))
    }
  }
  //Devuelve el número de bolas seguidas de un mismo color de arriba a abajo en una columna
  def getBallsColUpDown(position: List[Int], matrix: List[List[String]], color: String): Int = {
    if(position.head < 9) //No supera el margen inferior
    {
      //Comprobar si tienen el mismo color
      if(getValueListOfLists(position.head, (position.tail).head, matrix).!=(color)) 0
      else 1 + getBallsColUpDown(List(position.head + 1,(position.tail).head), matrix, color)
    }
    else 0 //Fuera de rango
  }
  //Devuelve el número de bolas seguidas de un mismo color de abajo a arriba en una columna
  def getBallsColDownUp(position: List[Int], matrix: List[List[String]], color: String): Int = {
    if(position.head >= 0) //No supera el margen superior
    {
      //Comprobar si tienen el mismo color
      if(getValueListOfLists(position.head, (position.tail).head, matrix).!=(color)) 0
      else 1 + getBallsColDownUp(List(position.head - 1,(position.tail).head), matrix, color)
    }
    else 0 //Fuera de rango
  }
  //Devuelve la lista de las posiciones de las bolas seguidas en una columna de un mismo color
  def getBolasListaColumna(position: List[Int], matrix: List[List[String]]): List[List[Int]]={
    (position.tail).head match { //Se compara con respecto a la columna de la posición
      case 0 => getBallsListColUpDown(List(position.head + 1,(position.tail).head), matrix, getValueListOfLists(position.head, (position.tail).head, matrix))
      case 8 => getBallsListColDownUp(List(position.head - 1,(position.tail).head), matrix, getValueListOfLists(position.head, (position.tail).head, matrix))
      case _ => getBallsListColUpDown(List(position.head + 1,(position.tail).head), matrix, getValueListOfLists(position.head, (position.tail).head, matrix)) ::: getBallsListColDownUp(List(position.head - 1,(position.tail).head), matrix, getValueListOfLists(position.head, (position.tail).head, matrix))
    }
  }
  //Devuelve la lista de las posiciones de las bolas seguidas en una columna de un mismo color de arriba a abajo
  def getBallsListColUpDown(position: List[Int], matrix: List[List[String]], color: String): List[List[Int]]={
    if((position.tail).head < 9 && position.head < 9)
    {
      if(getValueListOfLists(position.head, (position.tail).head, matrix).!=(color)) Nil
      else List(position) ::: getBallsListColUpDown(List(position.head + 1,(position.tail).head), matrix, color)
    }
    else Nil
  }
  //Devuelve la lista de las posiciones de las bolas seguidas en una columna de un mismo color de abajo a arriba
  def getBallsListColDownUp(position: List[Int], matrix: List[List[String]], color: String): List[List[Int]]={
    if((position.tail).head >= 0 && position.head >= 0)
    {
      if(getValueListOfLists(position.head, (position.tail).head, matrix).!=(color)) Nil
      else List(position) ::: getBallsListColDownUp(List(position.head - 1,(position.tail).head), matrix, color)
    }
    else Nil
  }
  //Devuelve la lista de las posiciones de las bolas seguidas en cada una de las diagonales de todo el tablero
  def getListPosDiagonals(matrix:List[List[String]], position:Int, listPositions : List[List[Int]]):List[List[Int]]={
    val coordX = position / 9
    val coordY = position % 9
    if(position < 81){
      if(isEmptyPosListOfLists(coordX, coordY, matrix)) getListPosDiagonals(matrix,position+1, listPositions)
      else
      {
        val diagonalRight1 = introducePosition(getListPosDiagonalRight(matrix,coordX+1,coordY, getValueListOfLists(coordX,coordY,matrix),false),List(coordX,coordY))
        /*Estructura diagonal:
         * 1 1 0
         * 0 1 1
         * 0 0 1
         * */
        val diagonalRight2 = introducePosition(getListPosDiagonalRight(matrix,coordX,coordY+1, getValueListOfLists(coordX,coordY,matrix),true),List(coordX,coordY))
        /*Estructura diagonal:
         * 1 0 0
         * 1 1 0
         * 0 1 1
         * */
        val diagonalLeft1 = introducePosition(getListPosDiagonalLeft(matrix,coordX-1,coordY, getValueListOfLists(coordX,coordY,matrix),false),List(coordX,coordY))
        /*Estructura diagonal:
         * 0 1 1
         * 1 1 0
         * 1 0 0
         * */
        val diagonalLeft2 = introducePosition(getListPosDiagonalLeft(matrix,coordX,coordY+1, getValueListOfLists(coordX,coordY,matrix),true),List(coordX,coordY))
        /*Estructura diagonal:
         * 0 0 1
         * 0 1 1
         * 1 1 0
         * */
        val listPos = introduceDiagonal(diagonalLeft2,introduceDiagonal(diagonalLeft1,introduceDiagonal(diagonalRight2,introduceDiagonal(diagonalRight1, listPositions))))
        getListPosDiagonals(matrix, position+1, listPos)
      }  
    }
    else listPositions
  }
  //Devuelve la lista de posiciones que se encuentran en una de las diagonales que van a derechas
  def getListPosDiagonalRight(matrix:List[List[String]],x: Int, y: Int, color: String, situation: Boolean): List[List[Int]]={
    if(situation) //Posición estudiada es la que se encuentra hacia abajo
    {
      //Considera que la posición no está vacía y es del mismo color que el pasado por parámetro
      if(!isEmptyPosUpDown(matrix, x, y) && isEqual(getValueListOfLists(x,y,matrix), color))
      {
        getListPosDiagonalRight(matrix,x+1,y,color,changeSituation(situation)):::List(List(x,y))
      }
      else Nil
    }
    else //Posición estudiada es la que se encuentra hacia la derecha
    {
      //Considera que la posición no está vacía y es del mismo color que el pasado por parámetro
      if(!isEmptyPosRightLeft(matrix, x, y) && isEqual(getValueListOfLists(x,y,matrix), color))
      {
        getListPosDiagonalRight(matrix,x,y+1,color,changeSituation(situation)):::List(List(x,y))
      }
      else Nil
    }
  }
  //Devuelve la lista de posiciones que se encuentran en una de las diagonales que van a izquierdas
  def getListPosDiagonalLeft(matrix:List[List[String]],x: Int, y: Int, color: String, situation: Boolean): List[List[Int]]={
    if(situation) //Posición estudiada es la que se encuentra hacia abajo
    {
      //Considera que la posición no está vacía y es del mismo color que el pasado por parámetro
      if(!isEmptyPosUpDown(matrix, x, y) && isEqual(getValueListOfLists(x,y,matrix), color))
      {
        getListPosDiagonalLeft(matrix,x-1,y,color,changeSituation(situation)):::List(List(x,y))
      }
      else Nil
    }
    else //Posición estudiada es la que se encuentra hacia la izquierda
    {
      //Considera que la posición no está vacía y es del mismo color que el pasado por parámetro
      if(!isEmptyPosRightLeft(matrix, x, y) && isEqual(getValueListOfLists(x,y,matrix), color))
      {
        getListPosDiagonalLeft(matrix,x,y+1,color,changeSituation(situation)):::List(List(x,y))
      }
      else Nil
    }
  }
  //Invierte el estado de la situación
  def changeSituation(situation: Boolean): Boolean={
    if(situation) false
    else true
  }
  //Une dos listas posiciones sin valores repetidos
  def introduceDiagonal(l1: List[List[Int]], l2:List[List[Int]]): List[List[Int]]={
    //Une dos listas posiciones si el número de posiciones es mayor a 4
    if(l1.length > 4) introduceDiagonalAux(l1,l2)
    else l2
  }
  //Une dos listas posiciones sin valores repetidos
  def introduceDiagonalAux(l1: List[List[Int]], l2:List[List[Int]]): List[List[Int]]={
    //Comprueba si la primera lista está vacía
    if(l1.!=(List(Nil)) && l1.!=(Nil))
    {
      //Comprueba si un elemento se encuentra en una lista y en caso contrario, lo introduce
      if(!containPosition(l2,l1.head)) introduceDiagonalAux(l1.tail, l2 ::: List(l1.head))
      else introduceDiagonalAux(l1.tail, l2)
    }
    else l2
  }
  //Devuelve la lista de las posiciones vacías de la matriz
  def getListFreePositions(matrix: List[List[String]], position: Int): List[List[Int]] = {
    val posX = position/9
    val posY = position%9
    if(position >= 81) Nil
    else
    {
      if(isEmptyPosListOfLists(posX, posY, matrix)) List(List(posX, posY)) ::: getListFreePositions(matrix, position + 1)
      else getListFreePositions(matrix, position + 1)
    }
  }
}