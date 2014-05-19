import scala.collection.immutable

/**
 * Created by pranav on 19/5/14.
 */
class bartercode {

  val numbergivenbarterrates = 4
  val numberofrequiredbarterrates = 2
  val givenbarterList = List("! 2 fork = 2 spoon","! 8 television = 166 knife","! 182 knife = 4 dvdplayer","! 10 microwave = 370 fork","! 14 toaster = 4 microwave","! 4 spoon = 2 knife")
  val requiredbarterlist = List("? dvdplayer = toaster","? television = microwave")

  def gcd(x:Int,y:Int):Int={
    if(y==0)
      return x
    else
      return gcd(y,x%y)
  }

  //Splits the query string into a list of words
  def splitting(barterList:List[String]):List[String]={
    barterList match {
      case Nil => Nil
      case head::tail => return head.split(" +").toList:::splitting(tail)
    }
  }

  //Items it gives the list of items
  def items(fullList:List[String]):List[String]={
    fullList match {
      case Nil => return Nil
      case a::b::c::d::e::f::tail => return List(c):::List(f):::items(tail)
    }
  }

  //itemswithvalue gives a 4 tuple (v1,v2,a,b) where (a,b) is the barter rate of the items with indices v1,v2
  def itemwithvalue(fullList:List[String],itemList:List[String]):List[(Int,Int,Int,Int)]={
    fullList match {
      case Nil => return Nil
      case a::b::c::d::e::f::tail => return List((itemList.indexOf(c),itemList.indexOf(f),b.toInt,e.toInt)):::List((itemList.indexOf(f),itemList.indexOf(c),e.toInt,b.toInt)):::itemwithvalue(tail,itemList)
    }
  }

  //adjacency list of a vertex v is a list of 3 tuples.(v2,b,c) => vertex v2 is adjacent to vertex v and b items of vertex v give c items of vertex v2 returned
  def adjacent(n:Int,itemmap:List[(Int,Int,Int,Int)]):List[(Int,Int,Int)]={
    itemmap match {
      case Nil => return Nil
      case (i,a,b,c)::tail => if(i==n) return List((a,b,c)):::adjacent(n,tail)
      else return adjacent(n,tail)
    }
  }

  def dfs (itemmap:List[(Int,Int,Int,Int)],destination:Int,conversion:(Int,Int),visitedvertices:List[Int],currentvertex:Int):(Int,Int) ={
    if(currentvertex==destination)
      return conversion

    val newvisitedvertices = visitedvertices:::List(currentvertex)

    val adjacencylist = adjacent(currentvertex,itemmap)

    val conversiondfs: (Int,Int) = forloop(adjacencylist, itemmap, destination, conversion, newvisitedvertices)

    if (conversiondfs._1 != -1)
      return conversiondfs

    return (-1,-1) //a default return statement which returns (-1,-1) meaning dfs from the currentvertex has not led to the destination
  }

  //forloop loops through all the adjacent vertices of a vertex
  def forloop(adjacencylist: List[(Int,Int,Int)],itemmap:List[(Int,Int,Int,Int)],destination:Int,conversion:(Int,Int),newvisitedvertices:List[Int]): (Int,Int) = {
    adjacencylist match {
      case (a, b, c) :: Nil =>
        if (!newvisitedvertices.contains(a)) {
          val gc: Int =gcd(conversion._1*b,conversion._2*c)
          val tmp =dfs(itemmap,destination,((conversion._1*b)/gc,(conversion._2*c)/gc),newvisitedvertices,a)
          if (tmp._1 != -1)
            return tmp
          else
            return (-1,-1)
        }
        else
          return (-1,-1)
      case (a, b, c) :: tail =>
        if (!newvisitedvertices.contains(a)) {
          val gc: Int =gcd(conversion._1*b,conversion._2*c)
          val tmp =dfs(itemmap,destination,((conversion._1*b)/gc,(conversion._2*c)/gc),newvisitedvertices,a)
          if (tmp._1 != -1)
            return tmp
          else
            return forloop(tail, itemmap, destination, conversion, newvisitedvertices)
        }
        else
          return forloop(tail, itemmap, destination, conversion, newvisitedvertices)
    }
  }

  def input (numbergivenbarterrates:Int,numberofrequiredbarterrates:Int,givenbarterList:List[String],requiredbarterlistquery:List[String]): List[String]=
  {

    val fullList=splitting(givenbarterList)
    val itemList= items(fullList).distinct
    val itemmap=itemwithvalue(fullList,itemList)


    return ( requiredbarterlistquery.foldLeft(List[String]())((b,a)=>b ::: List(finalAns(a,itemmap,itemList))))
  }

  def finalAns(query:String,itemmap:List[(Int,Int,Int,Int)],itemList:List[String]): String ={
    val query1=query.split(" +").toList
    val destinaton= itemList.indexOf(query1(3))
    val start=itemList.indexOf(query1(1))
    val answer=dfs(itemmap,destinaton,(1,1),Nil,start)
    if(answer == (-1,-1))
      return ("? " + itemList(start) + " = ? "+ itemList(destinaton))
    else
      return (answer._1 +" "+ itemList(start) + " = " + answer._2 + " "+ itemList(destinaton))
  }

  println(input(numbergivenbarterrates,numberofrequiredbarterrates,givenbarterList,requiredbarterlist))
}
