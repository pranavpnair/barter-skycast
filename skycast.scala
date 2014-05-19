/**
 * Created by pranav on 19/5/14.
 */
class skycast {

  val k=1
  val n=10000
  val l2=List(3 ,2, 3, 9999)
  val l5=List(10000 ,1, 4, 9998)

  def numberOfDigits(number: Int) : Int = {
    if(number < 10 )
      return 1
    else
      return 1 + numberOfDigits(number/10)
  }

  def shortestpathewithoutback (presentchannel: Int, nextchannel: Int, channelList: List[Int] ) : Int = {

    val indexofchannel1 = channelList.indexOf( presentchannel )
    val indexofchannel2 = channelList.indexOf( nextchannel )

    val directPathLength = math.abs( indexofchannel2 - indexofchannel1 )
    val cyclicPathLength = channelList.length - directPathLength

    val pathusingnumberbutton = numberOfDigits ( nextchannel )

    return math.min( directPathLength, math.min (cyclicPathLength , pathusingnumberbutton) )
  }

  def bestPath(previousChannel:Int,presentChannel:Int,nextChannel:Int,channelList:List[Int]):Int={

    val usingBack = 1 + shortestpathewithoutback ( previousChannel, nextChannel, channelList )
    val withoutBack = shortestpathewithoutback ( presentChannel, nextChannel, channelList )

    return math.min(usingBack, withoutBack )
  }

  def minClicks(channelList:List[Int],traversalList:List[Int],presentChannel:Int,indexOfPresentChannel:Int):Int={
    if(indexOfPresentChannel==traversalList.length-1)
      return 0

    val nextChannel=traversalList(indexOfPresentChannel+1)

    if(indexOfPresentChannel==0 )
    {
      return shortestpathewithoutback ( presentChannel, nextChannel, channelList ) + minClicks (channelList, traversalList,nextChannel, indexOfPresentChannel + 1 )
    }
    else
    {
      val previousChannel = traversalList ( indexOfPresentChannel - 1 )

      return bestPath (previousChannel, presentChannel, nextChannel, channelList ) + minClicks (channelList, traversalList, nextChannel, indexOfPresentChannel + 1 )
    }
  }

  def Input(lowestchannel:Int, highestchannel:Int, numberofblockedchannels:Int, blockedchannels:List[Int], numberoftraversalchannels:Int, traversalchannels:List[Int]): Int =
  {
    val channellistwithoutblockedchannels = ((lowestchannel to highestchannel).toList)
    val channellistwithblockedchannels =channellistwithoutblockedchannels diff blockedchannels

    val minimumnumberofclicks = numberOfDigits(traversalchannels(0)) + minClicks(channellistwithblockedchannels,traversalchannels,traversalchannels(0),0)

    return minimumnumberofclicks
  }

  println(Input(k,n,8,l2,8,l5))

}
