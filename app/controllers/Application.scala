package controllers


import play.api._
import play.api.mvc._
import scala.collection._
import scala.util.Random
import play.api.libs.iteratee.Iteratee
import play.api.libs.iteratee.Concurrent._
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import play.api.libs.json._
import models.SearchResult
import ExecutionContext.Implicits.global

object Application extends Controller {
  case class PlayerInfo(playerId:Int, channel:Channel[String])
  
  val playersMap:concurrent.Map[Int, PlayerInfo] = new concurrent.TrieMap()

  def playerWebSocket(playerId:Int) = WebSocket.using[String] { header =>
    
    val (enumerator, channel) = broadcast[String]
    val myPlayerInfo = PlayerInfo(playerId, channel)
    
    val in = Iteratee.foreach[String]{s =>
      println(s) //for debug only, client is not expected to send anything
    } map { _ =>
      println("player disconnected: "+playerId)
      playersMap.remove(playerId)
      channel.eofAndEnd
    }

    if(playersMap.putIfAbsent(playerId, myPlayerInfo).isDefined) {
      channel.eofAndEnd
      throw new IllegalStateException("Duplicate player ID")
    }
    
    (in, enumerator)
  }
  
  def index = Action {
    Ok(views.html.index())
  }

  private def printMyIp = {
    import java.net._;

    val thisIp = InetAddress.getLocalHost();
    println("My IP:"+thisIp.getHostAddress());
  }
  def player = Action {implicit requestHeader =>
    printMyIp
    val playerId = Random.nextInt(9999)
    Ok(views.html.player(playerId))
  }
  
  def remote(playerId:Int, search:String="") = Action {requestHeader =>
    printMyIp
    if(playersMap.contains(playerId)) {
      val results = youtubeSearch(search, playerId)
      Ok(views.html.remote(playerId, results))
    } else {
      println("no id "+playerId)
      Ok(views.html.disconnected(playerId))
    }
  }
  
  private def parse(search : String) : String =
  {
    //println(search)
    val pattern = ".*watch\\?v=(\\w*)$".r ;
    val res : String = search match {
        case pattern(group) => group
        case _ => ""
    }
    
    //println(res)
    return res;
  }
  
  private def youtubeSearch(search: String, playerId: Int): Iterable[SearchResult] = {
    if (search.isEmpty())
      return Nil

    // if user pastes full URL from YouTube, play it
    if ((search.startsWith("http://www.youtube.com")) || (search.startsWith("www.youtube.com"))) {
      
      // parse 'search' to retrieve the videoId
      val videoId : String = parse(search);	
      
      playersMap.get(playerId) match {
        case Some(playerInfo) =>
          playerInfo.channel.push(videoId);
        case _ =>
          println("no id " + playerId)
      }
      return Nil
    }

    // use youtube API to search    val apiKey = "AIzaSyBL6PS3qcjaI4KSCrysejNsFHNQkHtXShs"
    val apiKey = "AIzaSyBL6PS3qcjaI4KSCrysejNsFHNQkHtXShs"
    val youtubeApiUrl = "https://content.googleapis.com/youtube/v3/search"
    val request = libs.ws.WS.url(youtubeApiUrl).withQueryString(
            ("part","snippet"),
            ("q",search),
            ("key",apiKey),
            ("maxResults","10")            
        )
    val futureGet:Future[libs.ws.Response] = request.get()
    val getResponse = Await.result(futureGet, 10 seconds)
    
    val body = getResponse.body //handy for debugging
    
    val json = getResponse.json
    val items = (json\"items").as[JsArray].value
    for {item <- items
         _ = item if (item\"id"\"videoId").asOpt[String].nonEmpty
      } yield {
          val id = (item\"id"\"videoId").as[String]
          val thumbnails = (item\"snippet"\"thumbnails"\"medium"\"url").as[String]
          SearchResult ( id  , thumbnails)
      }
  }
  
  def remotePlay(playerId:Int, videoId:String, thumbnailUrl:String) = Action {
    playersMap.get(playerId) match {
      case Some(playerInfo) =>
        playerInfo.channel.push(videoId)
        Ok(views.html.remotePlay(playerId, videoId, thumbnailUrl))
      case _ =>
        println("no id "+playerId)
        Ok(views.html.disconnected(playerId))
    }
  }
  
  def connect = Action {
    Ok(views.html.connect())
  }
  
}