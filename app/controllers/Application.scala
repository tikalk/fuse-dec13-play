package controllers


import play.api._
import play.api.mvc._
import scala.collection._
import scala.util.Random
import play.api.libs.iteratee.Iteratee
import play.api.libs.iteratee.Enumerator
import play.api.libs.iteratee.Concurrent._
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import play.api.libs.json._
import models.SearchResult

object Application extends Controller {
  case class PlayerInfo(playerId:Int, channel:Channel[String])
  
  val playersMap:concurrent.Map[Int, PlayerInfo] = new concurrent.TrieMap()

  def playerWebSocket(playerId:Int) = WebSocket.using[String] { header =>
    
    val in = Iteratee.foreach[String]{s =>
      println(s) //for debug only, client is not expected to send anything
    } mapDone { _ =>
      println("player disconnected: "+playerId)
      playersMap.remove(playerId)
    }

    val (enumerator, channel) = broadcast[String]
    val myPlayerInfo = PlayerInfo(playerId, channel)
    
    if(playersMap.putIfAbsent(playerId, myPlayerInfo).isDefined)
      throw new IllegalStateException("Duplicate player ID")
    
    (in, enumerator)
  }
  
  def index = Action {
    Ok(views.html.index())
  }
  
  def player = Action {implicit requestHeader =>
    val playerId = Random.nextInt(9999)
    Ok(views.html.player(playerId))
  }
  
  def remote(playerId:Int, search:String="") = Action {requestHeader =>
    if(playersMap.contains(playerId)) {
      val results = youtubeSearch(search)
      Ok(views.html.remote(playerId, results))
    } else {
      println("no id "+playerId)
      Ok(views.html.disconnected(playerId))
    }
  }
  
  private def youtubeSearch(search:String):Iterable[SearchResult] = {
    if (search.isEmpty())
      return Nil
    import ExecutionContext.Implicits.global
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
    for(item <- items)
      yield {
        //println(item.toString)
        SearchResult (
          (item\"id"\"videoId").as[String],
          (item\"snippet"\"thumbnails"\"medium"\"url").as[String]
        )
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