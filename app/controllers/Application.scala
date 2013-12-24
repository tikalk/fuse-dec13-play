package controllers

import scala.collection._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Random

import models._
import models.JsonFormats._
import play.api._
import play.api.libs.iteratee.Concurrent._
import play.api.libs.iteratee.Iteratee
import play.api.mvc._
import play.modules.reactivemongo.MongoController
import play.modules.reactivemongo.json.collection.JSONCollection

import play.api.libs.json._
import models.SearchResult
import ExecutionContext.Implicits.global
import play.api.libs.ws.Response

object Application extends Controller with MongoController {
  def collection: JSONCollection = db.collection[JSONCollection]("videos")
  case class PlayerInfo(playerId:Int, channel:Channel[String])

  val apiKey = "AIzaSyBL6PS3qcjaI4KSCrysejNsFHNQkHtXShs"

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

  }
  def player = Action {implicit requestHeader =>
    printMyIp
    val playerId = Random.nextInt(9999)
    Ok(views.html.player(playerId))
  }

  def parseResponse(items: Seq[JsValue]):Seq[models.SearchResult]={

    for {item <- items
         _ = item if (item\"id"\"videoId").asOpt[String].nonEmpty
    } yield {
      val id = (item\"id"\"videoId").as[String]
      val thumbnails = (item\"snippet"\"thumbnails"\"medium"\"url").as[String]
      SearchResult ( id  , thumbnails)
    }
  }

  def remote(playerId:Int, search:String="") = Action.async {requestHeader =>
    printMyIp
    if(playersMap.contains(playerId)) {
      val resultsFuture = youtubeSearch(search)
      val timeoutFuture = play.api.libs.concurrent.Promise.timeout(0,  Duration(3000, MILLISECONDS))
      Future.firstCompletedOf(Seq(resultsFuture, timeoutFuture)).map {
        case response: Response =>{
          val json = response.json
          val items = (json\"items").as[JsArray].value

          Ok(views.html.remote(playerId, parseResponse(items)))

        }
        case i: Int => InternalServerError("Oooppppps!!!")
      }

    } else {
      println("no id "+playerId)
      Future.successful(Ok(views.html.disconnected(playerId)))
    }
  }

  private def youtubeSearch(search: String):Future[Response]={

    val youtubeApiUrl = "https://content.googleapis.com/youtube/v3/search"

    val request = libs.ws.WS.url(youtubeApiUrl).withQueryString(
      ("part","snippet"),
      ("q",search),
      ("key",apiKey),
      ("maxResults","10")
    )
    val futureGet:Future[libs.ws.Response] = request.get()
    futureGet
  }

  private def insertToDb(playerId:Int, videoId:String): Future[Boolean] = {
    val videoInfo = VideoInfo(playerId, videoId)
    // insert the user
    val futureResult = collection.insert(videoInfo)
    println("applied DB save")
    // when the insert is performed, send a OK 200 result
    futureResult.map(a => if(a.ok) true else false)
  }

  def remotePlay(playerId: Int, videoId: String, thumbnailUrl: String) = Action.async {
    playersMap.get(playerId) match {
      case Some(playerInfo) =>
        playerInfo.channel.push(videoId)
        val fRes: Future[Boolean] = insertToDb(playerId, videoId)
        fRes.map(b => if (b) {
          println("applied DB save success")
          Ok(views.html.remotePlay(playerId, videoId, thumbnailUrl))
        } else {
          println("Faled DB save")
          Ok(views.html.remotePlay(-1, "ERROR", "ERROR"))
        })

      case _ =>
        println("no id " + playerId)
        Future.successful(Ok(views.html.disconnected(playerId)))
    }
  }

  def connect = Action {
    Ok(views.html.connect())
  }

  def autocomplete(search: String) = Action.async {
    val youtubeApiUrl = "http://suggestqueries.google.com/complete/search"

    val request = libs.ws.WS.url(youtubeApiUrl).withQueryString(
      ("hl","en"),
      ("ds","yt"),
      ("client","youtube"),
      ("hjson","t"),
      ("cp","1"),
      ("format","5"),
      ("alt","json"),
      ("callback","?"),
      ("q",search),
      ("key",apiKey),
      ("maxResults","10")
    )

    val futureGet:Future[libs.ws.Response] = request.get()
    val resultsFuture = futureGet
    val timeoutFuture = play.api.libs.concurrent.Promise.timeout(0,  Duration(3000, MILLISECONDS))
    Future.firstCompletedOf(Seq(resultsFuture, timeoutFuture)).map {
      case response: Response =>{
        val json = response.json
        Ok(json)
      }
      case i: Int => InternalServerError("Oooppppps!!!")
    }
  }
}