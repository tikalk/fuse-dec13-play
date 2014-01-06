package controllers

import scala.collection._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
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

  def player = Action {implicit requestHeader =>
    val playerId = Random.nextInt(9999)
    Ok(views.html.player(playerId))
  }

  private def youtubeSearch(search: String):Future[Response]={
    val youtubeApiUrl = "https://content.googleapis.com/youtube/v3/search"

    val request = libs.ws.WS.url(youtubeApiUrl).withQueryString(
      ("part","id"),
      ("q",search),
      ("key",apiKey),
      ("maxResults","10")
    )
    val futureGet:Future[libs.ws.Response] = request.get()
    futureGet
  }

  def extractIds(searchResponse:Response):Iterable[String]={
    val json = searchResponse.json
    val items = (json\"items").as[JsArray].value
    for {
      item <- items
      id <- (item\"id"\"videoId").asOpt[String]
    } yield id
  }

  def getDetails(ids:Iterable[String]):Future[Response]={
    val youtubeApiUrl = "https://content.googleapis.com/youtube/v3/videos"

    val request = libs.ws.WS.url(youtubeApiUrl).withQueryString(
      ("part", "snippet,contentDetails,statistics,status"),
      ("id", ids.mkString(",")),
      ("key", apiKey)
    )
    val futureGet:Future[libs.ws.Response] = request.get()
    futureGet
  }

  def parseDetails(detailsResponse:Response):Seq[SearchResult]={
    val json = detailsResponse.json
    val items = (json\"items").as[JsArray].value
    for {
      item <- items
      id <- (item\"id").asOpt[String]
    } yield{
      val snippet = item\"snippet"
      val thumbnails = (snippet\"thumbnails"\"medium"\"url").as[String]
      val title = (snippet\"title").as[String]
      val description = (snippet\"description").as[String]
      val publishedAt = (snippet\"publishedAt").as[String]
      val viewCount = (item\"statistics"\"viewCount").as[String]
      SearchResult (id, thumbnails, title, description, publishedAt, viewCount)
    }
  }

  def remote(playerId:Int, search:String="") = Action.async {requestHeader =>
    if(playersMap.contains(playerId))
      for {
        searchRes <- youtubeSearch(search)
        ids = extractIds(searchRes)
        detailsRes <- getDetails(ids)
        details = parseDetails(detailsRes)
      }
        yield Ok(views.html.remote(playerId, details))
    else {
      println("no id "+playerId)
      Future.successful(Ok(views.html.disconnected(playerId)))
    }
  }


  private def insertToDb(playerId:Int, videoId:String): Future[Boolean] = {
    val videoInfo = VideoInfo(playerId, videoId)
    // insert the user
    val futureResult = collection.insert(videoInfo)
    println("applied DB save")
    // when the insert is performed, send a OK 200 result
    futureResult.map(_.ok)
  }

  def remotePlay(playerId: Int, videoId: String, thumbnailUrl: String) = Action.async {
    playersMap.get(playerId).map { playerInfo =>
        playerInfo.channel.push(videoId)

        insertToDb(playerId, videoId).map {
          case true =>
            println("applied DB save success")
            ""
          case false =>
            println("Faled DB save")
            "Warning: DB error, results not saved"
        } map (errorMsg => Ok(views.html.remotePlay(playerId, videoId, thumbnailUrl, errorMsg)))

    } getOrElse {
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

    request.get().map(response => Ok(response.json))
  }
}