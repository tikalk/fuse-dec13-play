package models


case class SearchResult(videoId:String, thumbnailUrl:String)
case class VideoInfo(playerId:Int, videoId:String)



object JsonFormats {
  import play.api.libs.json.Json
  import play.api.data._
  import play.api.data.Forms._

  // Generates Writes and Reads for Feed and User thanks to Json Macros

  implicit val videoInfoFormat = Json.format[VideoInfo]
}