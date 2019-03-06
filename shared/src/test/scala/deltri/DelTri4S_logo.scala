package deltri

import java.awt.Desktop.getDesktop
import java.nio.file.Files
import java.util.Arrays.asList

import scala.collection.mutable.ArrayBuffer

object DelTri4S_logo
{
  def main(args: Array[String]): Unit =
  {
    val lines = Vector(
      ( Vector(1.6510000228881836, 1.915583312511444, 2.2859999239444733, 4.1169165670871735, 4.720166474580765, 4.148666471242905, 2.338916629552841, 1.9896666407585144, 1.651000052690506),
        Vector(295.50775146484375, 295.5289514642209, 295.5289514642209, 294.9045314770192, 293.17945146374404, 291.43320143036544, 290.84053140692413, 290.8511314066127, 290.8723314059898) ),

      ( Vector(6.424083232879639, 6.127749919891357, 5.281083345413208, 3.958166718482971, 2.2013334035873413, 1.1641666889190674, 0, 0, 1.185333251953125, 2.2542498111724854, 3.958166480064392, 5.281083106994629, 6.127749681472778),
        Vector(293.1794128417969, 294.840992808342, 296.0157427787781, 296.71424275636673, 296.94708275794983, 296.90478275716305, 296.756591796875, 289.61284160614014, 289.4752516001463, 289.4329515993595, 289.6446215957403, 290.3113716095686, 291.47553162276745) ),

      ( Vector(7.422389030456543, 7.644639030098915, 8.247889056801796, 9.094555631279945, 10.08938904106617, 11.962639048695564, 12.650556042790413, 12.639976043254137, 12.618806043639779, 9.04163932800293, 9.496722638607025, 10.576222360134125, 11.422889351844788, 12.110806345939636, 12.322472348809242, 11.983806356787682, 11.507556363940239, 10.957222387194633, 10.364556416869164, 9.052222713828087, 8.131472691893578, 7.591722771525383),
        Vector(294.1107482910156, 292.819588303566, 291.8882483243942, 291.32733833789825, 291.1368383318186, 291.86708833277225, 293.99433828890324, 294.3012482970953, 294.58699829876423, 294.58699829876423, 295.35958828032017, 295.6453382819891, 295.57123827934265, 295.3701582849026, 296.6507383286953, 296.7777383327484, 296.8835783302784, 296.96827833354473, 296.9999783337116, 296.77772833406925, 296.17447830736637, 295.25372828543186) ),

      ( Vector(11.126556396484375, 11.052476398646832, 10.872560404241085, 10.565643392503262, 10.11056037992239, 9.655476681888103, 9.337976686656475, 9.136893384158611, 9.041643381118774),
        Vector(293.50750732421875, 293.11592733860016, 292.7772573530674, 292.5338473469019, 292.43854735046625, 292.5338473469019, 292.76667734980583, 293.11592733860016, 293.50750732421875) ),

      ( Vector(16.08997344970703, 14.968140482902527, 14.290807485580444, 13.952140480279922, 13.867470480501652, 13.867470480501652, 15.444387532770634, 15.444387532770634, 15.476137533783913, 15.592554531991482, 15.857137523591518, 16.31222053617239),
        Vector(296.9576721191406, 296.8095021247864, 296.42850211262703, 295.82525208592415, 295.0209220945835, 288.89317217469215, 288.6391721665859, 294.7034221589565, 295.0844221711159, 295.3701721727848, 295.5606721788645, 295.65587218105793) ),

      ( Vector(22.76177215576172, 22.76177215576172, 20.54985523223877, 20.54985523223877, 18.898855209350586, 18.898855209350586, 16.686938285827637, 16.686938285827637),
        Vector(289.517578125, 290.9251581430435, 290.9251581430435, 296.8518282175064, 296.8518282175064, 290.9251581430435, 290.9251581430435, 289.517578125) ),

      ( Vector(26.33811378479004, 25.840696781873703, 25.226863771677017, 24.867029786109924, 24.560113787651062, 24.560113787651062, 22.98319673538208, 22.98319673538208, 23.97802972793579, 25.26919674873352, 25.576113760471344, 25.935946762561798, 26.295779764652252, 26.602696776390076),
        Vector(292.6820068359375, 292.5761768370867, 292.5126768350601, 292.544376835227, 292.5972768366337, 296.85177674889565, 296.85177674889565, 291.5812765657902, 291.3061065673828, 291.16852656006813, 291.18972655944526, 291.23202656023204, 291.2955265622586, 291.38022656552494) ),

      ( Vector(29.02495574951172, 27.448040008544922, 27.448040008544922, 29.024955987930298),
        Vector(296.8518371582031, 296.8518371582031, 291.2850036621094, 291.2850036621094) ),

      ( Vector(29.17312240600586, 28.88737240433693, 28.2312054336071, 27.564455419778824, 27.28928941488266, 27.564455419778824, 28.2312054336071, 28.88737240433693),
        Vector(289.67633056640625, 290.36424058675766, 290.6076605916023, 290.36424058675766, 289.67633056640625, 288.99899059534073, 288.7449905872345, 288.99899059534073) ),

      ( Vector(33.14253234863281, 32.26411533355713, 31.480949342250824, 33.14253228902817),
        Vector(291.4437561035156, 292.6185060739517, 293.9202560186386, 293.9202560186386) ),

      ( Vector(34.68769836425781, 34.68769836425781, 35.47086536884308, 35.47086536884308, 34.68769836425781, 34.68769836425781, 33.14253234863281, 33.14253234863281, 29.97811532020569, 29.97811532020569, 30.560199320316315, 31.332782328128815, 32.242949306964874, 33.1954482793808),
        Vector(289.517578125, 293.9202380180359, 293.9202380180359, 295.21140801906586, 295.21140801906586, 296.85182797908783, 296.85182797908783, 295.21140801906586, 295.21140801906586, 294.0578280687332, 293.0100780725479, 291.80357801914215, 290.58648800849915, 289.5175780057907) ),

      ( Vector(38.62850570678711, 39.20000571012497, 39.57042270898819, 39.76092271506786, 39.813842713832855, 39.475176721811295, 38.31100967526436, 37.59134265780449, 36.94575968384743, 36.48009267449379, 36.30017668008804, 36.49067668616772, 37.030426666140556, 37.87709264457226, 38.99892668426037, 40.27950970828533, 41.16850970685482, 40.69225971400738, 40.00434271991253, 39.104759737849236, 38.24750976264477, 37.993509754538536, 38.08875975757837, 38.35334274917841, 38.75550974160433, 39.26350975781679, 40.2583427503705, 40.956842727959156, 41.38017571717501, 41.51775971800089, 40.79809270054102, 38.62850580364466, 37.75008878856897, 37.05158881098032, 36.53300581127405, 36.1731728091836, 36.638838805258274, 37.44317279011011),
        Vector(295.5924072265625, 295.53950722515583, 295.38075722754, 295.1373472213745, 294.83042722940445, 294.23775720596313, 293.7191771864891, 293.4334271848202, 293.02067717909813, 292.41742715239525, 291.5495971739292, 290.63942715525627, 289.9409271776676, 289.50700718164444, 289.3482571840286, 289.50700718164444, 289.85625717043877, 291.15800711512566, 290.8828471004963, 290.7558470964432, 290.92517709732056, 291.42259711027145, 291.7612571120262, 292.01525712013245, 292.2163471132517, 292.39625711739063, 292.81959711015224, 293.2958471029997, 293.9414270967245, 294.84100709855556, 296.43909715116024, 296.9999871701002, 296.93648716807365, 296.798907160759, 296.61898715794086, 296.4390771538019, 295.1267371326685, 295.454827144742) )
    )

    val plc = {
      val nodes = lines flatMap { case (x,y) => x zip y ensuring x.length == y.length }
      var off = 0
      val offs = ArrayBuffer(0)
      val segs = ArrayBuffer.empty[(Int,Int)]
      for( len <- lines map {_._1.length} )
      {
        assert( len > 2 )

        for( i <- 1 until len )
          segs += {(off+i-1, off+i)}
        segs += {(off+len-1, off)}

        off  += len
        offs += off
      }
      val borders = for( i <- Seq(0,3,9) ) yield offs(i)+1 -> offs(i)
      PLC(nodes, segs, confinedBySegments=true, orientedBoundarySegments=borders)
    }

//    val mesh = TriMeshMutable.empty
//    val mesh = TriMeshIndexed.empty
    val mesh = TriMeshTaped(TriMeshMutable.empty)
//    val mesh = TriMeshTaped(TriMeshIndexed.empty)
    CDT.triangulate(mesh,plc)
    println(mesh.nTris)
//    val (mesh,_) = TriMeshTaped.delaunayConstrained(plc)

    val tmp = Files.createTempFile("plot_",".html")
    Files.write( tmp, asList(mesh.toHtml) )
    getDesktop.browse(tmp.toUri)
  }
}