/* This file is part of DelTri4S.
 *
 * DelTri4S is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * DelTri4S is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with DelTri4S.  If not, see <https://www.gnu.org/licenses/>.
 */

import org.scalajs.jsenv.nodejs.NodeJSEnv
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

// TO TEST RUN
//   - deltriJVM/test
//   - deltriJS/test

lazy val deltri = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in( file(".") )
  .settings(
     name := "deltri4s",
     version := "0.1.1",
     scalaVersion := "2.12.8",
     scalacOptions ++= Seq(
       "-feature",
       "-deprecation"
     ),

     testFrameworks += new TestFramework("utest.runner.Framework"),

     libraryDependencies ++= Seq(
        "com.lihaoyi"   %%% "utest" % "0.6.6" % "test",
        "org.typelevel" %%% "spire" % "0.16.0"
     )
   )
  .jsSettings(
     jsEnv := new NodeJSEnv( NodeJSEnv.Config().withArgs("--max_old_space_size=6144" :: Nil) ),
     scalaJSUseMainModuleInitializer := true,
//     scalaJSUseMainModuleInitializer in Test := true, // <- enable this before using test:runMain
//     scalaJSStage := FastOptStage,
     scalaJSStage := FullOptStage,
     scalaJSOptimizerOptions ~= (
       _ withDisableOptimizer true // <- necessary until fix of https://github.com/scala-js/scala-js/issues/3575
     )
   )
  .jvmSettings(
   )

lazy val deltriJS = deltri.js
lazy val deltriJVM= deltri.jvm

//lazy val deltriBuild = project.in( file(".") )
//  .aggregate(deltriJS, deltriJVM)
//  .settings()
