package io

import scala.io.Source
import java.io.{FileNotFoundException, IOException}


object ReadFile {
	def read(filename: String) : List[List[(Double, Double)]] = {
			var pnts: List[(Double, Double)] = List.empty;
			var loops: List[List[(Double, Double)]] = List.empty;
	  try {
		  for (line <- Source.fromFile(filename).getLines()) {
		    if(line == "") {
		      loops = pnts.reverse::loops
		      pnts = List.empty
		    } else {
			    val splt = line.split(' ')
			    pnts = (splt(0).toDouble, splt(1).toDouble)::pnts
		    }
  		}
  		return pnts.reverse::loops
  	} catch {
  	case ex: FileNotFoundException => { 
  		println("Couldn't find that file.")
	  	return List.empty
	  }
	  case ex: IOException => {
	  	println("Had an IOException trying to read that file")
	  	return List.empty
	  }
	}
	}
}