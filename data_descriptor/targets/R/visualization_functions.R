library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)


pre_r_steps <-
  function() {
    char_vector <- c("Manual data download")
  
    names(char_vector) <- paste0("Step ", 1:(length(char_vector)))
    
    char_vector
  }

r_steps <-
  function() {
    char_vector <- 
      c(
      "Data import",
      "Study area definition",
      "Preprocessing in R",
      "Intitiate PostgreSQL steps",
      "Rasterization of study area",
      "Intitiate GRASS GIS steps"
    )
    
    names(char_vector) <- paste0("Step ", 2:(length(char_vector)+1))
    
    char_vector
  }

postgis_steps <-
  function() {
    char_vector <- c(
      "Linemerge",
      "Lines per hydrologic order"
      )
    
    relation_node <- 5
    names(char_vector) <- paste0("Step ", relation_node, ".", 1:(length(char_vector)))
    
    char_vector
  }

grassgis_steps <-
  function() {
    char_vector <- c(
      "Calculaction of EU-MOHP measures",
      "Export raster images to disk")
    
    relation_node <- 7
    names(char_vector) <- paste0("Step ", relation_node, ".", 1:(length(char_vector)))
    
    char_vector
  }

make_workflow_diagram <-
  function(path) {

    path %>% 
      dirname() %>% 
      fs::dir_create()
    
    diagramm <- DiagrammeR::grViz("
    digraph {
  graph [compound = true, nodesep = .5, ranksep = .25,
         color = crimson, outputorder=edgesfirst]

  node [shape = box, style = 'filled', fillcolor = white, fontname = Bahnschrift, 
        fontsize = 10, fontcolor = darkgray,
        shape = rectangle,
        color = darkslategray, fixedwidth = true, width = 2]

  edge [color = grey, arrowhead = none, arrowtail = none]
  

    subgraph cluster0 {
    style='rounded,filled';
    color=Gainsboro;
    fontsize=11;
    node [style=filled,color=white]
    label = 'R'
    '@@2-1' -> '@@2-2' -> '@@2-3' -> '@@2-4' -> '@@2-5' -> '@@2-6'
    }

    subgraph cluster1 {
    style='rounded,filled';
    color='#31648C';
    fontcolor='white';
    fontsize=11;
    node [style=filled,color=white]
    label = 'PostgreSQL + PostGIS'
    '@@3-1' -> '@@3-2'
    }

    subgraph cluster2 {
    style='rounded,filled';
    color='#0B8B36';
    fontcolor='white';
    fontsize=11;
    node [style=filled,color=white, width = 3]
    label = 'GRASS GIS'
    '@@4-1' -> '@@4-2'
    }


  '@@1' -> '@@2-1'        [lhead = cluster0]
  '@@2-4' -> '@@3-1'        [ltail = cluster0, lhead = cluster1]
  '@@2-6' -> '@@4'        [ltail = cluster0, lhead = cluster2]
  '@@3-2' -> '@@4'        [ltail = cluster1, lhead = cluster2]
  
  }
      [1]: paste0(names(pre_r_steps()), '\\n ', pre_r_steps())
      [2]: paste0(names(r_steps()), '\\n ', r_steps())                        
      [3]: paste0(names(postgis_steps()), '\\n ', postgis_steps())                        
      [4]: paste0(names(grassgis_steps()), '\\n ', grassgis_steps())                        
                                  ",
      height = 500
    )

    tmp <- capture.output(rsvg_pdf(charToRaw(export_svg(diagramm)), path))
    # cat("![Standards QA flowchart](stnds.qa.png){#fig:stnds.qa.flow}\n\n")
  }

# make_workflow_diagram("data_descriptor/test.pdf")