
#' CODIFICAR SOLAPAS DE  CADA TIPO DE PROYECTO
#'
#' Esta funcion esta creada para codificar la solapa de SC de una UG municipal
#' Requiere algunos pre requisitos: 
#' Debemos tener cargados algunos objetos previo a la utilización de esta funcion:
#' 
#' INDEC: la solapa con los códigos del INDEC
#' EXP: Listado de las UG con sus nombre y nro de expedientes
#' TABlA : La tabla con los códigos de tipo de proyectos y Ejes
#' CODISC : Solapa de proyectos socio comunitarios
#' 
#' @param 1 Los parametros a utilizar son los siguientes: MUNI = MUNICIPIO (en mayuscula), 
#' NPRO = numero de proyecto
#' @keywords Codificacion, Potenciar trabajo
#' @export
#' @examples
#' CODIFICAR(MORENO, 1) 
#' 
#' 
#' 
#' 
#' 



CODIFICADOSCM <- function(MUNI, NPRO) {
  
  EXP1 <- EXP %>% 
    filter(MUNICIPIO == 'MUNI')
  
  EXP2 <- EXP %>% 
    select(3,5)
  
  INDEC1 <- INDEC %>% 
    select(-(3:21)) %>% 
    mutate('PROV - DEP' = prov_depto) %>% 
    select(-(1))
  
  PRO1 <- CODISC %>%
    rename(N.DE.PROYECTO = 1) %>%
    rename(NOMBRE.GENERICO.DEL.PROYECTO = 4) %>% 
    fill(N.DE.PROYECTO, .direction = "down") %>% 
    filter(N.DE.PROYECTO == NPRO) %>% 
    rename(MUNICIPIO = 12)
  
  PRO1 <- PRO1 %>% 
    unite('PROV - DEP', c(13,12), sep = " ", remove = FALSE, na.rm = TRUE)
  
  PRO1BB <- PRO1[-1,] %>% 
    mutate('N.de.Expediente' = '')
  
  PRO1B <- PRO1 %>% 
    left_join(., INDEC1, by = 'PROV - DEP') %>% 
    slice(1)
  
  TABLASC <- TABLA %>% 
    slice(7:13) %>% 
    select(19:20) %>% 
    rename(EJE = 'PROYECTOS.SOCIO-PRODUCTIVOS') %>% 
    rename(EJES = 'Columna21')
  
  PRO1B <- PRO1B %>% 
    left_join(., TABLASC, by = 'EJE') %>% 
    left_join(., EXP2, by = 'MUNICIPIO') %>% 
    mutate(PROGRAMA = 2) %>% 
    mutate('TIPO DE ORGANIZACION' = 2) %>% 
    mutate('TIPO PROYECTO' = 2) %>% 
    unite('ID.DE.PROYECTO', c(21,24,25,26,22,1), sep = "-", remove = FALSE, na.rm = TRUE)
  
  
  PRO1A <-   bind_rows(PRO1B, PRO1BB)
  
  PRO1A <- PRO1A %>% 
    mutate(G1 = ID.DE.PROYECTO,
           G2 = ID.DE.PROYECTO) %>% 
    fill(G1, .direction = "down") %>% 
    fill(G2, .direction = "down")
  
  PRO1AT <- PRO1A [, c(2, 25, 24, 22, 13, 1, 26, 27, 23,3,
                       5, 4, 6, 7, 8 , 28 , 9, 10, 11, 12, 
                       14, 15, 29, 16, 17, 18, 19, 20, 21)]
  
  PRO1ATT <-  PRO1AT[!is.na(PRO1A$COSTO.UNITARIO.TOTAL),]
}







