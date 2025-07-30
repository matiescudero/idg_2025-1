# R/conexion_db.R

#' Conectar a base de datos PostgreSQL
#'
#' Esta función crea una conexión a una base de datos PostgreSQL usando RPostgres.
#'
#' @param dbname Nombre de la base (por ejemplo, "bd_censo").
#' @param host Dirección del host (por defecto "localhost").
#' @param port Puerto de conexión (por defecto 5434).
#' @param user Usuario de la base de datos (por defecto "postgres").
#' @param password Contraseña del usuario (por defecto "postgres").
#'
#' @return Objeto de conexión.
#' @export

conectar_db <- function(dbname,
                        host = "localhost",
                        port = 5434,
                        user = "postgres",
                        password = "postgres") {
  
  con <- RPostgres::dbConnect(
    RPostgres::Postgres(),
    dbname = dbname,
    host = host,
    port = port,
    user = user,
    password = password
  )
  
  return(con)
}
