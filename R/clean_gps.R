#' Clean your GPS removing repeated points
#'
#' @param gps Data.frame with your GPS feed, or a list with all GPS feeds
#' @return A cleaned GPS data.fra
#' @examples
#' add(1, 1)
#' add(10, 1)
#' @export
clean_gps <- function(gps) {

  gps[, vehicleid := as.numeric(V9)]
  gps[, hora := fasttime::fastPOSIXct(V4, tz="America/Fortaleza", fixed = 4)]
  gps <- setorder(gps, vehicleid, hora)
  gps[, id_gps := 1:.N, by = vehicleid]
  gps <- gps[, .(id_gps, vehicleid, hora, lon = V3, lat = V2)]

  # quick clean gps -----------------------------------------------------------------------------
  # remove points for teh same vehicle for the same time
  gps <- gps[!(hora == lag(hora) & vehicleid == lag(vehicleid))]

  # abrir stops
  stops_routes <- readRDS("data/stops_gtfs_routes.rds")
  routes_limits <- readRDS("data/routes_limits.rds")


  # 4) Fazer limpeza nos dados de GPS -----------------------------------------------------------

  #' Foram observados diversos pontos mortos de GPS na linha, o que estava prejudicando as analises
  #' Situacoes como centanas de registros de GPS de uma linha sendo identificados em uma mesma localidade
  #' Ainda nao eh possivel saber se essas concentracoes de pontos sao uma garagem, final de linha, ou
  #' qualquer outra coisa
  #' Para contornar isso, essa etapa identifica quando essas concentracoes acontecem e diminui a quantidade
  #' de pontos das concentracoes, de forma a garantir que os pontos de GPS que estao na base sejam de
  #' quando o veiculo esteja realmente em movimento


  # remove vehiles with less than 50 observations
  gps[, n := .N, by = vehicleid]
  gps <- gps[n >= 50]
  gps <- gps[, !"n"]

  # ou pode-se usar essa funcao
  dtHaversine <- function(lat_from, lon_from, lat_to, lon_to, r = 6378137){
    radians <- pi/180
    lat_to <- lat_to * radians
    lat_from <- lat_from * radians
    lon_to <- lon_to * radians
    lon_from <- lon_from * radians
    dLat <- (lat_to - lat_from)
    dLon <- (lon_to - lon_from)
    a <- (sin(dLat/2)^2) + (cos(lat_from) * cos(lat_to)) * (sin(dLon/2)^2)
    return(2 * atan2(sqrt(a), sqrt(1 - a)) * r)
  }

  # # 4.2) Estabelecer funcao para calcular dist entre um ponto e seu anterior
  # get.dist <- function(lon, lat) geosphere::distHaversine(tail(cbind(lon,lat),-1), head(cbind(lon,lat),-1))

  # 4.3) Calcular essa distancia, agrupando por veiculo
  gps_join_linha_fora1 <- copy(gps)

  # gps_join_linha_fora1[, dist := c(0, get.dist(as.numeric(lon), as.numeric(lat))), by = vehicleid]
  gps_join_linha_fora1[, `:=`(Lat_to = shift(lat, type = "lag"),
                              Lon_to = shift(lon, type = "lag")), by = vehicleid]
  gps_join_linha_fora1[, dist := dtHaversine(lat, lon, Lat_to, Lon_to)]
  # group_by(vehicleid) %>%
  # mutate(dist = c(0, get.dist(as.numeric(lon), as.numeric(lat))))
  # remove columns
  gps_join_linha_fora1 <- gps_join_linha_fora1[,  !c("Lat_to", "Lon_to")]

  #' Aqui, vamos estabelecer a distancia de 50 metros como uma distancia limite entre um ponto e seu anterior
  #' para identificar esses pontos como uma 'aglomeracao' onde o veiculo estava parado/pouco se mexendo
  #' se a distancia for maior que 50m, sera atribuido um conjunto de letras que sao diferentes
  #' se a distancia for menor que 50m, sera atribuido o numeral 1
  #' isso eh um recurso de programacao para ajudar no agrupamento desses pontos
  gps_join_linha_fora1[, oi := ifelse(dist > 20, c("a", "b", "c"), "1")]
  # mutate(oi = ifelse(dist > 20, c("a", "b", "c"), "1"))

  # agrupar esses pontos
  #' a funcao 'rleid' cria numeros que vao percorrendo o data.frame e se mantem iguais quando a coluna
  #' de referencia for igual
  #' por ex, quando a coluna de referencia estiver com os valores "a b c", a funcao vai retornar "1 2 3"
  #' porem, quando a coluna de referencia estiver com os valores "1 1 1", a funcao vai retornar "4 4 4",
  #' respeitando a sequencia que foi estabelecida
  gps_join_linha_fora1[, seq:= rleid(oi)]


  # Com as concentracoes ja identificadas, eh necessario identificar quando essas concentracoes
  # devem ser diminuidas
  # O criterio estabelecido aqui estabelece que uma concentracao com mais de 15 pontos de GPS deve ser
  # reduzida para 2 pontos, que vao representar o primeiro e o ultimo ponto da concentracao

  # Primeiramente, eh calculado o tamanho da concentracao (o tamanho de pontos dentro da concentracao)
  # A funcao 'add_count' adiciona uma nova coluna com a quantidade de cada 'seq'
  gps_join_linha_fora1 <- gps_join_linha_fora1 %>%
    add_count(seq)

  # Fazer entao o filtro para concentracoes que tenham mais de 15 pontos - essas vao ser reduzidas
  gps_join_linha_fora2 <- gps_join_linha_fora1 %>%
    # fazer o filtro de 15 pontos
    filter(n >= 10)  %>%
    # agruparar por veiculo e sequencia
    group_by(vehicleid, seq) %>%
    # a funcao 'slice' serve para extrair as observacoes por posicao - nesse caso, vamos tirar a
    # primeira (1) e a ultima (n())
    slice(1, 2, n()-1, n()) %>%
    ungroup()

  # Os pontos concentrados que foram reduzidos precisam ser colados a base original
  # Para isso, precisamos primeiro manter somente os nao-concentrados da base original
  gps_join_linha_fora1_new <- gps_join_linha_fora1 %>%
    # filtrar somente os nao-concetrados, ou seja, as concentracoes com menos de 15 pontos
    filter(n < 10) %>%
    ungroup()

  # Em seguida, juntar os nao-concentrados com os concentrados corridigos
  gps_clean <- gps_join_linha_fora1_new %>%
    # a funcao rbind faz essa juncao
    rbind(gps_join_linha_fora2) %>%
    # ordenar novamente por veiculo e hora
    arrange(vehicleid, hora) %>%
    select(-oi, -seq, -n)


}
