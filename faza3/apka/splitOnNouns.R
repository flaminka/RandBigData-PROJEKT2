#' Funkcja oddzielaj?ca s?owa 
#' 
#' Funkcja przekszta?ca wej?ciow? ramk? danych na dane z oddzielonymi poszczegulnymi s?owami z kolumny rzeczowniki w kolumnie o tej samej nazwie
#' 
#' @param dane - ramka danych do przekszta?cenia
#' 
#' @import dplyr
#' 

splitOnNouns<- function(dane){
  lista=lapply(1:nrow(dane), function(i){
      cat("\r", i)
    tmp=unlist(strsplit(as.character(dane$rzeczowniki[i]), split="|", fixed = T))%>%unique()
    ile=length(tmp)
    data.frame(id=as.character(rep(dane[i,2], ile)), parent_id=as.character(rep(dane[i,3], ile)),
               created_at=as.character(rep(dane[i,4], ile)), tread=as.character(rep(dane[i,5], ile)),    
               user_name=as.character(rep(dane[i,7], ile)), body=as.character(rep(dane[i,10], ile)), rzeczownik=tmp)
  })
  nowe=do.call("rbind", lista)
  a <- "a, aby, ach, acz, aczkolwiek, aj, albo, ale, ale?, ani, a?, bardziej, bardzo, bo, bowiem, by, byli, bynajmniej, by?, by?, by?a, by?o, by?y, b?dzie, b?d?, cali, ca?a, ca?y, ci, ci?, ciebie, co, cokolwiek, co?, czasami, czasem, czemu, czy, czyli, daleko, dla, dlaczego, dlatego, do, dobrze, dok?d, do??, du?o, dwa, dwaj, dwie, dwoje, dzi?, dzisiaj, gdy, gdyby, gdy?, gdzie, gdziekolwiek, gdzie?, i, ich, ile, im, inna, inne, inny, innych, i?, ja, j?, jak, jaka?, jakby, jaki, jakich?, jakie, jaki?, jaki?, jakkolwiek, jako, jako?, je, jeden, jedna, jedno, jednak, jednak?e, jego, jej, jemu, jest, jestem, jeszcze, je?li, je?eli, ju?, j?, ka?dy, kiedy, kilka, kim?, kto, ktokolwiek, kto?, kt?ra, kt?re, kt?rego, kt?rej, kt?ry, kt?rych, kt?rym, kt?rzy, ku, lat, lecz, lub, ma, maj?, ma?o, mam, mi, mimo, mi?dzy, mn?, mnie, mog?, moi, moim, moja, moje, mo?e, mo?liwe, mo?na, m?j, mu, musi, my, na, nad, nam, nami, nas, nasi, nasz, nasza, nasze, naszego, naszych, natomiast, natychmiast, nawet, ni?, nic, nich, nie, niech, niego, niej, niemu, nigdy, nim, nimi, ni?, no, o, obok, od, oko?o, on, ona, one, oni, ono, oraz, oto, owszem, pan, pana, pani, po, pod, podczas, pomimo, ponad, poniewa?, powinien, powinna, powinni, powinno, poza, prawie, przecie?, przed, przede, przedtem, przez, przy, roku, r?wnie?, sama, s?, si?, sk?d, sobie, sob?, spos?b, swoje, ta, tak, taka, taki, takie, tak?e, tam, te, tego, tej, temu, ten, teraz, te?, to, tob?, tobie, tote?, trzeba, tu, tutaj, twoi, twoim, twoja, twoje, twym, tw?j, ty, tych, tylko, tym, u, w, wam, wami, was, wasz, wasza, wasze, we, wed?ug, wiele, wielu, wi?c, wi?cej, wszyscy, wszystkich, wszystkie, wszystkim, wszystko, wtedy, wy, w?a?nie, z, za, zapewne, zawsze, ze, z?, znowu, zn?w, zosta?, ?aden, ?adna, ?adne, ?adnych, ?e, ?eby"
  stopwordsy <- unlist(strsplit(a, split=", ", fixed=T))
  stopwordsy <- c(stopwordsy, "go", "sam", "bez", "zl", "tamto", "raz", "?e", "sa")
  nowe%>%filter(!rzeczownik %in% stopwordsy)
}