#' Funkcja oddzielaj¹ca s³owa 
#' 
#' Funkcja przekszta³ca wejœciow¹ ramkê danych na dane z oddzielonymi poszczegulnymi s³owami z kolumny rzeczowniki w kolumnie o tej samej nazwie
#' 
#' @param dane - ramka danych do przekszta³cenia
#' 
#' @import dplyr
#' 
#' 

splitOnNouns<- function(dane){
  lista=lapply(1:nrow(dane), function(i){
    tmp=unlist(strsplit(as.character(dane$rzeczowniki[i]), split="|", fixed = T))%>%unique()
    ile=length(tmp)
    data.frame(id=as.character(rep(dane[i,2], ile)), parent_id=as.character(rep(dane[i,3], ile)),
               created_at=as.character(rep(dane[i,4], ile)), tread=as.character(rep(dane[i,5], ile)),    
               user_name=as.character(rep(dane[i,7], ile)), body=as.character(rep(dane[i,10], ile)), rzeczownik=tmp)
  })
  nowe=do.call("rbind", lista)
  a <- "a, aby, ach, acz, aczkolwiek, aj, albo, ale, ale¿, ani, a¿, bardziej, bardzo, bo, bowiem, by, byli, bynajmniej, byæ, by³, by³a, by³o, by³y, bêdzie, bêd¹, cali, ca³a, ca³y, ci, ciê, ciebie, co, cokolwiek, coœ, czasami, czasem, czemu, czy, czyli, daleko, dla, dlaczego, dlatego, do, dobrze, dok¹d, doœæ, du¿o, dwa, dwaj, dwie, dwoje, dziœ, dzisiaj, gdy, gdyby, gdy¿, gdzie, gdziekolwiek, gdzieœ, i, ich, ile, im, inna, inne, inny, innych, i¿, ja, j¹, jak, jakaœ, jakby, jaki, jakichœ, jakie, jakiœ, jaki¿, jakkolwiek, jako, jakoœ, je, jeden, jedna, jedno, jednak, jednak¿e, jego, jej, jemu, jest, jestem, jeszcze, jeœli, je¿eli, ju¿, j¹, ka¿dy, kiedy, kilka, kimœ, kto, ktokolwiek, ktoœ, która, które, którego, której, który, których, którym, którzy, ku, lat, lecz, lub, ma, maj¹, ma³o, mam, mi, mimo, miêdzy, mn¹, mnie, mog¹, moi, moim, moja, moje, mo¿e, mo¿liwe, mo¿na, mój, mu, musi, my, na, nad, nam, nami, nas, nasi, nasz, nasza, nasze, naszego, naszych, natomiast, natychmiast, nawet, ni¹, nic, nich, nie, niech, niego, niej, niemu, nigdy, nim, nimi, ni¿, no, o, obok, od, oko³o, on, ona, one, oni, ono, oraz, oto, owszem, pan, pana, pani, po, pod, podczas, pomimo, ponad, poniewa¿, powinien, powinna, powinni, powinno, poza, prawie, przecie¿, przed, przede, przedtem, przez, przy, roku, równie¿, sama, s¹, siê, sk¹d, sobie, sob¹, sposób, swoje, ta, tak, taka, taki, takie, tak¿e, tam, te, tego, tej, temu, ten, teraz, te¿, to, tob¹, tobie, tote¿, trzeba, tu, tutaj, twoi, twoim, twoja, twoje, twym, twój, ty, tych, tylko, tym, u, w, wam, wami, was, wasz, wasza, wasze, we, wed³ug, wiele, wielu, wiêc, wiêcej, wszyscy, wszystkich, wszystkie, wszystkim, wszystko, wtedy, wy, w³aœnie, z, za, zapewne, zawsze, ze, z³, znowu, znów, zosta³, ¿aden, ¿adna, ¿adne, ¿adnych, ¿e, ¿eby"
  stopwordsy <- unlist(strsplit(a, split=", ", fixed=T))
  stopwordsy <- c(stopwordsy, "go", "sam", "bez", "zl", "tamto", "raz", "¿e", "sa")
  nowe%>%filter(!rzeczownik %in% stopwordsy)
}